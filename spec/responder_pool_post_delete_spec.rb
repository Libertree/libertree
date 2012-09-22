require 'spec_helper'

describe Libertree::Server::Responder::PoolPost do
  describe 'rsp_pool_post_delete' do
    include_context 'with an INTRODUCEd and AUTHENTICATEd requester'

    context 'and the responder has a record of the member, the pool and the post' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        @post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for(:post, member_id: @member.id)
        )
        @pool = Libertree::Model::Pool.create(
          FactoryGirl.attributes_for(:pool, member_id: @member.id, sprung: true)
        )
      end

      it 'and a parameter is missing or blank, it responds with MISSING PARAMETER' do
        h = {
          'username'   => @member.username,
          'pool_id'    => @pool.remote_id,
          'post_id'    => @post.remote_id,
          'public_key' => @requester.public_key,
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          @s.process "POOL-POST-DELETE #{h_.to_json}"
          @s.should have_responded_with_code('MISSING PARAMETER')

          h_ = h.dup
          h_[key] = ''
          @s.process "POOL-POST-DELETE #{h_.to_json}"
          @s.should have_responded_with_code('MISSING PARAMETER')
        end
      end

      it "with a member username that isn't found, it responds with NOT FOUND" do
        h = {
          'username'   => 'nosuchusername',
          'pool_id'    => 99999999,
          'post_id'    => @post.remote_id,
          'public_key' => @requester.public_key,
        }
        @s.process "POOL-POST-DELETE #{h.to_json}"
        @s.should have_responded_with_code('NOT FOUND')
      end

      it "with a pool id that isn't found, it responds with NOT FOUND" do
        h = {
          'username'   => @member.username,
          'pool_id'    => 99999999,
          'post_id'    => @post.remote_id,
          'public_key' => @requester.public_key,
        }
        @s.process "POOL-POST-DELETE #{h.to_json}"
        @s.should have_responded_with_code('NOT FOUND')
      end

      it "with a post id that isn't found, it responds with NOT FOUND" do
        h = {
          'username'   => @member.username,
          'pool_id'    => @pool.remote_id,
          'post_id'    => 99999999,
          'public_key' => @requester.public_key,
        }
        @s.process "POOL-POST-DELETE #{h.to_json}"
        @s.should have_responded_with_code('NOT FOUND')
      end

      # For now, deletion silently fails in this case.

      # it 'with valid and recognized IDs, but no pool association, it responds with NOT FOUND' do
        # h = {
          # 'username'   => @member.username,
          # 'pool_id'    => @pool.remote_id,
          # 'post_id'    => @post.remote_id,
          # 'public_key' => @requester.public_key,
        # }
        # @s.process "POOL-POST-DELETE #{h.to_json}"
        # @s.should have_responded_with_code('NOT FOUND')
      # end

      context 'with valid data and a known association' do
        before :each do
          @pool << @post
          @h = {
            'username'   => @member.username,
            'pool_id'    => @pool.remote_id,
            'post_id'    => @post.remote_id,
            'public_key' => @requester.public_key,
          }
        end

        it 'responds with OK and dissociates the post from the pool' do
          @pool.includes?(@post).should be_true
          @s.process "POOL-POST-DELETE #{@h.to_json}"
          @s.should have_responded_with_code('OK')
          @pool.dirty.includes?(@post).should be_false
        end
      end
    end
  end
end
