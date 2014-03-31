require 'spec_helper'

describe Libertree::Server::Responder::PoolPost do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::PoolPost
    }
  end

  describe 'rsp_pool_post_delete' do
    include_context 'requester in a forest'

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
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameterError when a parameter is missing or blank' do
        h = {
          'username' => @member.username,
          'pool_id'  => @pool.remote_id,
          'post_id'  => @post.remote_id,
          'origin'   => @requester.domain,
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          expect { subject.rsp_pool_post_delete(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )

          h_ = h.dup
          h_[key] = ''
          expect { subject.rsp_pool_post_delete(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )
        end
      end

      it "raises NotFoundError with a member username that isn't found" do
        h = {
          'username' => 'nosuchusername',
          'pool_id'  => 99999999,
          'post_id'  => @post.remote_id,
          'origin'   => @requester.domain,
        }
        expect { subject.rsp_pool_post_delete(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      it "raises NotFoundError with a pool id that isn't found" do
        h = {
          'username' => @member.username,
          'pool_id'  => 99999999,
          'post_id'  => @post.remote_id,
          'origin'   => @requester.domain,
        }
        expect { subject.rsp_pool_post_delete(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      it "raises NotFoundError with a post id that isn't found" do
        h = {
          'username' => @member.username,
          'pool_id'  => @pool.remote_id,
          'post_id'  => 99999999,
          'origin'   => @requester.domain,
        }
        expect { subject.rsp_pool_post_delete(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end

      # For now, deletion silently fails in this case.

      # it 'raises NotFoundError with valid and recognized IDs, but no pool association' do
        # h = {
          # 'username' => @member.username,
          # 'pool_id'  => @pool.remote_id,
          # 'post_id'  => @post.remote_id,
          # 'origin'   => @requester.domain,
        # }
        # expect { subject.rsp_pool_post_delete(h) }.
        #   to raise_error( Libertree::Server::NotFoundError )
      # end

      context 'with valid data and a known association' do
        before :each do
          @pool << @post
          @h = {
            'username' => @member.username,
            'pool_id'  => @pool.remote_id,
            'post_id'  => @post.remote_id,
            'origin'   => @requester.domain,
          }
        end

        it 'raises no error and dissociates the post from the pool' do
          @pool.includes?(@post).should be_true
          expect { subject.rsp_pool_post_delete(@h) }.
            not_to raise_error
          @pool.dirty.includes?(@post).should be_false
        end
      end
    end
  end
end
