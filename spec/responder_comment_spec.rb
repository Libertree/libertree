require 'spec_helper'

describe Libertree::Server::Responder::Comment do
  describe 'rsp_comment' do

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      context 'when the requester has INTRODUCEd itself' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
          @s.process %<INTRODUCE { "public_key": #{@requester.public_key.to_json} } >
        end

        context 'when the requester has not AUTHENTICATEd itself' do
          it 'returns ERROR' do
            @s.process 'COMMENT { "anything": "anything" }'
            @s.should have_responded_with_code('ERROR')
            @s.response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end

          context 'and the responder has record of both the member and the post' do
            before :each do
              @member = Libertree::Model::Member.create(
                FactoryGirl.attributes_for(:member, :server_id => @requester.id)
              )
              @post = Libertree::Model::Post.create(
                FactoryGirl.attributes_for(:post, member_id: @member.id)
              )
            end

            it 'and a parameter is missing or blank, it responds with MISSING PARAMETER' do
              h = {
                'id'         => 999,
                'username'   => @member.username,
                'public_key' => @requester.public_key,
                'post_id'    => @post.remote_id,
                'text'       => 'A test comment.',
              }

              keys = h.keys
              keys.each do |key|
                h_ = h.reject { |k,v| k == key }
                @s.process "COMMENT #{h_.to_json}"
                @s.should have_responded_with_code('MISSING PARAMETER')

                h_ = h.dup
                h_[key] = ''
                @s.process "COMMENT #{h_.to_json}"
                @s.should have_responded_with_code('MISSING PARAMETER')
              end
            end

            it "with a member username that isn't found, it responds with NOT FOUND" do
              h = {
                'id'         => 999,
                'username'   => 'nosuchusername',
                'public_key' => @requester.public_key,
                'post_id'    => @post.remote_id,
                'text'       => 'A test comment.',
              }
              @s.process "COMMENT #{h.to_json}"
              @s.should have_responded_with_code('NOT FOUND')
            end

            it "with a post id that isn't found, it responds with NOT FOUND" do
              h = {
                'id'         => 999,
                'username'   => @member.username,
                'public_key' => @requester.public_key,
                'post_id'    => 99999999,
                'text'       => 'A test comment.',
              }
              @s.process "COMMENT #{h.to_json}"
              @s.should have_responded_with_code('NOT FOUND')
            end

            context 'with valid comment data, and a member that does not belong to the requester' do
              before :each do
                other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
                @member = Libertree::Model::Member.create(
                  FactoryGirl.attributes_for(:member, :server_id => other_server.id)
                )
              end

              it 'responds with NOT FOUND' do
                h = {
                  'id'         => 999,
                  'username'   => @member.username,
                  'public_key' => @requester.public_key,
                  'post_id'    => @post.remote_id,
                  'text'       => 'A test comment.',
                }
                @s.process "COMMENT #{h.to_json}"
                @s.should have_responded_with_code('NOT FOUND')
              end
            end

            context 'with valid comment data, and a post that does not belong to the requester' do
              before :each do
                other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
                member = Libertree::Model::Member.create(
                  FactoryGirl.attributes_for(:member, :server_id => other_server.id)
                )
                @post = Libertree::Model::Post.create(
                  FactoryGirl.attributes_for(:post, member_id: member.id)
                )
              end

              it 'responds with OK' do
                h = {
                  'id'         => 999,
                  'username'   => @member.username,
                  'public_key' => @requester.public_key,
                  'post_id'    => @post.remote_id,
                  'text'       => 'A test comment.',
                }
                @s.process "COMMENT #{h.to_json}"
                @s.should have_responded_with_code('OK')
              end
            end

            it 'with valid data it responds with OK' do
              h = {
                'id'         => 999,
                'username'   => @member.username,
                'public_key' => @requester.public_key,
                'post_id'    => @post.remote_id,
                'text'       => 'A test comment.',
              }
              @s.process "COMMENT #{h.to_json}"
              @s.should have_responded_with_code('OK')
            end
          end
        end
      end
    end
  end
end
