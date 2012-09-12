require 'spec_helper'

describe Libertree::Server::Responder::Post do
  describe 'rsp_post' do

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
            @s.process 'POST { "anything": "anything" }'
            @s.should have_responded_with_code('ERROR')
            @s.response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end

          context 'and the member is known' do
            before :each do
              @member = Libertree::Model::Member.create(
                FactoryGirl.attributes_for(:member, :server_id => @requester.id)
              )
            end

            it 'with a missing id it responds with MISSING PARAMETER' do
              h = {
                'username'   => @member.username,
                'visibility' => 'forest',
                'text'       => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              @s.should have_responded_with_code('MISSING PARAMETER')
            end

            it 'with a blank id it responds with MISSING PARAMETER' do
              h = {
                'username'   => @member.username,
                'id'         => '',
                'visibility' => 'forest',
                'text'       => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              @s.should have_responded_with_code('MISSING PARAMETER')
            end

            it "with a member username that isn't found it responds with NOT FOUND" do
              h = {
                'username'   => 'nosuchusername',
                'id'         => 4,
                'visibility' => 'forest',
                'text'       => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              @s.should have_responded_with_code('NOT FOUND')
            end

            context 'with valid post data, and a member that does not belong to the requester' do
              before :each do
                other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
                @member = Libertree::Model::Member.create(
                  FactoryGirl.attributes_for(:member, :server_id => other_server.id)
                )
              end

              it 'responds with NOT FOUND' do
                h = {
                  'username'   => @member.username,
                  'id'         => 5,
                  'visibility' => 'forest',
                  'text'       => 'A test post.',
                }
                @s.process "POST #{h.to_json}"
                @s.should have_responded_with_code('NOT FOUND')
              end
            end

            it 'with valid data it responds with OK' do
              h = {
                'username'   => @member.username,
                'id'         => 6,
                'visibility' => 'forest',
                'text'       => 'A test post.',
              }
              @s.process "POST #{h.to_json}"
              @s.should have_responded_with_code('OK')
            end

            context 'when a remote post exists already' do
              before :each do
                @post = Libertree::Model::Post.create(
                  FactoryGirl.attributes_for(:post, member_id: @member.id)
                )
                @initial_text = @post.text
              end

              it 'updates the post text and stores a post revision' do
                original_text = @post.text
                original_text.should_not == 'edited text'
                Libertree::Model::PostRevision.where( 'post_id' => @post.id ).count.should == 0

                h = {
                  'username'   => @member.username,
                  'id'         => @post.remote_id,
                  'visibility' => 'forest',
                  'text'       => 'edited text',
                }
                @s.process "POST #{h.to_json}"
                @s.should have_responded_with_code('OK')

                Libertree::Model::Post[@post.id].text.should == 'edited text'
                revisions = Libertree::Model::PostRevision.where( 'post_id' => @post.id )
                revisions.count.should == 1
                revisions[0].text.should == original_text
              end
            end
          end
        end
      end
    end
  end
end
