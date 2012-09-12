require 'spec_helper'

describe Libertree::Server::Responder::Post do
  describe 'rsp_post_delete' do
    include_context 'with an INTRODUCEd requester'

    context 'when the requester has AUTHENTICATEd itself' do
      before :each do
        @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
        @s.should have_responded_with_code('OK')
      end

      it 'with a missing id it responds with MISSING PARAMETER' do
        h = { }
        @s.process "POST-DELETE #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end


      it 'with a blank id it responds with MISSING PARAMETER' do
        h = {
          'id' => '',
        }
        @s.process "POST-DELETE #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end

      context 'given an existing post' do
        before :each do
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => @requester.id)
          )
          @post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for(:post, member_id: @member.id)
          )
        end

        it 'with valid data it responds with OK and deletes the local copy' do
          post_id = @post.id
          h = {
            'id' => @post.remote_id,
          }
          @s.process "POST-DELETE #{h.to_json}"
          @s.should have_responded_with_code('OK')

          Libertree::Model::Post[post_id].should be_nil
        end
      end
    end
  end
end
