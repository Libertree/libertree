require 'spec_helper'

describe Libertree::Server::Responder::Comment do
  describe 'rsp_comment_delete' do
    include_context 'with an INTRODUCEd requester'

    context 'when the requester has not AUTHENTICATEd itself' do
      it 'returns ERROR' do
        @s.process 'COMMENT-DELETE { "anything": "anything" }'
        @s.should have_responded_with_code('ERROR')
        @s.response['message'].should =~ /authenticate/i
      end
    end

    context 'when the requester has AUTHENTICATEd itself' do
      before :each do
        @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
        @s.should have_responded_with_code('OK')
      end

      it 'with a missing id it responds with MISSING PARAMETER' do
        h = { }
        @s.process "COMMENT-DELETE #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end


      it 'with a blank id it responds with MISSING PARAMETER' do
        h = {
          'id' => '',
        }
        @s.process "COMMENT-DELETE #{h.to_json}"
        @s.should have_responded_with_code('MISSING PARAMETER')
      end

      context 'given an existing comment' do
        before :each do
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => @requester.id)
          )
          @post = Libertree::Model::Post.create(
            FactoryGirl.attributes_for(:post, member_id: @member.id)
          )
          @comment = Libertree::Model::Comment.create(
            FactoryGirl.attributes_for(:comment, member_id: @member.id, post_id: @post.id)
          )
        end

        it 'with valid data it responds with OK and deletes the local copy' do
          comment_id = @comment.id
          h = {
            'id' => @comment.remote_id,
          }
          @s.process "COMMENT-DELETE #{h.to_json}"
          @s.should have_responded_with_code('OK')

          Libertree::Model::Comment[comment_id].should be_nil
        end
      end
    end
  end
end
