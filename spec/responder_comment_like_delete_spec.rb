require 'spec_helper'

describe Libertree::Server::Responder::CommentLike do
  describe 'rsp_comment_like_delete' do

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
            @s.process 'COMMENT-LIKE-DELETE { "anything": "anything" }'
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
            @s.process "COMMENT-LIKE-DELETE #{h.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')
          end

          it 'with a blank id it responds with MISSING PARAMETER' do
            h = {
              'id' => '',
            }
            @s.process "COMMENT-LIKE-DELETE #{h.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')
          end

          context 'given an existing like' do
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
              @like = Libertree::Model::CommentLike.create(
                FactoryGirl.attributes_for(:comment_like, member_id: @member.id, comment_id: @comment.id)
              )
            end

            it 'with valid data it responds with OK and deletes the local copy' do
              like_id = @like.id
              h = {
                'id' => @like.remote_id,
              }
              @s.process "COMMENT-LIKE-DELETE #{h.to_json}"
              @s.should have_responded_with_code('OK')

              Libertree::Model::CommentLike[like_id].should be_nil
            end
          end
        end
      end
    end
  end
end
