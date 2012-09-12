require 'spec_helper'

describe Libertree::Server::Responder::CommentLike do
  describe 'rsp_comment_like' do
    include_context 'with an INTRODUCEd requester'

    context 'when the requester has AUTHENTICATEd itself' do
      before :each do
        @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
        @s.should have_responded_with_code('OK')
      end

      context 'and the responder has record of both the member and the comment' do
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

        it 'and a parameter is missing or blank, it responds with MISSING PARAMETER' do
          h = {
            'id'         => 999,
            'username'   => @member.username,
            'public_key' => @requester.public_key,
            'comment_id' => @comment.remote_id,
          }

          keys = h.keys
          keys.each do |key|
            h_ = h.reject { |k,v| k == key }
            @s.process "COMMENT-LIKE #{h_.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')

            h_ = h.dup
            h_[key] = ''
            @s.process "COMMENT-LIKE #{h_.to_json}"
            @s.should have_responded_with_code('MISSING PARAMETER')
          end
        end

        it "with a member username that isn't found, it responds with NOT FOUND" do
          h = {
            'id'         => 999,
            'username'   => 'nosuchusername',
            'public_key' => @requester.public_key,
            'comment_id' => @comment.remote_id,
          }
          @s.process "COMMENT-LIKE #{h.to_json}"
          @s.should have_responded_with_code('NOT FOUND')
        end

        it "with a comment id that isn't found, it responds with NOT FOUND" do
          h = {
            'id'         => 999,
            'username'   => @member.username,
            'public_key' => @requester.public_key,
            'comment_id' => 99999999,
          }
          @s.process "COMMENT-LIKE #{h.to_json}"
          @s.should have_responded_with_code('NOT FOUND')
        end

        context 'with valid Like data, and a member that does not belong to the requester' do
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
              'comment_id' => @comment.remote_id,
            }
            @s.process "COMMENT-LIKE #{h.to_json}"
            @s.should have_responded_with_code('NOT FOUND')
          end
        end

        context 'with valid Like data, and a comment that does not belong to the requester' do
          before :each do
            other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
            member = Libertree::Model::Member.create(
              FactoryGirl.attributes_for(:member, :server_id => other_server.id)
            )
            ppost = Libertree::Model::Post.create(
              FactoryGirl.attributes_for(:post, member_id: member.id)
            )
            @comment = Libertree::Model::Comment.create(
              FactoryGirl.attributes_for(:comment, member_id: member.id, post_id: ppost.id)
            )
          end

          it 'responds with OK' do
            h = {
              'id'         => 999,
              'username'   => @member.username,
              'public_key' => @requester.public_key,
              'comment_id' => @comment.remote_id,
            }
            @s.process "COMMENT-LIKE #{h.to_json}"
            @s.should have_responded_with_code('OK')
          end
        end

        it 'with valid data it responds with OK' do
          h = {
            'id'         => 999,
            'username'   => @member.username,
            'public_key' => @requester.public_key,
            'comment_id' => @comment.remote_id,
          }
          @s.process "COMMENT-LIKE #{h.to_json}"
          @s.should have_responded_with_code('OK')
        end
      end
    end
  end
end
