require 'spec_helper'

describe Libertree::Server::Responder::CommentLike do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::CommentLike
    }
  end

  describe 'rsp_comment_like' do
    include_context 'requester in a forest'

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
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameter when a parameter is missing or blank' do
        h = {
          'id'         => 999,
          'username'   => @member.username,
          'origin'     => @comment.member.server.domain,
          'comment_id' => @comment.remote_id,
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          expect { subject.rsp_comment_like(h_) }.
            to raise_error( Libertree::Server::MissingParameter )

          h_ = h.dup
          h_[key] = ''
          expect { subject.rsp_comment_like(h_) }.
            to raise_error( Libertree::Server::MissingParameter )
        end
      end

      it "raises NotFound with a member username that isn't found" do
        h = {
          'id'         => 999,
          'username'   => 'nosuchusername',
          'origin'     => @comment.member.server.domain,
          'comment_id' => @comment.remote_id,
        }
        expect { subject.rsp_comment_like(h) }.
          to raise_error( Libertree::Server::NotFound )
      end

      it "raises NotFound with a comment id that isn't found" do
        h = {
          'id'         => 999,
          'username'   => @member.username,
          'origin'     => @comment.member.server.domain,
          'comment_id' => 99999999,
        }
        expect { subject.rsp_comment_like(h) }.
          to raise_error( Libertree::Server::NotFound )
      end

      context 'with valid Like data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFound' do
          h = {
            'id'         => 999,
            'username'   => @member.username,
            'origin'     => @comment.member.server.domain,
            'comment_id' => @comment.remote_id,
          }
          expect { subject.rsp_comment_like(h) }.
            to raise_error( Libertree::Server::NotFound )
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

        it 'raises no errors' do
          h = {
            'id'         => 999,
            'username'   => @member.username,
            'origin'     => @comment.member.server.domain,
            'comment_id' => @comment.remote_id,
          }
          expect { subject.rsp_comment_like(h) }.
            not_to raise_error
        end
      end

      it 'raises no errors with valid data' do
        h = {
          'id'         => 999,
          'username'   => @member.username,
          'origin'     => @comment.member.server.domain,
          'comment_id' => @comment.remote_id,
        }
        expect { subject.rsp_comment_like(h) }.
          not_to raise_error
      end
    end
  end
end
