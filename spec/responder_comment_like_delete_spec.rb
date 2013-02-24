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

  describe 'rsp_comment_like_delete' do
    include_context 'requester in a forest'

    it 'raises MissingParameter with a missing id' do
      h = { }
      expect { subject.rsp_comment_like_delete(h) }.
        to raise_error( Libertree::Server::MissingParameter )
    end

    it 'raises MissingParameter with a blank id' do
      h = {
        'id' => '',
      }
      expect { subject.rsp_comment_like_delete(h) }.
        to raise_error( Libertree::Server::MissingParameter )
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
        subject.instance_variable_set(:@server, @requester)
      end

      it 'deletes the local copy and raises no errors with valid data' do
        like_id = @like.id
        h = {
          'id' => @like.remote_id,
        }
        expect { subject.rsp_comment_like_delete(h) }.
          not_to raise_error

        Libertree::Model::CommentLike[like_id].should be_nil
      end
    end
  end
end
