require 'spec_helper'

describe Libertree::Server::Responder::Comment do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Comment
    }
  end

  describe 'rsp_comment_delete' do
    include_context 'requester in a forest'

    it 'raises MissingParameterError with a missing id' do
      h = { }
      expect { subject.rsp_comment_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
    end

    it 'raises MissingParameterError with a blank id' do
      h = {
        'id' => '',
      }
      expect { subject.rsp_comment_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
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
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises no errors with valid data and deletes the local copy' do
        comment_id = @comment.id
        h = {
          'id' => @comment.remote_id,
        }
        expect { subject.rsp_comment_delete(h) }.
          not_to raise_error

        Libertree::Model::Comment[comment_id].should be_nil
      end
    end
  end
end
