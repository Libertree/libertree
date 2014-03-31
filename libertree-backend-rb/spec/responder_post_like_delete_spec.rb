require 'spec_helper'

describe Libertree::Server::Responder::PostLike do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::PostLike
    }
  end

  describe 'rsp_post_like_delete' do
    include_context 'requester in a forest'

    it 'raises MissingParameterError with a missing id' do
      h = { }
      expect { subject.rsp_post_like_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
    end

    it 'raises MissingParameterError with a blank id' do
      h = {
        'id' => '',
      }
      expect { subject.rsp_post_like_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
    end

    context 'given an existing like' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        @post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for(:post, member_id: @member.id)
        )
        @like = Libertree::Model::PostLike.create(
          FactoryGirl.attributes_for(:post_like, member_id: @member.id, post_id: @post.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'deletes the local copy and raises no errors with valid data' do
        like_id = @like.id
        h = {
          'id' => @like.remote_id,
        }
        expect { subject.rsp_post_like_delete(h) }.
          not_to raise_error

        Libertree::Model::PostLike[like_id].should be_nil
      end
    end
  end
end
