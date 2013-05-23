require 'spec_helper'

describe Libertree::Server::Responder::Post do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Post
    }
  end

  describe 'rsp_post_delete' do
    include_context 'requester in a forest'

    it 'raises MissingParameter with a missing id' do
      h = { }
      expect { subject.rsp_post_delete(h) }.
        to raise_error( Libertree::Server::MissingParameter )
    end

    it 'raises MissingParameter with a blank id' do
      h = {
        'id' => '',
      }
      expect { subject.rsp_post_delete(h) }.
        to raise_error( Libertree::Server::MissingParameter )
    end

    context 'given an existing post' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        @post = Libertree::Model::Post.create(
          FactoryGirl.attributes_for(:post, member_id: @member.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises no errors and deletes the local copy with valid data' do
        post_id = @post.id
        h = {
          'id' => @post.remote_id,
        }
        expect { subject.rsp_post_delete(h) }.
          not_to raise_error

        Libertree::Model::Post[post_id].should be_nil
      end
    end
  end
end
