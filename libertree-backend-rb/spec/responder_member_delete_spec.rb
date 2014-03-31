require 'spec_helper'

describe Libertree::Server::Responder::Member do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Member
    }
  end

  describe 'rsp_member_delete' do
    include_context 'requester in a forest'

    it 'raises MissingParameterError with a missing username' do
      h = { }
      expect { subject.rsp_member_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
    end

    it 'raises MissingParameterError with a blank username' do
      h = {
        'username' => '',
      }
      expect { subject.rsp_member_delete(h) }.
        to raise_error( Libertree::Server::MissingParameterError )
    end

    context 'given an existing member' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'deletes the local member record and raises no errors with valid data' do
        username = @member.username
        h = {
          'username' => username,
        }
        expect { subject.rsp_member_delete(h) }.
          not_to raise_error

        Libertree::Model::Member[ :username => username ].should be_nil
      end
    end
  end
end
