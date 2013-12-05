require 'spec_helper'

describe Libertree::Server::Responder::Chat do
  let(:subject_class) { Class.new }
  let(:subject) { subject_class.new }

  before :each do
    subject_class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Chat
    }
  end

  describe 'rsp_chat' do
    include_context 'requester in a forest'

    context 'and the responder has no record of the sending member' do
      it 'raises NotFound' do
        subject.instance_variable_set(:@remote_tree, @requester)
        h = {
          'username' => 'sender',
          'recipient_username' => 'recipient',
          'text' => 'a chat message',
        }
        expect { subject.rsp_chat(h) }.
          to raise_error( Libertree::Server::NotFound )
      end
    end

    context 'and the responder has record of the sending member' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
        subject.instance_variable_set(:@remote_tree, @requester)
      end

      it 'raises MissingParameterError when a parameter is missing or blank' do
        h = {
          'username' => @member.username,
          'recipient_username' => 'recipient',
          'text' => 'a chat message',
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          expect { subject.rsp_chat(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )

          h_ = h.dup
          h_[key] = ''
          expect { subject.rsp_chat(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )
        end
      end

      context 'with valid message data, and a member that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFound' do
          h = {
            'username' => @member.username,
            'recipient_username' => 'recipient',
            'text' => 'a chat message',
          }
          expect { subject.rsp_chat(h) }.
            to raise_error( Libertree::Server::NotFound )
        end
      end

      context 'with valid message data, and a recipient that belongs to the requester' do
        before :each do
          @account = Libertree::Model::Account.create(
            FactoryGirl.attributes_for(:account)
          )
          @member_local = @account.member
        end

        it 'raises no errors with valid data' do
          h = {
            'username' => @member.username,
            'recipient_username' => @member_local.username,
            'text' => 'a chat message',
          }
          expect { subject.rsp_chat(h) }.
            not_to raise_error
        end
      end
    end
  end
end
