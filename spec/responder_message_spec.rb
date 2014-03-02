require 'spec_helper'

describe Libertree::Server::Responder::Message do
  subject {
    Class.new.new
  }

  before :each do
    subject.class.class_eval {
      include Libertree::Server::Responder::Helper
      include Libertree::Server::Responder::Message
    }
  end

  describe 'rsp_message' do
    include_context 'requester in a forest'
    before :each do
      subject.instance_variable_set(:@remote_tree, @requester)
    end

    context 'and the responder has no record of the sending member' do
      it 'raises NotFoundError' do
        h = {
          'username' => 'sender',
          'recipients' => [
            {
              'username' => 'recipient',
              'origin'   => 'does.not.matter',
            },
          ],
          'text' => 'a direct message',
        }
        expect { subject.rsp_message(h) }.
          to raise_error( Libertree::Server::NotFoundError )
      end
    end

    context 'and the responder has record of the sending member' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
      end

      it 'raises MissingParameterError when a parameter is missing or blank' do
        h = {
          'username' => @member.username,
          'recipients' => [
            {
              'username' => 'recipient',
              'origin'   => 'does.not.matter',
            },
          ],
          'text' => 'a direct message',
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          expect { subject.rsp_message(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )

          h_ = h.dup
          h_[key] = ''
          expect { subject.rsp_message(h_) }.
            to raise_error( Libertree::Server::MissingParameterError )
        end
      end

      context 'with valid message data, and a sender that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'raises NotFoundError' do
          h = {
            'username' => @member.username,
            'recipients' => [
              {
                'username' => 'recipient',
                'origin'   => 'does.not.matter',
              },
            ],
            'text' => 'a direct message',
          }
          expect { subject.rsp_message(h) }.
            to raise_error( Libertree::Server::NotFoundError )
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
            'recipients' => [
              {
                'username' => @member_local.username,
              },
            ],
            'text' => 'a direct message',
          }
          expect { subject.rsp_message(h) }.
            not_to raise_error
        end
      end

      context 'with valid message data, and two recipients, one local, and one on the requester' do
        before :each do
          @account = Libertree::Model::Account.create(
            FactoryGirl.attributes_for(:account)
          )
          @member_local = @account.member
          @member_remote = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => @requester.id)
          )
        end

        it 'with valid data it responds with OK' do
          h = {
            'username' => @member.username,
            'recipients' => [
              {
                'username' => @member_local.username,
              },
              {
                'username' => @member_remote.username,
                'origin'   => @member_remote.server.domain,
              },
            ],
            'text' => 'a direct message',
          }
          expect { subject.rsp_message(h) }.
            not_to raise_error
        end

        it 'with a single recipient it responds with OK' do
          h = {
            'username' => @member.username,
            'recipients' => {
              'username' => @member_remote.username,
              'origin'   => @member_remote.server.domain,
            },
            'text' => 'a direct message',
          }
          expect { subject.rsp_message(h) }.
            not_to raise_error
        end
      end
    end
  end
end
