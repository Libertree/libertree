require 'spec_helper'

describe Libertree::Server::Responder::Message do
  describe 'rsp_message' do
    include_context 'with an INTRODUCEd and AUTHENTICATEd requester'

    context 'and the responder has no record of the sending member' do
      it 'responds with NOT FOUND' do
        h = {
          'username' => 'sender',
          'recipients' => [
            {
              'username' => 'recipient',
              'public_key' => '69e2c3164868a8d9ba4c77db842553a13116bef8',
            },
          ],
          'text' => 'a direct message',
        }
        @s.process "MESSAGE #{h.to_json}"
        @s.should have_responded_with_code('NOT FOUND')
      end
    end

    context 'and the responder has record of the sending member' do
      before :each do
        @member = Libertree::Model::Member.create(
          FactoryGirl.attributes_for(:member, :server_id => @requester.id)
        )
      end

      it 'and a parameter is missing or blank, it responds with MISSING PARAMETER' do
        h = {
          'username' => @member.username,
          'recipients' => [
            {
              'username' => 'recipient',
              'public_key' => '69e2c3164868a8d9ba4c77db842553a13116bef8',
            },
          ],
          'text' => 'a direct message',
        }

        keys = h.keys
        keys.each do |key|
          h_ = h.reject { |k,v| k == key }
          @s.process "MESSAGE #{h_.to_json}"
          @s.should have_responded_with_code('MISSING PARAMETER')

          h_ = h.dup
          h_[key] = ''
          @s.process "MESSAGE #{h_.to_json}"
          @s.should have_responded_with_code('MISSING PARAMETER')
        end
      end

      context 'with valid message data, and a sender that does not belong to the requester' do
        before :each do
          other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
          @member = Libertree::Model::Member.create(
            FactoryGirl.attributes_for(:member, :server_id => other_server.id)
          )
        end

        it 'responds with NOT FOUND' do
          h = {
            'username' => @member.username,
            'recipients' => [
              {
                'username' => 'recipient',
                'public_key' => '69e2c3164868a8d9ba4c77db842553a13116bef8',
              },
            ],
            'text' => 'a direct message',
          }
          @s.process "MESSAGE #{h.to_json}"
          @s.should have_responded_with_code('NOT FOUND')
        end
      end

      context 'with valid message data, and a recipient that belongs to the requester' do
        before :each do
          @account = Libertree::Model::Account.create(
            FactoryGirl.attributes_for(:account)
          )
          @member_local = @account.member
        end

        it 'with valid data it responds with OK' do
          h = {
            'username' => @member.username,
            'recipients' => [
              {
                'username' => @member_local.username,
                'public_key' => 'public-key-of-mock-server',
              },
            ],
            'text' => 'a direct message',
          }
          @s.process "MESSAGE #{h.to_json}"
          @s.should have_responded_with_code('OK')
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
                'public_key' => 'public-key-of-mock-server',
              },
              {
                'username' => @member_remote.username,
                'public_key' => @requester.public_key,
              },
            ],
            'text' => 'a direct message',
          }
          @s.process "MESSAGE #{h.to_json}"
          @s.should have_responded_with_code('OK')
        end
      end
    end
  end
end
