require 'spec_helper'

describe Libertree::Server::Responder::Chat do
  describe 'rsp_chat' do

    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'CHAT { "anything": "anything" }'
        @s.should have_responded_with_code('ERROR')
        @s.response['message'].should =~ /introduce/i
      end
    end

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      context 'when the requester has INTRODUCEd itself' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
          @s.process %<INTRODUCE { "public_key": #{@requester.public_key.to_json} } >
        end

        context 'when the requester has not AUTHENTICATEd itself' do
          it 'returns ERROR' do
            @s.process 'CHAT { "anything": "anything" }'
            @s.should have_responded_with_code('ERROR')
            @s.response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end

          context 'and the responder has no record of the sending member' do
            it 'responds with NOT FOUND' do
              h = {
                'username' => 'sender',
                'recipient_username' => 'recipient',
                'text' => 'a chat message',
              }
              @s.process "CHAT #{h.to_json}"
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
                'recipient_username' => 'recipient',
                'text' => 'a chat message',
              }

              keys = h.keys
              keys.each do |key|
                h_ = h.reject { |k,v| k == key }
                @s.process "CHAT #{h_.to_json}"
                @s.should have_responded_with_code('MISSING PARAMETER')

                h_ = h.dup
                h_[key] = ''
                @s.process "CHAT #{h_.to_json}"
                @s.should have_responded_with_code('MISSING PARAMETER')
              end
            end

            context 'with valid message data, and a member that does not belong to the requester' do
              before :each do
                other_server = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
                @member = Libertree::Model::Member.create(
                  FactoryGirl.attributes_for(:member, :server_id => other_server.id)
                )
              end

              it 'responds with NOT FOUND' do
                h = {
                  'username' => @member.username,
                  'recipient_username' => 'recipient',
                  'text' => 'a chat message',
                }
                @s.process "CHAT #{h.to_json}"
                @s.should have_responded_with_code('NOT FOUND')
              end
            end

            context 'with valid message data, and a recipient that belongs to the requester' do
              before :each do
                @account = Libertree::Model::Account.create(
                  FactoryGirl.attributes_for(:account)
                )
                @member_local = Libertree::Model::Member.create(
                  FactoryGirl.attributes_for(:member, username: nil, account_id: @account.id)
                )
              end

              it 'with valid data it responds with OK' do
                h = {
                  'username' => @member.username,
                  'recipient_username' => @member_local.username,
                  'text' => 'a chat message',
                }
                @s.process "CHAT #{h.to_json}"
                @s.should have_responded_with_code('OK')
              end
            end
          end
        end
      end
    end
  end
end
