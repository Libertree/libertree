require 'spec_helper'

describe Libertree::Server::Responder::Member do
  describe 'rsp_member' do

    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'MEMBER { "anything": "anything" }'
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
            @s.process 'MEMBER { "anything": "anything" }'
            @s.should have_responded_with_code('ERROR')
            @s.response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end

          context 'and the member is known' do
            before :each do
              @member = Libertree::Model::Member.create(
                FactoryGirl.attributes_for(:member, :server_id => @requester.id)
              )
              Net::HTTP.any_instance.stub(:get)
              Net::HTTPResponse.any_instance.stub(:body)
              Socket.stub(:getaddrinfo) { [ [nil,nil,nil,@requester.ip] ] }
              File.stub(:open)
            end

            it 'with a missing username it responds with MISSING PARAMETER' do
              h = {
                'avatar_url' => 'http://libertree.net/images/avatars/1.png',
              }
              @s.process "MEMBER #{h.to_json}"
              @s.should have_responded_with_code('MISSING PARAMETER')
            end


            it 'with a blank id it responds with MISSING PARAMETER' do
              h = {
                'username' => '',
                'avatar_url' => 'http://libertree.net/images/avatars/1.png',
              }
              @s.process "MEMBER #{h.to_json}"
              @s.should have_responded_with_code('MISSING PARAMETER')
            end

            it 'with valid data it responds with OK' do
              h = {
                'username' => 'someuser',
                'avatar_url' => 'http://libertree.net/images/avatars/1.png',
              }
              @s.process "MEMBER #{h.to_json}"
              @s.should have_responded_with_code('OK')
            end
          end
        end
      end
    end
  end
end
