require 'spec_helper'

describe Libertree::Server::Responder::Member do
  before :each do
    @s = MockServer.new
  end

  describe 'rsp_member' do
    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'MEMBER { "anything": "anything" }'
        shouldda_responded_with_code 'ERROR'
        response['message'].should =~ /introduce/i
      end
    end

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create(
          FactoryGirl.attributes_for(:server).merge(
            { :public_key => $test_public_key }
          )
        )
      end

      context 'when the requester has INTRODUCEd itself' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
          @s.process %<INTRODUCE { "public_key": #{$test_public_key.to_json} } >
        end

        context 'when the requester has not AUTHENTICATEd itself' do
          it 'returns ERROR' do
            @s.process 'MEMBER { "anything": "anything" }'
            shouldda_responded_with_code 'ERROR'
            response['message'].should =~ /authenticate/i
          end
        end

        context 'when the requester has AUTHENTICATEd itself' do
          before :each do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            shouldda_responded_with_code 'OK'
          end

          it 'with a missing uuid it responds with ERROR' do
            @s.process 'MEMBER { "username": "yo" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'with a missing username it responds with ERROR' do
            @s.process 'MEMBER { "uuid": "bcad1067-cfb6-413b-b399-33828cb0c708" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'with an invalid uuid it responds with ERROR' do
            @s.process 'MEMBER { "uuid": "invaliduuid", "username": "user" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'with a blank uuid it responds with ERROR' do
            @s.process 'MEMBER { "uuid": "", "username": "user" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'with a blank username it responds with ERROR' do
            @s.process 'MEMBER { "uuid": "bcad1067-cfb6-413b-b399-33828cb0c708", "username": "" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'with valid data it responds with OK' do
            @s.process 'MEMBER { "uuid": "bcad1067-cfb6-413b-b399-33828cb0c708", "username": "myname" }'
            shouldda_responded_with_code 'OK'
          end
        end
      end
    end
  end
end
