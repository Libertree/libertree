require 'spec_helper'

describe Libertree::Server::Responder::Authentication do
  describe 'rsp_introduce' do
    it 'returns MISSING PARAMETER when the public_key is missing' do
      @s.process 'INTRODUCE { }'
      @s.should have_responded_with_code('MISSING PARAMETER')
      @s.process 'INTRODUCE { "public_key": "" }'
      @s.should have_responded_with_code('MISSING PARAMETER')
    end

    it 'returns OK when the public_key is unrecognized' do
      @s.process 'INTRODUCE { "public_key": "some brand new public key"}'
      @s.should have_responded_with_code('OK')
      @s.response['challenge'].should be_nil
    end

    it 'stores the server name when it is given' do
      Libertree::Model::Server[name_given: 'cool-server'].should be_nil
      @s.process 'INTRODUCE { "public_key": "some brand new public key", "name": "cool-server"}'
      @s.should have_responded_with_code('OK')
      @s.response['challenge'].should be_nil
      Libertree::Model::Server[name_given: 'cool-server'].should_not be_nil
    end

    context 'when the public_key is recognized' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      it 'returns OK and challenges the requester' do
        params = { 'public_key' => @requester.public_key }.to_json
        @s.process "INTRODUCE #{params}"
        @s.should have_responded_with_code('OK')
        @s.response['challenge'].should =~ /^(\S{60}\n){4}\S{44}\n$/m
      end
    end
  end

  describe 'rsp_authenticate' do
    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'AUTHENTICATE { "response": "challenge response" }'
        @s.should have_responded_with_code('ERROR')
        @s.response['message'].should =~ /introduce/i
      end
    end

    context 'with a known requester' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      context 'given a specific challenge string' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
        end

        context 'when the requester has INTRODUCEd itself' do
          before :each do
            @s.process %<INTRODUCE { "public_key": #{@requester.public_key.to_json} } >
          end

          it 'returns ERROR if the requester fails the challenge' do
            @s.process 'AUTHENTICATE { "response": "incorrect challenge response" }'
            @s.should have_responded_with_code('ERROR')
          end

          it 'returns OK if the requester provides the exact challenge string' do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            @s.should have_responded_with_code('OK')
          end
        end
      end
    end
  end
end
