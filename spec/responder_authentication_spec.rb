require 'spec_helper'

describe Libertree::Server::Responder::Authentication do
  before :each do
    @s = MockServer.new
  end

  describe 'rsp_introduce' do
    it 'returns OK when the public_key is unrecognized' do
      @s.process 'INTRODUCE { "public_key": "some brand new public key"}'
      shouldda_responded_with_code 'OK'
      response['challenge'].should be_nil
    end

    context 'when the public_key is recognized' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      it 'returns OK and challenges the requester' do
        params = { 'public_key' => @requester.public_key }.to_json
        @s.process "INTRODUCE #{params}"
        shouldda_responded_with_code 'OK'
        response['challenge'].should =~ /^-----BEGIN PGP MESSAGE-----.{200,}-----END PGP MESSAGE-----$/m
      end
    end
  end

  describe 'rsp_authenticate' do
    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR' do
        @s.process 'AUTHENTICATE { "response": "challenge response" }'
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

      context 'given a specific challenge string' do
        before :each do
          @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
        end

        context 'when the requester has INTRODUCEd itself' do
          before :each do
            @s.process %<INTRODUCE { "public_key": #{$test_public_key.to_json} } >
          end

          it 'returns ERROR if the requester fails the challenge' do
            @s.process 'AUTHENTICATE { "response": "incorrect challenge response" }'
            shouldda_responded_with_code 'ERROR'
          end

          it 'returns OK if the requester provides the exact challenge string' do
            @s.process 'AUTHENTICATE { "response": "abcdefghijklmnopqrstuvwxyz" }'
            shouldda_responded_with_code 'OK'
          end
        end
      end
    end
  end
end
