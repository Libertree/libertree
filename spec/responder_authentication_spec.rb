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

    describe 'when the public_key is recognized' do
      before :each do
        @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
      end

      it 'returns OK and challenges the requester' do
        params = { 'public_key' => @requester.public_key }.to_json
        @s.process "INTRODUCE #{params}"
        shouldda_responded_with_code 'OK'
        response['challenge'].should =~ /^\S{16,}$/
      end
    end
  end
end
