require 'spec_helper'

describe Libertree::Server::Responder::Authentication do
  before :each do
    @s = MockServer.new
  end

  describe 'rsp_introduce' do
    it 'returns OK when the public_key is unrecognized' do
      @s.process 'INTRODUCE { "public_key": "some brand new public key"}'
      shouldda_responded_with_code 'OK'
    end

    describe 'when the public_key is recognized' do
      it 'returns OK and challenges the requester ' do
        @s.process 'INTRODUCE { "public_key": "known public key"}'
        shouldda_responded_with_code 'OK'
        response['challenge'].should =~ /^\S{16,}$/
      end
    end
  end
end
