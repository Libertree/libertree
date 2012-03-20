require 'spec_helper'

describe Libertree::Server::Dispatcher do
  describe 'process' do
    before :each do
      @s = MockServer.new
    end

    it 'responds to malformed requests' do
      @s.process "malformed"
      shouldda_responded_with_code 'BAD REQUEST'
    end

    it 'responds to requests with non-JSON parameters' do
      @s.process 'SOME-COMMAND invalid;JSON'
      shouldda_responded_with( {
        'code' => 'BAD PARAMETER',
        'message' => "743: unexpected token at 'invalid;JSON'",
      } )
    end

    it 'responds to unknown commands' do
      @s.process 'NO-SUCH-COMMAND { "data": "foo" }'
      shouldda_responded_with_code 'UNKNOWN COMMAND'
    end
  end
end
