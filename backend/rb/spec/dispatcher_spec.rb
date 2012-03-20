require 'spec_helper'

describe Libertree::Server::Dispatcher do
  describe 'process' do
    before :each do
      @d = MockServer.new.extend(subject)
    end

    it 'responds to malformed requests' do
      @d.process "malformed"
      shouldda_responded_with_code 'BAD REQUEST'
    end

    it 'responds to requests with non-JSON parameters' do
      @d.process 'SOME-COMMAND invalid;JSON'
      shouldda_responded_with( {
        'code' => 'BAD PARAMETER',
        'message' => "743: unexpected token at 'invalid;JSON'",
      } )
    end

    it 'responds to unknown commands' do
      @d.process 'NO-SUCH-COMMAND { "data": "foo" }'
      shouldda_responded_with_code 'UNKNOWN COMMAND'
    end
  end
end
