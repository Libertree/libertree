require 'spec_helper'

describe Libertree::Server::Responder::Dispatcher do
  describe 'process' do
    it 'responds to requests with non-JSON parameters' do
      @s.process 'SOME-COMMAND invalid;JSON'
      @s.should have_responded_with( {
        'code' => 'BAD PARAMETER',
        'message' => "743: unexpected token at 'invalid;JSON'",
      } )
    end
  end
end
