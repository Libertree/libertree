require 'spec_helper'

describe Libertree::Server::Responder::Dispatcher do
  describe 'process' do
    it 'responds to malformed requests' do
      @s.process "malformed"
      @s.should have_responded_with_code('BAD REQUEST')
    end

    it 'responds to requests with non-JSON parameters' do
      @s.process 'SOME-COMMAND invalid;JSON'
      @s.should have_responded_with( {
        'code' => 'BAD PARAMETER',
        'message' => "743: unexpected token at 'invalid;JSON'",
      } )
    end

    it 'responds to unknown commands' do
      @s.process 'NO-SUCH-COMMAND { "data": "foo" }'
      @s.should have_responded_with_code('UNKNOWN COMMAND')
    end

    context 'when the requester has not INTRODUCEd itself' do
      it 'returns ERROR for all commands besides INTRODUCE' do
        commands = Libertree::Server::Responder::Dispatcher::VALID_COMMANDS - ['INTRODUCE',]
        commands.each do |command|
          @s.process %|#{command} { "anything": "anything" }|
          @s.should have_responded_with_code('ERROR')
          @s.response['message'].should =~ /introduce/i
        end
      end
    end

    context 'when the requester has INTRODUCEd but not AUTHENTICATEd itself' do
      include_context 'with an INTRODUCEd requester'

      it 'returns ERROR' do
        commands = Libertree::Server::Responder::Dispatcher::VALID_COMMANDS - ['INTRODUCE', 'AUTHENTICATE',]
        commands.each do |command|
          @s.process %|#{command} { "anything": "anything" }|
          @s.should have_responded_with_code('ERROR')
          @s.response['message'].should =~ /authenticate/i
        end
      end
    end
  end
end
