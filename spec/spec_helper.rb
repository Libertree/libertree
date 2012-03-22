require 'libertree/server'
require_relative 'factories'

$responses = []

class MockServer
  include Libertree::Server

  def initialize
    @ip_remote = '192.168.0.100'
  end

  def respond(data)
    $responses << data.to_json
  end

  def close_connection_after_writing
    # noop in testing
  end
end

def response
  JSON.parse $responses[-1]
end

def shouldda_responded_with( hash )
  response.should == hash
end

def shouldda_responded_with_code( code )
  ( response['code'] == code ).should(
    be_true,
    "Expected #{code.inspect}, got #{response['code'].inspect}.  Error message: #{response['message'].inspect}"
  )
end
