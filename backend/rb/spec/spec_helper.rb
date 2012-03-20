require 'libertree/server'

$responses = []

class MockServer
  include Libertree::Server

  def respond(data)
    $responses << data.to_json
  end
end

def shouldda_responded_with( hash )
  JSON.parse( $responses[-1] ).should == hash
end

def shouldda_responded_with_code( code )
  shouldda_responded_with 'code' => code
end
