require 'libertree/server'

$responses = []

class MockServer
  def respond(data)
    $responses << data.to_json
  end

  def respond_with_code(code)
    respond( { 'code' => code } )
  end
end

def shouldda_responded_with( hash )
  JSON.parse( $responses[-1] ).should == hash
end

def shouldda_responded_with_code( code )
  shouldda_responded_with 'code' => code
end
