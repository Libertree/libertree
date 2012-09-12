require 'ruby-debug'
require 'libertree/server'
require_relative 'factories'

class MockServer
  include Libertree::Server

  attr_reader :responses

  def initialize
    @ip_remote = '192.168.0.100'
    @responses = []
    Libertree::Server.log = File.open( 'test-server.log', 'a+' )
  end

  def respond(data)
    @responses << data.to_json
  end

  def close_connection_after_writing
    # noop in testing
  end

  def response
    JSON.parse @responses[-1]
  end

  def has_responded_with?(hash)
    response.should == hash
  end

  def has_responded_with_code?(code)
    has = ( response['code'] == code )
    if ! has
      $stderr.puts "Expected #{code.inspect}, got #{response['code'].inspect}.  Error message: #{response['message'].inspect}"
    end
    has
  end
end

if ENV['LIBERTREE_ENV'] != 'test'
  $stderr.puts "Refusing to run specs in a non-test environment.  Comment out the exit line if you know what you're doing."
  exit 1
end

RSpec.configure do |config|
  config.before(:each) do
    @s = MockServer.new
    Libertree::Server.conf = {}
    Libertree::DB.dbh.execute "SET client_min_messages TO 'warning';"
    Libertree::DB.dbh.execute 'TRUNCATE posts CASCADE'
    Libertree::DB.dbh.execute 'TRUNCATE servers CASCADE'
  end

  config.after(:each) do
    @s.unbind
  end
end

shared_context 'with a known requester' do
  before :each do
    @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
  end
end

shared_context 'with an INTRODUCEd requester' do
  include_context 'with a known requester'

  before :each do
    @s.stub(:challenge_new) { 'abcdefghijklmnopqrstuvwxyz' }
    @s.process %<INTRODUCE { "public_key": #{@requester.public_key.to_json} } >
  end
end
