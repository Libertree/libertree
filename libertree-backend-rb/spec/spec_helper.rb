if RUBY_VERSION =~ /^1\.9/
  require 'ruby-debug'
end
require 'libertree/db'

########################
# FIXME: M4DBI wants us to connect to the db before defining models.  As model
# definitions are loaded when 'libertree/server' is required, we have to do
# this first.
Libertree::DB.load_config "#{File.dirname( __FILE__ ) }/../database.yaml"
Libertree::DB.dbh
########################

require 'libertree/server'
require_relative 'factories'

Libertree::Server.log_handle = File.open( 'test-server.log', 'a+' )

if ENV['LIBERTREE_ENV'] != 'test'
  $stderr.puts "Refusing to run specs in a non-test environment.  Comment out the exit line if you know what you're doing."
  exit 1
end

RSpec.configure do |config|
  config.before(:each) do
    Libertree::Server.conf = {}
    Libertree::DB.dbh.execute "SET client_min_messages TO 'warning';"
    Libertree::DB.dbh.execute 'TRUNCATE posts CASCADE'
    Libertree::DB.dbh.execute 'TRUNCATE servers CASCADE'
  end
end

shared_context 'requester not in any forest' do
  before :each do
    @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
  end
end

shared_context 'requester in a forest' do
  before :each do
    @forest = Libertree::Model::Forest.create( FactoryGirl.attributes_for(:forest) )
    @requester = Libertree::Model::Server.create( FactoryGirl.attributes_for(:server) )
    @forest.add @requester
  end
end
