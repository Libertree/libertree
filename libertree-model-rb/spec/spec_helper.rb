if RUBY_VERSION =~ /^1\.9/
  require 'ruby-debug'
end
require_relative '../lib/libertree/db'

########################
# FIXME: Sequel wants us to connect to the db before defining models.  As model
# definitions are loaded when 'libertree/model' is required, we have to do
# this first.
Libertree::DB.load_config "#{File.dirname( __FILE__ ) }/../database.yaml"
Libertree::DB.dbh
########################

require_relative '../lib/libertree/model'
require_relative 'factories'

if ENV['LIBERTREE_ENV'] != 'test'
  $stderr.puts "Refusing to run specs in a non-test environment.  Comment out the exit line if you know what you're doing."
  exit 1
end

Libertree::Model::Server.own_domain = "localhost.localdomain"
