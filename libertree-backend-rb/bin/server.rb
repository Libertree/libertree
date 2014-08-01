require 'libertree/db'

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml> <database.yaml>"
  exit 1
end

if ARGV[1].nil?
  $stderr.puts "no database configuration file given; assuming #{File.dirname( __FILE__ ) }/../database.yaml"
  db_config = "#{File.dirname( __FILE__ ) }/../database.yaml"
else
  db_config = ARGV[1]
end

########################
# FIXME: M4DBI wants us to connect to the db before defining models.  As model
# definitions are loaded when 'libertree/server' is required, we have to do
# this first.
Libertree::DB.load_config db_config
Libertree::DB.dbh
########################

require 'libertree/server'
# TODO: take the appropriate setting from the config file
Libertree::Model::Account.set_auth_settings(:default, nil)

Thread.abort_on_exception = true
Libertree::Server.run ARGV[0]