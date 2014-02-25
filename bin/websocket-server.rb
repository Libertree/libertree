require 'libertree/db'

########################
# FIXME: M4DBI wants us to connect to the db before defining models.  As model
# definitions are loaded when 'libertree/model' is required, we have to do this first.
Libertree::DB.load_config "#{File.dirname( __FILE__ ) }/../database.yaml"
Libertree::DB.dbh
#########################

require 'libertree/model'
require 'libertree/websocket-server'

conf = YAML.load( File.read("#{ File.dirname( __FILE__ ) }/../config.yaml") )

pid_dir = File.join( File.dirname(__FILE__), 'pids' )
if ! Dir.exists?(pid_dir)
  Dir.mkdir pid_dir
end
pid_file = File.join(pid_dir, 'websocket-server.pid')
File.open(pid_file, 'w') do |f|
  f.print Process.pid
end

puts "Starting websocket server (pid #{Process.pid})..."
Libertree::WebsocketServer.start(conf)
