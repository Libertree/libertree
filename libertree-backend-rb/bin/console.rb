require 'irb'
require 'libertree/db'
require 'libertree/client'

########################
# FIXME: M4DBI wants us to connect to the db before defining models.  As model
# definitions are loaded when 'libertree/server' is required, we have to do
# this first.
Libertree::DB.load_config "#{File.dirname( __FILE__ ) }/../database.yaml"
Libertree::DB.dbh
########################

require 'libertree/server'
require_relative '../lib/jobs'

LM = Libertree::Model

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end

conf = YAML.load( File.read(ARGV[0]) )

# it's a little weird that all this client initialisation
# code is in lib/jobs, not in the client lib.
Jobs::Request.init_client_conf(conf)
@c = Jobs::Request.client
banner=<<EOF
== Libertree console ==

make requests like this:
    @c.request('some.target.domain', @c.req_introduce)

or like that:
    Jobs::Request::CHAT.perform({'chat_message_id' => 10})
EOF

puts banner
ARGV.clear
IRB.start
