require 'libertree/console'
require_relative '../../libertree-client-rb/lib/libertree/client'

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

Libertree::Console.init(db_config)

require 'libertree/server'
require_relative '../lib/jobs'

LM = Libertree::Model
conf = YAML.load( File.read(ARGV[0]) )

# it's a little weird that all this client initialisation
# code is in lib/jobs, not in the client lib.
Jobs::Request.init_client_conf(conf)
@c = Jobs::Request.client
banner=<<EOF

make requests like this:
    @c.request('some.target.domain', @c.req_introduce)

or like that:
    Jobs::Request::CHAT.perform({'chat_message_id' => 10})
EOF

puts banner
ARGV.clear
Libertree::Console.start
