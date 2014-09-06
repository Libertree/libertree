require 'libertree/console'

if ARGV[0].nil?
  $stderr.puts "no database configuration file given; assuming #{File.dirname( __FILE__ ) }/../database.yaml"
  db_config = "#{File.dirname( __FILE__ ) }/../database.yaml"
else
  db_config = ARGV[0]
end

Libertree::Console.init(db_config)
require_relative 'app'
ARGV.clear
Libertree::Console.start
