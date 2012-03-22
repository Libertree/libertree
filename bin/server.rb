require 'libertree/server'

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end

Libertree::Server.run ARGV[0]
