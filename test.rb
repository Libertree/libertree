require 'libertree/client'

key_pair = RCrypt.generate_key_pair

c = Libertree::Client.new( key_pair )
c.connect ARGV[0] || '127.0.0.1'
c.close

c = Libertree::Client.new( key_pair )
c.connect ARGV[0] || '127.0.0.1'
c.close
