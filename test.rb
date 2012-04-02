require 'libertree/client'

key_pair = RCrypt.generate_key_pair

c = Libertree::Client.new( public_key: key_pair[:public], private_key: key_pair[:private] )
c.connect ARGV[0] || '127.0.0.1'
c.close

c = Libertree::Client.new( public_key: key_pair[:public], private_key: key_pair[:private] )
c.connect ARGV[0] || '127.0.0.1'
c.close
