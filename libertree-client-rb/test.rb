require 'libertree/client'
require 'openssl'

key = OpenSSL::PKey::RSA.new 2048

c = Libertree::Client.new( public_key: key.public_key.to_pem, private_key: key )
c.connect ARGV[0] || '127.0.0.1'
c.close

c = Libertree::Client.new( public_key: key.public_key.to_pem, private_key: key )
c.connect ARGV[0] || '127.0.0.1'
c.close
