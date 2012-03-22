require 'gpgme'

require 'libertree/connection'
require 'pp'

if ARGV[0].nil?
  puts "#{$0} <ASCII-armoured public GPG key>"
  exit 1
end

public_key = File.read(ARGV[0])

c = Libertree::Connection.new('localhost')
response = c.request('INTRODUCE', 'public_key' => public_key)

challenge_encrypted = response['challenge']
crypto = GPGME::Crypto.new(always_trust: true)
challenge_decrypted = crypto.decrypt(challenge_encrypted).read
pp c.request('AUTHENTICATE', 'response' => challenge_decrypted)

c.close
