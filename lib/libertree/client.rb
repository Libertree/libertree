require 'gpgme'
require 'libertree/connection'

module Libertree
  class Client
    def initialize(public_key)
      @public_key = public_key
      @crypto = GPGME::Crypto.new(always_trust: true)
    end

    def connect(remote_host)
      @conn = Libertree::Connection.new(remote_host)

      response = @conn.request('INTRODUCE', 'public_key' => @public_key)
      challenge_encrypted = response['challenge']
      challenge_decrypted = @crypto.decrypt(challenge_encrypted).read
      response = @conn.request('AUTHENTICATE', 'response' => challenge_decrypted)

      if response['code'] != 'OK'
        raise "Failed to connect: #{response.inspect}"
      end

      if block_given?
        yield @conn
      end
    end

    def req_post(post)
      @conn.request(
        'POST',
        'username' => post.member.username,
        'id'       => post.id,
        'public'   => true,
        'text'     => post.text
      )
    end
  end
end
