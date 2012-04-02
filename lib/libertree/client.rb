require 'rcrypt'
require 'libertree/connection'

module Libertree
  class Client
    def initialize(key_pair)
      @public_key = key_pair[:public]
      @private_key = key_pair[:private]
    end

    def connect(remote_host)
      @conn = Libertree::Connection.new(remote_host)

      response = @conn.request('INTRODUCE', 'public_key' => @public_key)

      challenge_encrypted = response['challenge']
      if challenge_encrypted && response['code'] == 'OK'
        challenge_decrypted = RCrypt.decrypt(challenge_encrypted, @private_key)
        response = @conn.request('AUTHENTICATE', 'response' => challenge_decrypted)

        if response['code'] != 'OK'
          raise "Failed to connect: #{response.inspect}"
        end
      end

      if block_given?
        yield @conn
        @conn.close
      end
    end

    def close
      @conn.close
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
