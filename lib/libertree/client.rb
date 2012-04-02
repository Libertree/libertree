require 'rcrypt'
require 'libertree/connection'

module Libertree
  class Client
    def initialize( params = {} )
      @public_key = params[:public_key] or raise ":public_key required by Libertree::Client"
      @private_key = params[:private_key] or raise ":private_key required by Libertree::Client"
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
