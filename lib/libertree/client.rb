require 'base64'
require 'openssl'
require 'libertree/connection'

module Libertree
  class Client
    # @param [Hash] params Paramaters Hash
    # @option params [String] :public_key A public RSA key, the partner of the private key
    # @option params [String] :private_key A private RSA key, the partner of the public key
    # @option params [String] :avatar_url_base The base to prefix before member avatar_path when sending requests with an avatar_url parameter
    def initialize( params = {} )
      @public_key = params[:public_key] or raise ":public_key required by Libertree::Client"
      @private_key = params[:private_key] or raise ":private_key required by Libertree::Client"
      @avatar_url_base = params[:avatar_url_base]
    end

    def connect(remote_host)
      @conn = Libertree::Connection.new(remote_host)

      response = @conn.request('INTRODUCE', 'public_key' => @public_key)
      challenge_encrypted = response['challenge']

      if challenge_encrypted && response['code'] == 'OK'
        key = OpenSSL::PKey::RSA.new @private_key
        challenge_decrypted = key.private_decrypt(Base64.decode64(challenge_encrypted), OpenSSL::PKey::RSA::PKCS1_OAEP_PADDING)
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

    # ---------

    def req_comment(comment)
      post = comment.post
      server = post.member.server
      public_key = server ? server.public_key : @public_key
      @conn.request(
        'COMMENT',
        'id'         => comment.id,
        'post_id'    => post.public_id,
        'public_key' => public_key,
        'username'   => comment.member.username,
        'text'       => comment.text
      )
    end

    def req_comment_delete(comment_id)
      @conn.request(
        'COMMENT-DELETE',
        'id' => comment_id
      )
    end

    def req_comment_like(like)
      server = like.comment.member.server
      public_key = server ? server.public_key : @public_key
      @conn.request(
        'COMMENT-LIKE',
        'id'         => like.id,
        'comment_id' => like.comment.public_id,
        'public_key' => public_key,
        'username'   => like.member.username,
      )
    end

    def req_comment_like_delete(like_id)
      @conn.request(
        'COMMENT-LIKE-DELETE',
        'id' => like_id
      )
    end

    def req_member(member)
      @conn.request(
        'MEMBER',
        'username'   => member.username,
        'avatar_url' => "#{@avatar_url_base}#{member.avatar_path}",
        'profile' => {
          'name_display' => member.profile.name_display,
          'description'  => member.profile.description,
        }
      )
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

    def req_post_delete(post_id)
      @conn.request(
        'POST-DELETE',
        'id' => post_id
      )
    end

    def req_post_like(like)
      server = like.post.member.server
      public_key = server ? server.public_key : @public_key
      @conn.request(
        'POST-LIKE',
        'id'         => like.id,
        'post_id'    => like.post.public_id,
        'public_key' => public_key,
        'username'   => like.member.username,
      )
    end

    def req_post_like_delete(like_id)
      @conn.request(
        'POST-LIKE-DELETE',
        'id' => like_id
      )
    end
  end
end
