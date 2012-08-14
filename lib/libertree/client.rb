require 'base64'
require 'openssl'
require 'libertree/connection'

module Libertree
  class Client
    # @param [Hash] params Paramaters Hash
    # @option params [String] :public_key A public RSA key, the partner of the private key
    # @option params [String] :private_key A private RSA key, the partner of the public key
    # @option params [String] :frontend_url_base The URL base of local frontend resources
    # @option params [String] :server_name a short identifier that other servers will display beside member usernames
    def initialize( params = {} )
      @public_key = params[:public_key] or raise ":public_key required by Libertree::Client"
      @private_key = params[:private_key] or raise ":private_key required by Libertree::Client"
      @frontend_url_base = params[:frontend_url_base]
      @server_ip = params[:server_ip]
      @server_name = params[:server_name]
      @log = params[:log] || $stdout
      @log_identifier = params[:log_identifier] || "pid #{Process.pid}"

      @intro_params = { 'public_key' => @public_key, }
      if @server_name
        @intro_params['name'] = @server_name
      end
    end

    def connect(remote_host)
      @conn = Libertree::Connection.new(host: remote_host, log: @log, log_identifier: @log_identifier)

      response = @conn.request('INTRODUCE', @intro_params)
      if response.nil?
        raise "No response to INTRODUCE"
      end

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

    def req_chat(chat_message)
      @conn.request(
        'CHAT',
        'username'           => chat_message.sender.username,
        'recipient_username' => chat_message.recipient.username,
        'text'               => chat_message.text
      )
    end

    def req_comment(comment, references={})
      post = comment.post
      server = post.member.server
      public_key = server ? server.public_key : @public_key
      params = {
        'id'         => comment.id,
        'post_id'    => post.public_id,
        'public_key' => public_key,
        'username'   => comment.member.username,
        'text'       => comment.text
      }
      params.merge!('references' => references) unless references.empty?
      @conn.request('COMMENT', params)
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

    def req_forest(forest)
      return  if ! forest.local_is_member?
      @conn.request(
        'FOREST',
        'id'    => forest.id,
        'name'  => forest.name,
        'trees' => forest.trees.map { |t|
          { 'ip' => t.ip }
        } + [ { 'ip' => @server_ip } ]
      )
    end

    def req_member(member)
      params = {
        'username' => member.username,
        'profile'  => {
          'name_display' => member.profile.name_display,
          'description'  => member.profile.description,
        }
      }
      if member.avatar_path
        params.merge!('avatar_url' => "#{@frontend_url_base}#{member.avatar_path}")
      end
      @conn.request('MEMBER', params)
    end

    def req_message(message, usernames)
      @conn.request(
        'MESSAGE',
        'username'   => message.sender.account.username,
        'recipients' => usernames.map { |un|
          { 'username' => un }
        },
        'text'       => message.text
      )
    end

    def req_post(post,references={})
      params = {
        'username'   => post.member.username,
        'id'         => post.id,
        'visibility' => post.visibility,
        'public'     => true, # FIXME: backwards compatibility
        'text'       => post.text
      }
      params.merge!('references' => references) unless references.empty?
      @conn.request('POST', params)
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
