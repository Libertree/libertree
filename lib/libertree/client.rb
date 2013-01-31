require 'base64'
require 'openssl'
require 'socket'
require 'blather/client/client'

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
      @log = params.fetch(:log, $stdout)
      @log_identifier = params.fetch(:log_identifier, "pid #{Process.pid}")
      @socket = UNIXSocket.new params.fetch(:socket, '/tmp/libertree-relay')
    end

    def req_chat(chat_message)
      @conn.request(
        'CHAT',
        'username'           => chat_message.sender.username,
        'recipient_username' => chat_message.recipient.username,
        'text'               => chat_message.text
      )
    end


    private

    def log(s, level = nil)
      t = Time.now.strftime("%Y-%m-%d %H:%M:%S")
      if level
        l = "#{level} "
      end

      @log.puts "[#{t}] (#{@log_identifier}) #{l}#{s}"
    end

    def log_error(s)
      log s, 'ERROR'
    end

    def build_stanza( target, command, params )
      stanza = Blather::Stanza::Iq.new(:get, target)
      content = "<libertree><#{command.downcase}>#{params_to_xml(params)}</#{command.downcase}></libertree>"
      stanza.add_child content
      stanza
    end

    def params_to_xml(elem)
      case elem
      when Array
        # TODO: this squashes arrays
        # before:
        #   :abc => [1,2,3,4]
        # after:
        #   <abc>1234</abc>
        #
        # what this be better?
        #   <abc>
        #     <item>1</item>
        #     <item>2</item>
        #     <item>3</item>
        #     <item>4</item>
        #   </abc>
        elem.flat_map {|i| params_to_xml(i) }.join('')
      when Hash
        elem.reduce("") do |acc,i|
          acc << ("<#{i[0]}>"+ params_to_xml(i[1]) +"</#{i[0]}>")
          acc
        end
      else
        # TODO: encode entities
        elem.to_s
      end
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
      { 'comment' => params }
    end

    def req_comment_delete(comment_id)
      { 'comment-delete' => { 'id' => comment_id } }
    end

    def req_comment_like(like)
      server = like.comment.member.server
      public_key = server ? server.public_key : @public_key
      {
        'comment-like' => {
          'id'         => like.id,
          'comment_id' => like.comment.public_id,
          'public_key' => public_key,
          'username'   => like.member.username,
        }
      }
    end

    def req_comment_like_delete(like_id)
      { 'comment-like-delete' => { 'id' => like_id } }
    end

    def req_forest(forest)
      # TODO: changed IP address to domain
      return  if ! forest.local_is_member?
      {
        'forest' => {
          'id'    => forest.id,
          'name'  => forest.name,
          'trees' => forest.trees.map { |t|
            { 'ip' => t.ip }
          } + [ { 'ip' => @server_ip } ]
        }
      }
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
      { 'member' => params }
    end

    def req_message(message, usernames)
      {
        'message' => {
          'username'   => message.sender.account.username,
          'recipients' => usernames.map { |un|
            { 'username' => un }
          },
          'text'       => message.text
        }
      }
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
      { 'post' => params }
    end

    def req_pool(pool)
      {
        'pool' => {
          'username' => pool.member.username,
          'id'       => pool.id,
          'name'     => pool.name,
        }
      }
    end

    def req_pool_delete(pool)
      {
        'pool-delete' => {
          'username' => pool.member.username,
          'id'       => pool.id,
        }
      }
    end

    def req_pool_post(pool, post)
      server = post.member.server
      public_key = server ? server.public_key : @public_key
      {
        'pool-post' => {
          'username'   => pool.member.username,
          'pool_id'    => pool.id,
          'post_id'    => post.public_id,
          'public_key' => public_key,
        }
      }
    end

    def req_pool_post_delete(pool, post)
      server = post.member.server
      public_key = server ? server.public_key : @public_key
      {
        'pool-post-delete' => {
          'username'   => pool.member.username,
          'pool_id'    => pool.id,
          'post_id'    => post.public_id,
          'public_key' => public_key,
        }
      }
    end

    def req_post_delete(post_id)
      { 'post-delete' => { 'id' => post_id } }
    end

    def req_post_like(like)
      server = like.post.member.server
      public_key = server ? server.public_key : @public_key
      {
        'post-like' => {
          'id'         => like.id,
          'post_id'    => like.post.public_id,
          'public_key' => public_key,
          'username'   => like.member.username,
        }
      }
    end

    def req_post_like_delete(like_id)
      { 'post-like-delete' => { 'id' => like_id } }
    end
  end
end
