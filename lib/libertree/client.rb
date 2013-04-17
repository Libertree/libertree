require 'base64'
require 'openssl'
require 'socket'
require 'blather/client/client'
require_relative 'xml/helper'

module Libertree
  class Client
    include Libertree::XML::Helper

    # @param [Hash] params Paramaters Hash
    # @option params [String] :public_key A public RSA key, the partner of the private key
    # @option params [String] :private_key A private RSA key, the partner of the public key
    # @option params [String] :frontend_url_base The URL base of local frontend resources
    # @option params [String] :server_name a short identifier that other servers will display beside member usernames
    # @option params [String] :domain The XMPP component's JID domain (e.g. libertree.localhost.localdomain)
    # TODO: there's a lot of stuff we don't use
    def initialize( params = {} )
      @public_key = params[:public_key] or raise ":public_key required by Libertree::Client"
      @private_key = params[:private_key] or raise ":private_key required by Libertree::Client"
      @frontend_url_base = params[:frontend_url_base]
      @domain = params[:domain]
      @server_name = params[:server_name]
      @contact = params[:contact]
      @log = params.fetch(:log, $stdout)
      @log_identifier = params.fetch(:log_identifier, "pid #{Process.pid}")
      @socket = UNIXSocket.new params.fetch(:socket, '/tmp/libertree-relay')
    end

    # NOTE: This is different from the other req_* methods in that it needs to
    # be passed the target domain.  It builds a chat message directly, not an
    # IQ stanza payload.
    def req_chat(target, chat_message)
      stanza = Blather::Stanza::Message.new(
        "#{chat_message.recipient.username}@#{target}",
        chat_message.text,
        :chat
      )
      stanza.from = "#{chat_message.sender.username}@#{@domain}"
      stanza
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

    public

    # e.g.:
    #   request "lt.localhost", req_comment(what, ever)
    def request( target, params )
      log "REQUEST: >#{params.inspect}<"

      if params.class <= Blather::Stanza
        stanza = params
      else
        stanza = build_stanza( target, params )
      end

      msg = stanza.serialize(:save_with => Nokogiri::XML::Node::SaveOptions::AS_XML)+"\n"

      # write to socket and wait for response
      # TODO: handle timeout
      # TODO: adjust receive buffer
      @socket.send msg, 0
      response = @socket.recv 1024

      log "response: #{response}"
      # TODO: use xpath to get code; assume OK when response is empty
      #if response['code'] != 'OK'
      #  log_error "Not OK: #{response.inspect}"
      #end
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

    def req_introduce
      params = {
        'public_key' => @public_key
      }
      params.merge!('contact'     => @contact)      if @contact
      params.merge!('server_name' => @server_name)  if @server_name

      { 'introduce' => params }
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

    def req_member_delete(username)
      { 'member-delete' => { 'username' => username } }
    end

    # @param recipients [Array(Member)]
    def req_message(message, usernames)
      {
        'message' => {
          'username'   => message.sender.account.username,
          'recipients' => recipients.map { |recipient|
            {
              'username' => recipient.username,
              'public_key' => recipient.server ? recipient.server.public_key : @public_key,
            }
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
        'text'       => post.text
      }
      params.merge!('references' => references) unless references.empty?
      params.merge!('via' => post.via)  if post.via
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
