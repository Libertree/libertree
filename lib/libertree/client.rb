require 'base64'
require 'openssl'
require 'socket'
require 'timeout'
require 'blather/client/client'
require_relative 'xml/helper'
require_relative 'xml/parser'

module Libertree
  class Client
    include Libertree::XML::Helper

    # @param [Hash] params Paramaters Hash
    # @option params [String] :private_key A private RSA key, the partner of the public key
    # @option params [String] :frontend_url_base The URL base of local frontend resources
    # @option params [String] :server_name a short identifier that other servers will display beside member usernames
    # @option params [String] :domain The XMPP component's JID domain (e.g. libertree.localhost.localdomain)
    # @option params [String] :contact The administrator's email address
    def initialize( params = {} )
      private_key = params[:private_key] or raise ":private_key required by Libertree::Client"
      @public_key = private_key.public_key.to_pem
      @contact = params[:contact] or raise ":contact required by Libertree::Client"
      @domain = params[:domain] or raise ":domain required by Libertree::Client"

      @frontend_url_base = params[:frontend_url_base]
      @server_name = params[:server_name]

      @log = params[:log] || $stdout
      @log_identifier = params[:log_identifier] || "pid #{Process.pid}"
      @socket_file = params[:socket] || '/tmp/libertree-relay'
      @timeout = 90 # seconds

      connect
      listener
    end

    def connect
      begin
        @socket = UNIXSocket.new @socket_file
      rescue Errno::ECONNREFUSED, Errno::ENOENT => e
        log_error "#{e.message}, reconnecting"
        sleep 1
        retry
      end
    end

    def listener
      @replies = {}
      @expected = {}

      Thread.fork do
        @parser = Libertree::XML::Parser.new self
        loop do
          readable, _, _ = IO.select([@socket], nil, nil, 0.1)

          if readable
            chunk = @socket.recv(1024)
            connect  if chunk.empty?

            begin
              # we may not feed the whole chunk to the parser at once.
              # As soon as the parser reaches the end of the stanza it will
              # discard whatever else is in the queue.
              chunk.each_char do |char|
                @parser.receive_data char
              end
            rescue ParseError => e
              log_error "parse error: #{e}"
            end
          end
        end
      end
    end

    # NOTE: This is different from the other req_* methods.
    # It builds a chat message stanza, not an Iq stanza payload,
    # it derives the JID from the object and it writes directly
    # to the stream.
    def req_chat(chat_message)
      recipient = chat_message.recipient
      stanza = Blather::Stanza::Message.new(
        "#{recipient.username}@#{recipient.server.domain}",
        chat_message.text,
        :chat
      )
      stanza.from = "#{chat_message.sender.username}@#{@domain}"
      write_out stanza, false
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

    def write_out(stanza, wait_for_reply=true)
      if wait_for_reply
        key = "#{stanza.id}:#{stanza.to}"
        @expected[key] = Time.now
      end

      msg = stanza.serialize(:save_with => Nokogiri::XML::Node::SaveOptions::AS_XML)

      # write to socket
      begin
        @socket.send msg, 0

        # TODO: use a Queue instead of this weird cross-thread hash?
        if wait_for_reply
          begin
            Timeout.timeout(@timeout) do
              loop {
                if reply = @replies[key]
                  @replies.delete key
                  return reply
                end
                sleep 0.1
                # TODO: WTF? Without this, the listener will never see that the socket is readable
                @socket.send " ", 0
              }
            end
          rescue Timeout::Error
            @expected.delete key
            log_error "(timeout)"
            raise
          end
        else
          stanza
        end
      rescue Errno::EPIPE => e
        log_error "#{e.message}, reconnecting"
        sleep 1
        connect
        retry
      end
    end

    public

    # called by the parser
    def handle_stanza(stanza)
      # throw away the old parser
      @parser = Libertree::XML::Parser.new self

      # if this stanza is a reply to one of the stanzas we sent out before
      # record the reply
      key = "#{stanza.id}:#{stanza.from}"
      if @expected[key]
        log "got response after #{Time.now - @expected[key]} seconds"
        @expected.delete key
        @replies[key] = stanza
      end
    end

    def ping( target )
      stanza = Blather::Stanza::Iq::Ping.new(:get, target)
      write_out stanza
    end

    # e.g.:
    #   request "lt.localhost", req_comment(what, ever)
    def request( target, params )
      if params.nil? || params.empty?
        log_error "request: called with empty parameters"
        return
      end

      log "REQUEST: >#{params.inspect}<"

      stanza = build_stanza( target, params )
      response = write_out stanza

      # when the response is empty everything is okay
      if ! response.xpath("//error").empty?
        log_error "Not OK: #{response}"
      else
        log "response OK: #{response}"
      end

      response
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
      return  if ! forest.local_is_member?
      {
        'forest' => {
          'id'    => forest.id,
          'name'  => forest.name,
          'trees' => forest.trees.map { |t|
            { 'domain' => t.domain }
          } + [ { 'domain' => @domain } ]
        }
      }
    end

    def req_introduce
      params = {
        'public_key' => @public_key,
        'contact'    => @contact
      }
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
