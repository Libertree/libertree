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
        retrying = false
        log "Connected to #{@socket_file}"
      rescue Errno::ECONNREFUSED, Errno::ENOENT => e
        log_error "#{e.message}, reconnecting..."  unless retrying
        retrying = true
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
          next  if ! readable

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
        if ! wait_for_reply
          stanza
        else
          begin
            Timeout.timeout(@timeout) do
              loop {
                reply = @replies.delete(key)
                return reply  if reply
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
        end
      rescue Errno::EPIPE => e
        log_error "#{e.message}, reconnecting"
        sleep 1
        connect
        retry
      end
    end

    # TODO: use atom:link elements with rel=related for references.
    # example for a reference to a post with id 1234 and origin
    # oak.elephly.net that is referenced in the post text as
    # "http://maple.lt.net/posts/show/5678" (the sender is
    # maple.lt.net):
    #
    #    <link rel="related"
    #          href="xmpp:oak.elephly.net?;node=/posts;item=1234"
    #          libertree-match="http://maple.lt.net/posts/show/5678" />
    #
    # Here is another example with a reference to a comment.  The
    # comment's sender-local id is 999 and it's origin is again oak:
    #
    #    <link rel="related"
    #          href="xmpp:oak.elephly.net?;node=/comments;item=1234"
    #          libertree-match="http://maple.lt.net/posts/5678/comments/999" />
    #
    # Note that the comment's origin and origin id are sufficient to
    # allow the receiver to rebuild the full URL.  The post id does
    # not have to be sent along.

    # See for info about atom:link:
    #  <https://tools.ietf.org/html/rfc4287#section-4.2.7>
    # See for PubSub IRIs:
    #  <XEP-0060> and <RFC5122>

    def build_references_xml(refs, x)
      x.references  {
        refs.each do |ref|
          x.reference {
            x.match  ref['match']
            ['post', 'comment', 'spring'].each do |type|
              if ref[type]
                # Nokogiri uses `comment` for XML comments
                type_ = type == 'comment' ? 'comment_' : type
                x.send(type_) {
                  x.url     ref[type]['url']
                  x.id_     ref[type]['id']
                  x.origin  ref[type]['origin']  if ref[type]['origin']
                }
              end
            end
          }
        end
      }
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

    def req_comment(comment, references=[])
      post = comment.post
      server = post.member.server
      origin = server ? server.domain : @domain
      xml {|x|
        x.comment_ {
          x.id_         comment.id
          x.uid         comment.guid
          x.post_id     post.public_id
          x.origin      origin
          x.send('thr:in-reply-to', {ref: post.guid})
          x.username    comment.member.username
          x.text_       comment.text
          unless references.empty?
            build_references_xml references, x
          end
        }
      }
    end

    def req_comment_delete(comment_id)
      xml {|x| x.send('comment-delete') { x.id_  comment_id } }
    end

    def req_comment_like(like)
      server = like.comment.member.server
      origin = server ? server.domain : @domain
      xml {|x|
        x.send('comment-like') {
          x.id_         like.id
          x.send('thr:in-reply-to', {ref: like.comment.guid})
          x.comment_id  like.comment.public_id
          x.origin      origin
          x.username    like.member.username
        }
      }
    end

    def req_comment_like_delete(like_id)
      xml {|x| x.send('comment-like-delete') { x.id_  like_id } }
    end

    def req_forest(forest)
      return  if ! forest.local_is_member?  #TODO
      domains = forest.trees.map(&:domain) + [@domain]
      xml {|x|
        x.forest {
          x.id_    forest.id
          x.name   forest.name
          x.trees  {
            for domain in domains
              x.domain domain
            end
          }
        }
      }
    end

    def req_introduce
      xml {|x|
        x.introduce {
          x.public_key   @public_key
          x.contact      @contact
        }
      }
    end

    def req_member(member)
      xml {|x|
        x.member {
          x.username  member.username
          x.avatar_url  "#{@frontend_url_base}#{member.avatar_path}"  if member.avatar_path
          x.gateway_jid  member.account.gateway_jid  if member.account.gateway_jid
          x.profile {
            x.name_display  member.profile.name_display
            x.description   member.profile.description
          }
        }
      }
    end

    def req_member_delete(username)
      xml {|x| x.send('member-delete') { x.username  username } }
    end

    # @param recipients [Array(Member)]
    def req_message(message, recipients)
      xml {|x|
        x.message {
          x.username    message.sender.account.username
          x.id_         message.id
          x.text_       message.text
          x.recipients  {
            for member in recipients
              x.recipient {
                x.username  member.username
                x.origin    member.server ? member.server.domain : @domain
              }
            end
          }
        }
      }
    end

    def req_post(post, references=[])
      xml {|x|
        x.post {
          x.username    post.member.username
          x.id_         post.id
          x.uid         post.guid
          x.visibility  post.visibility
          x.text_       post.text
          x.via         post.via  if post.via
          unless references.empty?
            build_references_xml references, x
          end
        }
      }
    end

    def req_pool(pool)
      xml {|x|
        x.pool {
          x.username  pool.member.username
          x.id_       pool.id
          x.name      pool.name
        }
      }
    end

    def req_pool_delete(pool)
      xml {|x|
        x.send('pool-delete') {
          x.username  pool.member.username
          x.id_       pool.id
        }
      }
    end

    def req_pool_post(pool, post)
      server = post.member.server
      origin = server ? server.domain : @domain
      xml {|x|
        x.send('pool-post') {
          x.username  pool.member.username
          x.pool_id   pool.id
          x.post_id   post.public_id
          x.origin    origin
        }
      }
    end

    def req_pool_post_delete(pool, post)
      server = post.member.server
      origin = server ? server.domain : @domain
      xml {|x|
        x.send('pool-post-delete') {
          x.username  pool.member.username
          x.pool_id   pool.id
          x.post_id   post.public_id
          x.origin    origin
        }
      }
    end

    def req_post_delete(post_id)
      xml {|x|
        x.send('post-delete') { x.id_ post_id }
      }
    end

    def req_post_like(like)
      server = like.post.member.server
      origin = server ? server.domain : @domain
      xml {|x|
        x.send('post-like') {
          x.id_       like.id
          x.send('thr:in-reply-to', {ref: like.post.guid})
          x.post_id   like.post.public_id
          x.origin    origin
          x.username  like.member.username
        }
      }
    end

    def req_post_like_delete(like_id)
      xml {|x| x.send('post-like-delete') { x.id_  like_id } }
    end
  end
end
