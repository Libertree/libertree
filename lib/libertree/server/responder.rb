require 'libertree/server/responder/helper'

require 'libertree/server/responder/chat'
require 'libertree/server/responder/comment'
require 'libertree/server/responder/comment-like'
require 'libertree/server/responder/forest'
require 'libertree/server/responder/member'
require 'libertree/server/responder/message'
require 'libertree/server/responder/pool'
require 'libertree/server/responder/pool-post'
require 'libertree/server/responder/post'
require 'libertree/server/responder/post-like'

module Libertree
  module Server
    module Responder
      extend Blather::DSL
      extend Helper

      extend Chat
      extend Comment
      extend CommentLike
      extend Forest
      extend Member
      extend Message
      extend Pool
      extend PoolPost
      extend Post
      extend PostLike

      when_ready {
        puts "\nLibertree started."
        puts "Send messages to #{jid.stripped}."
      }

      VALID_COMMANDS =
        [ 'comment',
          'comment-delete',
          'comment-like',
          'comment-like-delete',
          'forest',
          'introduce',
          'member',
          'member-delete',
          'message',
          'pool',
          'pool-delete',
          'pool-post',
          'pool-post-delete',
          'post',
          'post-delete',
          'post-like',
          'post-like-delete',
        ]

      VALID_COMMANDS.each do |command|
        client.register_handler :iq,
          "/iq/ns:libertree/ns:#{command}", :ns => 'urn:libertree' do |stanza, xpath_result|

          @remote_domain = stanza.from.domain
          @remote_tree = Libertree::Model::Server[ :domain => @remote_domain ]

          # when we get messages from unknown remotes: abort connection
          if ( ! ['forest', 'introduce'].include? command ) &&
              ( ! @remote_tree || @remote_tree.forests.none?(&:local_is_member?) )
            response = error code: 'UNRECOGNIZED SERVER'
          else
            Libertree::Server.log "Received request: '#{command}' from #{stanza.from.stripped}"
            response = handle command, xpath_result.first
          end

          respond to: stanza, with: response
          halt # stop further processing
        end
      end

      message :chat? do |stanza|
        # when we get messages from unknown remotes: ignore for now
        # TODO: check recipient's roster instead
        @remote_tree = Libertree::Model::Server[ :domain => stanza.from.domain ]
        if ! @remote_tree || @remote_tree.forests.none?(&:local_is_member?)
          halt
        end

        params = {
          'username' => stanza.from.node,
          'recipient_username' => stanza.to.node,
          'text' => stanza.body
        }

        # handle chat message, ignore errors
        handle 'chat', params
        halt # stop further processing
      end

      # catch all
      iq do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end
      message do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end
      presence do |stanza|
        # TODO: not yet implemented
        # Presence stanzas are used to indicate chat availability for
        # remote users.
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end

      # handle fatal errors more nicely; default is to repeatedly raise an exception
      client.register_handler :stream_error, :name => :host_unknown do |err|
        Libertree::Server.log_error err.message
        Libertree::Server.quit
      end

      # Packs an optional XML fragment (opts[:with]) in a standard XMPP reply
      # to the provided stanza (opts[:to]) and sends out the reply stanza.
      def self.respond(opts)
        stanza = opts[:to]
        response = stanza.reply
        response.add_child opts[:with]  if opts[:with]
        write_to_stream response
      end

      # Converts the XML payload to a hash and passes it
      # to the method indicated by the command string.
      # The method is run for its side effects.
      def self.process(command, payload)
        parameters = if payload.class <= Hash
                       payload
                     else
                       xml_to_hash payload
                     end
        method = "rsp_#{command.gsub('-', '_')}".to_sym
        send  method, parameters
      end

      # Generates an error XML fragment given an optional
      # error code and an optional error message
      def self.error(opts={ code: 'ERROR' })
        Nokogiri::XML::Builder.new { |xml|
          xml.error {
            xml.code(opts[:code])
            xml.text_(opts[:text])  if opts[:text]
          }
        }.doc.root
      end

      # Processes a command with an XML nodeset of parameters.
      # Returns nil when no error occurred, otherwise returns
      # an XML fragment containing error code and error message.
      def self.handle(command, params)
        begin
          process command, params; nil # generate standard response
        rescue MissingParameter => e
          error code: 'MISSING PARAMETER', text: e.message
        rescue NotFound => e
          error code: 'NOT FOUND', text: e.message
        rescue InternalError => e
          Libertree::Server.log_error e.message
          error text: e.message
        end
      end

      # expose client (and with it the XMPP connection) to
      # the XMPP relay
      def self.connection
        client
      end
    end
  end
end
