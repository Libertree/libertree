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

require 'libertree/server/gateway'

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

      # set @client for the `respond` helper method
      @client = client
      Gateway.init(client)

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
            Libertree::Server.log_debug stanza.inspect
            response = handle command, xpath_result.first
          end

          respond to: stanza, with: response
          halt # stop further processing
        end
      end

      # respond to all other libertree stanzas with an error
      iq "/iq/ns:libertree", :ns => 'urn:libertree' do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
        halt
      end

      message :chat? do |stanza|
        # Is the sender registered with the gateway?
        account = Libertree::Model::Account[ gateway_jid: stanza.from.to_s ]
        if account
          username = account.username
        else
          # when we get messages from unknown remotes: ignore for now
          # TODO: check recipient's roster instead
          @remote_tree = Libertree::Model::Server[ :domain => stanza.from.domain ]
          if ! @remote_tree || @remote_tree.forests.none?(&:local_is_member?)
            @client.write Blather::StanzaError.new(stanza, 'registration-required', :cancel)
            halt
          end
          username = stanza.from.node
        end

        params = {
          'username' => username,
          'recipient_username' => stanza.to.node,
          'text' => stanza.body
        }

        # handle chat message, ignore errors
        handle 'chat', params
        halt # stop further processing
      end

      # only log errors, never respond to errors!
      iq :type => :error do |stanza|
        Libertree::Server.log_error stanza
        halt
      end

      # catch all
      message do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end

      # handle fatal errors more nicely; default is to repeatedly raise an exception
      client.register_handler :stream_error, :name => :host_unknown do |err|
        Libertree::Server.log_error err.message
        Libertree::Server.quit
      end

      # Converts the XML payload to a hash and passes it
      # to the method indicated by the command string.
      # The method is run for its side effects.
      def self.process(command, payload)
        if payload.class <= Hash
          params = payload
        else
          params = xml_to_hash(payload)
          params = params.values.first  if params
        end
        method = "rsp_#{command.gsub('-', '_')}".to_sym
        send  method, params
      end

      # Processes a command with an XML nodeset of parameters.
      # Returns nil when no error occurred, otherwise returns
      # an XML fragment containing error code and error message.
      def self.handle(command, params)
        begin
          process command, params; nil # generate standard response
        rescue MissingParameterError => e
          error code: 'MISSING PARAMETER', text: e.message
        rescue NotFoundError => e
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
