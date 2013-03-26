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

      # TODO: add introduce command to set remote server's
      # - name_given
      # - domain
      # - contact
      # - public key
      VALID_COMMANDS =
        [ 'chat',
          'comment',
          'comment-delete',
          'comment-like',
          'comment-like-delete',
          'forest',
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
          "/iq/ns:libertree/ns:#{command}", :ns => 'libertree' do |stanza, xpath_result|

          remote_tree = Libertree::Model::Server[ :domain => stanza.from.domain ]

          # when we get messages from unknown remotes: abort connection
          if command != 'forest' &&
              ( ! remote_tree || remote_tree.forests.none?(&:local_is_member?) )
            response = error code: 'UNRECOGNIZED SERVER'
          else
            Libertree::Server.log "Received request: '#{command}' from #{stanza.from.stripped}"

            response =
              begin
                process command, xpath_result.first; nil
              rescue MissingParameter => e
                error code: 'MISSING PARAMETER', text: e.message
              rescue NotFound => e
                error code: 'NOT FOUND', text: e.message
              rescue InternalError => e
                error text: e.message
              end
          end

          respond to: stanza, with: response
          halt # stop further processing
        end
      end

      # catch all
      iq do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end
      message do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end
      presence do |stanza|
        respond to: stanza, with: (error code: 'UNKNOWN COMMAND')
      end

      def self.respond(opts)
        stanza = opts[:to]
        response = stanza.reply
        response.add_child opts[:with]  if opts[:with]
        write_to_stream response
      end

      def self.process(command, payload)
        parameters = xml_to_hash payload
        method = "rsp_#{command.gsub('-', '_')}".to_sym
        send  method, parameters
      end

      def self.error(opts={ code: 'ERROR' })
        Nokogiri::XML::Builder.new { |xml|
          xml.error {
            xml.code(opts[:code])
            xml.text_(opts[:text])  if opts[:text]
          }
        }.doc.root
      end
    end
  end
end
