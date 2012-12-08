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

      include Chat
      include Comment
      include CommentLike
      include Forest
      include Member
      include Message
      include Pool
      include PoolPost
      include Post
      include PostLike

      when_ready {
        puts "\nLibertree started."
        puts "Send messages to #{jid.stripped}."
      }

      [ 'chat',
        'comment',
        'comment-delete',
        'comment-like',
        'comment-like-delete',
        'forest',
        'member',
        'message',
        'pool',
        'pool-delete',
        'pool-post',
        'pool-post-delete',
        'post',
        'post-delete',
        'post-like',
        'post-like-delete',
      ].each do |command|
        client.register_handler :iq,
          "/iq/ns:libertree/ns:#{command}", :ns => 'libertree' do |stanza, xpath_result|

          server = stanza.from.domain
          # TODO: look up the domain in the db
          #server = Libertree::Model::Server[ :domain => stanza.from.domain ]

          # when we get messages from unknown servers: abort connection
          if ! server
            response = error text: 'Unknown server.'
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

      def self.respond(opts)
        stanza = opts[:to]
        response = stanza.reply
        response.add_child opts[:with]  if opts[:with]

        puts response
        write_to_stream response
      end

      def self.process(command, payload)
        parameters = payload.children.reduce({}) {|acc, n| acc[n.name] = n.text; acc}
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

      # @param [Hash] params A Hash.
      # @param [Array] required_parameters The keys which are required.
      # @return [Array] The keys whose values are missing.
      def missing_parameters(params, *required_parameters)
        missing = []
        required_parameters.each do |rp|
          if params[rp].nil? || params[rp].respond_to?(:empty?) && params[rp].empty?
            missing << rp
          end
        end
        missing
      end

      # Calls #missing_parameters.  If any parameters are missing, raises
      # MissingParameters exception with the first missing parameter as message.
      # @return [nil] when there are no missing parameters
      # @raises [MissingParameter] when there is a missing parameter
      def require_parameters(*args)
        mp = missing_parameters(*args)
        if mp[0]
          fail MissingParameter, mp[0], nil
        end
      end

      def assert(obj, msg)
        if obj.nil?
          fail NotFound, msg, nil
        end
      end
    end
  end
end
