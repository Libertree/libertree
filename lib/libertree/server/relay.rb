require 'libertree/client'

module Libertree
  module Server
    # Whatever is received through the relay socket is parsed as an
    # XMPP stanza and passed through to the XMPP server. Any response
    # from the XMPP server is passed back through the socket to the
    # relay client (e.g. the job processor) to consume.
    module Relay

      def receive_data(chunk)
        # we may not feed the whole chunk to the parser at once.
        # As soon as the parser reaches the end of the stanza it will
        # discard whatever else is in the queue.
        chunk.each_char do |char|
          @parser.receive_data char
        end
      rescue ParseError => e
        Libertree::Server.log_error "XMPP relay parse: #{e}"
        @parser = Libertree::XML::Parser.new self
      end

      def post_init
        Libertree::Server.log_debug "XMPP relay: process connected"
        @parser = Libertree::XML::Parser.new self
      end

      def unbind
        Libertree::Server.log_debug "XMPP relay: process disconnected"
      end

      def handle_stanza(stanza)
        Libertree::Server.log_debug "XMPP relay: relaying stanza: #{stanza.inspect}"
        # throw away the old parser
        @parser = Libertree::XML::Parser.new self

        begin
          c = Libertree::Server::Responder.connection

          # TODO: timeout after n seconds
          c.write_with_handler(stanza) do |response|
            send_data response
          end
        rescue => e
          Libertree::Server.log_error "XMPP relay: #{e}; stanza: #{stanza.inspect}"
        end
      end
    end
  end
end
