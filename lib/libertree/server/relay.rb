module Libertree
  module Server
    module Relay
      include EM::P::LineText2

      def receive_line(line)
        return  if line.empty?

        # Blather::Stanza.parse does not throw a
        # clearly recognisable exception on parse failure,
        # so it has its own rescue block.
        begin
          stanza = Blather::Stanza.parse(line)
        rescue
          Libertree::Server.log_error "XMPP relay: not a valid stanza: #{line}"
          return
        end

        begin
          # TODO: expose client through proper interface
          c = Libertree::Server::Responder.send(:client)

          # TODO: timeout after n seconds
          c.write_with_handler(stanza) do |response|
            send_data response
          end
        rescue => e
          Libertree::Server.log_error "XMPP relay: #{e}"
        end
      end

      def post_init
        Libertree::Server.log "XMPP relay: process connected", "INFO"
      end

      def unbind
        Libertree::Server.log "XMPP relay: process disconnected", "INFO"
      end
    end
  end
end
