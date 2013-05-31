module Libertree
  module Server
    # Whatever is received through the relay socket is parsed as an
    # XMPP stanza and passed through to the XMPP server. Any response
    # from the XMPP server is passed back through the socket to the
    # relay client (e.g. the job processor) to consume.
    module Relay

      # The following class implementation has been adapted
      # from Blather::Stream::Parser
      class Parser < Nokogiri::XML::SAX::Document
        def initialize(caller)
          @caller = caller
          @parser = Nokogiri::XML::SAX::PushParser.new self
          @parser.options =
            Nokogiri::XML::ParseOptions::DEFAULT_XML |
            Nokogiri::XML::ParseOptions::NOENT
          @current = nil
          @namespaces = {}
          @namespace_definitions = []
        end

        def receive_data(string)
          @parser << string
        end

        def start_element_namespace(elem, attrs, prefix, uri, namespaces)
          args = [elem]
          args << @current.document  if @current
          node = Blather::XMPPNode.new *args
          node.document.root = node  unless @current

          attrs.each do |attr|
            node[attr.localname] = attr.value
          end

          ns_keys = namespaces.map { |pre, href| pre }
          @namespace_definitions.push []
          namespaces.each do |pre, href|
            next  if @namespace_definitions.flatten.include?(@namespaces[[pre, href]])
            ns = node.add_namespace(pre, href)
            @namespaces[[pre, href]] ||= ns
          end

          if prefix && ! ns_keys.include?(prefix)
            @namespaces[[prefix, uri]] ||= node.add_namespace(prefix, uri)
          end
          node.namespace = @namespaces[[prefix, uri]]

          @current << node  if @current
          @current = node
        end

        def end_element_namespace(elem, prefix, uri)
          if @current.parent != @current.document
            # travel up the stack
            @namespace_definitions.pop
            @current = @current.parent
          else
            # handle stanza and implode
            @caller.handle_stanza @current.to_stanza
          end
        end

        def characters(chars='')
          if @current
            @current << Nokogiri::XML::Text.new(chars, @current.document)
          end
        end

        def warning(msg)
          Libertree::Server.log "XMPP relay: #{msg}", "WARN"
        end

        def error(msg)
          raise ParseError.new(msg)
        end
      end

      def receive_data(string)
        @parser.receive_data string
      rescue ParseError => e
        Libertree::Server.log_error "XMPP relay parse: #{e}"
      end

      def post_init
        Libertree::Server.log "XMPP relay: process connected", "INFO"
        @parser = Parser.new self
      end

      def unbind
        Libertree::Server.log "XMPP relay: process disconnected", "INFO"
      end

      def handle_stanza(stanza)
        Libertree::Server.log_debug "XMPP relay: relaying stanza: #{stanza.inspect}"
        # throw away the old parser
        @parser = Parser.new self

        begin
          # TODO: expose client through proper interface
          c = Libertree::Server::Responder.send(:client)

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
