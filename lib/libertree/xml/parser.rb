require 'blather'

module Libertree
  module XML
    # The following class implementation has been adapted
    # from Blather::Stream::Parser
    class Parser < Nokogiri::XML::SAX::Document
      def initialize(caller)
        @caller = caller
        @parser = Nokogiri::XML::SAX::PushParser.new self
        @parser.options =
          Nokogiri::XML::ParseOptions::DEFAULT_XML |
          Nokogiri::XML::ParseOptions::NOENT |
          Nokogiri::XML::ParseOptions::NONET
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
          # NOTE: to_stanza results in a call to Blather::Stanza::Iq.new, which
          #       calls next_id (without using the new id). That is why the next
          #       stanza that is created appears to have skipped an id.
          #       This is harmless but a little confusing.
          @caller.handle_stanza @current.to_stanza
        end
      end

      def characters(chars='')
        if @current
          @current << Nokogiri::XML::Text.new(chars, @current.document)
        end
      end

      def warning(msg)
        if @caller.respond_to? :log
          @caller.log "parser: #{msg}"
        else
          $stderr.puts "parser: #{msg}"
        end
      end

      def error(msg)
        raise ParseError.new(msg)
      end
    end
  end
end
