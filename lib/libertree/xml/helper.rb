require 'blather/stanza'

module Libertree
  module XML
    module Helper
      def build_stanza(target, payload)
        stanza = Blather::Stanza::Iq.new(:set, target)
        content = "<libertree xmlns=\"urn:libertree\">#{payload}</libertree>"
        stanza.add_child content
        stanza
      end

      def xml &block
        Nokogiri::XML::Builder.new(&block).doc.root.to_xml
      end
    end
  end
end
