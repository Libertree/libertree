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
        Nokogiri::XML::Builder.new(&block).doc.root.serialize(:save_with => Nokogiri::XML::Node::SaveOptions::AS_XML)
      end
    end
  end
end
