require 'blather/stanza'
require 'cgi'

module Libertree
  module XML
    module Helper
      def build_stanza( target, params )
        stanza = Blather::Stanza::Iq.new(:set, target)
        content = "<libertree xmlns=\"libertree\">#{params_to_xml(params)}</libertree>"
        stanza.add_child content
        stanza
      end

      def params_to_xml(elem)
        case elem
        when Array
          # TODO: this squashes arrays
          # before:
          #   :abc => [1,2,3,4]
          # after:
          #   <abc>1234</abc>
          #
          # would this be better?
          #   <abc>
          #     <item>1</item>
          #     <item>2</item>
          #     <item>3</item>
          #     <item>4</item>
          #   </abc>
          elem.flat_map {|i| params_to_xml(i) }.join('')
        when Hash
          elem.reduce("") do |acc,i|
            acc << ("<#{i[0]}>"+ params_to_xml(i[1]) +"</#{i[0]}>")
            acc
          end
        else
          CGI.escapeHTML elem.to_s
        end
      end
    end
  end
end
