require 'blather/stanza'
require 'cgi'

module Libertree
  module XML
    module Helper
      def build_stanza( target, params )
        stanza = Blather::Stanza::Iq.new(:set, target)
        content = "<libertree xmlns=\"urn:libertree\">#{params_to_xml(params)}</libertree>"
        stanza.add_child content
        stanza
      end

      def params_to_xml(elem)
        case elem
        when Array
          elem.flat_map do |i|
            case i
            when Hash
              params_to_xml(i)
            else # unnamed element
              '<element>' + params_to_xml(i) + '</element>'
            end
          end.join
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
