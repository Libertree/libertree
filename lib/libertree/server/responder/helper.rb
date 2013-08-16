module Libertree
  module Server
    module Responder
      module Helper

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

        # @param [Nokogiri::XML::Element] xml An XML document
        # @return [Hash]
        def xml_to_hash(xml)
          value = if xml.children.length == 1 &&
                      xml.children.first.name == 'text'
                    xml.text
                  else
                    xml.children.reduce({}) do |result, n|
                      result.merge(xml_to_hash(n)) do |key, oldval, newval|
                        if oldval.is_a? Array
                          oldval << newval
                        else
                          [ oldval ] << newval
                        end
                      end
                    end
                  end
          { xml.name => value }
        end
      end
    end
  end
end
