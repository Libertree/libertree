module Libertree
  module Server
    module Responder
      module Helper
        # Packs an optional XML fragment or an array of fragments
        # (opts[:with]) in a standard XMPP reply to the provided stanza
        # (opts[:to]) and sends out the reply stanza.
        def respond(opts)
          stanza = opts[:to]
          response = stanza.reply
          if opts[:with]
            # we cannot use Array() here because it results in an empty
            # array when only one fragment is given
            fragments = opts[:with].is_a?(Array) ? opts[:with] : [opts[:with]]
            fragments.each do |child|
              response.add_child child
            end

            if fragments[0].node_name == "error"
              response.type = :error
            end
          end

          # It's ugly to rely on "ambient" instance variables but I
          # can't think of a cleaner implementation that doesn't
          # sacrifice simplicity.
          @client.write response
        end

        # @param [Hash] params A Hash.
        # @param [Array] required_parameters The keys which are required.
        # @return [Array] The keys whose values are missing.
        def missing_parameters(params, *required_parameters)
          return required_parameters  if params.nil?
          missing = []
          required_parameters.each do |rp|
            if params[rp].nil? || params[rp].respond_to?(:empty?) && params[rp].empty?
              missing << rp
            end
          end
          missing
        end

        # Calls #missing_parameters.  If any parameters are missing, raises
        # MissingParameterError exception with the first missing parameter as message.
        # @return [nil] when there are no missing parameters
        # @raises [MissingParameterError] when there is a missing parameter
        def require_parameters(*args)
          mp = missing_parameters(*args)
          if mp[0]
            fail MissingParameterError, mp[0], nil
          end
        end

        def fail_if_nil(obj, msg)
          if obj.nil?
            fail NotFoundError, msg, nil
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
