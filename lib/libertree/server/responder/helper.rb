module Libertree
  module Server
    module Responder
      module Helper
        # Packs an optional XML fragment or an array of fragments
        # (opts[:with]) in a standard XMPP reply to the provided stanza
        # (opts[:to]) and sends out the reply stanza.
        def respond(opts)
          stanza = opts[:to]
          response = stanza.reply!
          if opts[:with]
            # we cannot use Array() here because it results in an empty
            # array when only one fragment is given
            fragments = opts[:with].is_a?(Array) ? opts[:with] : [opts[:with]]
            fragments.each do |child|
              response.add_child child
            end

            if opts[:type]
              response.type = opts[:type]
            end
          end

          # It's ugly to rely on "ambient" instance variables but I
          # can't think of a cleaner implementation that doesn't
          # sacrifice simplicity.
          @client.write response
        end

        # Generates an error XML fragment given an optional
        # error code and an optional error message
        def error(opts={ code: 'ERROR' })
          Nokogiri::XML::Builder.new { |xml|
            xml.error {
              xml.code(opts[:code])
              xml.text_(opts[:text])  if opts[:text]
            }
          }.doc.root
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
          # remove empty text nodes
          xml.children.each {|n| n.unlink  if n.text? && n.text.strip.empty?}

          if xml.text? && ! xml.text.strip.empty?
            xml.text
          elsif xml.children.length == 1
            { xml.name => xml_to_hash(xml.children[0])}
          elsif ! xml.children.empty?
            children = xml.children.map {|n| xml_to_hash(n)}.compact
            if xml.children.map(&:name).uniq.compact.length > 1
              # flatten the array of hashes to a single hash
              { xml.name => children.reduce(&:merge) }
            else
              { xml.name => children }
            end
          end
        end
      end
    end
  end
end
