require 'libertree/server/responder/dispatcher'
require 'libertree/server/responder/authentication'
require 'libertree/server/responder/member'
require 'libertree/server/responder/post'

module Libertree
  module Server
    module Responder
      include Dispatcher
      include Authentication
      include Member
      include Post

      def respond(data)
        # TODO: Gracefully handle failure to convert to JSON
        send_data data.to_json + "\n"
      end

      def respond_with_code(code)
        respond 'code' => code
      end

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

      # Calls #missing_parameters.  If any parameters are missing, responds (to
      # the requester) with the first missing parameter.
      # @return [Boolean] whether there were missing parameters
      def require_parameters(*args)
        mp = missing_parameters(*args)
        if mp[0]
          respond( {
            'code' => 'MISSING PARAMETER',
            'parameter' => mp[0],
          } )
          true
        end
      end
    end
  end
end
