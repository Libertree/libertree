require 'libertree/server/responder/dispatcher'
require 'libertree/server/responder/authentication'
require 'libertree/server/responder/member'

module Libertree
  module Server
    module Responder
      include Dispatcher
      include Authentication
      include Member

      def respond(data)
        # TODO: Gracefully handle failure to convert to JSON
        send_data data.to_json + "\n"
      end

      def respond_with_code(code)
        respond 'code' => code
      end

    end
  end
end
