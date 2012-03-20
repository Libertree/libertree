# Execute the appropriate Ruby method with the appropriate arguments
# based on an incoming LTP request.

module Libertree
  module Server
    module Dispatcher
      def process(request_raw)
        if request_raw !~ /^(\S)+ (.+)$/
          respond_with_code 'BAD REQUEST'
          return
        end

        command, parameters_raw = $1, $2
        begin
          parameters = JSON.parse(parameters_raw)
        rescue JSON::ParserError => e
          respond( {
            'code' => 'BAD PARAMETER',
            'message' => e.message
          } )
          return
        end

        case request_raw
        when 'INTRODUCE'
        else
          respond_with_code 'UNKNOWN COMMAND'
        end
      end
    end
  end
end
