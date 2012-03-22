# Execute the appropriate Ruby method with the appropriate arguments
# based on an incoming LTP request.

module Libertree
  module Server
    module Dispatcher
      def process(request_raw)
        if request_raw !~ /^(\S+) (.+)$/
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

        case command
        when 'AUTHENTICATE', 'INTRODUCE' # , ...
          if command != 'INTRODUCE' && ! introduced?
            respond 'code' => 'ERROR', 'message' => 'Not INTRODUCEd.'
          else
            method = "rsp_#{command.downcase.gsub('-', '_')}".to_sym
            send  method, parameters
          end
        else
          respond 'code' => 'UNKNOWN COMMAND', 'message' => "Received command: #{command}"
        end
      end
    end
  end
end
