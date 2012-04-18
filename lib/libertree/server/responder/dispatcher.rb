# Execute the appropriate Ruby method with the appropriate arguments
# based on an incoming LTP request.

module Libertree
  module Server
    module Responder
      module Dispatcher

        def process(request_raw)
          # This is a hack.  TODO: Find out where these empty lines are coming from.
          return  if request_raw.strip == ''

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

          puts "Received request: #{command}"

          case command
          when 'AUTHENTICATE', 'COMMENT', 'COMMENT-DELETE', 'INTRODUCE', 'MEMBER', 'POST', 'POST-DELETE', 'POST-LIKE' # , ...
            if ! introduced? && command != 'INTRODUCE'
              respond 'code' => 'ERROR', 'message' => 'Not INTRODUCEd.'
            elsif introduced? && ! authenticated? && command != 'AUTHENTICATE'
              respond 'code' => 'ERROR', 'message' => 'Not AUTHENTICATEd.'
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
end
