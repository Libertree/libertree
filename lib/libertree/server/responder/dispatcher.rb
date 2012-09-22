# Execute the appropriate Ruby method with the appropriate arguments
# based on an incoming LTP request.

module Libertree
  module Server
    module Responder
      module Dispatcher

        VALID_COMMANDS = [
          'AUTHENTICATE', 'CHAT', 'COMMENT', 'COMMENT-DELETE', 'COMMENT-LIKE',
          'COMMENT-LIKE-DELETE', 'FOREST', 'INTRODUCE', 'MEMBER', 'MESSAGE',
          'POOL', 'POOL-DELETE', 'POOL-POST', 'POOL-POST-DELETE', 'POST',
          'POST-DELETE', 'POST-LIKE', 'POST-LIKE-DELETE',
        ]

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

          log "Received request: #{command}"

          if ! VALID_COMMANDS.include?(command)
            respond 'code' => 'UNKNOWN COMMAND', 'message' => "Received command: #{command}"
          else
            if ! introduced? && command != 'INTRODUCE'
              respond 'code' => 'ERROR', 'message' => 'Not INTRODUCEd.'
            elsif introduced? && ! authenticated? && command != 'AUTHENTICATE'
              respond 'code' => 'ERROR', 'message' => 'Not AUTHENTICATEd.'
            else
              method = "rsp_#{command.downcase.gsub('-', '_')}".to_sym
              send  method, parameters
            end
          end
        end

      end
    end
  end
end
