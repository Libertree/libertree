module Libertree
  module Server
    module Responder
      module Message
        def rsp_message(params)
          return  if require_parameters(params, 'username', 'recipients', 'text')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            if member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
              return
            end

            message = Libertree::Model::Message.create_with_recipients(
              sender_member_id: member.id,
              text: params['text'],
              recipient_member_usernames: params['recipients'].map { |r| r['username'] }
            )

            respond_with_code 'OK'
          rescue PGError => e
            log "Error in rsp_message: #{e.message}"
            respond_with_code 'ERROR'
          end
        end

        # TODO
        # def rsp_message_delete(params)
          # return  if require_parameters(params, 'id')

          # begin
            # # TODO
          # rescue PGError => e
            # respond_with_code 'ERROR'
          # end
        # end
      end
    end
  end
end
