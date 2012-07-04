module Libertree
  module Server
    module Responder
      module Chat
        def rsp_chat(params)
          return  if require_parameters(params, 'username', 'recipient_username', 'text')

          begin
            from_member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            if from_member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
              return
            end

            to_account = Model::Account[
              'username' => params['recipient_username'],
            ]
            if to_account.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized recipient username: #{params['recipient_username'].inspect}"
              } )
              return
            end
            to_member = to_account.member

            chat_message = Libertree::Model::ChatMessage.create(
              from_member_id: from_member.id,
              to_member_id: to_member.id,
              text: params['text']
            )

            respond_with_code 'OK'
          rescue PGError => e
            log "Error in rsp_chat: #{e.message}"
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
