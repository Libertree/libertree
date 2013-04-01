module Libertree
  module Server
    module Responder
      module Message
        def rsp_message(params)
          return  if require_parameters(params, 'username', 'recipients', 'text')

          begin
            sender_member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            if sender_member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
              return
            end

            member_ids = params['recipients'].reduce([]) { |ids, recipient|
              origin = Model::Server[ 'public_key' => recipient['public_key'] ]
              if origin
                member = Model::Member['username' => recipient['username'], 'server_id' => origin.id]
                ids << member.id  if member
              elsif origin.nil? && recipient['public_key'] == @public_key
                # origin is this local server
                account = Model::Account['username' => recipient['username']]
                if account
                  ids << account.member.id
                end
              end

              ids
            }

            message = Libertree::Model::Message.create_with_recipients(
              sender_member_id: sender_member.id,
              text: params['text'],
              recipient_member_ids: member_ids
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
