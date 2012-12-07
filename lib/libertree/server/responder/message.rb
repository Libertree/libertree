module Libertree
  module Server
    module Responder
      module Message
        def rsp_message(params)
          require_parameters(params, 'username', 'recipients', 'text')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            message = Libertree::Model::Message.create_with_recipients(
              sender_member_id: member.id,
              text: params['text'],
              recipient_member_usernames: params['recipients'].map { |r| r['username'] }
            )
          rescue PGError => e
            log "Error in rsp_message: #{e.message}"
            fail InternalError, '', nil
          end
        end

        # TODO
        # def rsp_message_delete(params)
          # require_parameters(params, 'id')

          # begin
            # # TODO
          # rescue PGError => e
            # log "Error in rsp_message_delete: #{e.message}"
            # fail InternalError, '', nil
          # end
        # end
      end
    end
  end
end
