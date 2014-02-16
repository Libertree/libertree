module Libertree
  module Server
    module Responder
      module Chat
        def rsp_chat(params)
          require_parameters(params, 'username', 'recipient_username', 'text')

          begin
            from_member = Model::Member[
              'username' => params['username'],
              'server_id' => @remote_tree.id,
            ]
            fail_if_nil from_member, "Unrecognized member username: #{params['username'].inspect}"

            to_account = Model::Account[
              'username' => params['recipient_username'],
            ]
            fail_if_nil to_account, "Unrecognized recipient username: #{params['recipient_username'].inspect}"
            to_member = to_account.member

            Libertree::Model::ChatMessage.create(
              from_member_id: from_member.id,
              to_member_id: to_member.id,
              text: params['text']
            )
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
