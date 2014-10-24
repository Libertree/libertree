module Libertree
  module Server
    module Responder
      module Message
        def rsp_message(params)
          require_parameters(params, 'username', 'recipients', 'text')

          begin
            sender_member = Model::Member[
              username:  params['username'],
              server_id: @remote_tree.id,
            ]
            fail_if_nil sender_member, "Unrecognized member username: #{params['username'].inspect}"

            recipients = [params['recipients']].flatten.map {|m| m['recipient']}
            members = recipients.reduce([]) { |ms, recipient|
              origin = Model::Server[ domain: recipient['origin'] ]
              if origin
                member = Model::Member[ username: recipient['username'], server_id: origin.id]
                ms << member  if member
              elsif origin.nil? && recipient['origin'] == Server.conf['domain']
                # origin is this local server
                account = Model::Account[ username: recipient['username'] ]
                if account
                  ms << account.member
                end
              end

              ms
            }

            member_ids = members.map(&:id)
            if params['id']
              remote_id = params['id'].to_i
            end

            Libertree::Model::Message.create_with_recipients(
              sender_member_id: sender_member.id,
              text: params['text'],
              remote_id: remote_id,
              recipient_member_ids: member_ids
            )
          rescue LibertreeError => e
            raise e
          rescue => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
