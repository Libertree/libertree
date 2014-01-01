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

            members = params['recipients'].reduce({:local => [], :remote => []}) { |ms, recipient|
              origin = Model::Server[ 'public_key' => recipient['public_key'] ]
              if origin
                member = Model::Member['username' => recipient['username'], 'server_id' => origin.id]
                ms[:remote] << member  if member
              elsif origin.nil? && recipient['public_key'] == @public_key
                # origin is this local server
                account = Model::Account['username' => recipient['username']]
                if account
                  ms[:local] << account.member
                end
              end

              ms
            }

            member_ids = members[:remote].map(&:id) + members[:local].map(&:id)
            message = Libertree::Model::Message.create_with_recipients(
              sender_member_id: sender_member.id,
              text: params['text'],
              recipient_member_ids: member_ids
            )

            # forward via email for those local recipients who requested it
            members[:local].map(&:account).select {|a| a.email && a.forward_dms_via_email }.
              each do |account|
                Libertree::Model::Job.create(
                  task: 'email',
                  params: {
                    'to'      => account.email,
                    'subject' => '[Libertree] Direct message', # TODO: translate
                    'body'    => "#{sender_member.handle} wrote:\n\n#{params['text']}"
                  }.to_json
                )
            end

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
