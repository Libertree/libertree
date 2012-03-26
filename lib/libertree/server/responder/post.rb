module Libertree
  module Server
    module Responder
      module Post
        def rsp_post(params)
          return  if require_parameters(params, 'member_uuid', 'uuid', 'public', 'text')

          begin
            member = Model::Member['uuid' => params['member_uuid']]
            if member.nil? || member.server_id != @server.id
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member uuid: #{params['member_uuid'].inspect}"
              } )
            else
              Model::Post.find_or_create(
                'member_id' => member.id,
                'uuid' => params['uuid'],
                'public' => params['public'],
                'text' => params['text']
              )
              respond_with_code 'OK'
            end
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
