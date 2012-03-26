module Libertree
  module Server
    module Responder
      module Member
        def rsp_member(params)
          return  if missing_parameter(params, 'uuid', 'username')

          begin
            Model::Member.find_or_create(
              'uuid' => params['uuid'],
              'username' => params['username'],
              'server_id' => @server.id
            )
            respond_with_code 'OK'
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
