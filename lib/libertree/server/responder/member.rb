module Libertree
  module Server
    module Responder
      module Member
        def rsp_member(params)
          return  if require_parameters(params, 'username', 'avatar_url')

          begin
            # TODO: Fetch avatar

            Model::Member.find_or_create(
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
