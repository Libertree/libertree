module Libertree
  module Server
    module Responder
      module Post
        def rsp_post(params)
          return  if require_parameters(params, 'username', 'id', 'public', 'text')

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
            else
              Model::Post.find_or_create(
                'member_id' => member.id,
                'remote_id' => params['id'],
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
