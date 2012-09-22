module Libertree
  module Server
    module Responder
      module Pool
        def rsp_pool(params)
          return  if require_parameters(params, 'username', 'id', 'name')

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
              pool_name = params['name']

              # <Pistos> There's a microscopic risk of a race condition here (find/create),
              # but it's so small, I guess we'll ignore it for now.

              pool = Model::Pool[
                'member_id' => member.id,
                'remote_id' => params['id']
              ]
              if pool
                pool.name = pool_name
              else
                Model::Pool.create(
                  'member_id' => member.id,
                  'remote_id' => params['id'],
                  'name'      => pool_name,
                  'sprung'    => true
                )
              end

              respond_with_code 'OK'
            end
          rescue PGError => e
            respond_with_code 'ERROR'
            log "ERROR on POOL request: #{e.message}"
          end
        end

        def rsp_pool_delete(params)
          return  if require_parameters(params, 'id', 'username')

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
              pool = Model::Pool[
                'member_id' => member.id,
                'remote_id' => params['id']
              ]
              if pool
                pool.delete_cascade
                respond_with_code 'OK'
              else
                respond( {
                  'code' => 'NOT FOUND',
                  'message' => "Unrecognized pool: #{params['id']}"
                } )
              end
            end
          rescue PGError => e
            respond_with_code 'ERROR'
            log "ERROR on POOL request: #{e.message}"
          end
        end
      end
    end
  end
end
