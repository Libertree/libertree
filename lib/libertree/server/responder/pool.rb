module Libertree
  module Server
    module Responder
      module Pool
        def rsp_pool(params)
          require_parameters(params, 'username', 'id', 'name')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

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
          rescue PGError => e
            log "ERROR on POOL request: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_pool_delete(params)
          require_parameters(params, 'id', 'username')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            pool = Model::Pool[
              'member_id' => member.id,
              'remote_id' => params['id']
            ]
            assert pool, "Unrecognized pool: #{params['id']}"
            pool.delete_cascade
          rescue PGError => e
            log "ERROR on POOL request: #{e.message}"
            fail InternalError, '', nil
          end
        end
      end
    end
  end
end
