module Libertree
  module Server
    module Responder
      module Forest
        def rsp_forest(params)
          require_parameters(params, 'name', 'trees')

          begin
            forest = Model::Forest[
              origin_server_id: @remote_tree.id,
              remote_id: params['id']
            ]
            if forest
              forest.name = params['name']
            else
              forest = Model::Forest.create(
                origin_server_id: @remote_tree.id,
                remote_id: params['id'],
                name: params['name']
              )
            end

            trees = params['trees'].reject { |t|
              t['domain'] == Server.conf['domain']
            }
            forest.set_trees_by_domain trees
          rescue PGError => e
            log "Error on FOREST request: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_introduce(params)
          require_parameters(params, 'public_key')
          # TODO: record provided parameters for this server,
          # ignore domain parameter if provided
          # store the following:
          # - name_given
          # - contact
          # - public key
        end

      end
    end
  end
end
