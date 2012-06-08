module Libertree
  module Server
    module Responder
      module Forest
        def rsp_forest(params)
          return  if require_parameters(params, 'name', 'trees')

          begin
            forest = Model::Forest[
              origin_server_id: @server.id,
              remote_id: params['id']
            ]
            if forest
              forest.name = params['name']
            else
              forest = Model::Forest.create(
                origin_server_id: @server.id,
                remote_id: params['id'],
                name: params['name']
              )
            end

            forest.set_trees_by_ip params['trees']
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
