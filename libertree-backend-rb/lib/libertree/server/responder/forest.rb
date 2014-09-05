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
              forest.save
            else
              forest = Model::Forest.create(
                origin_server_id: @remote_tree.id,
                remote_id: params['id'],
                name: params['name']
              )
            end

            trees = params['trees'].reject { |tree|
              tree['domain'] == Server.conf['domain']
            }
            forest.set_trees_by_domain trees.map {|t| t['domain']}
          rescue PGError => e
            fail InternalError, "Error on FOREST request: #{e.message}", nil
          end
        end

        def rsp_introduce(params)
          require_parameters(params, 'public_key', 'contact')

          if @remote_tree.nil?
            @remote_tree = Model::Server.create(
              domain:     @remote_domain,
              public_key: params['public_key'],
              contact:    params['contact'],
            )

            Libertree::Server.log "#{@remote_domain} is a new server (id: #{@remote_tree.id})."
          else
            Libertree::Server.log "updating server record for #{@remote_domain} (id: #{@remote_tree.id})."
            # TODO: validate before storing these values
            @remote_tree.public_key = params['public_key']
            @remote_tree.contact    = params['contact']
            @remote_tree.save
          end
        end

      end
    end
  end
end
