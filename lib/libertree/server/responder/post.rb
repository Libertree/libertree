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

        def rsp_post_delete(params)
          return  if require_parameters(params, 'id')

          begin
            posts = Model::Post.
              where( 'remote_id' => params['id'] ).
              reject { |p| p.server != @server }

            if posts.empty?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized post ID: #{params['id'].inspect}"
              } )
            else
              posts[0].delete_cascade  # there should only be one post
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
