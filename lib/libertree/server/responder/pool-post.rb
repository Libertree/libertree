module Libertree
  module Server
    module Responder
      module PoolPost
        def rsp_pool_post(params)
          return  if require_parameters(params, 'username', 'pool_id', 'post_id', 'public_key')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post_delete
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            if member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
              return
            end

            pool = Model::Pool[
              'member_id' => member.id,
              'remote_id' => params['pool_id'],
            ]
            if pool.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"
              } )
              return
            end

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != @public_key
              # TODO: Is this revealing too much to the requester?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized origin server."
              } )
              return
            end

            if origin.nil?
              # origin is supposedly this local server
              post = Model::Post[ params['post_id'] ]
            else
              posts = Model::Post.where( remote_id: params['post_id'] )
              posts.reject! { |p|
                p.member.server != origin
              }
              post = posts[0]  # There should only be one or none
            end

            if post.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized post (#{params['post_id']})."
              } )
              return
            end

            pool << post

            respond_with_code 'OK'
          rescue PGError => e
            respond_with_code 'ERROR'
            log "ERROR on POOL request: #{e.message}"
          end
        end

        def rsp_pool_post_delete(params)
          return  if require_parameters(params, 'username', 'pool_id', 'post_id', 'public_key')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            if member.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized member username: #{params['username'].inspect}"
              } )
              return
            end

            pool = Model::Pool[
              'member_id' => member.id,
              'remote_id' => params['pool_id'],
            ]
            if pool.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"
              } )
              return
            end

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != @public_key
              # TODO: Is this revealing too much to the requester?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized origin server."
              } )
              return
            end

            if origin.nil?
              # origin is supposedly this local server
              post = Model::Post[ params['post_id'] ]
            else
              posts = Model::Post.where( remote_id: params['post_id'] )
              posts.reject! { |p|
                p.member.server != origin
              }
              post = posts[0]  # There should only be one or none
            end

            if post.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized post (#{params['post_id']})."
              } )
              return
            end

            # Try to remove, regardless of whether or not it really is there.
            pool.remove_post post

            respond_with_code 'OK'
          rescue PGError => e
            respond_with_code 'ERROR'
            log "ERROR on POOL-POST request: #{e.message}"
          end
        end
      end
    end
  end
end
