module Libertree
  module Server
    module Responder
      module PoolPost
        def rsp_pool_post(params)
          require_parameters(params, 'username', 'pool_id', 'post_id', 'public_key')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post_delete
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @remote_tree.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            pool = Model::Pool[
              'member_id' => member.id,
              'remote_id' => params['pool_id'],
            ]
            assert pool, "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != @public_key
              # TODO: Is this revealing too much to the requester?
              fail NotFound, 'Unrecognized origin server.', nil
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

            assert post, "Unrecognized post (#{params['post_id']})."
            pool << post
          rescue PGError => e
            log "ERROR on POOL request: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_pool_post_delete(params)
          require_parameters(params, 'username', 'pool_id', 'post_id', 'public_key')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @remote_tree.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            pool = Model::Pool[
              'member_id' => member.id,
              'remote_id' => params['pool_id'],
            ]
            assert pool, "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != @public_key
              # TODO: Is this revealing too much to the requester?
              fail NotFound, 'Unrecognized origin server.', nil
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

            assert post, "Unrecognized post (#{params['post_id']})."

            # Try to remove, regardless of whether or not it really is there.
            pool.remove_post post
          rescue PGError => e
            log "ERROR on POOL-POST request: #{e.message}"
            fail InternalError, '', nil
          end
        end
      end
    end
  end
end
