module Libertree
  module Server
    module Responder
      module PoolPost
        def rsp_pool_post(params)
          require_parameters(params, 'username', 'pool_id', 'post_id', 'origin')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post_delete
            member = Model::Member[
              username: params['username'],
              server_id: @remote_tree.id,
            ]
            fail_if_nil member, "Unrecognized member username: #{params['username'].inspect}"

            pool = Model::Pool[
              member_id: member.id,
              remote_id: params['pool_id'],
            ]
            fail_if_nil pool, "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ domain: params['origin'] ]
            if origin.nil? && params['origin'] != Server.conf['domain']
              # TODO: Is this revealing too much to the requester?
              fail NotFoundError, 'Unrecognized origin server.', nil
            end

            if origin.nil?
              # origin is supposedly this local server
              post = Model::Post[ params['post_id'].to_i ]
            else
              posts = Model::Post.where( remote_id: params['post_id'].to_i ).
                find_all {|p| p.member.server == origin }
              post = posts[0]  # There should only be one or none
            end

            fail_if_nil post, "Unrecognized post (#{params['post_id']})."
            pool << post
          rescue PGError => e
            fail InternalError, "ERROR on POOL request: #{e.message}", nil
          end
        end

        def rsp_pool_post_delete(params)
          require_parameters(params, 'username', 'pool_id', 'post_id', 'origin')

          begin
            # TODO: This can be DRYed up with the code in rsp_pool_post
            member = Model::Member[
              username: params['username'],
              server_id: @remote_tree.id,
            ]
            fail_if_nil member, "Unrecognized member username: #{params['username'].inspect}"

            pool = Model::Pool[
              member_id: member.id,
              remote_id: params['pool_id'],
            ]
            fail_if_nil pool, "Unrecognized pool for given member: pool_id #{params['pool_id']}, username #{params['username'].inspect}"

            # TODO: These next two code paragraphs could be DRYed up along with
            # the code found in server/responder/post-like.rb .
            origin = Model::Server[ domain: params['origin'] ]
            if origin.nil? && params['origin'] != Server.conf['domain']
              # TODO: Is this revealing too much to the requester?
              fail NotFoundError, 'Unrecognized origin server.', nil
            end

            if origin.nil?
              # origin is supposedly this local server
              post = Model::Post[ params['post_id'].to_i ]
            else
              posts = Model::Post.where( remote_id: params['post_id'].to_i ).
                find_all {|p| p.member.server == origin }
              post = posts[0]  # There should only be one or none
            end

            fail_if_nil post, "Unrecognized post (#{params['post_id']})."

            # Try to remove, regardless of whether or not it really is there.
            pool.remove_post post
          rescue PGError => e
            fail InternalError, "ERROR on POOL-POST request: #{e.message}", nil
          end
        end
      end
    end
  end
end
