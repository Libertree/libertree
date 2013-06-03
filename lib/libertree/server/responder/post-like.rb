module Libertree
  module Server
    module Responder
      module PostLike
        def rsp_post_like(params)
          require_parameters(params, 'id', 'username', 'public_key', 'post_id')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @remote_tree.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != Server.conf['public_key']
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

            assert post, 'Unrecognized post.'

            like = Model::PostLike.find_or_create(
              'member_id' => member.id,
              'post_id' => post.id,
              'remote_id' => params['id'],
            )
          rescue PGError => e
            fail InternalError, "Error in rsp_post_like: #{e.message}", nil
          end
        end

        def rsp_post_like_delete(params)
          require_parameters(params, 'id')

          begin
            likes = Model::PostLike.
              where( 'remote_id' => params['id'] ).
              find_all { |like| like.member.server == @remote_tree }

            assert likes[0], "Unrecognized like ID: #{params['id'].inspect}"
            likes[0].delete  # there should only be one Like
          rescue PGError => e
            fail InternalError, "Error in rsp_post_like_delete: #{e.message}", nil
          end
        end
      end
    end
  end
end
