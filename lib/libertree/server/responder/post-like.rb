module Libertree
  module Server
    module Responder
      module PostLike
        def rsp_post_like(params)
          require_parameters(params, 'id', 'username', 'origin', 'post_id')

          begin
            member = Model::Member[
              username: params['username'],
              server_id: @remote_tree.id,
            ]
            fail_if_nil member, "Unrecognized member username: #{params['username'].inspect}"

            origin = Model::Server[ domain: params['origin'] ]
            if origin.nil? && params['origin'] != Server.conf['domain']
              # TODO: Is this revealing too much to the requester?
              fail NotFoundError, 'Unrecognized origin server.', nil
            end

            if origin.nil?
              # origin is supposedly this local server
              post = Model::Post[ params['post_id'].to_i ]
            else
              posts = Model::Post.where( remote_id: params['post_id'].to_i ).all
              posts.reject! { |p|
                p.member.server != origin
              }
              post = posts[0]  # There should only be one or none
            end

            fail_if_nil post, 'Unrecognized post.'

            Model::PostLike.find_or_create(
              member_id: member.id,
              post_id: post.id,
              remote_id: params['id'],
            )
          rescue LibertreeError => e
            raise e
          rescue => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end

        def rsp_post_like_delete(params)
          require_parameters(params, 'id')

          begin
            likes = Model::PostLike.
              where( remote_id: params['id'] ).
              find_all { |like| like.member.server == @remote_tree }

            fail_if_nil likes[0], "Unrecognized like ID: #{params['id'].inspect}"
            likes[0].delete  # there should only be one Like
          rescue LibertreeError => e
            raise e
          rescue => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
