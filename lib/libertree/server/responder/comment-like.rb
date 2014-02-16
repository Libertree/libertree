module Libertree
  module Server
    module Responder
      module CommentLike
        def rsp_comment_like(params)
          require_parameters(params, 'id', 'username', 'origin', 'comment_id')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @remote_tree.id,
            ]
            fail_if_nil member, "Unrecognized member username: #{params['username'].inspect}"

            origin = Model::Server[ domain: params['origin'] ]
            if origin.nil? && params['origin'] != Server.conf['domain']
              # TODO: Is this revealing too much to the requester?
              fail NotFoundError, 'Unrecognized origin server.', nil
            end

            if origin.nil?
              # origin is supposedly this local server
              comment = Model::Comment[ params['comment_id'] ]
            else
              comments = Model::Comment.where( remote_id: params['comment_id'] )
              comments.reject! { |p|
                p.member.server != origin
              }
              comment = comments[0]  # There should only be one or none
            end

            fail_if_nil comment, 'Unrecognized comment.'

            Model::CommentLike.find_or_create(
              'member_id' => member.id,
              'comment_id' => comment.id,
              'remote_id' => params['id'],
            )
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end

        def rsp_comment_like_delete(params)
          require_parameters(params, 'id')

          begin
            likes = Model::CommentLike.
              where( 'remote_id' => params['id'] ).
              find_all { |like| like.member.server == @remote_tree }

            fail_if_nil likes[0], "Unrecognized like ID: #{params['id'].inspect}"
            likes[0].delete  # there should only be one Like
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
