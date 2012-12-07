module Libertree
  module Server
    module Responder
      module CommentLike
        def rsp_comment_like(params)
          require_parameters(params, 'id', 'username', 'public_key', 'comment_id')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            origin = Model::Server[ public_key: params['public_key'] ]
            if origin.nil? && params['public_key'] != @public_key
              # TODO: Is this revealing too much to the requester?
              fail NotFound, 'Unrecognized origin server.', nil
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

            assert comment, 'Unrecognized comment.'

            like = Model::CommentLike.find_or_create(
              'member_id' => member.id,
              'comment_id' => comment.id,
              'remote_id' => params['id'],
            )
          rescue PGError => e
            log "Error in rsp_comment_like: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_comment_like_delete(params)
          require_parameters(params, 'id')

          begin
            likes = Model::CommentLike.
              where( 'remote_id' => params['id'] ).
              find_all { |like| like.member.server == @server }

            assert likes[0], "Unrecognized like ID: #{params['id'].inspect}"
            likes[0].delete  # there should only be one Like
          rescue PGError => e
            log "Error in rsp_comment_like_delete: #{e.message}"
            fail InternalError, '', nil
          end
        end
      end
    end
  end
end
