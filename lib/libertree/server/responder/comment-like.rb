module Libertree
  module Server
    module Responder
      module CommentLike
        def rsp_comment_like(params)
          return  if require_parameters(params, 'id', 'username', 'public_key', 'comment_id')

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
              return
            end

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
              comment = Model::Comment[ params['comment_id'] ]
            else
              comments = Model::Comment.where( remote_id: params['comment_id'] )
              comments.reject! { |p|
                p.member.server != origin
              }
              comment = comments[0]  # There should only be one or none
            end

            if comment.nil?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized comment."
              } )
              return
            end

            like = Model::CommentLike.find_or_create(
              'member_id' => member.id,
              'comment_id' => comment.id,
              'remote_id' => params['id'],
            )

            respond_with_code 'OK'
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end

        def rsp_comment_like_delete(params)
          return  if require_parameters(params, 'id')

          begin
            likes = Model::CommentLike.
              where( 'remote_id' => params['id'] ).
              keep_if { |like| like.member.server == @server }

            if likes.empty?
              respond( {
                'code' => 'NOT FOUND',
                'message' => "Unrecognized like ID: #{params['id'].inspect}"
              } )
            else
              likes[0].delete  # there should only be one Like
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
