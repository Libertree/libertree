module Libertree
  module Server
    module Responder
      module Comment
        def rsp_comment(params)
          return  if require_parameters(params, 'username', 'public_key', 'post_id', 'text')

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
                'message' => "Unrecognized post."
              } )
              return
            end

            comment = Model::Comment.find_or_create(
              'member_id' => member.id,
              'post_id' => post.id,
              # TODO: Sanitize with Loofah
              'text' => params['text']
            )
            post.mark_as_unread_by_all
            post.notify_participants_about_comment comment

            respond_with_code 'OK'
          rescue PGError => e
            respond_with_code 'ERROR'
          end
        end
      end
    end
  end
end
