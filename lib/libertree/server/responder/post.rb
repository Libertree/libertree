require 'libertree/references'

module Libertree
  module Server
    module Responder
      module Post
        def rsp_post(params)
          require_parameters(params, 'username', 'id', 'visibility', 'text')

          begin
            member = Model::Member[
              'username' => params['username'],
              'server_id' => @server.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            if params.has_key? 'references'
              post_text = Libertree::References::replace(params['text'], params['references'], @server.id, @public_key)
            else
              post_text = params['text']
            end

            # <Pistos> There's a microscopic risk of a race condition here (find/create),
            # but it's so small, I guess we'll ignore it for now.

            post = Model::Post[
              'member_id' => member.id,
              'remote_id' => params['id']
            ]
            if post
              post.revise post_text
            else
              Model::Post.create(
                'member_id'  => member.id,
                'remote_id'  => params['id'],
                'visibility' => params['visibility'],
                'text'       => post_text,
                'via'        => params['via']
              )
            end
          rescue PGError => e
            log "Error in rsp_post: #{e.message}"
            fail InternalError, '', nil
          end
        end

        def rsp_post_delete(params)
          require_parameters(params, 'id')

          begin
            posts = Model::Post.
              where( 'remote_id' => params['id'] ).
              reject { |p| p.server != @server }

            assert posts[0], "Unrecognized post ID: #{params['id'].inspect}"
            posts[0].delete_cascade  # there should only be one post
          rescue PGError => e
            log "Error in rsp_post_delete: #{e.message}"
            fail InternalError, '', nil
          end
        end
      end
    end
  end
end
