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
              'server_id' => @remote_tree.id,
            ]
            assert member, "Unrecognized member username: #{params['username'].inspect}"

            if params.has_key? 'references'
              refs = params['references']['reference']
              post_text = Libertree::References::replace(params['text'], refs, @remote_tree.id, Server.conf['domain'])
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
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end

        def rsp_post_delete(params)
          require_parameters(params, 'id')

          begin
            posts = Model::Post.
              where( 'remote_id' => params['id'] ).
              reject { |p| p.server != @remote_tree }

            assert posts[0], "Unrecognized post ID: #{params['id'].inspect}"
            posts[0].delete_cascade  # there should only be one post
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
