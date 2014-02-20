require 'libertree/references'

module Libertree
  module Server
    module Responder
      module Comment
        def rsp_comment(params)
          require_parameters(params, 'id', 'username', 'origin', 'post_id', 'text')

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
              post = Model::Post[ params['post_id'] ]
            else
              posts = Model::Post.where( remote_id: params['post_id'] )
              posts.reject! { |p|
                p.member.server != origin
              }
              post = posts[0]  # There should only be one or none
            end
            fail_if_nil post, 'Unrecognized post.'

            if params.has_key? 'references'
              refs = params['references'].map {|r| r['reference']}
              comment_text = Libertree::References::replace(params['text'], refs, @remote_tree.id, Server.conf['domain'])
            else
              comment_text = params['text']
            end

            Model::Comment.find_or_create(
              member_id: member.id,
              post_id:   post.id,
              remote_id: params['id'],
              # TODO: Sanitize with Loofah
              text:      comment_text
            )
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end

        def rsp_comment_delete(params)
          require_parameters(params, 'id')

          begin
            comments = Model::Comment.
              where( remote_id: params['id'] ).
              reject { |c| c.member.server != @remote_tree }

            fail_if_nil comments[0], "Unrecognized comment ID: #{params['id'].inspect}"
            comments[0].delete_cascade  # there should only be one comment
          rescue PGError => e
            fail InternalError, "Error in #{__method__}: #{e.message}", nil
          end
        end
      end
    end
  end
end
