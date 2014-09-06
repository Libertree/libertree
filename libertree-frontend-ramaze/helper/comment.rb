module Ramaze
  module Helper
    module Comment
      def comment_link(comment)
        "/posts/show/#{comment.post.id}/#{comment.id}#comment-#{comment.id}"
      end

      # get a hash indexed by commenter member handle yielding:
      # - all comment ids of this member in this thread
      # - the display name
      # - the member id

      # TODO: find an fast, efficient way to exclude commenters after
      # a certain comment id.  We need this for the case when a
      # comment contains the handle of a later commenter.
      def commenters(comments)
        @commenters ||= comments.reduce({}) do |hash,comment|
          handle = comment.member.handle
          if hash[handle]
            hash[handle][:comment_ids] << comment.id
          else
            hash[handle] = {
              comment_ids: [comment.id],
              id: comment.member.id,
              name: comment.member.name_display
            }
          end
          hash
        end
      end
    end
  end
end
