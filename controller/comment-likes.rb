module Controller
  class CommentLikes < Base
    map '/likes/comments'
    before_all do
      default_before_filter
    end
    layout nil

    def create(comment_id)
      comment = Libertree::Model::Comment[ comment_id.to_i ]

      if comment
        like = Libertree::Model::CommentLike.find_or_create(
          member_id:  account.member.id,
          comment_id: comment.id,
        )

        # bypass cache: fetch the likes by comment_id
        likes = Libertree::Model::CommentLike.where(comment_id: comment_id.to_i)

        return {
          'comment_like_id' => like.id,
          'num_likes'       => n_('1 like', '%d likes', likes.count) % likes.count,
          'liked_by'        => likes.map { |l| l.member.name_display }.join(', '),
        }.to_json
      end

      ""
    end

    def destroy(comment_like_id)
      like = Libertree::Model::CommentLike[ comment_like_id.to_i ]
      if like && like.member == account.member
        like.delete_cascade
      end

      # bypass cache: fetch the likes by comment_id
      likes = Libertree::Model::CommentLike.where(comment_id: like.comment_id.to_i)

      return {
        'num_likes' => likes.count,
        'text'      => n_('1 like', '%d likes', likes.count) % likes.count,
        'liked_by'  => likes.map { |l| l.member.name_display }.join(', ')
      }.to_json
    end
  end
end
