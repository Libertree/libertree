module Controller
  class PostLikes < Base
    map '/likes/posts'
    before_all do
      default_before_filter
    end
    layout nil

    def create(post_id)
      post = Libertree::Model::Post[ post_id.to_i ]

      if post
        # TODO: Check that the member is allowed to view and like the post.
        like = Libertree::Model::PostLike.find_or_create(
          member_id: account.member.id,
          post_id:   post.id,
        )

        # bypass cache: fetch the likes by comment_id
        likes = Libertree::Model::PostLike.where(post_id: post_id.to_i)

        # TODO: Use partial for number of likes
        return {
          'post_like_id' => like.id,
          'num_likes'    => likes.count,
          'liked_by'     => _('Liked by %s') % likes.map { |l| l.member.name_display }.join(', '),
        }.to_json
      end

      ""
    end

    def destroy(post_like_id)
      like = Libertree::Model::PostLike[ post_like_id.to_i ]
      if like && like.member == account.member
        like.delete_cascade
      end

      # bypass cache: fetch the likes by comment_id
      likes = Libertree::Model::PostLike.where(post_id: like.post_id.to_i)

      return {
        'num_likes'    => likes.count,
        'liked_by'     => _('Liked by %s') % likes.map { |l| l.member.name_display }.join(', '),
      }.to_json
    end
  end
end
