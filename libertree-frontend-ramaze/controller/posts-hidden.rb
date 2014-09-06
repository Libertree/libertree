module Controller
  class PostsHidden < Base
    map '/posts/hidden'
    before_all do
      default_before_filter
    end
    layout nil

    def create(post_id)
      post = Libertree::Model::Post[ post_id.to_i ]

      if post
        Libertree::Model::PostHidden.find_or_create(
          account_id: account.id,
          post_id:    post.id,
        )

        return { 'success' => true }.to_json
      end

      ""
    end

    def destroy(post_hidden_id)
      ph = Libertree::Model::PostHidden[ post_hidden_id.to_i ]
      if ph && ph.account == account
        ph.delete
      end
    end
  end
end
