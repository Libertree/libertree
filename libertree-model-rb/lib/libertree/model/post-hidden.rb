module Libertree
  module Model
    class PostHidden < Sequel::Model(:posts_hidden)
      def account
        @account ||= Account[self.account_id]
      end

      def post
        @post ||= Post[self.post_id]
      end

      def forests
        if self.post.remote?
          self.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end
    end
  end
end
