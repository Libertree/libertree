module Libertree
  module Model
    class PoolPost < Sequel::Model(:pools_posts)
      def pool
        Pool[self.pool_id]
      end

      def post
        Post[self.post_id]
      end

      def member
        pool.member
      end
    end
  end
end
