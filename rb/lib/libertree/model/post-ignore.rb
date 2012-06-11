module Libertree
  module Model
    class PostIgnore < M4DBI::Model(:post_ignores)
      def account
        @account ||= Account[self.account_id]
      end

      def post
        @post ||= Post[self.post_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def forests
        if self.post.remote?
          self.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end

      def create(*args)
        ignore = create
        DB.dbh.d(
          %{
            DELETE FROM river_posts rp
            USING rivers r
            WHERE
              rp.post_id = ?
              AND r.id = rp.river_id
              AND r.account_id = ?
          },
          self.post_id,
          self.account_id
        )
        ignore
      end

      def delete
        self.account.rivers.each do |r|
          r.try_post self.post
        end
        super
      end
    end
  end
end
