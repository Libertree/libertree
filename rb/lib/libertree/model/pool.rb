module Libertree
  module Model
    class Pool < M4DBI::Model(:pools)
      def member
        @member ||= Member[self.member_id]
      end

      def posts
        @posts ||= Post.prepare(
          %{
            SELECT
              p.*
            FROM
                posts p
              , pools_posts pp
            WHERE
              p.id = pp.post_id
              AND pp.pool_id = ?
            ORDER BY
              p.id DESC
          }
        ).s(self.id).map { |row| Post.new row }
      end

      def includes?(post)
        posts.include? post
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM pools_posts WHERE pool_id = ?", self.id
        self.delete
      end

      def <<(post)
        DB.dbh.i(
          %{
            INSERT INTO pools_posts (
              pool_id, post_id
            )  SELECT
              ?, ?
            WHERE NOT EXISTS(
              SELECT 1
              FROM pools_posts
              WHERE
                pool_id = ?
                AND post_id = ?
            )
          },
          self.id,
          post.id,
          self.id,
          post.id
        )
      end

      def remove_post(post)
        DB.dbh.d  "DELETE FROM pools_posts WHERE pool_id = ? AND post_id = ?", self.id, post.id
      end
    end
  end
end
