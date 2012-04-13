module Libertree
  module Model
    class River < M4DBI::Model(:rivers)
      def posts(limit = 30)
        Post.s(
          %{
            SELECT * FROM (
              SELECT p.*
              FROM
                  river_posts rp
                , posts p
              WHERE
                p.id = rp.post_id
                AND rp.river_id = ?
              ORDER BY p.id DESC
              LIMIT #{limit.to_i}
            ) AS x
            ORDER BY id
          },
          self.id
        )
      end

      def query_components
        @query_components ||= self.query.split(/\s+/)
      end

      def try_post(post)
        return  if DB.dbh.sc "SELECT EXISTS( SELECT 1 FROM river_posts WHERE river_id = ? AND post_id = ? LIMIT 1 )", self.id, post.id

        if query_components.include?(':tree')
          return  if post.member.account.nil?
        end

        DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id
      end
    end
  end
end
