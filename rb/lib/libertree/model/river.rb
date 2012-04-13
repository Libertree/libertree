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

        parts = query_components
        if parts.include?(':tree')
          return  if post.member.account.nil?
        end
        parts.delete ':forest'
        parts.delete ':tree'

        parts.each do |term|
          return  if term =~ /^-./ && post.text =~ /(?:^|\b)#{term[1..-1]}(?:\b|$)/i
        end

        term_match = false
        parts.each do |term|
          term_match ||= ( /(?:^|\b)#{term}(?:\b|$)/ii === post.text )
        end
        return  if ! term_match

        DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM river_posts WHERE river_id = ?", self.id
        delete
      end
    end
  end
end
