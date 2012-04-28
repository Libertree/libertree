require 'date'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)

      def member
        @member ||= Member[self.member_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def remote?
        !! remote_id
      end

      def local?
        ! remote_id
      end

      def public_id
        self.remote_id || self.id
      end

      def server
        member.server
      end

      def read_by?(account)
        DB.dbh.sc  "SELECT EXISTS( SELECT 1 FROM posts_read WHERE post_id = ? AND account_id = ? LIMIT 1 )", self.id, account.id
      end

      def mark_as_read_by(account)
        DB.dbh.execute(
          %{
            INSERT INTO posts_read ( post_id, account_id )
            SELECT ?, ?
            WHERE NOT EXISTS (
              SELECT 1
              FROM posts_read
              WHERE
                post_id = ?
                AND account_id = ?
            )
          },
          self.id,
          account.id,
          self.id,
          account.id
        )
        DB.dbh.delete(
          %{
            DELETE FROM river_posts rp
            USING rivers r
            WHERE
              rp.river_id = r.id
              AND r.account_id = ?
              AND r.query LIKE '%:unread%'
              AND rp.post_id = ?
          },
          account.id,
          self.id
        )
      end

      def mark_as_unread_by(account)
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ? AND account_id = ?", self.id, account.id
        self.add_to_matching_rivers
      end

      def mark_as_unread_by_all
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ?", self.id
        self.add_to_matching_rivers
      end

      def self.mark_all_as_read_by(account)
        DB.dbh.execute(
          %{
            INSERT INTO posts_read ( post_id, account_id )
            SELECT
                p.id
              , ?
            FROM
              posts p
            WHERE NOT EXISTS (
              SELECT 1
              FROM posts_read pr2
              WHERE
                pr2.post_id = p.id
                AND pr2.account_id = ?
            )
          },
          account.id,
          account.id
        )
        DB.dbh.delete(
          %{
            DELETE FROM river_posts rp
            USING rivers r
            WHERE
              rp.river_id = r.id
              AND r.account_id = ?
              AND r.query LIKE '%:unread%'
          },
          account.id
        )
      end

      def comments
        Comment.s "SELECT * FROM comments WHERE post_id = ? ORDER BY id", self.id
      end

      def likes
        PostLike.s "SELECT * FROM post_likes WHERE post_id = ? ORDER BY id DESC", self.id
      end

      def notify_about_comment(comment)
        notification_attributes = {
          'type'       => 'comment',
          'comment_id' => comment.id,
        }
        accounts = []
        local_post_author = comment.post.member.account
        accounts << local_post_author

        comment_author = comment.member.account
        comments.each do |c|
          accounts << c.member.account
        end

        accounts.uniq!
        accounts.compact!
        accounts.delete comment_author
        accounts.each do |a|
          a.notify_about notification_attributes
        end
      end

      def notify_about_like(like)
        notification_attributes = {
          'type'         => 'post-like',
          'post_like_id' => like.id,
        }
        local_post_author = like.post.member.account
        like_author = like.member.account

        if local_post_author && local_post_author != like_author
          local_post_author.notify_about notification_attributes
        end
      end

      def glimpse( length = 60 )
        if self.text.length <= length
          self.text
        else
          self.text[0...length] + '...'
        end
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM posts_read WHERE post_id = ?", self.id
        DB.dbh.delete "DELETE FROM river_posts WHERE post_id = ?", self.id
        delete
      end

      def add_to_matching_rivers
        River.each do |river|
          river.try_post self
        end
      end

      def self.create(*args)
        post = super
        post.add_to_matching_rivers
        if post.member.account
          post.mark_as_read_by post.member.account
        end
        post
      end

      def self.add_recent_to_river(river, n = 100)
        posts = self.s("SELECT * FROM posts ORDER BY id DESC LIMIT #{n.to_i}")
        posts.each do |p|
          river.try_post p
        end
      end

      def like_by(member)
        PostLike[ member_id: member.id, post_id: self.id ]
      end

      def self.search(q)
        self.s("SELECT * FROM posts WHERE text ILIKE '%' || ? || '%' ORDER BY time_created DESC LIMIT 42", q)
      end

      # TODO: Optionally restrict by account, so as not to reveal too much to browser/client
      # i.e. rivers not belonging to current account
      def rivers_belonged_to
        River.s(
          %{
            SELECT
              r.*
            FROM
                rivers r
              , river_posts rp
            WHERE
              rp.river_id = r.id
              AND rp.post_id = ?
          },
          self.id
        )
      end
    end
  end
end
