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
      end

      def mark_as_unread_by(account)
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ? AND account_id = ?", self.id, account.id
      end

      def mark_as_unread_by_all
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ?", self.id
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

      def glimpse
        if self.text.length < 61
          self.text
        else
          self.text[0...60] + '...'
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
        post
      end

      def self.find_or_create(*args)
        post = super
        post.add_to_matching_rivers
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
    end
  end
end
