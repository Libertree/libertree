require 'date'
require 'libertree/has-renderable-text'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      include Libertree::HasRenderableText

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

      def notify_about_comment(comment)
        notification_attributes = {
          'type'       => 'comment',
          'comment_id' => comment.id,
        }
        local_post_author = comment.post.member.account
        if local_post_author
          local_post_author.notify_about notification_attributes
        end

        author = comment.member.account
        accounts = []
        comments.each do |c|
          accounts << c.member.account
        end
        accounts.uniq.compact.each do |a|
          if a != author
            a.notify_about notification_attributes
          end
        end
      end

      def glimpse
        if self.text.length < 61
          self.text
        else
          self.text[0...60] + '...'
        end
      end
    end
  end
end
