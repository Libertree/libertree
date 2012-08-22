require 'json'

module Libertree
  module Model
    class Notification < M4DBI::Model(:notifications)
      def account
        @account ||= Account[self.account_id]
      end

      def data
        JSON.parse self['data']
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def subject
        @subject ||= case self.data['type']
        when 'comment'
          Libertree::Model::Comment[ self.data['comment_id'] ]
        when 'comment-like'
          Libertree::Model::CommentLike[ self.data['comment_like_id'] ]
        when 'message'
          Libertree::Model::Message[ self.data['message_id'] ]
        when 'post-like'
          Libertree::Model::PostLike[ self.data['post_like_id'] ]
        end
      end

      def self.for_account_and_comment_id(account, comment_id)
        self.prepare(
          %|
            SELECT *
            FROM notifications
            WHERE
              account_id = ?
              AND data = '{"type":"comment","comment_id":#{comment_id.to_i}}'
          |
        ).s(account.id).
          map { |row| self.new row }
      end

      def self.mark_seen_for_account_and_comment_id(account, comment_id)
        self.for_account_and_comment_id(account, comment_id).each do |n|
          n.seen = true
        end
        account.dirty
      end

      # TODO: We do a mass update using the result of this query
      # We may need to optimize this to do the UPDATE in SQL directly.
      def self.for_account_and_post(account, post)
        r = []

        post.likes.each do |like|
          r += self.prepare(
            %|
              SELECT *
              FROM notifications
              WHERE
                account_id = ?
                AND data = '{"type":"post-like","post_like_id":#{like.id}}'
            |
          ).s(account.id).
            map { |row| self.new row }
        end

        post.comments.each do |comment|
          r += self.prepare(
            %|
              SELECT *
              FROM notifications
              WHERE
                account_id = ?
                AND data = '{"type":"comment","comment_id":#{comment.id}}'
            |
          ).s(account.id).
            map { |row| self.new row }

          comment.likes.each do |like|
            r += self.prepare(
              %|
                SELECT *
                FROM notifications
                WHERE
                  account_id = ?
                  AND data = '{"type":"comment-like","comment_like_id":#{like.id}}'
              |
            ).s(account.id).
              map { |row| self.new row }
          end
        end

        r
      end

      def self.mark_seen_for_account_and_message(account, message)
        DB.dbh.u(
          %|
            UPDATE notifications
            SET seen = TRUE
            WHERE
              account_id = ?
              AND data = '{"type":"message","message_id":#{message.id.to_i}}'
          |,
          account.id
        )
        account.dirty
      end
    end
  end
end
