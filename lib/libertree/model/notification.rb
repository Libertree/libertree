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
        when 'springing'
          Libertree::Model::PoolPost[ self.data['pool_post_id'] ]
        end
      end

      def self.mark_seen_for_account_and_comment_id(account, comment_id)
        DB.dbh.u(
          %|
            UPDATE notifications
            SET seen = TRUE
            WHERE
              account_id = ?
              AND data = '{"type":"comment","comment_id":#{comment_id.to_i}}'
          |,
          account.id
        )

        Comment[comment_id.to_i].likes.each do |like|
          DB.dbh.u(
            %|
              UPDATE notifications
              SET seen = TRUE
              WHERE
                account_id = ?
                AND data = '{"type":"comment-like","comment_like_id":#{like.id}}'
            |,
            account.id
          )
        end

        account.dirty
      end

      def self.mark_seen_for_account_and_post(account, post)
        # TODO: Is it more efficient to build a (?,?,?...) list and do the
        # update in a single query?
        post.likes.each do |like|
          DB.dbh.u(
            %|
              UPDATE notifications
              SET seen = TRUE
              WHERE
                account_id = ?
                AND data = '{"type":"post-like","post_like_id":#{like.id}}'
            |,
            account.id
          )
        end
        account.dirty
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
