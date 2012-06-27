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
        self.s(
          %|
            SELECT *
            FROM notifications
            WHERE
              account_id = ?
              AND data = '{"type":"comment","comment_id":#{comment_id.to_i}}'
          |,
          account.id
        )
      end

      # TODO: We do a mass update using the result of this query
      # We may need to optimize this to do the UPDATE in SQL directly.
      def self.for_account_and_post(account, post)
        r = []

        post.likes.each do |like|
          r += self.s(
            %|
              SELECT *
              FROM notifications
              WHERE
                account_id = ?
                AND data = '{"type":"post-like","post_like_id":#{like.id}}'
            |,
            account.id
          )
        end

        post.comments.each do |comment|
          r += self.s(
            %|
              SELECT *
              FROM notifications
              WHERE
                account_id = ?
                AND data = '{"type":"comment","comment_id":#{comment.id}}'
            |,
            account.id
          )

          comment.likes.each do |like|
            r += self.s(
              %|
                SELECT *
                FROM notifications
                WHERE
                  account_id = ?
                  AND data = '{"type":"comment-like","comment_like_id":#{like.id}}'
              |,
              account.id
            )
          end
        end

        r
      end

      def self.create(*args)
        message = super
        message.recipients.each do |recipient|
          if recipient.account
            recipient.account.notify_about  'type' => 'message', 'message_id' => message.id
          end
        end
        message
      end
    end
  end
end
