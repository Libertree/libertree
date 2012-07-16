module Libertree
  module Model
    class CommentLike < M4DBI::Model(:comment_likes)
      after_create do |like|
        return unless like.is_local?
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:COMMENT-LIKE',
            params: { 'comment_like_id' => like.id, }
          },
          *like.forests
        )
      end

      before_delete do |like|
        return unless like.is_local?
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:COMMENT-LIKE-DELETE',
            params: { 'comment_like_id' => like.id, }
          },
          *like.forests
        )
      end

      def is_local?
        self.remote_id.nil?
      end

      def member
        @member ||= Member[self.member_id]
      end

      def comment
        @comment ||= Comment[self.comment_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def delete_cascade
        DB.dbh.d %|DELETE FROM notifications WHERE data = '{"type":"comment-like","comment_like_id":#{self.id}}'|
        self.delete
      end

      def self.create(*args)
        like = super
        like.comment.notify_about_like like
        like
      end

      def forests
        if self.comment.post.remote?
          self.comment.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end
    end
  end
end
