module Libertree
  module Model
    class CommentLike < Sequel::Model(:comment_likes)
      def after_create
        super
        if self.local? && self.comment.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:COMMENT-LIKE',
              params: { 'comment_like_id' => self.id, }
            },
            *self.forests
          )
        end
      end

      def local?
        self.remote_id.nil?
      end

      def member
        @member ||= Member[self.member_id]
      end

      def comment
        @comment ||= Comment[self.comment_id]
      end

      def before_destroy
        if self.local? && self.comment.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:COMMENT-LIKE-DELETE',
              params: { 'comment_like_id' => self.id, }
            },
            *self.forests
          )
        end
        super
      end

      # TODO: the correct method to call is "destroy"
      def delete
        self.before_destroy
        super
      end

      def delete_cascade
        self.before_destroy
        DB.dbh[ "SELECT delete_cascade_comment_like(?)", self.id ].get
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
