module Libertree
  module Model
    class PostLike < M4DBI::Model(:post_likes)
      after_create do |like|
        return unless like.local?
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:POST-LIKE',
            params: { 'post_like_id' => like.id, }
          },
          *like.forests
        )
      end

      before_delete do |like|
        return unless like.local?
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:POST-LIKE-DELETE',
            params: { 'post_like_id' => like.id, }
          },
          *like.forests
        )
      end

      def local?
        self.remote_id.nil?
      end

      def member
        @member ||= Member[self.member_id]
      end

      def post
        @post ||= Post[self.post_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def delete_cascade
        DB.dbh.d %|DELETE FROM notifications WHERE data = '{"type":"post-like","post_like_id":#{self.id}}'|
        self.delete
      end

      def self.create(*args)
        like = super
        like.post.notify_about_like like
        account = like.member.account
        if account
          like.post.mark_as_read_by account
        end
        like
      end

      def forests
        if self.post.remote?
          self.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end
    end
  end
end
