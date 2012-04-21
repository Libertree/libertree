module Libertree
  module Model
    class PostLike < M4DBI::Model(:post_likes)
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

      def after_create
        self.post.notify_about_like self
      end
      def self.create(*args)
        like = super
        like.after_create
        like
      end
      def self.find_or_create(*args)
        like = super
        like.after_create
        like
      end
    end
  end
end
