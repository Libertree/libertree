module Libertree
  module Model
    class Comment < M4DBI::Model(:comments)
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
        Notification.d %|DELETE FROM notifications WHERE data = '{"type":"comment","comment_id":#{self.id}}'|
      end
    end
  end
end
