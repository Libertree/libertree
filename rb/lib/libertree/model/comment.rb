module Libertree
  module Model
    class Comment < M4DBI::Model(:comments)
      def member
        @member ||= Member[self.member_id]
      end

      def post
        Post[self.post_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
    end
  end
end
