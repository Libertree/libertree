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
    end
  end
end
