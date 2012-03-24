module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      def member
        Member[self.member_id]
      end
    end
  end
end
