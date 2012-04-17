module Libertree
  module Model
    class Profile < M4DBI::Model(:profiles)
      def member
        Member[ self.member_id ]
      end
    end
  end
end
