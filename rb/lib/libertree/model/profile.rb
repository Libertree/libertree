module Libertree
  module Model
    class Profile < M4DBI::Model(:profiles)
      def member
        Member[ self.member_id ]
      end

      def self.search(query)
        s  "SELECT * FROM profiles WHERE name_display ILIKE '%' || ? || '%' OR description ILIKE '%' || ? || '%'", query, query
      end
    end
  end
end
