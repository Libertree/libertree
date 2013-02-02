module Libertree
  module Model
    class Profile < M4DBI::Model(:profiles)
      after_update do |profile_before, profile|
        if profile.member.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER',
              params: { 'member_id' => profile.member.id, }
            }
          )
        end
      end

      def member
        @member ||= Member[ self.member_id ]
      end

      def self.search(query)
        s  "SELECT * FROM profiles WHERE name_display ILIKE '%' || ? || '%' OR description ILIKE '%' || ? || '%'", query, query
      end
    end
  end
end
