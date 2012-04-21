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
        DB.dbh.d %|DELETE FROM notifications WHERE data = '{"type":"comment","comment_id":#{self.id}}'|
        self.delete
      end

      # TODO: DRY up with Post#glimpse
      def glimpse
        if self.text.length < 61
          self.text
        else
          self.text[0...60] + '...'
        end
      end

      def self.search(q)
        self.s("SELECT * FROM comments WHERE text ILIKE '%' || ? || '%' ORDER BY time_created DESC LIMIT 42", q)
      end
    end
  end
end
