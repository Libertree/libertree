module Libertree
  module Model
    # Provides a "search" class method to a class that has a "text" field
    # and a time_created field.
    module HasSearchableText
      def search(q, limit = 42)
        self.prepare(
          "SELECT * FROM #{self.table} WHERE to_tsvector('simple', text) @@ plainto_tsquery('simple', ?) ORDER BY time_created DESC LIMIT #{limit.to_i}"
        ).s(q).map { |row|
          self.new row
        }
      end
    end
  end
end
