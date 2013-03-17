module Libertree
  module Model
    # Provides a "search" class method to a class that has a "text" field
    # and a time_created field.
    module HasSearchableText
      def search(q, limit = 42)
        words = q.split
        term_expressions = words.map { |word|
          "( text ~* ( E'\\\\m' || ? || E'\\\\M' ) )"
        }
        where_clause = term_expressions.join(" AND ")
        self.prepare(
          "SELECT * FROM #{self.table} WHERE #{where_clause} ORDER BY time_created DESC LIMIT #{limit.to_i}"
        ).s(*words).map { |row|
          self.new row
        }
      end
    end
  end
end
