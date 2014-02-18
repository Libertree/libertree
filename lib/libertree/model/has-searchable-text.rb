module Libertree
  module Model
    # Provides a "search" class method to a class that has a "text" field
    # and a time_created field.
    module HasSearchableText
      def search(q, limit = 42)
        self.where("to_tsvector('simple', text) @@ plainto_tsquery('simple', ?)", q).
          reverse_order(:time_created).
          limit(limit.to_i).
          all
      end
    end
  end
end
