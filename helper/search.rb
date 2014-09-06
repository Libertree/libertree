module Ramaze
  module Helper
    module Search
      def highlight_query(text, query)
        text.gsub(query, "<em class='highlight'>#{query}</em>")
      end
    end
  end
end


