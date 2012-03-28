require 'redcarpet'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      def member
        Member[self.member_id]
      end

      def text_rendered
        markdown = Redcarpet::Markdown.new(
          Redcarpet::Render::HTML,
          autolink: true,
          space_after_headers: true
        )
        markdown.render text
      end
    end
  end
end
