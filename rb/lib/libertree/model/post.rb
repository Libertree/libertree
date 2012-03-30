require 'redcarpet'
require 'date'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      def member
        @member ||= Member[self.member_id]
      end

      def text_rendered
        markdown = Redcarpet::Markdown.new(
          Redcarpet::Render::HTML,
          autolink: true,
          space_after_headers: true
        )
        markdown.render text
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
    end
  end
end
