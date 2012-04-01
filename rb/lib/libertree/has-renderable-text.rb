require 'redcarpet'

module Libertree
  module HasRenderableText
    def text_rendered
      @markdown ||= Redcarpet::Markdown.new(
        Redcarpet::Render::HTML,
        autolink: true,
        space_after_headers: true
      )
      @markdown.render self.text
    end
  end
end
