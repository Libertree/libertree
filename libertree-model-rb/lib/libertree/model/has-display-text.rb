require 'libertree/render'

module Libertree
  module Model
    # Provides a "glimpse" instance method to objects that have a "text" field.
    module HasDisplayText
      def glimpse( length = 60 )
        set = Render.to_html_nodeset(self.text, [:no_images, :filter_html])
        set.xpath('.//blockquote').each(&:remove)
        plain_text = set.inner_text.strip.gsub("\n", ' ')

        snippet = plain_text[0...length]
        if plain_text.length > length
          snippet += '...'
        end
        snippet
      end

      def text_as_html
        Render.to_html_nodeset(self.text)
      end
    end
  end
end
