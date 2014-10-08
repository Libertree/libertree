module Controller
  class Docs < Base
    map '/docs'

    layout :default

    def index(lang, document)
      @view = 'docs'

      # leave only safe characters in the document name
      filename_template = "#{Ramaze.options.views[0]}/docs/%s/#{document.gsub(/[^a-zA-Z_-]/,'')}.markdown"

      # restrict language to whitelist and default to en_GB
      if Libertree::LANG.map(&:first).include?(lang) &&
          File.exists?(filename_template % lang)
        filename = filename_template % lang
      else
        filename = filename_template % 'en_GB'
      end

      if File.exists? filename
        contents = IO.read(filename)
        @rendered_page = Libertree.render_unsafe(contents)
      else
        # TODO: print nice error message and index of existing pages
        "Sorry, no such document."
      end
    end
  end
end
