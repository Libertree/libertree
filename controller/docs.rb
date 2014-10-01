module Controller
  class Docs < Base
    map '/docs'

    layout :default

    def index(lang, document)
      @view = 'docs'

      # leave only safe characters in the document name
      document.gsub!(/[^a-zA-Z_-]/,'')

      # restrict to whitelist of languages
      lang = if Libertree::LANG.map(&:first).include?(lang) &&
                 File.directory?("#{Ramaze.options.views[0]}/docs/#{lang}/")
               lang
             else
               'en_GB'
             end
      dir = "#{Ramaze.options.views[0]}/docs/#{lang}/"
      filename = "#{dir}/#{document}.markdown"

      if File.exists? filename
        contents = IO.read(filename)
        return Libertree.render_unsafe(contents)
      else
        # TODO: print nice error message and index of existing pages
        "Sorry, no such document."
       end
    end
  end
end
