module Controller
  class Docs < Base
    map '/docs'

    layout :default

    # leave only safe characters in the document name
    PATH = lambda {|doc| "#{Ramaze.options.views[0]}/docs/%s/#{doc.gsub(/[^a-zA-Z_-]/,'')}.markdown" }

    def index(lang='en_GB')
      @view = 'docs'
      redirect r(:show, lang, 'index')
    end

    def show(lang, document)
      @view = 'docs'

      # restrict language to whitelist and default to en_GB
      if Libertree::LANG.map(&:first).include?(lang) &&
         File.exists?(PATH.call(document) % lang)
        filename = PATH.call(document) % lang
      else
        filename = PATH.call(document) % 'en_GB'
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
