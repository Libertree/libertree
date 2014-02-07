require 'oembed'
require_relative 'embedding/custom-providers'

module Libertree
  module Embedder
    OEmbed::Providers.register(
      OEmbed::Providers::Youtube,
      OEmbed::Providers::Vimeo,
      OEmbed::Providers::Flickr,
      OEmbed::Providers::SoundCloud
    )

    # Ask Youtube to return HTTPS embed codes to keep their iframes
    # from ending up empty when the page is served over HTTPS
    OEmbed::Providers::Youtube.endpoint += "?scheme=https"

    def self.get(url)
      Libertree::Embedding::CustomProviders.get(url) || OEmbed::Providers.get(url, {width: 500}).html
    end

    def self.autoembed(text)
      urls = extract_urls(text)
      urls.each do |url|
        cached = Libertree::Model::EmbedCache[ url: url ]
        next  if cached

        Libertree::Model::Job.find_or_create(
          task: 'http:embed',
          params: {
            'url' => url
          }.to_json
        )
      end
    end

    # @param [Nokogiri::HTML::DocumentFragment] parsed HTML tree
    def self.inject_objects(html)
      # extract every URL from a paragraph and append embed object if supported
      html.css('p').each do |p|
        # collect urls, ignore relative links
        urls = p.xpath(".//a[not(starts-with(@href,'/'))]/@href").map(&:value).reverse
        urls.each do |url|
          cached = Libertree::Model::EmbedCache.
            s("SELECT * FROM embed_cache WHERE url IN (?,?)", url, url.gsub('&amp;', '&'))

          if cached
            p.add_next_sibling("<div class='embed'>#{cached[:object]}</div>")
          end
        end
      end
      html
    end

    def self.supported
      @supported ||= Regexp.union(
        OEmbed::Providers.urls.keys.concat Libertree::Embedding::CustomProviders.urls.keys
      )
    end

    def self.extract_urls(text)
      # Strip off trailing closing parentheses that may be followed by any
      # number of periods/commas.  More often than not they are not part of the
      # URL.  We match against the reverse of the URL to avoid complicating the
      # regular expression with non-greedy look-behind.

      urls = URI.extract(text).map { |url|
        matches = url.reverse.match(/^(?:\.*[,\?]*\.*\)?)?(.+)/)
        if matches
          matches[1].reverse
        else
          url
        end
      }
      urls.find_all {|u| u =~ self.supported}.map(&:strip).uniq
    end

  end
end
