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

    def self.get(url)
      Libertree::Embedding::CustomProviders.get(url) || OEmbed::Providers.get(url, {width: 500}).html
    end

    def self.autoembed(text)
      urls = extract_urls(text)
      urls.each do |url|
        Libertree::Model::Job.create(
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
          # TODO: why hit the db twice?
          cached = (
            Libertree::Model::EmbedCache[ url: url ] ||
            Libertree::Model::EmbedCache[ url: url.gsub('&amp;', '&') ]
          )
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
      urls = URI.extract text
      urls.find_all {|u| u =~ self.supported}.map(&:strip)
    end

  end
end
