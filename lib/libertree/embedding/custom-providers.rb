require 'nokogiri'

module Libertree
  module Embedding
    class Error < StandardError; end

    module CustomProviders
      def self.urls
        {
          Youku.format => Youku,
          FreeMusicArchive.format => FreeMusicArchive,
          Jamendo.format => Jamendo,
          TED.format => TED,
          Bandcamp.format => Bandcamp,
        }
      end

      def self.get(url)
        self.urls.each do |k,v|
          if url =~ k
            return v.get(url)
          end
        end
        nil
      end

      class Youku
        def self.format
          %r{http://v\.youku.com/v_show/id_(.+)\.html}
        end

        def self.get(url)
          m = url.match self.format
          if m
            return <<OBJECT
<embed src="http://player.youku.com/player.php/sid/#{m[1]}/v.swf" allowFullScreen="true" quality="high" width="480" height="400" align="middle" allowScriptAccess="always" type="application/x-shockwave-flash"></embed>
OBJECT
          else
            raise Libertree::Embedding::Error, "failed to extract video id"
          end
        end
      end

      class FreeMusicArchive
        def self.format
          %r{http://freemusicarchive\.org/music/.+}
        end

        def self.get(url)
          return unless url =~ self.format

          uri = URI.parse(url)
          Timeout.timeout(10) do
            Net::HTTP.start(uri.host) do |http|
              resp = http.get(uri.path)
              html = Nokogiri::HTML(resp.body)
              e = html.css('.inp-embed-code input')[0]
              if e
                return e.attr('value')
              else
                raise Libertree::Embedding::Error, "failed to find embedding code"
              end
            end
          end
        end
      end

      class Jamendo
        def self.format
          %r{http://www.jamendo\.com/.+/track/.+}
        end

        def self.get(url)
          return unless url =~ self.format

          uri = URI.parse(url)
          Timeout.timeout(10) do
            Net::HTTP.start(uri.host) do |http|
              resp = http.get(uri.path)
              html = Nokogiri::HTML(resp.body)
              e = html.css('.preview object')[0]
              if e
                return e.to_html
              else
                raise Libertree::Embedding::Error, "failed to find embedding code"
              end
            end
          end
        end
      end

      class TED
        def self.format
          %r{http://www\.ted\.com/talks/.+\.html}
        end

        def self.get(url)
          return unless url =~ self.format

          uri = URI.parse(url)
          Timeout.timeout(10) do
            Net::HTTP.start(uri.host) do |http|
              resp = http.get(uri.path)
              html = Nokogiri::HTML(resp.body)
              e = html.css('#embedthisvideo')[0]
              if e
                return e.attr('value')
              else
                raise Libertree::Embedding::Error, "failed to find embedding code"
              end
            end
          end
        end
      end

      class Bandcamp
        def self.format
          %r{https?://.*\.bandcamp\.com/(track|album)/.+}
        end

        def self.get(url)
          return unless url =~ self.format

          type = $1

          uri = URI.parse(url)
          Timeout.timeout(10) do
            Net::HTTP.start(uri.host) do |http|
              resp = http.get(uri.path)
              html = Nokogiri::HTML(resp.body)
              node = html.xpath("//meta[@property='og:video']/@content")
              if node.empty?
                raise Libertree::Embedding::Error, "failed to find embedding code"
              else
                if node.text.match(/#{type}=(\d+)/)
                  "<iframe width=\"400\" height=\"100\" frameborder=\"0\" allowtransparency=\"true\" src=\"https://bandcamp.com/EmbeddedPlayer/size=venti/#{type}=#{$1}\"></iframe>"
                else
                  raise Libertree::Embedding::Error, "failed to find #{type} id"
                end
              end
            end
          end
        end
      end

    end
  end
end
