module Libertree
  module References

    # @param refs [Hash] TODO: explain the refs param, and give an example of it
    def self.replace(text, refs, server_id)
      refs.each do |url, segments|
        substitution = segments.entries.reduce(url) do |res, pair|
          segment, ref = pair
          if ref.has_key? 'origin'
            server = Model::Server[ public_key: ref['origin'] ]
          else
            server = Model::Server[ server_id ]
          end
          next res  if server.nil?

          if segment =~ /posts/
            model = Model::Post
          else
            model = Model::Comment
          end
          entities = model.where( remote_id: ref['id'].to_i ).
                     reject {|e| e.member.server != server }
          if entities.empty?
            next res
          else
            next res.sub(segment, segment.sub(/\d+/, entities[0].id.to_s))
          end
        end

        if url[/^http/]
          # Chop off everything before /posts/show to turn this into a relative URL.
          new_url = substitution.partition("/posts/").drop(1).join("")
        else
          new_url = substitution
        end
        text.gsub!(url, new_url)
      end

      text
    end

    def self.extract(text, server_name)
      # - match absolute links only if they mention the host of this server
      # - match relative links ("/posts/123") when they are beginning the
      #   line (^) or when they are preceded by a space-like character (\s)
      # - capture the matched url
      #
      # Known problems:
      #   - links in verbatim sections are identified, because we extract
      #     references from the raw markdown, not the rendered text.

      pattern = %r{(?<url>(https?://#{server_name}|\s|\()/posts/show/(?<post_id>\d+)(#comment-(?<comment_id>\d+))?)}

      refs = {}
      text.scan(pattern) do |url, post_id, comment_id|
        next  if post_id.nil?

        post = Libertree::Model::Post[ post_id.to_i ]
        next  if post.nil?

        ref = {}

        map = { 'id' => post.remote_id || post_id.to_i }
        if post.server
          map.merge!( { 'origin' => post.server.public_key } )
        end
        ref["/posts/show/#{post_id}"] = map

        comment = Libertree::Model::Comment[ comment_id.to_i ]
        if comment
          map = { 'id' => comment.remote_id || comment_id.to_i }
          if comment.server
            map.merge!({ 'origin' => comment.server.public_key })
          end
          ref["#comment-#{comment_id}"] = map
        end

        refs[url] = ref  if ref.any?
      end

      refs
    end

  end
end
