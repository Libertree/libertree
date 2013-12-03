module Libertree
  module References

    # @param refs [Array]:
    #
    #   Every reference contains an extracted URL, including the preceeding
    #   character in the case of relative URLs, e.g. "http://tree.org/posts/show/123"
    #   or "(/posts/show/123".
    #
    #   A substring that identifies an entity (a post or a comment) is called a
    #   segment. The URL " /posts/show/366/128#comment-128" has the following
    #   two segments:
    #
    #     "/posts/show/366" (a post)
    #     "128#comment-128" (a comment)
    #
    #   For every segment the matching URL substring ("url"), the id
    #   of the local copy of the entity that is referenced by the
    #   segment ("id") and the domain of the origin of the entity
    #   ("origin") is recorded. The origin domain is not included if
    #   the server that sends the references is the origin of the
    #   described entity.
    #
    #   The following is an example of a refs array for a post that
    #   contains three URLs to local entities.

    #     [{ "match" => "(/posts/show/366",
    #        "post" => {
    #          "url" => "/posts/show/366",
    #          "id" => 366,
    #          "origin" => "some.remote.tree" }},
    #      { "match" => " /posts/show/366/128#comment-128",
    #        "post" => {
    #          "url" => "/posts/show/366",
    #          "id" => 365,
    #          "origin" => "some.remote.tree" },
    #        "comment" => {
    #          "url" => "/128#comment-128",
    #          "id" => 127,
    #          "origin" => "some.remote.tree" }},
    #      { "match" => "http://never-mind.org/posts/show/366/128",
    #        "post" => {
    #          "url" => "/posts/show/366",
    #          "id" => 365,
    #          "origin" => "some.remote.tree" },
    #        "comment" => {
    #          "url" => "/128",
    #          "id" => 127,
    #          "origin" => "some.remote.tree" }}]
    #
    #   This is the translation to XML:
    #
    #     <references>
    #       <reference>
    #         <match>"(/posts/show/366"</match>
    #         <post>
    #           <url>/posts/show/366</url>
    #           <id>366</id>
    #           <origin>some.remote.tree</origin>
    #         </post>
    #       </reference>
    #
    #       <reference>
    #         <match>" /posts/show/366/128#comment-128"</match>
    #         <post>
    #           <url>/posts/show/366</url>
    #           <id>365</id>
    #           <origin>some.remote.tree</origin>
    #         </post>
    #         <comment>
    #           <url>/128#comment-128</url>
    #           <id>127</id>
    #           <origin>some.remote.tree</origin>
    #         </comment>
    #       </reference>
    #
    #       <reference>
    #         <match>"http://never-mind.org/posts/show/366/128"</match>
    #         <post>
    #           <url>"/posts/show/366"</url>
    #           <id>365</id>
    #           <origin>some.remote.tree</origin>
    #         </post>
    #         <comment>
    #           <url>"/128"</url>
    #           <id>127</id>
    #           <origin>some.remote.tree</origin>
    #         </comment>
    #       </reference>
    #     <references>

    def self.replace(text_, refs, server_id, own_domain)
      text = text_.dup

      # refs can be an array of references or just a single reference
      # Make sure it's an array.
      refs = Array(refs)

      refs.each do |ref|
        url = ref['match']
        substitution = ref.entries.drop(1).reduce(url) do |res, pair|
          type, ref = pair
          segment = ref['url']

          if ref.has_key? 'origin'
            if ref['origin'].to_s == own_domain
              local = true
            else
              server = Model::Server[ domain: ref['origin'].to_s ]
            end
          else
            server = Model::Server[ server_id ]
          end
          next res  if server.nil? && ! local

          if type == "post"
            model = Model::Post
            target = segment
          else
            model = Model::Comment

            # Look-behind does not support pattern repetition with * or +,
            # so we have to first extract the post id from the url.
            post_id = res.match(%r{/posts/show/(?<post_id>\d+)/})['post_id']
            target = %r{(?<=/posts/show/#{post_id})#{Regexp.quote(segment)}}
          end

          if local
            entities = model.where( id: ref['id'].to_i )
          else
            entities = model.where( remote_id: ref['id'].to_i ).
                       reject {|e| e.member.server != server }
          end

          if entities.empty?
            next res
          else
            updated_segment = segment.gsub(/\d+/, entities[0].id.to_s)
            next res.sub(target, updated_segment)
          end
        end

        if url[/^http/]
          # Chop off everything before /posts/show to turn this into a relative URL.
          new_url = substitution.partition("/posts/").drop(1).join("")
        else
          new_url = substitution
        end

        # only replace full matches
        text.gsub!(%r{#{Regexp.quote(url)}(?!(/|#))}, new_url)
      end

      text
    end

    def self.extract(text, server_name)
      # - match absolute links only if they mention the host of this server
      # - match relative links ("/posts/show/123") when they are beginning the
      #   line (^) or when they are preceded by a space-like character (\s)
      # - capture the matched url
      #
      # Known problems:
      #   - links in verbatim sections are identified, because we extract
      #     references from the raw markdown, not the rendered text.

      pattern = %r{(?<url>(#{server_name}|\s|\()(?<post_url>/posts/show/(?<post_id>\d+))(?<comment_url>/(?<comment_id>\d+)(#comment-\d+)?)?)}

      refs = []
      text.scan(pattern) do |url, post_url, post_id, comment_url, comment_id|
        next  if post_id.nil?

        post = Libertree::Model::Post[ post_id.to_i ]
        next  if post.nil?

        ref = { 'match' => url }

        map = {
          'url' => post_url,
          'id'  => post.remote_id || post_id.to_i,
        }
        if post.server
          map['origin'] = post.server.domain
        end
        ref['post'] = map

        comment = Libertree::Model::Comment[ comment_id.to_i ]
        if comment
          map = {
            'url' => comment_url,
            'id'  => comment.remote_id || comment_id.to_i
          }
          if comment.server
            map['origin'] = comment.server.domain
          end
          ref['comment'] = map
        end

        refs << ref
      end

      refs
    end

  end
end
