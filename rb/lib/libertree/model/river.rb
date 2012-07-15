module Libertree
  module Model
    class River < M4DBI::Model(:rivers)
      def account
        @account ||= Account[self.account_id]
      end

      def posts( opts = {} )
        limit = opts.fetch(:limit, 30)
        older_than = Time.at( opts.fetch(:older_than, Time.now.to_i) )

        if opts[:order_by] == :comment
          Post.s(
            %{
              SELECT * FROM (
                SELECT
                    p.*
                FROM
                    river_posts rp
                  , view__posts_with_time_updated_overall p
                WHERE
                  p.id = rp.post_id
                  AND rp.river_id = ?
                  AND p.time_updated_overall < ?
                  AND NOT EXISTS(
                    SELECT 1
                    FROM posts_hidden pi
                    WHERE
                      pi.account_id = ?
                      AND pi.post_id = rp.post_id
                  )
                ORDER BY p.time_updated_overall DESC
                LIMIT #{limit}
              ) AS x
              ORDER BY time_updated_overall
            },
            self.id,
            older_than.strftime("%Y-%m-%d %H:%M:%S.%6N%z"),
            self.account.id
          )
        else
          Post.s(
            %{
              SELECT * FROM (
                SELECT
                  p.*
                FROM
                    river_posts rp
                  , posts p
                WHERE
                  p.id = rp.post_id
                  AND rp.river_id = ?
                  AND p.time_created < ?
                  AND NOT EXISTS(
                    SELECT 1
                    FROM posts_hidden pi
                    WHERE
                      pi.account_id = ?
                      AND pi.post_id = rp.post_id
                  )
                ORDER BY p.time_created DESC
                LIMIT #{limit}
              ) AS x
              ORDER BY id
            },
            self.id,
            older_than.strftime("%Y-%m-%d %H:%M:%S.%6N%z"),
            self.account.id
          )
        end
      end

      def query_components
        @query_components ||= self.query.scan(/([+-]?"[^"]+")|([+-]?:from ".+?")|([+-]?:river ".+?")|(\S+)/).map { |c|
          c[3] || c[2] || c[1] || c[0].gsub(/^([+-])"/, "\\1").gsub(/^"|"$/, '')
        }
        @query_components.dup
      end

      def term_matches_post?(term, post)
        case term
        when /^:from "(.+?)"$/
          post.member.name_display == $1
        when /^:river "(.+?)"$/
          river = River[label: $1]
          river && river.matches_post?(post)
        else
          /(?:^|\b|\s)#{term}(?:\b|\s|$)/i === post.text
        end
      end

      def matches_post?(post)
        parts = query_components

        # Scope limiters

        parts.delete ':forest'
        return false  if parts.include?(':tree') && post.member.account.nil?
        parts.delete ':tree'
        return false  if parts.include?(':unread') && post.read_by?( self.account )
        parts.delete ':unread'
        return false  if parts.include?(':liked') && ! post.liked_by?( self.account.member )
        parts.delete ':liked'

        # Negations: Must not satisfy any of the conditions

        parts.dup.each do |term|
          if term =~ /^-(.+)$/
            positive_term = $1
            parts.delete term
            return false  if term_matches_post?(positive_term, post)
          end
        end

        # Requirements: Must satisfy every required condition

        matches_all = true
        parts.dup.each do |term|
          if term =~ /^\+(.+)$/
            actual_term = $1
            parts.delete term
            matches_all &&= term_matches_post?(actual_term, post)
          end
        end
        return false  if ! matches_all

        # Regular terms: Must satisfy at least one condition

        if parts.any?
          term_match = false
          parts.each do |term|
            term_match ||= term_matches_post?(term, post)
          end
          return false  if ! term_match
        end

        true
      end

      def try_post(post)
        # TODO: We may be able to fold these two EXISTS clauses into the INSERT query at the end of this method
        return  if DB.dbh.sc "SELECT EXISTS( SELECT 1 FROM river_posts WHERE river_id = ? AND post_id = ? LIMIT 1 )", self.id, post.id
        return  if DB.dbh.sc "SELECT EXISTS( SELECT 1 FROM posts_hidden WHERE account_id = ? AND post_id = ? LIMIT 1 )", self.account.id, post.id

        if self.matches_post?(post)
          DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id
        end
      end

      def refresh_posts( n = 512 )
        DB.dbh.d  "DELETE FROM river_posts WHERE river_id = ?", self.id
        posts = Post.s("SELECT * FROM posts ORDER BY id DESC LIMIT #{n.to_i}")
        posts.each do |p|
          self.try_post p
        end
      end

      # @param params Untrusted parameter Hash.  Be careful, this input comes from the outside world.
      def revise( params )
        self.label = params['label'].to_s
        self.query = params['query'].to_s
        refresh_posts
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM river_posts WHERE river_id = ?", self.id
        delete
      end

      def self.create(*args)
        river = super
        Post.add_recent_to_river river
        river
      end

      def self.ensure_beginner_rivers_for(account)
        River.find_or_create(
          account_id: account.id,
          label: 'Posts from my tree',
          query: ':tree'
        )
        River.find_or_create(
          account_id: account.id,
          label: 'Posts from the forest',
          query: ':forest'
        )
        River.find_or_create(
          account_id: account.id,
          label: 'Unread posts from the forest',
          query: ':unread'
        )
      end

      def home?
        self.home
      end

      def to_hash
        {
          'id'    => self.id,
          'label' => self.label,
          'query' => self.query,
        }
      end
    end
  end
end
