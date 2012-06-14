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
        @query_components ||= self.query.scan(/(-?"[^"]+")|(-?:from ".+?")|(\S+)/).map { |c|
          c[2] || c[1] || c[0].gsub(/^-"/, '-').gsub(/^"|"$/, '')
        }
        @query_components.dup
      end

      def term_matches_post?(term, post)
        if term =~ /^:from "(.+?)"$/
          term_match ||= ( post.member.name_display == $1 )
        else
          term_match ||= ( /(?:^|\b|\s)#{term}(?:\b|\s|$)/i === post.text )
        end
      end

      def try_post(post)
        # TODO: We may be able to fold these two EXISTS clauses into the INSERT query at the end of this method
        return  if DB.dbh.sc "SELECT EXISTS( SELECT 1 FROM river_posts WHERE river_id = ? AND post_id = ? LIMIT 1 )", self.id, post.id
        return  if DB.dbh.sc "SELECT EXISTS( SELECT 1 FROM posts_hidden WHERE account_id = ? AND post_id = ? LIMIT 1 )", self.account.id, post.id

        parts = query_components
        return  if parts.include?(':tree') && post.member.account.nil?
        return  if parts.include?(':unread') && post.read_by?( self.account )
        # TODO: Maybe just delete everything starting with a colon?
        parts.delete ':forest'
        parts.delete ':tree'
        parts.delete ':unread'

        # Negations: Must not satisfy any of the conditions

        parts.dup.each do |term|
          if term =~ /^-(.+)$/
            positive_term = $1
            parts.delete term
            return  if term_matches_post?(positive_term, post)
          end
        end

        # TODO: Requirements: Must satisfy every required condition

        # Regular terms: Must satisfy at least one condition

        if parts.any?
          term_match = false
          parts.each do |term|
            term_match ||= term_matches_post?(term, post)
          end
          return  if ! term_match
        end

        DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id
      end

      def refresh_posts( n = 100 )
        DB.dbh.d  "DELETE FROM river_posts WHERE river_id = ?", self.id
        posts = Post.s("SELECT * FROM posts ORDER BY id DESC LIMIT #{n.to_i}")
        posts.each do |p|
          self.try_post p
        end
      end

      def revise( params )
        self.label = params['label']
        self.query = params['query'].downcase
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
    end
  end
end
