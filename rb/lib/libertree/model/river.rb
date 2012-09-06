module Libertree
  module Model
    class River < M4DBI::Model(:rivers)
      def account
        @account ||= Account[self.account_id]
      end

      def posts( opts = {} )
        limit = opts.fetch(:limit, 30)
        if opts[:newer]
          time_comparator = '>'
        else
          time_comparator = '<'
        end
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")

        # TODO: prepared statement?
        if opts[:order_by] == :comment
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
                  AND GREATEST(p.time_commented, p.time_updated) #{time_comparator} ?
                  AND NOT EXISTS(
                    SELECT 1
                    FROM posts_hidden pi
                    WHERE
                      pi.account_id = ?
                      AND pi.post_id = rp.post_id
                  )
                ORDER BY GREATEST(p.time_commented, p.time_updated) DESC
                LIMIT #{limit}
              ) AS x
              ORDER BY GREATEST(time_commented, time_updated)
            },
            self.id,
            time,
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
                  AND p.time_created #{time_comparator} ?
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
            time,
            self.account.id
          )
        end
      end

      def query_components
        full_query = self.query
        if ! self.appended_to_all
          full_query += ' ' + self.account.rivers_appended.map(&:query).join(' ')
          full_query.strip!
        end
        # TODO: This is getting bulky and ugly...
        @query_components ||= full_query.scan(/([+-]?"[^"]+")|([+-]?:from ".+?")|([+-]?:river ".+?")|([+-]?:contact-list ".+?")|([+-]?:visibility [a-z-]+)|([+-]?:word-count [<>] ?[0-9]+)|(\S+)/).map { |c|
          c[6] || c[5] || c[4] || c[3] || c[2] || c[1] || c[0].gsub(/^([+-])"/, "\\1").gsub(/^"|"$/, '')
        }
        @query_components.dup
      end

      def term_matches_post?(term, post)
        case term
        when /^:forest$/
          true  # Every post is a post in the forest.  :forest is sort of a no-op term
        when /^:tree$/
          post.member.account
        when /^:unread$/
          ! post.read_by?(self.account)
        when /^:liked$/
          post.liked_by? self.account.member
        when /^:commented$/
          post.commented_on_by? self.account.member
        when /^:subscribed$/
          self.account.subscribed_to? post
        when /^:contact-list "(.+?)"$/
          self.account.has_contact_list_by_name_containing_member?  $1, post.member
        when /^:from "(.+?)"$/
          post.member.name_display == $1
        when /^:river "(.+?)"$/
          river = River[label: $1]
          river && river.matches_post?(post)
        when /^:visibility ([a-z-]+)$/
          post.visibility == $1
        when /^:word-count < ?([0-9]+)$/
          n = $1.to_i
          post.text.scan(/\S+/).count < n
        when /^:word-count > ?([0-9]+)$/
          n = $1.to_i
          post.text.scan(/\S+/).count > n
        else
          /(?:^|\b|\s)#{Regexp.escape(term)}(?:\b|\s|$)/i === post.text
        end
      end

      def matches_post?(post)
        parts = query_components

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
        return  if River.prepare("SELECT EXISTS( SELECT 1 FROM river_posts WHERE river_id = ? AND post_id = ? LIMIT 1 )").sc( self.id, post.id )
        return  if River.prepare("SELECT EXISTS( SELECT 1 FROM posts_hidden WHERE account_id = ? AND post_id = ? LIMIT 1 )").sc( self.account.id, post.id )

        if self.matches_post?(post)
          DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id
        end
      end

      def refresh_posts( n = 512 )
        DB.dbh.d  "DELETE FROM river_posts WHERE river_id = ?", self.id
        # TODO: prepared statement?
        posts = Post.s("SELECT * FROM posts ORDER BY id DESC LIMIT #{n.to_i}")
        posts.each do |p|
          self.try_post p
        end
      end

      # @param params Untrusted parameter Hash.  Be careful, this input usually comes from the outside world.
      def revise( params )
        self.label = params['label'].to_s
        self.query = params['query'].to_s

        n = River.num_appended_to_all
        self.appended_to_all = !! params['appended_to_all']
        if River.num_appended_to_all != n || self.appended_to_all
          job_data = {
            task: 'river:refresh-all',
            params: {
              'account_id' => self.account_id,
            }.to_json
          }
          existing_jobs = Job.pending_where(
            %{
              task = ?
              AND params = ?
            },
            job_data[:task],
            job_data[:params]
          )
          if existing_jobs.empty?
            Job.create job_data
          end
        end

        if ! self.appended_to_all
          Libertree::Model::Job.create(
            task: 'river:refresh',
            params: {
              'river_id' => self.id,
            }.to_json
          )
        end
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM river_posts WHERE river_id = ?", self.id
        if self.appended_to_all
          Libertree::Model::Job.create(
            task: 'river:refresh-all',
            params: {
              'account_id' => self.account_id,
            }.to_json
          )
        end
        self.delete
      end

      def self.num_appended_to_all
        DB.dbh.sc "SELECT COUNT(*) FROM rivers WHERE appended_to_all"
      end

      def self.create(*args)
        n = River.num_appended_to_all
        river = super

        if River.num_appended_to_all != n
          Libertree::Model::Job.create(
            task: 'river:refresh-all',
            params: {
              'account_id' => river.account_id,
            }.to_json
          )
        end

        if ! river.appended_to_all
          Libertree::Model::Job.create(
            task: 'river:refresh',
            params: {
              'river_id' => river.id,
            }.to_json
          )
        end

        river
      end

      def self.ensure_beginner_rivers_for(account)
        River.find_or_create(
          account_id: account.id,
          label: 'Posts from the forest',
          query: ':forest'
        )
        River.find_or_create(
          account_id: account.id,
          label: 'Unread posts from the forest',
          query: '+:unread'
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

      def being_processed?
        !! Job[
          task: 'river:refresh',
          params: %|{"river_id":#{self.id}}|,
          time_finished: nil
        ]
      end
    end
  end
end
