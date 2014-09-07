module Libertree
  module Model
    class River < Sequel::Model(:rivers)
      def account
        @account ||= Account[self.account_id]
      end

      def should_contain?( post )
        ! self.contains?(post) && ! post.hidden_by?(self.account) && self.matches_post?(post)
      end

      def contains?( post )
        Libertree::DB.dbh[ "SELECT river_contains_post(?, ?)", self.id, post.id ].single_value
      end

      def add_post( post )
        Libertree::DB.dbh[ "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", self.id, post.id ].get
      end

      def posts( opts = {} )
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")
        Post.s(%{SELECT * FROM posts_in_river(?,?,?,?,?,?)},
               self.id,
               self.account.id,
               time,
               opts[:newer],
               opts[:order_by] == :comment,
               opts.fetch(:limit, 30))
      end

      def parsed_query(override_cache=false)
        return @parsed_query  if @parsed_query && ! override_cache

        full_query = self.query
        if ! self.appended_to_all
          full_query += ' ' + self.account.rivers_appended.map(&:query).join(' ')
          full_query.strip!
        end

        @parsed_query = Libertree::Query.new(full_query, self.account.id, self.id).parsed
      end

      def term_matches_post?(term, post, data)
        case term
        when 'flag'
          # TODO: most of these are slow
          case data
          when 'forest'
            true  # Every post is a post in the forest.  :forest is sort of a no-op term
          when 'tree'
            post.local?
          when 'unread'
            ! post.read_by?(self.account)
          when 'liked'
            post.liked_by? self.account.member
          when 'commented'
            post.commented_on_by? self.account.member
          when 'subscribed'
            self.account.subscribed_to? post
          end
        when 'contact-list'
          data.last.include? post.member_id
        when 'from'
          post.member_id == data
        when 'river'
          data.matches_post?(post)
        when 'visibility'
          post.visibility == data
        when 'word-count'
          case data
          when /^< ?([0-9]+)$/
            n = $1.to_i
            post.text.scan(/\S+/).count < n
          when /^> ?([0-9]+)$/
            n = $1.to_i
            post.text.scan(/\S+/).count > n
          end
        when 'spring'
          data.includes?(post)
        when 'via'
          post.via == data
        when 'tag'
          post.hashtags.include? data
        when 'phrase', 'word'
          /(?:^|\b|\s)#{Regexp.escape(data)}(?:\b|\s|$)/i === post.text
        end
      end

      def matches_post?(post, ignore_keys=[])
        # Negations: Must not satisfy any of the conditions
        # Requirements: Must satisfy every required condition
        # Regular terms: Must satisfy at least one condition

        conditions = {
          negations:    [],
          requirements: [],
          regular:      []
        }

        query = self.parsed_query
        keys = query.keys - ignore_keys
        keys.each do |term|
          test = lambda {|data| term_matches_post?(term, post, data)}
          query[term].keys.each do |group|
            conditions[group] += query[term][group].map(&test)
          end
        end

        conditions[:negations].none? &&
          conditions[:requirements].all? &&
          (conditions[:regular].count > 0 ? conditions[:regular].any? : true)
      end

      def refresh_posts( n = 512 )
        # delete posts early to avoid confusion about posts that don't
        # match the new query
        DB.dbh[ "DELETE FROM river_posts WHERE river_id = ?", self.id ].get

        # TODO: this is slow despite indices.
        #posts = Post.where{|p| ~Sequel.function(:post_hidden_by_account, p.id, account.id)}

        # get posts that are not hidden by account and get cracking
        posts = Post.filter_by_query(self.parsed_query, self.account, Post.not_hidden_by(account))

        # get up to n posts
        # this is faster than using find_all on the set
        count = 0
        matching = []
        posts.reverse_order(:id).each do |post|
          break  if count >= n

          if res = self.matches_post?(post, ['flag', 'word', 'phrase', 'tag', 'visibility', 'from', 'via'])
            count += 1
            matching << post
          end
        end

        if matching.any?
          DB.dbh[ "INSERT INTO river_posts SELECT ?, id FROM posts WHERE id IN ?", self.id, matching.map(&:id)].get
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
        self.save
      end

      def delete_cascade(force=false)
        if ! force && self.appended_to_all
          Libertree::Model::Job.create(
            task: 'river:refresh-all',
            params: {
              'account_id' => self.account_id,
            }.to_json
          )
        end
        DB.dbh["SELECT delete_cascade_river(?)", self.id].get
      end

      def self.num_appended_to_all
        self.where(:appended_to_all).count
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

      def mark_all_posts_as_read
        DB.dbh[ %{SELECT mark_all_posts_in_river_as_read_by(?,?)},
                self.id,
                self.account.id ].get
      end
    end
  end
end
