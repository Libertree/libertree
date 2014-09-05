# encoding: utf-8

require 'date'
require_relative '../embedder'

module Libertree
  module Model
    class Post < Sequel::Model(:posts)
      include IsRemoteOrLocal
      extend HasSearchableText
      include HasDisplayText

      def before_create
        self.hashtags = self.extract_hashtags
        super
      end

      def after_create
        super
        if self.local? && self.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => self.id, }
            },
            *self.forests
          )
        end
        Libertree::Embedder.autoembed(self.text)
        self.notify_mentioned
      end

      def before_update
        self.hashtags = self.extract_hashtags
        super
      end

      def after_update
        super
        has_distributable_difference = (
          self.previous_changes.include?(:text) ||
          self.previous_changes.include?(:visibility)
        )

        # TODO: deny change of visibility to 'tree' visibility?
        #       or trigger deletion on remotes?
        if self.local? && self.distribute? && has_distributable_difference
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => self.id, }
            },
            *self.forests
          )
        end
        Libertree::Embedder.autoembed(self.text)
      end

      # TODO: DB: replace with association
      def member
        @member ||= Member[self.member_id]
      end

      def time_updated_overall
        [time_commented, time_updated].compact.max
      end

      def read_by?(account)
        DB.dbh[ "SELECT EXISTS( SELECT 1 FROM posts_read WHERE post_id = ? AND account_id = ? LIMIT 1 )", self.id, account.id ].single_value
      end

      def mark_as_read_by(account)
        DB.dbh[ "SELECT mark_post_as_read_by( ?, ? )", self.id, account.id ].get
      end

      def mark_as_unread_by(account)
        DB.dbh[  "DELETE FROM posts_read WHERE post_id = ? AND account_id = ?", self.id, account.id ].get
        account.rivers.each do |river|
          if river.should_contain? self
            DB.dbh[ "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", river.id, self.id ].get
          end
        end
      end

      def mark_as_unread_by_all( options = {} )
        except_accounts = options.fetch(:except, [])
        if except_accounts.any?
          DB.dbh[:posts_read].where('post_id = ? AND NOT account_id IN ?', self.id, except_accounts.map(&:id)).delete
        else
          DB.dbh[ "DELETE FROM posts_read WHERE post_id = ?", self.id ].get
        end

        Libertree::Model::Job.create(
          task: 'post:add-to-rivers',
          params: { 'post_id' => self.id, }.to_json
        )
      end

      def self.mark_all_as_read_by(account)
        DB.dbh[ %{SELECT mark_all_posts_as_read_by(?)}, account.id ].get
      end

      # @param [Hash] opt options for restricting the comment set returned.  See Comment.on_post .
      def comments(opt = nil)
        opt ||= {}  # We put this here instead of in the method signature because sometimes nil is literally sent
        Comment.on_post(self, opt)
      end

      def commented_on_by?(member)
        DB.dbh[
          %{
            SELECT EXISTS(
              SELECT 1
              FROM comments
              WHERE
                post_id = ?
                AND member_id = ?
            )
          },
          self.id,
          member.id
        ].single_value
      end

      def likes
        return @likes   if @likes
        @likes = PostLike.where(post_id: self.id).reverse_order(:id)
      end

      def notify_about_comment(comment)
        notification_attributes = {
          'type'       => 'comment',
          'comment_id' => comment.id,
        }
        accounts = comment.post.subscribers
        if comment.member.account
          accounts = accounts.select {|a| a.id != comment.member.account.id }
        end
        accounts.each do |a|
          if ! comment.post.hidden_by?(a)
            a.notify_about notification_attributes
          end
        end
      end

      def notify_about_like(like)
        notification_attributes = {
          'type'         => 'post-like',
          'post_like_id' => like.id,
        }
        local_post_author = like.post.member.account
        like_author = like.member.account

        if local_post_author && (!like_author || local_post_author.id != like_author.id)
          local_post_author.notify_about notification_attributes
        end
      end

      def notify_mentioned
        notification_attributes = {
          'type'    => 'mention',
          'post_id' => self.id,
        }

        mentioned_accounts.each do |a|
          a.notify_about notification_attributes
        end
      end

      def mentioned_accounts
        accounts = Set.new

        # find all JIDs first
        # TODO: username matching (left here for compatibility reasons) is deprecated
        self.text_as_html.xpath('.//span[@rel="username"]').each do |n|
          handle = n.content[1..-1].downcase
          if account = Account[ username: handle ]
            accounts << account
          elsif member = Member.with_handle(handle)
            accounts << member.account
          end
        end

        # remove post author from result set
        accounts.delete(self.member.account)
        accounts.to_a
      end

      def before_destroy
        if self.local? && self.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST-DELETE',
              params: { 'post_id' => self.id, }
            },
            *self.forests
          )
        end
        super
      end

      # TODO: the correct method to call is "destroy"
      def delete
        self.before_destroy
        super
      end

      # NOTE: deletion is NOT distributed when force=true
      def delete_cascade(force=false)
        self.before_destroy  unless force
        # clear cached posts
        Libertree::MODELCACHE.delete(self.cache_key)
        Libertree::MODELCACHE.delete("#{self.cache_key}:get_full")

        DB.dbh[ "SELECT delete_cascade_post(?)", self.id ].get
      end

      def self.create(*args)
        post = super
        Libertree::Model::Job.create(
          task: 'post:add-to-rivers',
          params: { 'post_id' => post.id, }.to_json
        )
        if post.member.account
          post.mark_as_read_by post.member.account
          post.member.account.subscribe_to post
        end
        post
      end

      # This is a search, not a create
      def like_by(member)
        PostLike[ member_id: member.id, post_id: self.id ]
      end
      def liked_by?(member)
        !! like_by(member)
      end

      # TODO: Optionally restrict by account, so as not to reveal too much to browser/client
      # i.e. rivers not belonging to current account
      def rivers_belonged_to(account = nil)
        query_params = [self.id]

        if account
          account_clause = "AND r.account_id = ?"
          query_params << account.id
        end

        River.s(
          %{
            SELECT
              r.*
            FROM
                rivers r
              , river_posts rp
            WHERE
              rp.river_id = r.id
              AND rp.post_id = ?
              #{ account_clause }
          },
          *query_params
        )
      end

      def extract_hashtags
        self.text_as_html.xpath('.//span[@rel="hashtag"]').map do |n|
          n.content[1..-1] =~ /([\p{Word}_-]+)/i
          $1.downcase  if $1
        end
      end

      def self.with_tag( opts = {} )
        return []  if opts[:tag].nil? || opts[:tag].empty?
        tags = Sequel.pg_array(Array(opts[:tag]).map(&:downcase))
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")
        Post.s(%{SELECT * FROM tagged_posts(?, ?, ?, ?, ?)},
               tags,
               time,
               opts[:newer],
               opts[:order_by] == :comment,
               opts.fetch(:limit, 30))
      end

      def subscribers
        Account.s(%{
          SELECT a.*
          FROM accounts a, post_subscriptions ps
          WHERE ps.post_id = ? AND a.id = ps.account_id}, self.id)
      end

      def revise(text_new, visibility = self.visibility)
        PostRevision.create(
          post_id: self.id,
          text:    self.text
        )
        self.update(
          text:         text_new,
          visibility:   visibility,
          time_updated: Time.now
        )

        # clear cached posts
        Libertree::MODELCACHE.delete(self.cache_key)
        Libertree::MODELCACHE.delete("#{self.cache_key}:get_full")
        mark_as_unread_by_all
      end

      def hidden_by?(account)
        DB.dbh[ "SELECT post_hidden_by_account(?, ?)", self.id, account.id ].single_value
      end

      def self.not_hidden_by(account, posts=self)
        posts.
          qualify.
          left_outer_join(:posts_hidden,
                          :posts_hidden__post_id => :posts__id,
                          :posts_hidden__account_id => account.id).
          where(:posts_hidden__post_id => nil)
      end

      def self.read_by(account, posts=self)
        posts.
          qualify.
          join(:posts_read,
               :posts_read__post_id => :posts__id,
               :posts_read__account_id => account.id)
      end

      def self.unread_by(account, posts=self)
        posts.
          qualify.
          left_outer_join(:posts_read,
                          :posts_read__post_id => :posts__id,
                          :posts_read__account_id => account.id).
          where(:posts_read__post_id => nil)
      end

      def self.liked_by(member, posts=self)
        posts.
          qualify.
          join(:post_likes,
               :post_likes__post_id => :posts__id,
               :post_likes__member_id => member.id)
      end

      def self.without_liked_by(member, posts=self)
        posts.
          qualify.
          join(:post_likes,
               :post_likes__post_id => :posts__id,
               :post_likes__member_id => member.id)
      end

      def self.commented_on_by(member, posts=self)
        posts.
          where(:posts__id => Comment.
                select(:post_id).
                distinct(:post_id).
                where(:member_id => member.id))
      end

      def self.without_commented_on_by(member, posts=self)
        posts.
          exclude(:posts__id => Comment.
                  select(:post_id).
                  distinct(:post_id).
                  where(:member_id => member.id))
      end

      def self.subscribed_to_by(account, posts=self)
        posts.
          qualify.
          join(:post_subscriptions,
               :post_subscriptions__post_id => :posts__id,
               :post_subscriptions__account_id => account.id)
      end

      def self.without_subscribed_to_by(account, posts=self)
        posts.
          qualify.
          left_outer_join(:post_subscriptions,
                          :post_subscriptions__post_id => :posts__id,
                          :post_subscriptions__account_id => account.id).
          where(:post_subscriptions__post_id => nil)
      end

      def self.filter_by_query(parsed_query, account, posts=self)
        flags = parsed_query['flag']
        if flags
          flags[:negations].each do |flag|
            case flag
            when 'tree'
              posts = posts.exclude(:remote_id => nil)
            when 'unread'
              posts = self.read_by(account, posts)
            when 'liked'
              posts = self.without_liked_by(account.member, posts)
            when 'commented'
              posts = self.without_commented_on_by(account.member, posts)
            when 'subscribed'
              posts = self.without_subscribed_to_by(account, posts)
            end
          end

          flags[:requirements].each do |flag|
            case flag
            when 'tree'
              posts = posts.where(:remote_id => nil)
            when 'unread'
              posts = self.unread_by(account, posts)
            when 'liked'
              posts = self.liked_by(account.member, posts)
            when 'commented'
              posts = self.commented_on_by(account.member, posts)
            when 'subscribed'
              posts = self.subscribed_to_by(account, posts)
            end
          end

          sets = flags[:regular].map do |flag|
            case flag
            when 'tree'
              posts.where(:remote_id => nil)
            when 'unread'
              self.unread_by(account, posts)
            when 'liked'
              self.liked_by(account.member, posts)
            when 'commented'
              self.commented_on_by(account.member, posts)
            when 'subscribed'
              self.subscribed_to_by(account, posts)
            end
          end.compact

          unless sets.empty?
            posts = sets.reduce do |res, set|
              res.union(set)
            end
          end
        end

        tags = parsed_query['tag']
        if tags.values.flatten.count > 0
          # Careful!  Don't trust user input!
          # remove any tag that includes array braces
          excluded = tags[:negations].delete_if    {|t| t =~ /[{}]/ }
          required = tags[:requirements].delete_if {|t| t =~ /[{}]/ }
          regular  = tags[:regular].delete_if      {|t| t =~ /[{}]/ }

          posts = posts.exclude(Sequel.pg_array_op(:hashtags).contains("{#{excluded.join(',')}}"))  unless excluded.empty?
          posts = posts.where  (Sequel.pg_array_op(:hashtags).contains("{#{required.join(',')}}"))  unless required.empty?
          posts = posts.where  (Sequel.pg_array_op(:hashtags).overlaps("{#{regular.join(',')}}" ))  unless regular.empty?
        end

        phrases = parsed_query['phrase']
        if phrases.values.flatten.count > 0
          req_patterns = phrases[:requirements].map {|phrase| /(^|\b|\s)#{Regexp.escape(phrase)}(\b|\s|$)/ }
          neg_patterns = phrases[:negations].map    {|phrase| /(^|\b|\s)#{Regexp.escape(phrase)}(\b|\s|$)/ }
          reg_patterns = phrases[:regular].map      {|phrase| /(^|\b|\s)#{Regexp.escape(phrase)}(\b|\s|$)/ }

          unless req_patterns.empty?
            posts = posts.grep(:text, req_patterns, { all_patterns: true, case_insensitive: true })
          end

          unless neg_patterns.empty?
            # NOTE: using Regexp.union results in a postgresql error when the phrase includes '#', or '?'
            pattern = "(#{neg_patterns.map(&:source).join('|')})"
            posts = posts.exclude(Sequel.ilike(:text, pattern))
          end

          unless reg_patterns.empty?
            posts = posts.grep(:text, reg_patterns, { all_patterns: false, case_insensitive: true })
          end
        end

        words = parsed_query['word']
        if words.values.flatten.count > 0
          # strip query characters
          words.each_pair {|k,v| words[k].each {|word| word.gsub!(/[\(\)&|!]/, '')}}

          # filter by simple terms first to avoid having to check so many posts
          # TODO: prevent empty arguments to to_tsquery
          posts = posts.where(%{to_tsvector('simple', text)
                               @@ (to_tsquery('simple', ?)
                               && to_tsquery('simple', ?)
                               && to_tsquery('simple', ?))},
                             words[:negations].map{|w| "!#{w}" }.join(' & '),
                             words[:requirements].join(' & '),
                             words[:regular].join(' | '))
        end

        { 'visibility' => :visibility,
          'from'       => :member_id,
          'via'        => :via,
        }.each_pair do |key, column|
          set = parsed_query[key]
          if set.values.flatten.count > 0
            excluded = set[:negations]
            required = set[:requirements]
            regular  = set[:regular]

            posts = posts.exclude(column => excluded)  unless excluded.empty?
            posts = posts.where(column => required)    unless required.empty?

            unless regular.empty?
              posts = regular.
                map {|value| posts.where(column => value)}.
                reduce {|res, set| res.union(set)}
            end
          end
        end

        posts
      end

      def collected_by?(account)
        DB.dbh[ "SELECT account_collected_post(?, ?)", account.id, self.id ].single_value
      end

      def to_hash
        {
          'id'           => self.id,
          'time_created' => self.time_created,
          'time_updated' => self.time_updated,
          'text'         => self.text,
        }
      end

      def v_internet?
        self.visibility == 'internet'
      end
      def v_forest?
        self.visibility == 'forest' || self.visibility == 'internet'
      end
      def distribute?
        self.visibility != 'tree'
      end

      def self.as_nested_json(id)
        post = self[id]
        JSON[ post.to_json( :include => {
                              :member => {},
                              :likes => {
                                :include => {
                                  :member => {}
                                }},
                              :comments => {
                                :include => {
                                  :member => {},
                                  :likes => {
                                    :include => {
                                      :member => {}
                                    }
                                  }
                                }
                              }
                            }) ]
      end

      # Expand and embed all associated records.
      def self.get_full(id)
        if cached = Libertree::MODELCACHE.get("#{self.cache_key(id)}:get_full")
          return cached
        end

        post = self[id]
        return  unless post

        # cache member records
        members = Hash.new
        members.default_proc = proc do |hash, key|
          member = Member[ key ]
          name = member.name_display
          member.define_singleton_method(:name_display) { name }
          hash[key] = member
        end

        post_likes = post.likes

        like_proc = proc do |like|
          like.define_singleton_method(:member) { members[like.member_id] }
          like
        end

        get_comments = lambda do
          comments = Comment.on_post(post)
          comment_likes = CommentLike.where('comment_id IN ?', comments.map(&:id)).reduce({}) do |hash, like|
            if hash[like.comment_id]
              hash[like.comment_id] << like
            else
              hash[like.comment_id] = [like]
            end
            hash
          end

          comments.map do |comment|
            likes = if comment_likes[comment.id]
                      comment_likes[comment.id].map{|l| like_proc.call(l)}
                    else
                      []
                    end

            comment.define_singleton_method(:member) { members[comment.member_id] }
            comment.define_singleton_method(:likes)  { likes }
            comment.define_singleton_method(:post)   { post }

            comment
          end
        end

        comments = get_comments.call

        # enhance post object with expanded associations
        post.define_singleton_method(:member) { members[post.member_id] }
        post.define_singleton_method(:likes)  { post_likes.map{|l| like_proc.call(l)} }
        post.define_singleton_method(:comments) {|opts={}|
          if opts[:refresh_cache]
            comments = get_comments.call
          end

          res = comments
          if opts
            res = res.find_all {|c| c.id >= opts[:from_id].to_i}  if opts[:from_id]
            res = res.find_all {|c| c.id < opts[:to_id].to_i}     if opts[:to_id]
            res = res.last(opts[:limit].to_i)                     if opts[:limit]
          end
          res
        }

        Libertree::MODELCACHE.set("#{self.cache_key(id)}:get_full", post, 60)
        post
      end

      # @return [Post] the earliest Post which contains a URL that the
      #   prospective post text contains
      # This method only searches recent posts, not necessarily every post in the DB.
      def self.urls_already_posted?(prospective_post_text)
        posts_found = []

        prospective_post_text.scan(
          %r{(?<=\]\()(https?://\S+?)(?=\))|(?:^|\b)(https?://\S+)(?=\s|$)}
        ) do |match1, match2|
          url = match1 || match2
          posts = self.s(
            %{
              SELECT *
              FROM (
                SELECT *
                FROM posts
                ORDER BY id DESC
                LIMIT 256
              ) AS subquery
              WHERE text ~ ( '(^|\\A|\\s)' || ? || '(\\)|\\Z|\\s|$)' )
              OR text ~ ( '\\]\\(' || ? || '\\)' )
              ORDER by time_created
            },
            Regexp.escape(url),
            Regexp.escape(url)
          )
          if posts.count > 0 && posts.count < 4
            posts_found << posts[0]
          end
        end

        posts_found.sort_by(&:time_created)[0]
      end
    end
  end
end
