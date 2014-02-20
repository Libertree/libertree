require 'date'
require_relative '../embedder'

module Libertree
  module Model
    class Post < Sequel::Model(:posts)
      include IsRemoteOrLocal
      extend HasSearchableText

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
        @member = Member[self.member_id]
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
          ids = except_accounts.map { |a| a.id }
          placeholders = ( ['?'] * ids.count ).join(', ')
          DB.dbh[ "DELETE FROM posts_read WHERE post_id = ? AND NOT account_id IN (#{placeholders})", self.id, *ids ].get
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

      # @param [Hash] opt options for restricting the comment set returned.  See Comment.to_post .
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
        accounts.delete comment.member.account
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

        if local_post_author && local_post_author != like_author
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
        pattern = %r{(?:\W|^)@(\w+)}
        author_name = self.member.username
        usernames = self.text.scan(pattern).flatten.uniq - [author_name]
        return []  if usernames.empty?

        Libertree::Model::Account.where(username: usernames).all
      end

      def glimpse( length = 60 )
        if self.text.length <= length
          self.text
        else
          self.text[0...length] + '...'
        end
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

      def self.with_tag( opts = {} )
        # TODO:
        # hashtags should have their own column to improve performance and to
        # exclude posts that contain strings in verbatim sections that only
        # look like hashtags

        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")
        Post.s(%{SELECT * FROM tagged_posts(?, ?, ?, ?, ?)},
               opts[:tag],
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
        mark_as_unread_by_all
      end

      def hidden_by?(account)
        DB.dbh[ "SELECT post_hidden_by_account(?, ?)", self.id, account.id ].single_value
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
    end
  end
end
