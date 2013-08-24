require 'date'
require_relative '../embedder'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      include IsRemoteOrLocal
      extend HasSearchableText

      after_create do |post|
        if post.local? && post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => post.id, }
            },
            *post.forests
          )
        end
        Libertree::Embedder.autoembed(post.text)
      end

      after_update do |post_before, post|
        has_distributable_difference = (
          post_before['text'] != post.text ||
          post_before['visibility'] != post.visibility
        )

        # TODO: deny change of visibility to 'tree' visibility?
        #       or trigger deletion on remotes?
        if post.local? && post.distribute? && has_distributable_difference
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => post.id, }
            },
            *post.forests
          )
        end
        Libertree::Embedder.autoembed(post.text)
      end

      def member
        if $m4dbi_cache_id
          @member = Member.cached_fetch($m4dbi_cache_id, self.member_id)
        else
          @member = Member[self.member_id]
        end
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
      def time_commented
        if self['time_commented']
          DateTime.parse self['time_commented']
        end
      end
      def time_updated
        DateTime.parse self['time_updated']
      end
      def time_updated_overall
        [time_commented, time_updated].compact.max
      end

      def read_by?(account)
        DB.dbh.sc  "SELECT EXISTS( SELECT 1 FROM posts_read WHERE post_id = ? AND account_id = ? LIMIT 1 )", self.id, account.id
      end

      def mark_as_read_by(account)
        DB.dbh.execute  "SELECT mark_post_as_read_by( ?, ? )", self.id, account.id
      end

      def mark_as_unread_by(account)
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ? AND account_id = ?", self.id, account.id
        account.rivers.each do |river|
          if river.should_contain? self
            DB.dbh.i "INSERT INTO river_posts ( river_id, post_id ) VALUES ( ?, ? )", river.id, self.id
          end
        end
      end

      def mark_as_unread_by_all( options = {} )
        except_accounts = options.fetch(:except, [])
        if except_accounts.any?
          ids = except_accounts.map { |a| a.id }
          placeholders = ( ['?'] * ids.count ).join(', ')
          DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ? AND NOT account_id IN (#{placeholders})", self.id, *ids
        else
          DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ?", self.id
        end

        Libertree::Model::Job.create(
          'task' => 'post:add-to-rivers',
          'params' => { 'post_id' => self.id, }.to_json
        )
      end

      def self.mark_all_as_read_by(account)
        DB.dbh.execute(
          %{
            INSERT INTO posts_read ( post_id, account_id )
            SELECT
                p.id
              , ?
            FROM
              posts p
            WHERE NOT EXISTS (
              SELECT 1
              FROM posts_read pr2
              WHERE
                pr2.post_id = p.id
                AND pr2.account_id = ?
            )
          },
          account.id,
          account.id
        )
        DB.dbh.delete(
          %{
            DELETE FROM river_posts rp
            USING rivers r
            WHERE
              rp.river_id = r.id
              AND r.account_id = ?
              AND (
                r.query LIKE ':unread%'
                OR r.query LIKE '% :unread%'
                OR r.query LIKE '%+:unread%'
              )
          },
          account.id
        )
      end

      # @param [Hash] opt options for restricting the comment set returned.  See Comment.to_post .
      def comments(opt = nil)
        opt ||= {}  # We put this here instead of in the method signature because sometimes nil is literally sent
        Comment.on_post(self, opt)
      end

      def commented_on_by?(member)
        DB.dbh.sc(
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
        )
      end

      def likes
        return @likes   if @likes
        stm = PostLike.prepare("SELECT * FROM post_likes WHERE post_id = ? ORDER BY id DESC")
        @likes = stm.s(self.id).map { |row| PostLike.new row }
        stm.finish
        @likes
      end

      def notify_about_comment(comment)
        notification_attributes = {
          'type'       => 'comment',
          'comment_id' => comment.id,
        }
        accounts = Libertree::Model::Account.subscribed_to(comment.post)
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

      def glimpse( length = 60 )
        if self.text.length <= length
          self.text
        else
          self.text[0...length] + '...'
        end
      end

      def before_delete
        if self.local? && self.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST-DELETE',
              params: { 'post_id' => self.id, }
            },
            *self.forests
          )
        end
      end

      def delete
        self.before_delete
        super
      end

      # NOTE: deletion is NOT distributed when force=true
      def delete_cascade(force=false)
        self.before_delete  unless force
        DB.dbh.execute "SELECT delete_cascade_post(?)", self.id
      end

      def self.create(*args)
        post = super
        Libertree::Model::Job.create(
          'task' => 'post:add-to-rivers',
          'params' => { 'post_id' => post.id, }.to_json
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

        # TODO: can this be turned into a prepared statement?
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
              SELECT
                p.*
              FROM
                posts p
              WHERE
                text ~* (E'(^|\\\\s)#' || ? || E'(\\\\M|\\\\s|$|[[:punct:]])')
                AND GREATEST(p.time_commented, p.time_updated) #{time_comparator} ?
              ORDER BY GREATEST(p.time_commented, p.time_updated) DESC
              LIMIT #{limit}
            },
            opts[:tag],
            time
          )
        else
          Post.s(
            %{
              SELECT
                p.*
              FROM
                posts p
              WHERE
                text ~* (E'(^|\\\\s)#' || ? || E'(\\\\M|\\\\s|$|[[:punct:]])')
                AND p.time_created #{time_comparator} ?
              ORDER BY p.time_created DESC
              LIMIT #{limit}
            },
            opts[:tag],
            time
          )
        end
      end

      def revise(text_new, visibility = self.visibility)
        PostRevision.create(
          'post_id' => self.id,
          'text'    => self.text
        )
        self.set(
          'text'         => text_new,
          'visibility'   => visibility,
          'time_updated' => Time.now
        )
        mark_as_unread_by_all
      end

      def hidden_by?(account)
        stm = self.prepare("SELECT post_hidden_by_account(?, ?)")
        retval = stm.sc(self.id, account.id)
        stm.finish
        retval
      end

      def collected_by?(account_or_member)
        if account_or_member.respond_to?(:member)
          member = account_or_member.member
        else
          member = account_or_member
        end
        member.pools.any? { |p| p.includes?(self) }
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
