require 'date'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)

      after_create do |post|
        if post.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => post.id, }
            },
            *post.forests
          )
        end
      end

      after_update do |post|
        if post.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST',
              params: { 'post_id' => post.id, }
            },
            *post.forests
          )
        end
      end

      before_delete do |post|
        if post.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST-DELETE',
              params: { 'post_id' => post.id, }
            },
            *post.forests
          )
        end
      end

      def member
        @member ||= Member[self.member_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
      def time_updated
        DateTime.parse self['time_updated']
      end

      def remote?
        !! remote_id
      end

      def local?
        ! remote_id
      end

      def public_id
        self.remote_id || self.id
      end

      def server
        member.server
      end

      def read_by?(account)
        DB.dbh.sc  "SELECT EXISTS( SELECT 1 FROM posts_read WHERE post_id = ? AND account_id = ? LIMIT 1 )", self.id, account.id
      end

      def mark_as_read_by(account)
        DB.dbh.execute(
          %{
            INSERT INTO posts_read ( post_id, account_id )
            SELECT ?, ?
            WHERE NOT EXISTS (
              SELECT 1
              FROM posts_read
              WHERE
                post_id = ?
                AND account_id = ?
            )
          },
          self.id,
          account.id,
          self.id,
          account.id
        )
        DB.dbh.delete(
          %{
            DELETE FROM river_posts rp
            USING rivers r
            WHERE
              rp.river_id = r.id
              AND r.account_id = ?
              AND r.query LIKE '%:unread%'
              AND rp.post_id = ?
          },
          account.id,
          self.id
        )
      end

      def mark_as_unread_by(account)
        DB.dbh.execute  "DELETE FROM posts_read WHERE post_id = ? AND account_id = ?", self.id, account.id
        self.add_to_matching_rivers
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

        self.add_to_matching_rivers
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
              AND r.query LIKE '%:unread%'
          },
          account.id
        )
      end

      # @param [Hash] opt options for restricting the comment set returned.  See Comment.to_post .
      def comments(opt = nil)
        opt ||= {}  # We put this here instead of in the method signature because sometimes nil is literally sent
        @comments ||= Hash.new
        @comments[opt.to_s] ||= Comment.on_post(self, opt)
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
        @likes ||= PostLike.s("SELECT * FROM post_likes WHERE post_id = ? ORDER BY id DESC", self.id)
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

      def delete_cascade
        self.comments.each {|c| c.delete_cascade }
        self.likes.each {|l| l.delete_cascade }
        DB.dbh.delete "DELETE FROM posts_read WHERE post_id = ?", self.id
        DB.dbh.delete "DELETE FROM posts_hidden WHERE post_id = ?", self.id
        DB.dbh.delete "DELETE FROM pools_posts WHERE post_id = ?", self.id
        DB.dbh.delete "DELETE FROM river_posts WHERE post_id = ?", self.id
        DB.dbh.delete "DELETE FROM post_subscriptions WHERE post_id = ?", self.id
        # TODO: Do we want to keep these revisions?
        DB.dbh.delete "DELETE FROM post_revisions WHERE post_id = ?", self.id
        delete
      end

      def add_to_matching_rivers
        River.each do |river|
          river.try_post self
        end
      end

      def self.create(*args)
        post = super
        post.add_to_matching_rivers
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

      def self.search(q)
        self.s("SELECT * FROM posts WHERE text ILIKE '%' || ? || '%' ORDER BY time_created DESC LIMIT 42", q)
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

      def forests
        if self.remote?
          self.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end

      def hidden_by?(account)
        DB.dbh.sc  "SELECT EXISTS( SELECT 1 FROM posts_hidden WHERE account_id = ? AND post_id = ? )", account.id, self.id
      end

      def collected_by?(account)
        account.pools.any? { |p| p.includes?(self) }
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
    end
  end
end
