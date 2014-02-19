module Libertree
  module Model
    class Pool < Sequel::Model(:pools)
      include IsRemoteOrLocal

      def create_pool_post_job(post)
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:POOL-POST',
            params: {
              'pool_id' => self.id,
              'post_id' => post.id,
            }
          },
          *self.forests
        )
      end

      def create_pool_delete_job
        Libertree::Model::Job.create_for_forests(
          {
            task: 'request:POOL-DELETE',
            params: { 'pool_id' => self.id, }
          },
          *self.forests
        )
      end

      def after_create
        super
        if self.local? && self.sprung?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POOL',
              params: { 'pool_id' => self.id, }
            },
            *self.forests
          )
        end
      end

      def after_update
        super
        if self.local?
          if ! self.sprung?
            self.create_pool_delete_job
          else
            if self.previous_changes.include?(:sprung)
              Libertree::Model::Job.create_for_forests(
                {
                  task: 'request:POOL',
                  params: { 'pool_id' => self.id, }
                },
                *self.forests
              )
              self.posts.last(16).each do |post|
                self.create_pool_post_job(post)
              end
            end
          end
        end
      end

      def before_destroy
        if self.local? && self.sprung?
          self.create_pool_delete_job
        end
        super
      end

      def member
        @member ||= Member[self.member_id]
      end

      def posts( opts = {} )
        @posts ||= Hash.new
        return @posts[opts]  if @posts[opts]

        limit = opts.fetch(:limit, 30)
        if opts[:newer]
          time_comparator = '>'
        else
          time_comparator = '<'
        end
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")

        @posts[opts] = Post.s(
          %{
            SELECT
              p.*
            FROM
                posts p
              , pools_posts pp
            WHERE
              p.id = pp.post_id
              AND pp.pool_id = ?
              AND p.time_created #{time_comparator} ?
            ORDER BY
              p.id DESC
            LIMIT #{limit}
          },
          self.id, time
        )
      end

      def includes?(post)
        DB.dbh[ "SELECT EXISTS( SELECT 1 FROM pools_posts WHERE post_id = ? AND pool_id = ? LIMIT 1 )", post.id, self.id ].single_value
      end

      # NOTE: deletion is NOT distributed
      def delete_cascade
        DB.dbh[ "SELECT delete_cascade_pool(?)", self.id ].get
      end

      def dirty
        @posts = nil
        self
      end

      def notify_about_springing(pool_post)
        pool = pool_post.pool
        return  if ! pool.sprung

        post = pool_post.post
        local_post_author = post.member.account
        pool_owner = pool.member.account

        if local_post_author && local_post_author != pool_owner
          local_post_author.notify_about( {
            'type'         => 'springing',
            'pool_post_id' => pool_post.id,
          } )
        end
      end

      def <<(post)
        pool_post = PoolPost[ pool_id: self.id, post_id: post.id ]
        if pool_post.nil?
          pool_post_created = true
          pool_post = PoolPost.create(
            'pool_id' => self.id,
            'post_id' => post.id
          )
        end

        self.dirty
        if self.sprung? && pool_post_created
          self.notify_about_springing pool_post
          if self.local?
            create_pool_post_job(post)
          end
        end
      end

      def remove_post(post)
        DB.dbh[  "DELETE FROM pools_posts WHERE pool_id = ? AND post_id = ?", self.id, post.id ].get
        self.dirty
        if self.local? && self.sprung?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POOL-POST-DELETE',
              params: {
                'pool_id' => self.id,
                'post_id' => post.id,
              }
            },
            *self.forests
          )
        end
      end

      def sprung?
        self.sprung
      end
    end
  end
end
