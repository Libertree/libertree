module Libertree
  module Model
    class Pool < M4DBI::Model(:pools)
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

      after_create do |pool|
        if pool.local? && pool.sprung?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POOL',
              params: { 'pool_id' => pool.id, }
            },
            *pool.forests
          )
        end
      end

      after_update do |pool|
        if pool.local? && pool.sprung?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POOL',
              params: { 'pool_id' => pool.id, }
            },
            *pool.forests
          )
          pool.posts.last(16).each do |post|
            pool.create_pool_post_job(post)
          end
        end
      end

      before_delete do |pool|
        if pool.local? && pool.sprung?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POOL-DELETE',
              params: { 'pool_id' => pool.id, }
            },
            *pool.forests
          )
        end
      end

      def member
        @member ||= Member[self.member_id]
      end

      def posts
        @posts ||= Post.prepare(
          %{
            SELECT
              p.*
            FROM
                posts p
              , pools_posts pp
            WHERE
              p.id = pp.post_id
              AND pp.pool_id = ?
            ORDER BY
              p.id DESC
          }
        ).s(self.id).map { |row| Post.new row }
      end

      def includes?(post)
        posts.include? post
      end

      def delete_cascade
        DB.dbh.delete "DELETE FROM pools_posts WHERE pool_id = ?", self.id
        self.delete
      end

      def dirty
        @posts = nil
        self
      end

      def <<(post)
        insertion_result = DB.dbh.i(
          %{
            INSERT INTO pools_posts (
              pool_id, post_id
            )  SELECT
              ?, ?
            WHERE NOT EXISTS(
              SELECT 1
              FROM pools_posts
              WHERE
                pool_id = ?
                AND post_id = ?
            )
          },
          self.id,
          post.id,
          self.id,
          post.id
        )
        self.dirty

        if self.local? && self.sprung? && insertion_result.affected_count > 0
          create_pool_post_job(post)
        end
      end

      def remove_post(post)
        DB.dbh.d  "DELETE FROM pools_posts WHERE pool_id = ? AND post_id = ?", self.id, post.id
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
        self['sprung']
      end
    end
  end
end
