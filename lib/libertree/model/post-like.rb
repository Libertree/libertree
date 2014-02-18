module Libertree
  module Model
    class PostLike < Sequel::Model(:post_likes)
      def after_create
        super
        if self.local? && self.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST-LIKE',
              params: { 'post_like_id' => self.id, }
            },
            *self.forests
          )
        end
      end

      def local?
        self.remote_id.nil?
      end

      def member
        @member ||= Member[self.member_id]
      end

      def post
        @post ||= Post[self.post_id]
      end

      def before_destroy
        if self.local? && self.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:POST-LIKE-DELETE',
              params: { 'post_like_id' => self.id, }
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

      def delete_cascade
        self.before_destroy
        DB.dbh[ "SELECT delete_cascade_post_like(?)", self.id ].get
      end

      def self.create(*args)
        like = super
        like.post.notify_about_like like
        account = like.member.account
        if account
          like.post.mark_as_read_by account
        end
        like
      end

      def forests
        if self.post.remote?
          self.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end
    end
  end
end
