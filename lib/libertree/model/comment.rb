module Libertree
  module Model
    class Comment < Sequel::Model(:comments)
      include IsRemoteOrLocal
      extend HasSearchableText
      include HasDisplayText

      def after_create
        super
        if self.local? && self.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:COMMENT',
              params: { 'comment_id' => self.id, }
            },
            *self.forests
          )
        end
      end

      # TODO: DB: association
      def member
        @member = Member[self.member_id]
      end

      # TODO: DB: association
      def post
        @post = Post[self.post_id]
      end

      def before_destroy
        if self.post
          remaining_comments = self.post.comments - [self]
          self.post.time_commented = remaining_comments.map(&:time_created).max
        end

        if self.local? && self.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:COMMENT-DELETE',
              params: { 'comment_id' => self.id, }
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
        DB.dbh[ "SELECT delete_cascade_comment(?)", self.id ].get
      end

      def self.create(*args)
        comment = super
        account = comment.member.account
        post = comment.post

        post.time_commented = comment.time_created
        post.mark_as_unread_by_all  except: [account]
        if account
          post.mark_as_read_by account
          account.subscribe_to comment.post
        end
        post.notify_about_comment comment
        post.save

        comment
      end

      def likes
        @likes ||= CommentLike.where(comment_id: self.id).reverse_order(:id)
      end

      def notify_about_like(like)
        notification_attributes = {
          'type'         => 'comment-like',
          'comment_like_id' => like.id,
        }
        local_comment_author = like.comment.member.account
        like_author = like.member.account

        if local_comment_author && (!like_author || local_comment_author.id != like_author.id)
          local_comment_author.notify_about notification_attributes
        end
      end

      def like_by(member)
        # take advantage of cached self.likes
        if self.likes.is_a? Array
          self.likes.find {|like| like.member.id == member.id}
        else
          CommentLike[ member_id: member.id, comment_id: self.id ]
        end
      end

      # overriding method from IsRemoteOrLocal
      def forests
        if self.post.remote?
          self.post.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end

      def to_hash
        {
          'id'           => self.id,
          'time_created' => self.time_created,
          'time_updated' => self.time_updated,
          'text'         => self.text,
          'post_id'      => self.post_id,
        }
      end

      # TODO: When more visibilities come, restrict this result set by visibility
      def self.comments_since_id(comment_id)
        self.where{ id > comment_id }.order(:id)
      end

      # @param [Hash] opt options for restricting the comment set returned
      # @option opts [Fixnum] :from_id Only return comments with id greater than or equal to this id
      # @option opts [Fixnum] :to_id Only return comments with id less than this id
      def self.on_post(post, opt = {})
        # reverse ordering is required in order to get the *last* n
        # comments, rather than the first few when using :limit
        res = Comment.where( :post_id => post.id ).reverse_order(:id)
        res = res.where{ id >= opt[:from_id].to_i }  if opt[:from_id]
        res = res.where{ id < opt[:to_id].to_i }     if opt[:to_id]
        res = res.limit(opt[:limit].to_i)            if opt[:limit]
        res.all.reverse
      end
    end
  end
end
