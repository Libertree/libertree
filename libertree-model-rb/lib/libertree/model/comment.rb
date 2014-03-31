module Libertree
  module Model
    class Comment < Sequel::Model(:comments)
      extend HasSearchableText

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

      def local?
        ! remote_id
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

      # TODO: DRY up with Post#glimpse
      def glimpse( length = 60 )
        t = self.text.lines.reject { |l| l =~ /^> / }.join("\n")
        if t.strip.empty?
          t = self.text
        end
        t.strip!

        if t.length <= length
          t
        else
          t[0...length] + '...'
        end
      end

      def self.create(*args)
        comment = super
        account = comment.member.account
        comment.post.time_commented = comment.time_created
        comment.post.mark_as_unread_by_all  except: [account]
        if account
          comment.post.mark_as_read_by account
          account.subscribe_to comment.post
        end
        comment.post.notify_about_comment comment
        comment
      end

      def likes
        return @likes  if @likes
        @likes = CommentLike.s("SELECT * FROM comment_likes WHERE comment_id = ? ORDER BY id DESC", self.id)
      end

      def notify_about_like(like)
        notification_attributes = {
          'type'         => 'comment-like',
          'comment_like_id' => like.id,
        }
        local_comment_author = like.comment.member.account
        like_author = like.member.account

        if local_comment_author && local_comment_author != like_author
          local_comment_author.notify_about notification_attributes
        end
      end

      def like_by(member)
        CommentLike[ member_id: member.id, comment_id: self.id ]
      end

      def server
        self.member.server
      end

      def public_id
        self.remote_id || self.id
      end

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
        self.s(
          %{
            SELECT
              c.*
            FROM
              comments c
            WHERE
              c.id > ?
            ORDER BY
              c.id
          },
          comment_id
        )
      end

      # @param [Hash] opt options for restricting the comment set returned
      # @option opts [Fixnum] :from_id Only return comments with id greater than or equal to this id
      # @option opts [Fixnum] :to_id Only return comments with id less than this id
      def self.on_post(post, opt = {})
        params = [ post.id, ]
        if opt[:from_id]
          from_clause = "AND id >= ?"
          params << opt[:from_id].to_i
        end
        if opt[:to_id]
          to_clause = "AND id < ?"
          params << opt[:to_id].to_i
        end
        if opt[:limit]
          limit_clause = "LIMIT #{opt[:limit].to_i}"
        end

        Comment.s("SELECT * FROM comments WHERE post_id = ? #{from_clause} #{to_clause} ORDER BY id DESC #{limit_clause}", *params).sort_by(&:id)
      end
    end
  end
end
