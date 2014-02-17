module Libertree
  module Model
    class Comment < Sequel::Model(:comments)
      extend HasSearchableText

      after_create do |comment|
        if comment.local? && comment.post.distribute?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:COMMENT',
              params: { 'comment_id' => comment.id, }
            },
            *comment.forests
          )
        end
      end

      def local?
        ! remote_id
      end

      def member
        if $m4dbi_cache_id
          @member = Member.cached_fetch($m4dbi_cache_id, self.member_id)
        else
          @member = Member[self.member_id]
        end
      end

      def post
        if $m4dbi_cache_id
          @post = Post.cached_fetch($m4dbi_cache_id, self.post_id)
        else
          @post = Post[self.post_id]
        end
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def before_delete
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
      end

      def delete
        self.before_delete
        super
      end

      # NOTE: deletion is NOT distributed when force=true
      def delete_cascade(force=false)
        self.before_delete  unless force
        DB.dbh.execute "SELECT delete_cascade_comment(?)", self.id
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
        stm = CommentLike.prepare("SELECT * FROM comment_likes WHERE comment_id = ? ORDER BY id DESC")
        @likes = stm.s(self.id).map { |row| CommentLike.new row }
        stm.finish
        @likes
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
        stm = self.prepare(
          %{
            SELECT
              c.*
            FROM
              comments c
            WHERE
              c.id > ?
            ORDER BY
              c.id
          }
        )
        comments = stm.s(
          comment_id
        ).map { |row|
          self.new row
        }
        stm.finish
        comments
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

        stm = Comment.prepare("SELECT * FROM comments WHERE post_id = ? #{from_clause} #{to_clause} ORDER BY id DESC #{limit_clause}")
        comments = stm.s( *params ).map { |row|
          self.new row
        }.sort_by { |c| c.id }
        stm.finish
        comments
      end
    end
  end
end
