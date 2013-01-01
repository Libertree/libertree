module Libertree
  module Model
    class Member < M4DBI::Model(:members)

      after_create do |member|
        if member.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER',
              params: { 'member_id' => member.id, }
            }
          )
        end
      end

      after_update do |member|
        if member.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER',
              params: { 'member_id' => member.id, }
            }
          )
        end
      end

      before_delete do |member|
        # TODO: expand later for more granularity:
        #       - only abandon (not delete) posts and comments
        #       - only empty posts if they contain a discussion
        #       - only empty and anonymise comments
        #       - etc.
        if member.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER-DELETE',
              params: { 'member_id' => member.id, }
            }
          )
        end
      end

      def local?
        ! self.account.nil?
      end

      def account
        @account ||= Account[self.account_id]
      end

      def server
        if $m4dbi_cached_fetches
          @server = Server.cached_fetch(self.server_id)
        else
          @server = Server[self.server_id]
        end
      end
      alias :tree :server

      def name_display
        @name_display ||= ( profile && profile.name_display || self.handle )
      end

      def handle
        if self['username']
          self['username'] + "@#{server.name_display}"
        else
          account.username
        end
      end

      def self.with_handle(h)
        if h =~ /^(.+?)@(.+?)$/
          username = $1
          host = $2
          row = self.prepare(
            %{
              SELECT m.*
              FROM
                  members m
                , servers s
              WHERE
                m.username = ?
                AND s.id = m.server_id
                AND (
                  s.name_given = ?
                  OR s.ip::TEXT = ? || '/32'
                )
            }
          ).s1(
            username,
            host,
            host
          )
          self.new(row)  if row
        else
          row = self.prepare(
            %{
              SELECT
                m.*
              FROM
                  members m
                , accounts a
              WHERE
                a.id = m.account_id
                AND a.username = ?
            },
          ).s1(h)
          self.new(row)  if row
        end
      end

      def username
        self['username'] || account.username
      end

      def profile
        @profile ||= Profile[ member_id: self.id ]
      end

      def self.create(*args)
        member = super
        Profile.create( member_id: member.id )
        member
      end

      def posts( opts = {} )
        limit = opts.fetch(:limit, 30)
        if opts[:newer]
          time_comparator = '>'
        else
          time_comparator = '<'
        end
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")

        # TODO: prepared statement?
        Post.s(
          %{
            SELECT
              p.*
            FROM
              posts p
            WHERE
              member_id = ?
              AND p.time_created #{time_comparator} ?
            ORDER BY p.time_created DESC
            LIMIT #{limit}
          },
          self.id,
          time
        )
      end

      def comments(n = 10)
        Comment.s  "SELECT * FROM comments WHERE member_id = ? ORDER BY id DESC LIMIT #{n.to_i}", self.id
      end

      def pools
        @pools ||= Pool.where( member_id: self.id )
      end
      def springs
        @springs ||= Pool.where( member_id: self.id, sprung: true )
      end

      def online?
        self.account && self.account.online?
      end

      def delete_cascade
        self.pools.each(&:delete_cascade)
        self.posts.each(&:delete_cascade)
        self.comments.each(&:delete_cascade)
        self.profile.delete
        self.delete
      end
    end
  end
end
