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
        if h =~ /@/
          row = self.prepare(
            %{
              SELECT m.*
              FROM
                  members m
                , servers s
              WHERE
                s.id = m.server_id
                AND COALESCE( s.name_given, s.ip::TEXT ) = ?
            }
          ).s1(h)
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

      def posts(n = 8)
        Post.s  "SELECT * FROM posts WHERE member_id = ? ORDER BY id DESC LIMIT #{n.to_i}", self.id
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
    end
  end
end
