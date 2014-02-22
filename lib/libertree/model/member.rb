module Libertree
  module Model
    class Member < Sequel::Model(:members)

      def after_create
        super
        if self.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER',
              params: {
                'username' => self.account.username,
                'avatar_url' => self.avatar_path
              }
            }
          )
        end
      end

      # TODO: merge with after_create
      def after_update
        super
        if self.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER',
              params: {
                'username' => self.account.username,
                'avatar_url' => self.avatar_path
              }
            }
          )
        end
      end

      def before_destroy
        # TODO: expand later for more granularity:
        #       - only abandon (not delete) posts and comments
        #       - only empty posts if they contain a discussion
        #       - only empty and anonymise comments
        #       - etc.
        if self.local?
          Libertree::Model::Job.create_for_forests(
            {
              task: 'request:MEMBER-DELETE',
              params: { 'username' => self.account.username, }
            }
          )
        end
        super
      end

      def local?
        ! self.account.nil?
      end

      def account
        @account ||= Account[self.account_id]
      end

      # TODO: DB: association
      #many_to_one :server
      def server
        @server = Server[self.server_id]
      end
      alias :tree :server

      def name_display
        @name_display ||= ( profile && profile.name_display || self.handle )
      end

      def handle
        if self.username && self.server
          self.username + "@#{self.server.name_display}"
        else
          account.username  if account
        end
      end

      def self.with_handle(h)
        if h =~ /^(.+?)@(.+?)$/
          username = $1
          host = $2
          self.s(
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
                  OR s.domain = ?
                )
            },
            username, host, host
          ).first
        else
          self.s(
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
            h
          ).first
        end
      end

      def username
        if val = super
          val
        elsif a = self.account
          a.username
        end
      end

      # TODO: DB: association
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
        @pools ||= Pool.where( member_id: self.id ).all
      end
      def springs
        @springs ||= Pool.where( member_id: self.id, sprung: true ).all
      end

      def online?
        self.account && self.account.online?
      end

      def delete_cascade
        DB.dbh[ "SELECT delete_cascade_member(?)", self.id ].get
      end

      def dirty
        @account = nil
        @name_display = nil
        @profile = nil
        @pools = nil
        @springs = nil
      end

      def self.search(name)
        self.s(%{SELECT m.* FROM members m
                 LEFT OUTER JOIN accounts a ON (a.id = m.account_id)
                 LEFT OUTER JOIN profiles p ON (m.id = p.member_id)
                 WHERE m.username     ILIKE '%' || ? || '%'
                    OR a.username     ILIKE '%' || ? || '%'
                    OR p.name_display ILIKE '%' || ? || '%'
                }, name, name, name)
      end
    end
  end
end
