module Libertree
  module Model
    class Member < Sequel::Model(:members)

      def after_create
        super
        self.distribute
      end

      def after_update
        super
        self.distribute
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

      def distribute
        return  if ! self.local?
        Libertree::Model::Job.create_for_forests(
          { task: 'request:MEMBER',
            params: { 'username' => self.account.username }
          }
        )
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
        @server ||= Server[self.server_id]
      end
      alias :tree :server

      def name_display
        @name_display ||= ( profile && profile.name_display || self.username )
      end

      def handle
        if self.username && self.server
          self.username + "@#{self.server.name_display}"
        elsif account
          account.username + "@#{Server.own_domain}"
        end
      end

      def self.with_handle(h)
        if h =~ /^(.+?)@(.+?)$/
          username = $1
          host = $2
          if host == Server.own_domain
            local = true
          end
        else
          username = h
          local = true
        end

        if local
          self.qualify.
            join(:accounts, :id=>:account_id).
            where(:accounts__username => username).
            limit(1).
            first
        else
          # TODO: servers.name_given is no longer used.  Remove it after
          # migrating user rivers/contact lists etc.
          self.qualify.
            join(:servers, :id=>:server_id).
            where(:members__username => username).
            where(Sequel.or(:servers__domain => host, :servers__name_given => host)).
            limit(1).
            first
        end
      end

      # TODO: this is a temporary helper because with_handle no longer includes display name matches.
      # This will eventually be removed when river :from queries no longer accept display names.
      def self.with_display_name(name)
        self.qualify.
          join(:profiles, :member_id=>:id).
          where(:profiles__name_display => name).
          limit(1).
          first
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
        time = Time.at( opts.fetch(:time, Time.now.to_f) ).strftime("%Y-%m-%d %H:%M:%S.%6N%z")
        time_clause = if opts[:newer]
                        proc { time_created > time }
                      else
                        proc { time_created < time }
                      end

        res = Post.where(member_id: self.id).
          where(&time_clause).
          reverse_order(:time_created).
          limit(limit)

        # optionally restrict to Internet visible posts
        res = res.where(visibility: 'internet')  if opts[:public]
        res
      end

      def comments(n = 10)
        Comment.where(member_id: self.id).reverse_order(:id).limit(n)
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
