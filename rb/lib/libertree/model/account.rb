require 'bcrypt'
require 'securerandom'

module Libertree
  module Model
    class Account < M4DBI::Model(:accounts)

      # These two password methods provide a seamless interface to the BCrypted
      # password.  The pseudo-field "password" can be treated like a normal
      # String field for reading and writing.
      def password
        @password ||= BCrypt::Password.new(password_encrypted)
      end

      def password=( new_password )
        @password = BCrypt::Password.create(new_password)
        self.password_encrypted = @password
      end

      # Used by Ramaze::Helper::UserHelper.
      # @return [Account] authenticated account, or nil on failure to authenticate
      def self.authenticate(creds)
        account = Account[ 'username' => creds['username'] ]
        if account && account.password == creds['password']
          account
        end
      end

      def member
        @member ||= Member['account_id' => self.id]
      end

      def notify_about(data)
        Notification.create(
          account_id: self.id,
          data: data.to_json
        )
      end

      def notifications( limit = 128 )
        @notifications ||= Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? ORDER BY id DESC LIMIT #{limit.to_i}",
          self.id
        )
      end

      def notifications_unseen
        @notifications_unseen ||= Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? AND seen = FALSE ORDER BY id",
          self.id
        )
      end

      def num_notifications_unseen
        @num_notifications_unseen ||= Libertree::DB.dbh.sc("SELECT COUNT(*) FROM notifications WHERE account_id = ? AND seen = FALSE", self.id)
      end

      def rivers
        River.s "SELECT * FROM rivers WHERE account_id = ? ORDER BY position ASC, id DESC", self.id
      end

      def self.create(*args)
        account = super
        member = Member.create( account_id: account.id )
        River.ensure_beginner_rivers_for account
        account.rivers.find { |r| r.query == ':unread' }.home = true
        account
      end

      def first_unread_post
        Post.s1(
          %{
            SELECT
              p.*
            FROM
              posts p
            WHERE
              p.id = (
                SELECT MIN(p2.id)
                FROM posts p2
                WHERE NOT EXISTS(
                  SELECT 1
                  FROM posts_read pr
                  WHERE pr.account_id = ?
                )
              )
          },
          self.id
        )
      end

      def home_river
        River.s1 "SELECT * FROM rivers WHERE account_id = ? AND home = TRUE", self.id
      end

      def home_river=(river)
        DB.dbh.u "UPDATE rivers SET home = FALSE WHERE account_id = ?", self.id
        DB.dbh.u "UPDATE rivers SET home = TRUE WHERE account_id = ? and id = ?", self.id, river.id
      end

      def invitations_not_accepted
        Invitation.s "SELECT * FROM invitations WHERE inviter_account_id = ? AND account_id IS NULL ORDER BY id", self.id
      end

      def new_invitation
        if invitations_not_accepted.count < 5
          Invitation.create( inviter_account_id: self.id )
        end
      end

      def generate_api_token
        self.api_token = SecureRandom.hex(16)
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def api_time_last
        if self['api_time_last']
          DateTime.parse self['api_time_last']
        end
      end

      # @param [Time] time The time to compare to
      # @return [Boolean] whether or not the API was last used by this account
      #                   more recently than the given Time
      def api_last_used_more_recently_than(time)
        self.api_time_last && self.api_time_last.to_time > time
      end

      # Clears some memoized data
      def dirty
        @notifications = nil
        @notifications_unseen = nil
        @num_notifications_unseen = nil
      end

      def admin?
        self.admin
      end
    end
  end
end
