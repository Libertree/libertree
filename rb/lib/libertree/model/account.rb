require 'bcrypt'

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

      def notifications
        Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? ORDER BY id DESC",
          self.id
        )
      end

      def notifications_unseen
        Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? AND seen = FALSE ORDER BY id",
          self.id
        )
      end

      def num_notifications_unseen
        Libertree::DB.dbh.sc "SELECT COUNT(*) FROM notifications WHERE account_id = ? AND seen = FALSE", self.id
      end

      def rivers
        River.s "SELECT * FROM rivers WHERE account_id = ? ORDER BY position ASC, id DESC", self.id
      end
    end
  end
end
