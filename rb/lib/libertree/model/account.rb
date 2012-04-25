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

      def notifications( limit = 128 )
        Notification.s(
          "SELECT * FROM notifications WHERE account_id = ? ORDER BY id DESC LIMIT #{limit.to_i}",
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

      def self.create(*args)
        account = super
        River.create(
          account_id: account.id,
          label: 'Posts from my tree',
          query: ':tree'
        )
        River.create(
          account_id: account.id,
          label: 'Posts from the forest',
          query: ':forest'
        )
        River.create(
          account_id: account.id,
          label: 'Unread posts from the forest',
          query: ':unread'
        )
        account
      end

      def self.find_or_create(*args)
        account = super
        # TODO: DRY this up along with self.create
        River.create(
          account_id: account.id,
          label: 'Posts from my tree',
          query: ':tree'
        )
        River.create(
          account_id: account.id,
          label: 'Posts from the forest',
          query: ':forest'
        )
        River.create(
          account_id: account.id,
          label: 'Unread posts from the forest',
          query: ':unread'
        )
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
    end
  end
end
