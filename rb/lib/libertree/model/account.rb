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
    end
  end
end
