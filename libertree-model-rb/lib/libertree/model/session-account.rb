module Libertree
  module Model
    class SessionAccount < Sequel::Model(:sessions_accounts)
      set_primary_key [:sid]
      def account
        @account ||= Account[self.account_id]
      end
    end
  end
end
