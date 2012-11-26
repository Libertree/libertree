module Libertree
  module Model
    class SessionAccount < M4DBI::Model(:sessions_accounts, pk: [:sid])
      def account
        @account ||= Account[self.account_id]
      end
    end
  end
end
