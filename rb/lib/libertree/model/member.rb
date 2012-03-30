module Libertree
  module Model
    class Member < M4DBI::Model(:members)
      def account
        @account ||= Account[self.account_id]
      end

      def name_display
        username || account.username
      end
    end
  end
end
