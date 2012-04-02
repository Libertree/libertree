module Libertree
  module Model
    class Member < M4DBI::Model(:members)
      def account
        @account ||= Account[self.account_id]
      end

      def name_display
        username || account.username
      end

      def avatar_path
        self['avatar_path'] || '/images/avatars/default.png'
      end
    end
  end
end
