require 'json'

module Libertree
  module Model
    class Notification < M4DBI::Model(:notifications)
      def account
        @account ||= Account[self.account_id]
      end

      def data
        JSON.parse self['data']
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
    end
  end
end
