module Libertree
  module Model
    class Server < M4DBI::Model(:servers)
      def name_display
        self.name_given || self.ip
      end
    end
  end
end
