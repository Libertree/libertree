module Libertree
  module Model
    class Server < M4DBI::Model(:servers)
      def name_display
        self.name_given || self.ip
      end

      def forests
        Forest.s(
          %{
            SELECT
              f.*
            FROM
                forests f
              , forests_servers fs
            WHERE
              fs.server_id = ?
              AND f.id = fs.forest_id
          },
          self.id
        )
      end
    end
  end
end
