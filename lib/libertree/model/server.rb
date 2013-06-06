module Libertree
  module Model
    class Server < M4DBI::Model(:servers)
      def name_display
        self.name_given || self.domain || self.ip || "(unknown)"
      end

      def forests
        Forest.prepare(
          %{
            SELECT
              f.*
            FROM
                forests f
              , forests_servers fs
            WHERE
              fs.server_id = ?
              AND f.id = fs.forest_id
          }
        ).s(self.id).
          map { |row| Forest.new row }
      end
    end
  end
end
