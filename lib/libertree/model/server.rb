module Libertree
  module Model
    class Server < M4DBI::Model(:servers)
      def name_display
        self.name_given || self.domain || self.ip || "(unknown)"
      end

      def forests
        stm = Forest.prepare(
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
        )
        records = stm.s(self.id).map { |row| Forest.new row }
        stm.finish
        records
      end
    end
  end
end
