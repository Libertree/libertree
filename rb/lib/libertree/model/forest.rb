module Libertree
  module Model
    class Forest < M4DBI::Model(:forests)
      def servers
        Server.s(
          %{
            SELECT
              s.*
            FROM
                forests_servers fs
              , servers s
            WHERE
              fs.forest_id = ?
              AND s.id = fs.server_id
          },
          self.id
        )
      end
    end
  end
end
