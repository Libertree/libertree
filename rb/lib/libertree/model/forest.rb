module Libertree
  module Model
    class Forest < M4DBI::Model(:forests)
      def trees
        @trees ||= Server.s(
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
      alias :servers :trees

      def add(server)
        DB.dbh.i  "INSERT INTO forests_servers ( forest_id, server_id ) VALUES ( ?, ? )", self.id, server.id
      end

      def remove(server)
        DB.dbh.d  "DELETE FROM forests_servers WHERE forest_id = ? AND server_id = ?", self.id, server.id
      end

      def local?
        ! origin_server_id
      end
    end
  end
end
