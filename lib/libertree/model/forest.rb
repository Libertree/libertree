module Libertree
  module Model
    class Forest < M4DBI::Model(:forests)
      def trees
        stm = Server.prepare(
          %{
            SELECT
              s.*
            FROM
                forests_servers fs
              , servers s
            WHERE
              fs.forest_id = ?
              AND s.id = fs.server_id
          }
        )
        records = stm.s(self.id).map { |row| Server.new row }
        stm.finish
        records
      end
      alias :servers :trees

      def add(server)
        DB.dbh.i(
          %{
            INSERT INTO forests_servers (
              forest_id, server_id
            ) SELECT
              ?, ?
            WHERE NOT EXISTS(
              SELECT 1
              FROM forests_servers fs2
              WHERE
                fs2.forest_id = ?
                AND fs2.server_id = ?
            )
          },
          self.id,
          server.id,
          self.id,
          server.id
        )
      end

      def remove(server)
        DB.dbh.d  "DELETE FROM forests_servers WHERE forest_id = ? AND server_id = ?", self.id, server.id
      end

      def local?
        ! origin_server_id
      end
      def self.all_local_is_member
        where  local_is_member: true
      end

      def origin
        Server[origin_server_id]
      end

      def local_is_member?
        local_is_member
      end

      # @param [Array] trees An Array of Hashes, each having a 'domain' key.
      def set_trees_by_domain( trees )
        DB.dbh.d  "DELETE FROM forests_servers WHERE forest_id = ?", self.id
        trees.each do |tree|
          add  Model::Server.find_or_create( domain: tree['domain'] )
        end
      end
    end
  end
end
