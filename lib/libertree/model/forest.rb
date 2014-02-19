module Libertree
  module Model
    class Forest < Sequel::Model(:forests)
      def trees
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
      alias :servers :trees

      def add(server)
        DB.dbh[
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
        ].get
      end

      def remove(server)
        DB.dbh[ "DELETE FROM forests_servers WHERE forest_id = ? AND server_id = ?", self.id, server.id ].get
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

      # @param [Array] trees An Array of Strings.
      def set_trees_by_domain( trees )
        DB.dbh[ "DELETE FROM forests_servers WHERE forest_id = ?", self.id ].get
        trees.each do |tree|
          add  Model::Server.find_or_create( domain: tree )
        end
      end
    end
  end
end
