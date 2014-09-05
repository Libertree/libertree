module Libertree
  module Model
    class Server < Sequel::Model(:servers)
      @@own_domain = nil

      def self.own_domain=(domain)
        @@own_domain = domain
      end

      def self.own_domain
        @@own_domain
      end

      def name_display
        self.domain || self.ip || "(unknown)"
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
