module Libertree
  module Model
    module IsRemoteOrLocal
      def public_id
        self.remote_id || self.id
      end

      def remote?
        !! remote_id
      end

      def local?
        ! remote_id
      end

      def server
        @server ||= self.member.server
      end

      def forests
        if self.remote?
          self.server.forests
        else
          Libertree::Model::Forest.all_local_is_member
        end
      end
    end
  end
end

