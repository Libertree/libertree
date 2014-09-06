module Controller
  module Admin
    class Servers < Controller::Admin::Base
      map '/admin/servers'

      before_all do
        if action.view_value.nil?
          require_admin
          init_locale
        end
      end

      layout :default

      # TODO:
      def _index
        @servers = Libertree::Model::Server.all
      end
    end
  end
end
