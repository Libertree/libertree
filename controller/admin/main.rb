module Controller
  module Admin
    class Main < Controller::Admin::Base
      map '/admin'

      before_all do
        if action.view_value.nil?
          require_admin
          init_locale
        end
        @view = 'admin'
      end

      layout :default

      def index
        @forests = Libertree::Model::Forest.all.sort_by(&:name)
        @servers = Libertree::Model::Server.all.sort_by(&:name_display)
        @local_host = request.host
      end

    end
  end
end
