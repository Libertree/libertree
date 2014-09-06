module Controller
  module Admin
    class Base < Controller::Base
      protected

      def require_admin
        require_login
        if ! account.admin?
          flash[:error] = _('Administrative privileges required.')
          redirect Controller::Main.r(:/)
        end
      end
    end
  end
end
