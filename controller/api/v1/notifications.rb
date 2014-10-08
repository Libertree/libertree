module Controller
  module API
    module V1
      class Notifications < Base
        map '/api/v1/notifications'

        layout nil

        before_all do
          set_account_from_token
        end

        def unseen
          if ! request.get?
            respond '', 405
          end
          @account.notifications_unseen.take(100).map(&:data).to_json
        end
      end
    end
  end
end
