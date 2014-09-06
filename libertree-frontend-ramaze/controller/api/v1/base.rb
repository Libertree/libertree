module Controller
  module API
    module V1
      class Base < Ramaze::Controller
        layout nil

        def set_account_from_token
          if request['token'].nil?
            respond '', 400
          end

          @account = Libertree::Model::Account[ api_token: request['token'].to_s ]
          if @account.nil?
            respond '', 404
          end

          # Throttling.
          if @account.api_last_used_more_recently_than(Time.now - ($conf['api_min_time_between'] || 5))
            @account = nil
            respond '', 503
          end

          @account.api_time_last = DateTime.now
          @account.save
        end

      end
    end
  end
end
