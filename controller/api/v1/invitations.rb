module Controller
  module API
    module V1
      class Invitations < Base
        map '/api/v1/invitations'

        layout nil

        before_all do
          set_account_from_token
        end

        def create
          if ! request.post?
            respond '', 405
          end

          invitation = @account.new_invitation
          if invitation.nil?
            { 'success' => false }.to_json
          else
            {
              'success' => true,
              'code' => invitation.code,
            }.to_json
          end
        end
      end
    end
  end
end
