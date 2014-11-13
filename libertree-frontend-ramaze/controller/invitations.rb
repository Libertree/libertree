module Controller
  class Invitations < Base
    map '/invitations'
    before_all do
      default_before_filter
    end

    def create
      if ! $conf['invitations']
        flash[:error] = _('Invitations have been disabled.')
      elsif account.new_invitation.nil?
        flash[:error] = _('Failed to create invitation.  You may only have up to 5 unaccepted invitations at once.')
      end

      redirect_referrer
    end

    def index
      @view = "accounts edit"
      @host = request.host_with_port
      @invitations = account.invitations_not_accepted
    end
  end
end
