module Controller
  class Main < Base
    map '/'

    before_all do
      if action.view_value.nil?
        init_locale
      end
    end

    layout do |path|
      case path
      when 'textarea_save'
        nil
      when 'textarea_clear'
        nil
      when 'search'
        :default
      else
        :splash
      end
    end

    def index
      if logged_in?
        redirect Home.r(:/)
      else
        redirect r(:login)
      end
    end

    def login
      init_locale

      @view = 'splash'
      if logged_in?
        redirect Home.r(:/)
      end

      if account_login( request.subset('password_reset_code') )
        redirect Accounts.r(:change_password)
      end

      @logging_in = true
      if request.post?
        a = account_login( request.subset('username', 'password') )
        if a
          session[:saved_text] = Hash.new
          session[:chats_closed] = Set.new
          if $post_login_path
            # Used in spec suite
            redirect $post_login_path
          elsif session[:back]
            target = session[:back]
            session[:back] = nil
            redirect target
          else
            redirect Controller::Home.r(:/)
          end
        else
          flash[:error] = _('Invalid credentials.')
          redirect r(:login)
        end
      end
    end

    def logout
      session[:saved_text] = nil
      session[:chats_closed] = nil
      account_logout
      flash[:notice] = _('You have been logged out.')
      redirect r(:login)
    end

    # TODO: Move to Accounts controller?
    def signup
      @view = 'signup'
      redirect '/intro'  if logged_in?

      @invitation_code = request['invitation_code'].to_s.sub(%r{http?://#{request.host_with_port}/signup\?invitation_code=},"")

      return  if ! request.post?

      invitation = Libertree::Model::Invitation[ code: @invitation_code ]
      if invitation.nil?
        flash[:error] = _('A valid invitation code is required.')
        return
      end

      if ! invitation.account_id.nil?
        flash[:error] = _('This invitation code has already been used. Try another one!')
        return
      end

      if request['password'].to_s != request['password-confirm'].to_s
        flash[:error] = _('You mistyped your password.')
        return
      end

      username = request['username'].to_s.strip

      # TODO: Constrain email addresses, or at least strip out unsafe HTML, etc. with Loofah, or such.
      email = request['email'].to_s.strip
      if email.empty?
        email = nil
      end

      begin
        a = Libertree::Model::Account.create(
          username: username,
          password_encrypted: BCrypt::Password.create( request['password'].to_s ),
          email: email
        )
        if $conf['post_tools_default'].to_s == 'icons'
          a.settings.icons = true
          a.settings.save
        end
        invitation.account_id = a.id
        invitation.save

        account_login request.subset('username', 'password')
        flash[:error] = nil
        redirect Intro.r(:/)
      rescue Sequel::UniqueConstraintViolation => e
        if e.message =~ /accounts_username_key/
          flash[:error] = _('Username %s is taken.  Please choose another.') % request['username'].inspect
        else raise e end
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /username_valid/
          flash[:error] = _('Username must be at least 2 characters long and consist only of lowercase letters, numbers, underscores and dashes.')
        else raise e end
      end
    end

    def _render
      require_login
      respond Libertree.render( request['s'].to_s,
                                account.settings,
                                [ Libertree.method(:jid_renderer) ] )
    end

    # This is not in the Posts controller because we will handle many other search
    # types from the one search box in the near future.
    def search
      redirect_referrer  if ! request.post?
      require_login

      @q = request['q'].to_s
      redirect_referrer  if @q.empty?

      @posts    = []
      @comments = []
      @profiles = []

      query = Libertree::Query.new(@q, account.id)
      if ! query.parsed.empty?
        @posts = Libertree::Model::Post.filter_by_query(query.parsed, account).reverse_order(:id).take(50)

        if ! query.simple.empty?
          @comments = Libertree::Model::Comment.search(query.simple)
          @profiles = Libertree::Model::Profile.search(query.simple)
        end
      end
      @view = 'search'
    end

    def textarea_save
      # Check valid session first.
      if session[:saved_text]
        session[:saved_text][ request['id'].to_s ] = request['text'].to_s
      end
      nil
    end

    def textarea_clear(id)
      # Check valid session first.
      if session[:saved_text]
        session[:saved_text][id] = nil
      end
      nil
    end

    def request_password_reset
      @view = 'signup'
      Ramaze::Log.debug request.inspect
      return  if ! request.post?

      a = Libertree::Model::Account.set_up_password_reset_for( request['email'].to_s )
      if a
        # TODO: Make a generic method for queuing email
        Libertree::Model::Job.create(
          task: 'email',
          params: {
            'to'      => request['email'].to_s,
            'subject' => _('[Libertree] Password reset'),
            'body'    => %{
Someone (IP address: #{request.ip}) has requested that a password reset link
be sent to this email address.  If you wish to change your Libertree password
now, visit:

#{$conf['frontend_url_base']}/login?password_reset_code=#{a.password_reset_code}

This link is only valid for 1 hour.
            }
          }.to_json
        )
      end

      flash[:notice] = _('A password reset link has been sent for the account with that email address.')

      redirect_referrer
    end

    # Used in specs, to reduce spec suite execution time
    def test_user_logged_in
      'Test user logged in'
    end
  end
end
