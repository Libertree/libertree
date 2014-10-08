module Controller
  class Base < Ramaze::Controller
    helper :user, :xhtml, :age, :comment, :member, :wording, :views, :post, :search
    trait :user_model => ::Libertree::Model::Account

    engine :erb

    layout do |path|
      if path =~ /error/
        nil
      else
        :default
      end
    end

    def default_before_filter
      if action.view_value.nil?
        require_login
        init_locale
      end
    end

    def lang(locale)
      session[:locale] = locale
      if request.env['HTTP_REFERER'] =~ %r{/lang/}
        redirect Home.r(:/)
      else
        redirect_referrer
      end
    end

    def init_locale
      FastGettext.locale = (
        logged_in? && account.locale ||
        session[:locale] ||
        request.env['HTTP_ACCEPT_LANGUAGE'] ||
        'en_GB'
      )
    end

    def require_login
      if ! $skip_authentication && ! logged_in? && action.name != 'login' && action.name != 'logout'
        flash[:error] = s_('not-authenticated|Please log in.')
        case request.fullpath
        when %r{seen|/_}
          # don't store redirect target in the case of AJAX partials
        else
          session[:back] = request.fullpath
        end
        redirect Main.r(:login)
      end

      if logged_in?
        @num_unseen = account.num_notifications_unseen
        account.time_heartbeat = Time.now
        account.save
        # TODO: We may be able to get rid of these two session initializations,
        # but existing sessions would need to be killed at upgrade time, or
        # there may be "unexpected nil" errors
        session[:saved_text] ||= Hash.new
        session[:chats_closed] ||= Set.new
        Libertree::Model::SessionAccount.find_or_create(
          sid: session.sid,
          account_id: account.id
        )
      end
    end

    # Alias methods because Ramaze::Helper::UserHelper has "user" hard-coded
    def account
      user
    end
    def account=(a)
      user = a
    end
    def account_login(*args)
      user_login *args
    end
    def account_logout(*args)
      session_account = Libertree::Model::SessionAccount[sid: session.sid]
      if session_account
        session_account.delete
      end
      user_logout *args
    end

    def error
      @view = "splash"
      @e = request.env[Rack::RouteExceptions::EXCEPTION]
      Ramaze::Log.error @e.message
      Ramaze::Log.error @e.backtrace.join("\n\t")
      @t = Time.now.gmtime
    end

    def self.action_missing(path)
      return if path == '/error_404'
      # No normal action, runs on bare metal
      try_resolve('/error_404')
    end

    def error_404
      @view = "splash"
      render_file "#{Ramaze.options.views[0]}/404.erb"
    end

  end
end
