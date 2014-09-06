require 'gpgme'
module Controller
  class Accounts < Base
    map '/accounts'
    before_all do
      default_before_filter
    end

    layout do |path|
      case path
      when 'heartbeat'
        nil
      else
        :default
      end
    end

    def edit
      @view = "accounts edit"
      @export_filename = "libertree-data-#{account.username}-#{Time.now.strftime('%Y-%m-%d')}.json"
    end

    def update
      redirect_referrer  if ! request.post?

      if request['excerpt_max_height'].nil? || request['excerpt_max_height'].to_s.empty?
        account.settings.excerpt_max_height = nil
      else
        account.settings.excerpt_max_height = request['excerpt_max_height'].to_i
      end

      if request['custom_link'] && ! request['custom_link'].to_s.empty?
        account.settings.custom_link = request['custom_link'].to_s
      else
        account.settings.custom_link = nil
      end

      if request['email'].nil? || request['email'].to_s.strip.empty?
        account.email = nil
      else
        account.email = request['email'].to_s
      end

      account.settings.forward_dms_via_email = !! request['forward_dms_via_email']
      account.settings.custom_css = request['custom_css'].to_s
      account.settings.custom_js = request['custom_js'].to_s
      account.settings.autoembed = !! request['autoembed']
      account.settings.filter_images = !! request['filter_images']
      account.settings.thumbnail = !! request['thumbnail']
      account.settings.hide_markdown_bar = !! request['hide_markdown_bar']
      account.settings.icons = ( request['post_tools_icons'].to_s == 'icons' )

      # TODO: move validation to the account model?
      if request['pubkey'].nil? || request['pubkey'].to_s.strip.empty?
        account.pubkey = nil
      else
        # import pubkey into temporary keyring to verify it
        GPGME::Engine.home_dir = Dir.tmpdir
        result = GPGME::Key.import request['pubkey'].to_s

        if result.considered == 1 && result.secret_read == 1
          # Delete the key immediately from the keyring and
          # alert the user in case a secret key was uploaded
          keys = GPGME::Key.find(:secret, result.imports.first.fpr)
          keys.first.delete!(true)  # force deletion of secret key
          keys = nil; result = nil
          flash[:error] = _('You imported a secret key!  Although we did not store it you should consider this key compromised.')
          redirect_referrer
        elsif result.considered == 1 && (result.imported == 1 || result.unchanged == 1)
          # We do not check whether the key matches the given email address.
          # This is not necessary, because we don't search the keyring to get
          # the encryption key when sending emails.  Instead, we just take
          # whatever key the user provided.

          account.pubkey = request['pubkey'].to_s
        else
          flash[:error] = _('The provided public key could not be read.  Is it a valid PGP public key?')
          redirect_referrer
        end
      end

      if $conf['themes'].include? request['theme'].to_s
        account.settings.theme = request['theme'].to_s
      else
        account.settings.theme = $conf['themes'].first
      end
      account.locale = request['locale'].to_s
      account.settings.new_post_in_river = !! request['new_post_in_river']
      account.settings.auto_resize_textareas = !! request['auto_resize_textareas']
      session[:locale] = account.locale

      begin
        account.save
        account.settings.save
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /valid_excerpt_max_height/
          flash[:error] = _('Post excerpt maximum height: Please enter a number greater than or equal to zero, or no number for no maximum.')
          redirect_referrer
        else raise e end
      end

      flash[:notice] = _('Settings saved.')
      redirect_referrer
    end

    def generate_api_token
      account.generate_api_token
      redirect_referrer
    end

    def clear_api_token
      account.api_token = nil
      account.save
      redirect_referrer
    end

    def api
      @view = "accounts edit"
      @host = request.host_with_port
    end

    def font(choice = nil)
      case choice
      when 'small'
        account.font_css = 'fonts-small'
      else
        account.font_css = nil
      end

      account.save
      redirect_referrer
    end

    def change_password
      @view = "accounts edit"
      return  if ! request.post?

      if request['password'].to_s != request['password_again'].to_s
        flash[:error] = _('Passwords did not match.  Please reenter.')
      else
        account.password = request['password'].to_s
        account.password_reset_code = nil
        account.password_reset_expiry = nil
        account.save
        flash[:notice] = _('Password changed.')
        redirect r(:edit)
      end
    end

    provide(:json, :type => 'application/json') do |action, value|
      state = JSON::Ext::Generator::State.new
      state.indent = "  "
      state.array_nl = "\n"
      state.object_nl = "\n"
      value.to_json(state)
    end

    def data(filename = nil)
      account.data_hash
    end

    def delete
      @view = "accounts edit"
      return  if ! request.post?

      if account.username != request['username'].to_s
        flash[:error] = s_('account-delete|The username you provided does not match your username. Please input your own username to confirm account deletion.')
      else
        account_id_to_delete = account.id
        account_logout
        Libertree::Model::Account[account_id_to_delete].delete_cascade

        flash[:notice] = s_('account-delete|Your account has been deleted.')
        # TODO: redirect to goodbye page instead
        redirect Main.r(:login)
      end
    end

    def heartbeat
      account.time_heartbeat = Time.now
      account.save
    end
  end
end
