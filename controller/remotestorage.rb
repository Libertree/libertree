module Controller
  class RemoteStorage < Base
    map '/remotestorage'
    before_all do
      default_before_filter
    end

    layout :default

    def connection(update_token=nil)
      @view = "accounts edit"
      @storage = account.remote_storage_connection ||
        Libertree::Model::RemoteStorageConnection.create({account_id: account.id})

      if request.post?
        handle = request['handle'].to_s.strip
        if handle.empty?
          flash[:error] = s_('remote-storage|The remote storage handle may not be empty.')
          redirect r(:remotestorage)
        end

        @storage.handle = handle

        begin
          info = Libertree::RemoteStorage.finger(handle)
        rescue Curl::Err::HostResolutionError => e
          flash[:error] = s_('remote-storage|The remote storage provider could not be found.')
          redirect r(:connection)
        rescue Curl::Err::ConnectionFailedError, Curl::Err::TimeoutError => e
          flash[:error] = s_('remote-storage|The connection to the remote storage provider timed out.')
          redirect r(:connection)
        end

        if info.nil?
          flash[:error] = s_('remote-storage|The remote server does not offer remote storage.')
          redirect r(:connection)
        end

        @storage.storage_url = info['href']
        @storage.save

        # Store random update token in session and add it to the
        # redirect URL.  When we receive a GET request with the
        # access_token, check whether the update token matches.  This
        # is to make it much less likely that the user inadvertendly
        # updates the stored access_token with a simple GET request.

        random_token = (0..16).map { ('a'..'z').to_a.sample }.join
        session['update_token'] = random_token
        redirect Libertree::RemoteStorage.auth_request_url(info, random_token)

      elsif request['access_token'] && (update_token == session['update_token'].to_s)
        session['update_token'] = nil

        # update access_token and reload page
        @storage.access_token = request['access_token'].to_s
        @storage.save

        flash[:notice] = _("Your remote storage account (%s) has been connected!") % @storage.handle
        redirect r(:connection)
      end
    end

    def files
      @view = "accounts edit"
      @storage = account.remote_storage_connection

      if @storage.nil? || @storage.access_token.nil?
        flash[:error] = s_("remote-storage|Please connect your remote storage account first.")
        redirect r(:connection)
      end

      begin
        @files = Libertree::RemoteStorage.get("public/libertree/", @storage)
      rescue => e
        # TODO: report connection error in view
      end
    end

    def upload
      # With JavaScript we could directly upload the file to the remote storage
      # without first uploading it to Libertree.  As we need to set an authorisation
      # header, we cannot do this without JavaScript, so this controller action
      # should be used whenever JavaScript is not activated.

      # The disadvantages of synchronous uploads:
      # - it takes twice as long to upload a file
      # - the request might time out
      # - somewhat larger file uploads are not feasible

      redirect_referrer  if ! request.post?

      @storage = account.remote_storage_connection
      if @storage.nil? || @storage.access_token.nil?
        flash[:error] = s_("remote-storage|Please connect your remote storage account first.")
        redirect r(:connection)
      end

      begin
        if Libertree::RemoteStorage.upload(request['file'], @storage)
          flash[:notice] = s_("remote-storage|Successfully uploaded %s to remote storage.") %
            request['file'][:filename]
        else
          flash[:error] = s_("remote-storage|An error occurred. Please try again later.")
        end
      rescue => e
        flash[:error] = s_("remote-storage|An error occurred. Please try again later.")
      end

      redirect r(:files)
    end

    def delete_file(filename)
      redirect_referrer  if filename.nil?

      @storage = account.remote_storage_connection
      if @storage.nil? || @storage.access_token.nil?
        flash[:error] = s_("remote-storage|Please connect your remote storage account first.")
        redirect_referrer
      end

      if Libertree::RemoteStorage.delete("public/libertree/#{filename}", @storage)
        flash[:notice] = s_("remote-storage|File %s has been deleted.") % filename
      else
        flash[:error] = s_("remote-storage|An error occurred. Please try again later.")
      end

      redirect_referrer
    end

    def destroy
      redirect_referrer  if ! request.post?
      storage = account.remote_storage_connection
      redirect_referrer  if ! storage

      storage.delete
      flash[:notice] = _("The connection to your remote storage account has been removed.")
      redirect r(:connection)
    end
  end
end
