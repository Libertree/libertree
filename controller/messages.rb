module Controller
  class Messages < Base
    map '/messages'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ %r{^_}
        nil
      else
        :default
      end
    end

    def index
      @messages = account.messages
      @view = 'messages'
    end

    # TODO: the first two arguments are ignored.  They are expected in the other post loaders.
    def _more( ignore, older_or_newer = 'older', time = Time.now.to_i )
      @messages = account.messages(limit: 8, time: time.to_f)
      render_file "#{Ramaze.options.views[0]}/messages/_records.xhtml"
    end

    def create
      redirect_referrer  if ! request.post?

      if request['text'].to_s.empty?
        flash[:error] = _('You cannot send an empty message.')
        redirect_referrer
      end

      if request['recipients'].to_s.empty?
        flash[:error] = _('You did not specify a recipient. Please try again.')
        redirect_referrer
      end

      begin
        message = Libertree::Model::Message.create_with_recipients(
          sender_member_id: account.member.id,
          text: request['text'].to_s,
          recipient_member_ids: request['recipients'].split(",")
        )
      rescue Sequel::DatabaseError => e
        if e.message =~ /value too long/
          flash[:error] = _('Your message is longer than 4096 characters. Please shorten it and try again.')
          redirect_referrer
        else raise e end
      end

      session[:saved_text]["textarea-message-new"] = nil

      redirect r(:show, message.id)
    end

    def show(message_id)
      @view = 'messages'
      @message = Libertree::Model::Message[message_id.to_i]
      redirect_referrer  if @message.nil?
      redirect_referrer  if ! @message.visible_to?(account)
      Libertree::Model::Notification.mark_seen_for_account_and_message(account, @message)
      other_participants = [@message.sender] + @message.recipients - [account.member]
      @participants = other_participants.map {|m| {"id"=>m.id, "text"=>m.handle}}
    end

    def _new; end

    def delete(message_id)
      @message = Libertree::Model::Message[message_id.to_i]

      if Ramaze::Current.action.wish == 'json'
        if @message.nil? || ! @message.visible_to?(account)
          return { 'success' => false }
        end

        if @message.delete_for_participant(account.member)
          { 'success' => true }
        else
          {
            'success' => false,
            'error'   => _('Failed to delete the message.  Please try again later.')
          }
        end
      else
        if @message.nil? || ! @message.visible_to?(account)
          redirect_referrer
        end

        if @message.delete_for_participant(account.member)
          flash[:notice] = _('The message has been deleted.')
          redirect r(:index)
        else
          flash[:error] = _('Failed to delete the message.  Please try again later.')
          redirect_referrer
        end
      end
    end

    provide(:json, type: 'application/json') { |action,value| value.to_json }
    def search
      query = request['q'].to_s
      return '[]'  if query.empty?

      Libertree::Model::Member.search(query).map do |m|
        display = m.name_display
        handle = m.handle
        if handle != display
          display += " (#{handle})"
        end
        { 'id' => m.id,
          'text' => display }
      end
    end
  end
end
