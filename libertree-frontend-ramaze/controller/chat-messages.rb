module Controller
  class ChatMessages < Base
    map '/chat'
    before_all do
      default_before_filter
    end
    layout nil

    def _index
      @contacts_online = account.contacts_mutual.find_all { |c| c.online? }
      @n = account.num_chat_unseen
      @partners = account.
        chat_partners_current.
        reject { |m|
          session[:chats_closed].include?(m.id) # && ! m.has_unseen_from_other
        }
      @partner_active = @partners.find(&:has_unseen_from_other) || @partners[0]
    end

    def _tab(member_id, active = false)
      @partner = Libertree::Model::Member[member_id.to_i]
      @chat_messages = Libertree::Model::ChatMessage.between(account, @partner)
      @active = active
      if @active
        @n = 0
      else
        @n = account.num_chat_unseen_from_partner(@partner)
      end
    end

    def _log(member_id, active = false)
      @partner = Libertree::Model::Member[member_id.to_i]
      @chat_messages = Libertree::Model::ChatMessage.between(account, @partner)
      @active = active
      session[:chats_closed].delete @partner.id
    end

    def _message(chat_message_id)
      @message = Libertree::Model::ChatMessage[ chat_message_id.to_i ]
      if @message.from_member_id != account.member.id && @message.to_member_id != account.member.id
        @message = nil
      end
    end

    def create
      if ! request.post?
        return { 'success' => false }.to_json
      end

      begin
        Libertree::Model::ChatMessage.create(
          from_member_id: account.member.id,
          to_member_id: request['to_member_id'].to_i,
          text: request['text'].to_s
        )

        { 'success' => true }.to_json
      rescue Sequel::CheckConstraintViolation => e
        if e.message =~ /text_not_empty/
          { 'success' => false }.to_json
        else
          raise e
        end
      end
    end

    def seen(member_id)
      Libertree::Model::ChatMessage.mark_seen_between(account, member_id)
      account.dirty
      account.num_chat_unseen
    end

    def closed(member_id)
      session[:chats_closed] << member_id.to_i
    end
  end
end
