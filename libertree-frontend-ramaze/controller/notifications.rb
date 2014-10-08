module Controller
  class Notifications < Base
    map '/notifications'
    before_all do
      default_before_filter
    end

    layout do |path|
      if path =~ %r{(_index|seen)}
        nil
      else
        :default
      end
    end

    def index
      @view = "notifications"
      notifs = account.notifications.find_all {|n| n.subject }

      # This is only needed because we share the view code with _index
      @grouped_notifs = notifs.map {|n| [n]}
    end

    def _index
      @grouped_notifs = account.notifications_unseen_grouped
      @n = account.num_notifications_unseen
      @n_more = @n - @grouped_notifs.flat_map(&:count).reduce(0, &:+)
    end

    def seen(*notification_ids)
      Libertree::Model::Notification.mark_seen_for_account(account, notification_ids)
      account.num_notifications_unseen
    end

    def unseen(*notification_ids)
      Libertree::Model::Notification.mark_unseen_for_account(account, notification_ids)
      account.num_notifications_unseen
    end

    def seen_comments(*comment_ids)
      Libertree::Model::Notification.mark_seen_for_account_and_comment_id(account, comment_ids)
      account.num_notifications_unseen
    end

    def num_unseen
      account.num_notifications_unseen
    end
  end
end
