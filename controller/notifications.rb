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

    # Unfortnately, we have some complexity in these two index methods.
    # This is because coalescing notifications is complex.
    # The main index method does not coalesce, but the view code
    # is mostly shared, so the same instance variables are set up.

    def index
      @view = "notifications"
      @sets = Hash.new { |h,k| h[k] = [] }
      @set_keys = Array.new # so we have a display order
      notifs = account.notifications
      notifs.each do |n|
        next  if n.subject.nil?
        @set_keys << n.id
        @sets[n.id] = [n]
      end
    end

    def _index
      @sets = Hash.new { |h,k| h[k] = [] }
      @set_keys = Array.new # so we have a display order
      notifs = account.notifications_unseen
      notifs.each do |n|
        next  if n.subject.nil?

        target = case n.subject
                 when Libertree::Model::Comment, Libertree::Model::PostLike
                   n.subject.post
                 when Libertree::Model::CommentLike
                   n.subject.comment
                 else
                   n.subject
                 end

        key = [target, n.subject.class]
        @set_keys << key
        @sets[key] << n
      end
      @set_keys = @set_keys.uniq[0...5]

      @n = notifs.count
      sets_ = @sets.dup
      sets_.delete_if { |k,v| @set_keys.include? k }
      @n_more = sets_.values.reduce(0) { |sum, notif_array| sum + notif_array.size }
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
