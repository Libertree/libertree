module Ramaze
  module Helper
    module Age
      def ago(datetime)
        total_seconds = DateTime.now.to_time.to_i - datetime.to_time.to_i
        total_minutes = total_seconds / 60
        total_hours = total_minutes / 60

        display_seconds = total_seconds % 60
        display_minutes = total_minutes % 60
        display_hours = total_hours % 60
        display_days = total_hours / 24

        if display_days > 0
          n_('1 day ago', '%d days ago', display_days) % display_days
        elsif display_hours > 0
          n_('1 hour ago', '%d hours ago', display_hours) % display_hours
        elsif display_minutes > 0
          n_('1 minute ago', '%d minutes ago', display_minutes) % display_minutes
        else
          #n_('1 second ago', '%d seconds ago', display_seconds) % display_seconds
          _('seconds ago')
        end
      end
    end
  end
end
