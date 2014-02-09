module Libertree
  module Model
    class AccountSettings < M4DBI::Model(:account_settings, pk: [:account_id])
      def theme
        @theme ||= super
      end

      def dirty
        @theme = nil
      end
    end
  end
end
