module Libertree
  module Model
    class AccountSettings < Sequel::Model(:account_settings)
      set_primary_key [:account_id]
      def theme
        @theme ||= super
      end

      def dirty
        @theme = nil
      end
    end
  end
end
