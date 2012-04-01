require 'libertree/has-renderable-text'

module Libertree
  module Model
    class Comment < M4DBI::Model(:comments)
      include Libertree::HasRenderableText

      def member
        @member ||= Member[self.member_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end
    end
  end
end
