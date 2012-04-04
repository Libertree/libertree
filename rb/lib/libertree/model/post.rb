require 'date'
require 'libertree/has-renderable-text'

module Libertree
  module Model
    class Post < M4DBI::Model(:posts)
      include Libertree::HasRenderableText

      def member
        @member ||= Member[self.member_id]
      end

      # RDBI casting not working with TIMESTAMP WITH TIME ZONE ?
      def time_created
        DateTime.parse self['time_created']
      end

      def remote?
        !! remote_id
      end

      def local?
        ! remote_id
      end

      def public_id
        self.remote_id || self.id
      end
    end
  end
end
