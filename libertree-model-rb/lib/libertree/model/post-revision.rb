module Libertree
  module Model
    class PostRevision < Sequel::Model(:post_revisions)
      def post
        @post ||= Post[self.post_id]
      end
    end
  end
end
