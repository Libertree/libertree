module Ramaze
  module Helper
    module Post
      def visibility_description(post)
        case post.visibility
        when 'internet'
          _('This post and its comments are visible to the whole Internet')
        when 'forest'
          _('This post and its comments are only visible to members of this Libertree forest')
        end
      end
    end
  end
end
