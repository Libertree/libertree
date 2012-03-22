module Libertree
  module Authenticatable
    def authenticated?
      @authenticated_
    end

    def authenticated=(boolean)
      @authenticated_ = !! boolean
    end
  end
end
