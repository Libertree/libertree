module Sequel
  module Plugins
    module Compat
      module ClassMethods
        def s(*args)
          Sequel::Model.db[*args].map {|row| self.new(row)}
        end
      end
    end
  end
end
