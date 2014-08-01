module Sequel
  module Plugins
    module Compat
      module ClassMethods
        def s(*args)
          Sequel::Model.db[*args].map {|row| self.new(row)}
        end

        def s_wrap(*args)
          Sequel::Model.db[*args].map {|row| self.send(:wrap_dataset, row)}
        end

        private
        # Creates a model object and augments it with methods for
        # otherwise undefined values.  There are no setters for those
        # keys that are no valid column identifiers.
        def wrap_dataset(values)
          undefined = values.keys - self.columns
          object = self.new values.reject{|key| undefined.include? key}
          klass = class << object; self; end
          undefined.each do |method_name|
            klass.send(:define_method, method_name) { values[method_name] }
          end
          object
        end
      end
    end
  end
end
