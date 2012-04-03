require 'json'

module Libertree
  module Model
    class Job < M4DBI::Model(:jobs)
      def params
        JSON.parse self['params']
      end
    end
  end
end
