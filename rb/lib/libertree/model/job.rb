require 'json'

module Libertree
  module Model
    class Job < M4DBI::Model(:jobs)
      def params
        JSON.parse self['params']
      end

      # First parameter can be a Forest Array.
      # Otherwise, assumed to create for all member forests.
      def self.create_for_forests(create_args, *forests)
        if forests.empty?
          forests = Forest.all_local_is_member
        end

        trees = Set.new
        forests.each do |f|
          if f.local_is_member?
            trees += f.trees
          end
        end
        trees.each do |tree|
          params = ( create_args[:params] || create_args['params'] || Hash.new )
          params['server_id'] = tree.id
          Libertree::Model::Job.create(
            task: create_args[:task],
            params: params.to_json
          )
        end
      end
    end
  end
end
