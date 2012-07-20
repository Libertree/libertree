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

      # @return [Job] nil if no job was reserved
      def self.reserve(tasks)
        placeholders = ( ['?'] * tasks.count ).join(', ')
        job = self.s1("SELECT * FROM jobs WHERE task IN (#{placeholders}) AND pid IS NULL AND tries < 11 AND time_to_start <= NOW() ORDER BY time_to_start ASC LIMIT 1", *tasks)
        return nil  if job.nil?

        self.update(
          {
            id: job.id,
            pid: nil,
          },
          {
            pid: Process.pid,
            time_started: Time.now
          }
        )

        job = Job[job.id]
        if job.pid == Process.pid
          job
        end
      end

      def unreserve
        new_tries = self.tries+1
        self.set(
          time_started: nil,
          pid: nil,
          tries: new_tries,
          time_to_start: Time.now + 60 * Math::E**new_tries
        )
      end
    end
  end
end
