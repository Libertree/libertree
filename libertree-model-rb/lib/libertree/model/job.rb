require 'json'

module Libertree
  module Model
    class Job < Sequel::Model(:jobs)
      MAX_TRIES = 11

      def params
        if val = super
          JSON.parse val
        end
      end

      def retry!
        self.pid = self.time_started = self.time_finished = nil
        self.time_to_start = Time.now
        self.tries = 0
        self.save
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
        job = self.where("task IN ? AND pid IS NULL AND tries < #{MAX_TRIES} AND time_to_start <= NOW()", tasks).order(:time_to_start).limit(1).first
        return nil  if job.nil?

        self.where({ id: job.id, pid: nil }).
          update({ pid: Process.pid, time_started: Time.now })

        job = Job[job.id]
        if job.pid == Process.pid
          job
        end
      end

      def unreserve
        new_tries = self.tries+1
        self.update(
          time_started: nil,
          pid: nil,
          tries: new_tries,
          time_to_start: Time.now + 60 * Math::E**new_tries
        )
      end

      def self.pending_where(*args)
        query = args[0]
        params = args[1..-1]

        self.where(
          query + %{
            AND time_finished IS NULL
            AND tries < ?
          },
          *params,
          MAX_TRIES
        )
      end

      def self.unfinished(task=nil)
        if task
          self.where("task = ? AND time_finished IS NULL", task).all
        else
          self.where("time_finished IS NULL").all
        end
      end
    end
  end
end
