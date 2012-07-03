module Libertree
  class JobProcessor
    def initialize(log_path, queue)
      @queue = queue
      if log_path
        @log = File.open( log_path, 'a+' )
        @log.sync = true
      else
        @log = $stdout
      end
      @pid = Process.pid
      @log_identifier = "jobp #{@pid}"

      if @log.respond_to? :path
        puts "pid #{@pid} logging to #{File.absolute_path(@log.path)}"
      end
    end

    def log(s, level = nil)
      t = Time.now.strftime("%Y-%m-%d %H:%M:%S")
      if level
        l = "#{level} "
      end

      @log.puts "[#{t}] (#{@log_identifier}) #{l}#{s}"
    end

    def log_error(s)
      log s, 'ERROR'
    end

    # @return [Job] nil if no job was reserved
    def reserve
      job = Libertree::Model::Job.s1("SELECT * FROM jobs WHERE queue = '#{@queue}' AND pid IS NULL AND tries < 5 AND time_to_start <= NOW() ORDER BY time_to_start ASC LIMIT 1")
      return  if job.nil?

      Libertree::Model::Job.update(
        {
          id: job.id,
          pid: nil,
        },
        {
          pid: Process.pid,
          time_started: Time.now
        }
      )

      job = Libertree::Model::Job[job.id]
      if job.pid == Process.pid
        job
      end
    end

    def run
      quit = false

      terminate = Proc.new {
        quit = true
        puts "Quitting."
      }
      Signal.trap("TERM", &terminate)
      Signal.trap("INT" , &terminate)

      until quit
        job = reserve
        if job
          process job
        end
        3.times { sleep 1 unless quit }
      end
    end

    def process(job)
      log "Processing: job #{job.id}"

      begin
        task = job.task.split('::').reduce(JobProcessor, :const_get)
        complete = task.perform(job.params)
        if complete
          job.time_finished = Time.now
        else
          # Return job to queue.
          job.set(
            time_started: nil,
            pid: nil,
            tries: job.tries + 1,
            time_to_start: Time.now + 60*5
          )
        end

        log "Leaving: job #{job.id}"
      rescue NameError
        log_error "Skipping unknown task #{job.task} in job #{job.id}"
      rescue Exception => e
        log_error "Error processing job #{job.id}: #{e.class} #{e.message}\n" + e.backtrace.join("\n\t")
      end
    end
  end
end
