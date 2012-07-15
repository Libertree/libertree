module Libertree
  class JobProcessor

    def initialize(config_filename)
      @config_filename = config_filename
      @conf = YAML.load( File.read(config_filename) )

      if @conf['log_path']
        @log = File.open( @conf['log_path'], 'a+' )
        @log.sync = true
      else
        @log = $stdout
      end
      @pid = Process.pid
      @log_identifier = "jobp #{@pid}"

      if @log.respond_to? :path
        puts "pid #{@pid} logging to #{File.absolute_path(@log.path)}"
      end

      @queue = @conf['queue']
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

    def run
      quit = false

      terminate = Proc.new {
        quit = true
        puts "Quitting."
      }
      Signal.trap("TERM", &terminate)
      Signal.trap("INT" , &terminate)

      until quit
        job = Libertree::Model::Job.reserve(@queue)
        if job
          process job
        end
        3.times do
          sleep 1 unless quit
        end
      end
    end

    def process(job)
      log "Processing: job #{job.id}"

      begin
        # Try to convert the task name into the name of a class
        # that is defined in the Jobs module. We cannot use String#capitalize
        # here as it would lowercase everything but the first letter.
        task = job.task.split(':').
                 map {|t| (t[0].upcase + t[1..-1]).gsub('-','_') }.
                 reduce(Jobs, :const_get)
        log "performing #{task}"
        complete = task.perform(job.params)
        if complete
          job.time_finished = Time.now
        else
          # Return job to queue.
          job.unreserve
        end

        log "Leaving: job #{job.id}"
      rescue NameError
        log_error "Skipping unknown task #{job.task} in job #{job.id}"
        job.unreserve
      rescue StandardError => e
        log_error "Error processing job #{job.id}: #{e.class} #{e.message}\n" + e.backtrace.join("\n\t")
        job.unreserve
      end
    end
  end
end
