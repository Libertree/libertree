module Libertree
  class JobProcessor

    def initialize(config_filename)
      pid_dir = File.join( File.dirname(__FILE__), '..', 'pids' )
      if ! Dir.exists?(pid_dir)
        Dir.mkdir pid_dir
      end
      pid_file = File.join(pid_dir, 'job-processor.pid')
      @pid = Process.pid
      File.open(pid_file, 'w') do |f|
        f.print @pid
      end

      @config_filename = config_filename
      @conf = YAML.load( File.read(config_filename) )

      if @conf['log_path']
        @log = File.open( @conf['log_path'], 'a+' )
        @log.sync = true
      else
        @log = $stdout
      end
      @log_identifier = "jobp #{@pid}"
      if @log.respond_to? :path
        puts "pid #{@pid} logging to #{File.absolute_path(@log.path)}"
      end
    end

    def conf
      @conf
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
        job = Libertree::Model::Job.reserve(Jobs.list.keys)
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
        task = Jobs.list[job.task]
        complete = task.send(:perform, job.params)

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
