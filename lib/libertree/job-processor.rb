require 'fileutils'

module Libertree
  class RetryJob     < StandardError; end
  class JobFailed    < StandardError; end
  class JobUndefined < StandardError; end
  class JobInvalid   < StandardError; end

  class JobProcessor

    def initialize(config_filename)
      @config_filename = config_filename
      @conf = YAML.load( File.read(config_filename) )

      if @conf && @conf['pid_dir']
        if ! Dir.exists?(@conf['pid_dir'])
          Dir.mkdir @conf['pid_dir']
        end
        pid_filepath = ENV['LIBERTREE_PID_FILEPATH'] || File.join(@conf['pid_dir'], 'job-processor.pid')
        @pid = Process.pid
        File.open(pid_filepath, 'w') do |f|
          f.print @pid
        end
      end

      if @conf && @conf['log_path']
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
      @conf.merge({ 'log_handle' => @log, 'log_identifier' => @log_identifier })
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

    # @param tasks [Array<String>] which specific tasks to work on
    def run(tasks = Jobs.list.keys)
      quit = false

      terminate = Proc.new {
        quit = true
        puts "Quitting."
      }
      Signal.trap("TERM", &terminate)
      Signal.trap("INT" , &terminate)
      Signal.trap("HUP") do
        puts "\nReloading configuration."
        @log.close
        self.send(:initialize, @config_filename)
      end

      until quit
        job = Libertree::Model::Job.reserve(tasks)
        if job
          process job
        else
          3.times do
            sleep 1  if ! quit
          end
        end
      end
    end

    def process(job)
      log "Processing: job #{job.id}"

      begin
        task = Jobs.list[job.task]
        raise Libertree::JobUndefined unless task
        task.send(:perform, job.params)
        job.time_finished = Time.now
        job.save

        log "Leaving: job #{job.id}"
      rescue Libertree::JobInvalid => e
        reason = "Invalid job #{job.id}: #{e.message}\n"
        log_error reason
        job.delete
      rescue Libertree::JobFailed => e
        reason = "Failed job #{job.id}: #{e.message}\n"
        log_error reason
        # TODO: mark the job as failed instead of unreserving it
        job.retry_reason = reason
        job.save
        job.unreserve
      rescue Libertree::RetryJob => e
        log_error "Retry job #{job.id} later: #{e.message}\n"
        # TODO: mark the job as failed instead of unreserving it
        job.retry_reason = e.message
        job.save
        job.unreserve
      rescue Libertree::JobUndefined
        log_error "Skipping unknown task #{job.task} in job #{job.id}"
        job.unreserve
      rescue StandardError => e
        log_error "Error processing job #{job.id}: #{e.class} #{e.message}\n" + e.backtrace.join("\n\t")
        job.retry_reason = e.message
        job.save
        job.unreserve
      end
    end
  end
end
