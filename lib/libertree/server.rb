require 'json'
require 'fileutils'
require 'blather/client/dsl'

require 'libertree/model'
require 'libertree/server/responder'

module Libertree
  module Server

    class ConfigurationError < StandardError
    end

    class << self
      attr_accessor :conf
      attr_accessor :log
    end

    include Responder

    def log(s, level = nil)
      t = Time.now.strftime("%Y-%m-%d %H:%M:%S")
      if level
        l = "#{level} "
      end

      if @server
        id = "server #{@server.id}"
      else
        id = @ip_remote
      end

      Libertree::Server.log.puts "[#{t}] (#{id}) #{l}#{s}"
    end

    def log_error(s)
      log s, 'ERROR'
    end

    def self.load_config(config_filename)
      @conf = YAML.load( File.read(config_filename) )
      missing = []
      [
        'ip_listen',
        'ip_public',
        'private_key_path',
      ].each do |required_key|
        if @conf[required_key].nil?
          missing << required_key
        end
      end

      if missing.any?
        raise ConfigurationError.new("Configuration error: Missing required configuration keys: #{missing.join(', ')}")
      end
    end

    def self.run(config_filename)
      quit = false

      Signal.trap("HUP") do
        puts "\nRestarting server."
        EventMachine.stop_event_loop
      end

      terminate = Proc.new {
        quit = true
        puts "Terminating server."
        EventMachine.stop_event_loop
      }
      Signal.trap("TERM", &terminate)
      Signal.trap("INT" , &terminate)

      until quit
        begin
          load_config config_filename

          if @conf['pid_dir']
            if ! Dir.exists?(@conf['pid_dir'])
              FileUtils.mkdir_p @conf['pid_dir']
            end
            pid_file = File.join(@conf['pid_dir'], 'server.pid')
            File.open(pid_file, 'w') do |f|
              f.print Process.pid
            end
          end

          if @conf['log_path']
            @log = File.open( @conf['log_path'], 'a+' )
            @log.sync = true
            puts "Logging to #{File.absolute_path(@log.path)}"
          else
            @log = $stdout
          end
        rescue ConfigurationError => e
          $stderr.puts e.message
          exit 2
        rescue StandardError => e
          $stderr.puts e.message
          if @conf
            puts "Ignoring changes to configuration."
          else
            puts "Aborting."
            exit 1
          end
        end


        if @log.respond_to? :path
          @log.close
        end
      end
    end
  end
end
