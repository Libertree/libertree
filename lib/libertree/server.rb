require 'json'
require 'fileutils'
require 'blather/client/dsl'
require 'yaml'

require 'libertree/model'
require 'libertree/server/responder'
require 'libertree/server/relay'

module Libertree
  module Server

    class ConfigurationError < StandardError; end
    class MissingParameter < StandardError; end
    class NotFound < StandardError; end
    class InternalError < StandardError; end

    class << self
      attr_accessor :conf
      attr_accessor :log_handle
    end

    include Responder
    include Relay

    def self.log(s, level = nil)
      t = Time.now.strftime("%Y-%m-%d %H:%M:%S")
      if level
        l = "#{level} "
      end

      if @server
        id = "server #{@server.id}"
      else
        id = @ip_remote
      end

      Libertree::Server.log_handle.puts "[#{t}] (#{id}) #{l}#{s}"
    end

    def self.log_error(s)
      self.log s, 'ERROR'
    end

    def self.load_config(config_filename)
      @conf = YAML.load( File.read(config_filename) )
      missing = []
      [
        'xmpp_server',
        'domain',
        'shared_secret',
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
            @log_handle = File.open( @conf['log_path'], 'a+' )
            @log_handle.sync = true
            puts "Logging to #{File.absolute_path(@log_handle.path)}"
          else
            @log_handle = $stdout
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

        host   = @conf['xmpp_server']
        domain = @conf['domain']
        secret = @conf['shared_secret']
        port   = @conf['port'].to_i || 5347
        socket = @conf['relay_socket'] || "/tmp/libertree-relay"

        Responder.setup domain, secret, host, port
        begin
          EventMachine.run {
            Responder.run
            EventMachine.start_unix_domain_server socket, Relay
          }
        rescue Blather::Stream::ConnectionFailed
          log_error "No connection to the XMPP server on #{host}; retrying."
          sleep 3
        end

        if @log_handle.respond_to? :path
          @log_handle.close
        end
      end
    end
  end
end
