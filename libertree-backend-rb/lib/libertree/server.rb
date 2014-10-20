require 'json'
require 'fileutils'
require 'blather/client/dsl'
require 'yaml'

require 'libertree/model'
require 'libertree/server/responder'
require 'libertree/server/relay'
require 'libertree/server/websocket'


module Libertree
  module Server
    class ConfigurationError < StandardError; end
    class MissingParameterError < StandardError; end
    class NotFoundError < StandardError; end
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

      Libertree::Server.log_handle.puts "[#{t}] #{l}#{s}"
    end

    def self.log_error(s)
      self.log s, 'ERROR'
    end

    def self.log_debug(s)
      self.log s, 'DEBUG'  if @debug
    end

    def self.load_config(config_filename)
      # load defaults first, then merge
      @conf = YAML.load(File.read("#{File.dirname( __FILE__ ) }/../../defaults.yaml")).
        merge YAML.load(File.read(config_filename))
      missing = []
      [
        'xmpp_server',
        'domain',
        'shared_secret',
        'private_key_path',
      ].each do |required_key|
        if @conf[required_key].nil?
          missing << required_key
        end
      end

      if missing.any?
        raise ConfigurationError.new("Configuration error: Missing required configuration keys: #{missing.join(', ')}")
      end

      if ! File.exists?(@conf['private_key_path'])
        raise ConfigurationError.new("Configuration error: private key file does not exist.}")
      end
    end

    def self.conf
      @conf
    end

    def self.quit
      @quit = true
      puts "Terminating server."
      EventMachine.stop_event_loop  if EventMachine.reactor_running?
    end

    def self.run(config_filename)
      @quit = false

      Signal.trap("HUP") do
        puts "\nRestarting server."
        EventMachine.stop_event_loop
      end

      terminate = Proc.new { self.quit }
      Signal.trap("TERM", &terminate)
      Signal.trap("INT" , &terminate)

      until @quit
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
            log_dir = File.dirname(@conf['log_path'])
            if ! Dir.exists?(log_dir)
              FileUtils.mkdir_p log_dir
            end
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

        @debug = true  if @conf['debug']
        host   = @conf['xmpp_server']
        domain = @conf['domain']
        secret = @conf['shared_secret']
        port   = @conf['port'].to_i || 5347
        socket = @conf['relay_socket'] || "/tmp/libertree-relay"

        # create directory holding the socket file
        FileUtils.mkdir_p(File.dirname(socket))

        # TODO: take the appropriate setting from the config file
        Libertree::Model::Account.set_auth_settings(:default, nil)
        Libertree::Model::Server.own_domain = domain

        Responder.setup domain, secret, host, port
        begin
          EventMachine.run {
            Responder.run
            EventMachine.start_unix_domain_server socket, Relay
            Websocket.run(@conf)
          }
        rescue Blather::Stream::ConnectionTimeout
          log_error "Connection to the XMPP server on #{host} timed out; retrying."
          sleep 3
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
