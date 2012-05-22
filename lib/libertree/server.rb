require 'eventmachine'
require 'json'
require 'socket'
require 'openssl'
require 'base64'

require 'libertree/authenticatable'
require 'libertree/model'
require 'libertree/server/responder'

module Libertree
  module Server
    PORT = 14404

    class << self
      attr_accessor :conf
      attr_accessor :log
    end

    include Responder

    # EventMachine callbacks

    def post_init
      # TODO: Not sure if there isn't a better place to read in the local public key
      key = OpenSSL::PKey::RSA.new File.read( Libertree::Server.conf['private_key_path'] )
      @public_key = key.public_key.to_pem
      port, @ip_remote = Socket.unpack_sockaddr_in(get_peername)
      log "#{@ip_remote} connected."
    end

    def receive_data(data)
      begin
        process data
      rescue Exception => e
        log_error( e.message + "\n" + e.backtrace.reject { |s| s =~ %r{/gems/} }[0..5].join("\n\t") )
      end
    end

    def unbind
      log "#{@ip_remote} disconnected."
      if @server
        @server.challenge = nil
        @server = nil
      end
    end

    # -------

    def introduced?
      @server && @server.public_key
    end

    def authenticated?
      @server && @server.authenticated?
    end

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
      [
      ].each do |required_key|
        if @conf[required_key].nil?
          raise "Configuration error: #{required_key} is required."
        end
      end
    end

    def self.run(config_filename)
      load_config config_filename
      if @conf['log_path']
        @log = File.open( @conf['log_path'], 'w+' )
      else
        @log = $stdout
      end

      EventMachine.run do
        host = @conf['host_listen'] || '127.0.0.1'
        EventMachine.start_server( host, PORT, self )
        puts "Libertree started."
        puts "Listening on #{host}, port #{PORT}."
        puts "Logging to #{File.absolute_path(@log.path)}"
      end
    end
  end
end
