require 'socket'
require 'json'
require 'timeout'

module Libertree
  # Lower level connection class.  Wrapper for TCPSocket.
  # @param [Hash] args
  class Connection
    def initialize( args )
      host = args[:host]
      port = args.fetch(:port, 14404)
      @log = args.fetch(:log, $stdout)
      @log_identifier = args.fetch(:log_identifier, "pid #{Process.pid}")

      log "Connecting to #{host}:#{port}"
      @s = TCPSocket.new(host, port)
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

    def request( command, params )
      log "REQUEST: >#{command} #{params.to_json.strip}<"
      @s.puts "#{command} #{params.to_json.strip}"
      begin
        Timeout.timeout(10) do
          response = JSON.parse(@s.gets)
          if response['code'] != 'OK'
            log_error "Not OK: #{response.inspect}"
          end
          response
        end
      rescue Timeout::Error
        log_error "(timeout)"
      end
    end

    def close
      @s.close
    end
  end
end





