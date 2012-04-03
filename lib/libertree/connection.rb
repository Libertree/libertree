require 'socket'
require 'json'
require 'timeout'

module Libertree
  # Lower level connection class.  Wrapper for TCPSocket.
  class Connection
    def initialize( host, port = 14404 )
      @s = TCPSocket.new(host, port)
    end

    def request( command, params )
      $stderr.puts "REQUEST: >#{command} #{params.to_json.strip}<"
      @s.puts "#{command} #{params.to_json.strip}"
      begin
        Timeout.timeout(10) do
          response = JSON.parse(@s.gets)
          if response['code'] != 'OK'
            $stderr.puts "Not OK: #{response.inspect}"
          end
          response
        end
      rescue Timeout::Error
        $stderr.puts "(timeout)"
      end
    end

    def close
      @s.close
    end
  end
end





