require 'socket'
require 'json'

module Libertree
  # Lower level connection class.  Wrapper for TCPSocket.
  class Connection
    def initialize( host, port = 14404 )
      @s = TCPSocket.new(host, port)
    end

    def request( command, params )
      @s.puts "#{command} #{params.to_json}"
      JSON.parse @s.gets
    end

    def close
      @s.close
    end
  end
end





