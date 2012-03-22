require 'socket'
require 'json'
require 'libertree/server'

module Libertree
  class Connection
    def initialize( host, port = Libertree::Server::PORT )
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
