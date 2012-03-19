require 'eventmachine'
require 'libertree'

module Libertree
  module Server
    PORT = 14404

    def initialize
    end

    def post_init
      puts "Connection received."
    end

    def receive_data(data)
      sleep 5
      p data
    end

    def self.run
      EventMachine.run do
        EventMachine.start_server '127.0.0.1', PORT, self
        puts "Libertree started."
        puts "Listening on port #{PORT}."
      end
    end
  end
end
