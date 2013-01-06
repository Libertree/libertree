require 'socket'
require 'json'
require 'timeout'
require 'blather/client/dsl'

module Libertree
  # Lower level connection class.  Wrapper for TCPSocket.
  # @param [Hash] args
  class Connection
    extend Blather::DSL

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

    def build_stanza( target, command, params )
      stanza = Blather::Stanza::Iq.new(:get, target)
      content = "<libertree><#{command.downcase}>#{params_to_xml(params)}</#{command.downcase}></libertree>"
      stanza.add_child content
      stanza
    end

    def params_to_xml(elem)
      case elem
      when Array
        elem.flat_map {|i| params_to_xml(i) }.join('')
      when Hash
        elem.reduce("") do |acc,i|
          acc << ("<#{i[0]}>"+ params_to_xml(i[1]) +"</#{i[0]}>")
          acc
        end
      else
        elem.to_s
      end
    end

    def close
      @s.close
    end
  end
end





