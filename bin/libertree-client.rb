require 'libertree/client'
require 'readline'
require 'openssl'

Tree = Struct.new(:ip)
Forest = Struct.new(:id, :name, :trees)
Member = Struct.new(:username, :avatar_path)

if ARGV.size < 1
  $stderr.puts "#{$0} <private key file> [avatar_url_base]"
  exit 1
end

key = OpenSSL::PKey::RSA.new File.read(ARGV[0])
socket = '/tmp/libertree-relay'

begin
  client = Libertree::Client.new(
    private_key: key,
    public_key: key.public_key.to_pem,
    avatar_url_base: ARGV[2],
    socket: socket
  )

  while input = Readline.readline("libertree> ", true)
    next  if input.nil? || input.empty?

    command, *params = input.split(/\s+/)
    param = params[0]
    case command.downcase
    when /^f/  # forest
      forest = Forest.new(
        rand(99),
        params[0],
        params[1..-1].map { |p|
          Tree.new(p)
        }
      )
      client.req_forest forest
    when /^m/  # member
      member = Member.new(params[0], params[1])
      client.req_member member
    end
  end
rescue Errno::ECONNREFUSED
  $stderr.puts "Failed to connect to socket at #{socket}"
  exit 1
rescue Errno::EPIPE
  $stderr.puts "Broken pipe. Reconnecting."
  retry
end
