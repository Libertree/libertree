require 'libertree/client'
require 'readline'

Member = Struct.new(:username, :avatar_path)

if ARGV.size < 2
  $stderr.puts "#{$0} <public key file> <private key file> [avatar_url_base]"
  exit 1
end

client = Libertree::Client.new(
  public_key: File.read(ARGV[0]),
  private_key: File.read(ARGV[1]),
  avatar_url_base: ARGV[2]
)

while input = Readline.readline("libertree> ", true)
  command, *params = input.split(/\s+/)
  param = params[0]
  case command.downcase
  when /^c/  # connect
    client.connect param
  when /^m/  # member
    member = Member.new(params[0], params[1])
    client.req_member member
  end
end
