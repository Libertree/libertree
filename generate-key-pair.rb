require 'rcrypt'

pubkey_file = ARGV[0] || 'public.key'
privkey_file = ARGV[1] || 'private.key'

if File.exist?(pubkey_file)
  $stderr.puts "#{pubkey_file} exists.  Not overwriting; aborting."
  exit 1
end
if File.exist?(privkey_file)
  $stderr.puts "#{privkey_file} exists.  Not overwriting; aborting."
  exit 2
end

pair = RCrypt.generate_key_pair
File.open(pubkey_file, 'w') { |f| f.puts pair[:public] }
File.open(privkey_file, 'w') { |f| f.puts pair[:private] }
File.chmod(0600, privkey_file)

puts "Generated #{pubkey_file} and #{privkey_file}."
