require 'openssl'

privkey_file = ARGV[0] || 'private.key'

if File.exist?(privkey_file)
  $stderr.puts "#{privkey_file} exists.  Not overwriting; aborting."
  exit 1
end

key = OpenSSL::PKey::RSA.new(2048, 65537)
File.open(privkey_file, 'w') { |f| f.puts key.to_pem }
File.chmod(0600, privkey_file)

puts "Generated #{privkey_file}."
