require 'ruby-debug'
require_relative '../lib/libertree/model'
require_relative 'factories'

if ENV['LIBERTREE_ENV'] != 'test'
  $stderr.puts "Refusing to run specs in a non-test environment.  Comment out the exit line if you know what you're doing."
  exit 1
end
