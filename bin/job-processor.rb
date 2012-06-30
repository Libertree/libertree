require 'job-processor'
require 'jobs'

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end

JobProcessor.register Tasks
JobProcessor.new( ARGV[0], "test" ).run
