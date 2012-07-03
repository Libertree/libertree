require 'libertree/job-processor'
require_relative '../lib/jobs'

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end

jobp = JobProcessor.new( ARGV[0], "test" )
jobp.extend Jobs
jobp.run
