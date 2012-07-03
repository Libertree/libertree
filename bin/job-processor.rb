require 'libertree/job-processor'
require_relative '../lib/jobs'

log_file = ARGV[0]
queue = "test"

jobp = JobProcessor.new( log_file, queue )
jobp.extend Jobs
jobp.run
