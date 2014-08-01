require 'libertree/db'
require 'libertree/job-processor'

########################
# FIXME: M4DBI wants us to connect to the db before defining models.  As model
# definitions are loaded when 'lib/jobs' is required, we have to do this first.
Libertree::DB.load_config "#{File.dirname( __FILE__ ) }/../database.yaml"
Libertree::DB.dbh
########################

require_relative '../lib/jobs'

if ARGV[0].nil?
  $stderr.puts "#{$0} <config.yaml>"
  exit 1
end

jobp = Libertree::JobProcessor.new( ARGV[0] )
jobp.extend Jobs

Jobs::Http::Avatar.options = {
  :avatar_dir => jobp.conf['avatar_dir']
}
Jobs::Email::Simple.from = jobp.conf['smtp']['from_address']
Jobs::Request.init_client_conf(jobp.conf)
Mail.defaults do
  delivery_method :smtp, {
    :address              => jobp.conf['smtp']['host'],
    :port                 => jobp.conf['smtp']['port'],
    :user_name            => jobp.conf['smtp']['username'],
    :password             => jobp.conf['smtp']['password'],
    :authentication       => jobp.conf['smtp']['authentication'],
    :domain               => jobp.conf['smtp']['helo_domain'],
    :ssl                  => jobp.conf['smtp']['ssl'],
    :enable_starttls_auto => jobp.conf['smtp']['starttls_auto'],
  }
end

if ENV['LIBERTREE_TASKS']
  tasks = ENV['LIBERTREE_TASKS'].split(/[ ,;]+/)
else
  tasks = Jobs.list.keys
end

jobp.log "Processing tasks: #{tasks.inspect}"
jobp.run tasks
