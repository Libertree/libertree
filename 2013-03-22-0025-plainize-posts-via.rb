require 'm4dbi'
require 'rdbi-driver-postgresql'
require 'syck'
require 'markdown'

all_confs = Syck.load( File.read("#{ File.dirname( __FILE__ ) }/config/database.yaml") )
env = ENV['LIBERTREE_ENV'] || 'development'
conf_db = all_confs[env]

$dbh ||= M4DBI.connect(
  :PostgreSQL,
  host:     conf_db['host'],
  database: conf_db['database'],
  username: conf_db['username'],
  password: conf_db['password']
)

require 'libertree/model'
Libertree::DB.config = conf_db

require_relative 'lib/libertree/render'

# -------

Libertree::Model::Post.each do |post|
  if post.via
    post.via = Libertree.plain(post.via)
    $stdout.print '.'; $stdout.flush
  end
end

puts
