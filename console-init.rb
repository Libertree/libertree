require 'libertree/db'
Libertree::DB.load_config 'database.yaml'
Libertree::DB.dbh  # connect
require 'libertree/model'
include Libertree::Model
