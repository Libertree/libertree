require 'libertree/db'

# Connect to the DB so the ORM can get what it needs to get
Libertree::DB.dbh

require_relative 'model/server'
