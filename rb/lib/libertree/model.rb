require 'libertree/db'

# Connect to the DB so the ORM can get what it needs to get
Libertree::DB.dbh

require_relative 'model/account'
require_relative 'model/member'
require_relative 'model/server'

# require_relative 'model/profile'
# require_relative 'model/post'
# require_relative 'model/comment'
# require_relative 'model/sharing'
# require_relative 'model/contact-category'
# require_relative 'model/contact'
