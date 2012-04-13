require 'libertree/db'

# Connect to the DB so the ORM can get what it needs to get
Libertree::DB.dbh

require_relative 'model/account'
require_relative 'model/comment'
require_relative 'model/job'
require_relative 'model/member'
require_relative 'model/notification'
require_relative 'model/post'
require_relative 'model/river'
require_relative 'model/server'
require_relative 'model/session-account'

# require_relative 'model/profile'
# require_relative 'model/sharing'
# require_relative 'model/contact-category'
# require_relative 'model/contact'

M4DBI::Model.one_to_many( Libertree::Model::Post, Libertree::Model::Comment, :comments, :post, :post_id )
M4DBI::Model.one_to_many( Libertree::Model::Account, Libertree::Model::River, :rivers, :account, :account_id )
