require 'libertree/db'
Libertree::DB.dbh
require 'libertree/model/account'

print "Username: "; $stdout.flush
username = $stdin.gets
print "Password: "; $stdout.flush
password = $stdin.gets

Libertree::Model::Account.create( username: username, password_encrypted: BCrypt::Password.create(password) )
