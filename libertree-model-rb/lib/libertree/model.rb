require 'libertree/db'
require_relative 'compat'
Sequel::Model.plugin :dirty
Sequel::Model.plugin :compat
Sequel::Model.unrestrict_primary_key
Sequel::Model.plugin :json_serializer

require_relative 'model/is-remote-or-local'
require_relative 'model/has-searchable-text'
require_relative 'model/has-display-text'

require_relative 'model/account'
require_relative 'model/account-settings'
require_relative 'model/chat-message'
require_relative 'model/comment'
require_relative 'model/comment-like'
require_relative 'model/contact-list'
require_relative 'model/forest'
require_relative 'model/embed-cache'
require_relative 'model/invitation'
require_relative 'model/job'
require_relative 'model/member'
require_relative 'model/message'
require_relative 'model/node'
require_relative 'model/node_affiliation'
require_relative 'model/node_subscription'
require_relative 'model/notification'
require_relative 'model/pool'
require_relative 'model/pool-post'
require_relative 'model/post'
require_relative 'model/post-hidden'
require_relative 'model/post-like'
require_relative 'model/post-revision'
require_relative 'model/profile'
require_relative 'model/river'
require_relative 'model/remote-storage-connection'
require_relative 'model/server'
require_relative 'model/session-account'
require_relative 'model/url-expansion'

require 'timedcache'
Libertree::MODELCACHE = TimedCache.new
Libertree::Model::Server.plugin  :caching, Libertree::MODELCACHE, :ttl=>86400
Libertree::Model::Member.plugin  :caching, Libertree::MODELCACHE, :ttl=>1800
Libertree::Model::Profile.plugin :caching, Libertree::MODELCACHE, :ttl=>1800
Libertree::Model::Post.plugin    :caching, Libertree::MODELCACHE, :ttl=>120
Libertree::Model::Comment.plugin :caching, Libertree::MODELCACHE, :ttl=>120
