source 'http://rubygems.org'
gem 'ramaze', git: 'git://github.com/Ramaze/ramaze.git'
gem 'innate', git: 'git://github.com/Ramaze/innate.git'
gem 'json'
gem 'pg'
gem 'sequel'
gem 'bcrypt-ruby'
gem 'dalli'  # memcached client
gem 'unicorn'

gem 'mini_magick'
gem 'sass'
gem 'ruby-oembed'
gem 'fast_gettext'
gem 'gpgme'  # to verify PGP public keys before storing them

group 'extensions' do
  gem 'nokogiri'
  gem 'curb'           # libcurl-dev (Debian) / libcurl-devel (Fedora)
  gem 'ruby-filemagic' # libmagic-dev (Debian) / file-devel (Fedora)
end

gem 'libertree-model', git: 'git://github.com/Libertree/libertree-model-rb.git'

group 'development' do
  gem 'rspec'
  gem 'capybara'
  gem 'selenium-webdriver'
  gem 'factory_girl'
  gem 'racksh'
  gem 'linecache19', git: 'git://github.com/mark-moseley/linecache', platforms: [:ruby_19]
  gem 'ruby-debug-base19x', '~> 0.11.30.pre4', platforms: [:ruby_19]
  gem 'ruby-debug19', platforms: [:ruby_19]
  gem 'pry-byebug', platforms: [:ruby_20]
  gem 'thin'  # For debugging, because it doesn't time out the connection like unicorn
end
