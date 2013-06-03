source 'http://rubygems.org'
gem 'm4dbi'
gem 'rdbi', :git => 'git://github.com/RDBI/rdbi.git', :branch => 'sth-leak'
gem 'rdbi-driver-postgresql', :git => 'git://github.com/RDBI/rdbi-driver-postgresql.git', :branch => 'fix-execute-memory-leak'
gem 'pony'
gem 'libertree-model', :git => 'git://github.com/Libertree/libertree-model-rb.git', :branch => 'xmpp'
gem 'libertree-client', :git => 'git://github.com/Libertree/libertree-client-rb.git', :branch => 'xmpp'
gem 'syck', :platforms => [:ruby_20]
gem 'json'

group 'extensions' do
  gem 'pg'
  gem 'blather'
  gem 'bcrypt-ruby'
end

group 'development' do
  gem 'rspec'
  gem 'linecache19', :git => 'git://github.com/mark-moseley/linecache', :platforms => [:ruby_19]
  gem 'ruby-debug-base19x', '~> 0.11.30.pre4', :platforms => [:ruby_19]
  gem 'ruby-debug19', :platforms => [:ruby_19]
  gem 'debugger', platforms: [:ruby_20]
  gem 'factory_girl'
end
