source 'http://rubygems.org'
gem 'm4dbi'
gem 'rdbi', :git => 'git://github.com/RDBI/rdbi.git', :branch => 'sth-leak'
gem 'rdbi-driver-postgresql', :git => 'git://github.com/RDBI/rdbi-driver-postgresql.git', :branch => 'fix-execute-memory-leak'
gem 'syck', :platforms => [:ruby_20]

group 'extensions' do
  gem 'pg'
  gem 'bcrypt-ruby'
end

group 'development' do
  gem 'linecache19', :git => 'git://github.com/mark-moseley/linecache', :platforms => [:ruby_19]
  gem 'ruby-debug-base19x', '~> 0.11.30.pre4', :platforms => [:ruby_19]
  gem 'ruby-debug19', :platforms => [:ruby_19]
  gem 'factory_girl'
  gem 'rspec'
end
