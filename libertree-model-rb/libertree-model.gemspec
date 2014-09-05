Gem::Specification.new do |s|
  s.name        = 'libertree-model'
  s.version     = '0.9.1'
  s.date        = '2014-08-19'
  s.summary     = "Database library for Libertree"
  s.description = "Database library for Libertree"
  s.authors     = ["Pistos", "rekado"]
  # s.email       = ''
  s.files       = Dir["lib/**/*"]
  s.homepage    = 'http://libertreeproject.org/'

  s.add_dependency 'ruby-oembed', '~> 0.8.8'
  s.add_dependency 'pg'
  s.add_dependency 'sequel'
  s.add_dependency 'timedcache'
  s.add_dependency 'bcrypt-ruby', '= 3.0.1'
  s.add_dependency 'nokogiri', '~> 1.5'
  s.add_dependency 'net-ldap', '~> 0.5.1'
  s.add_dependency 'parkdown', '~> 1.4.23'
end
