Gem::Specification.new do |s|
  s.name        = 'libertree-model'
  s.version     = '0.0.1'
  s.date        = '2013-06-04'
  s.summary     = "Database library for Libertree"
  s.description = "Database library for Libertree"
  s.authors     = ["Pistos", "rekado"]
  # s.email       = ''
  s.files       = Dir["lib/**/*"]
  s.homepage    = 'http://libertreeproject.org/'

  s.add_dependency 'ruby-oembed', '~> 0.8.8'
  s.add_dependency 'pg', '= 0.9.0'
  s.add_dependency 'bcrypt-ruby', '= 3.0.1'
  s.add_dependency 'nokogiri', '= 1.6.0'
end
