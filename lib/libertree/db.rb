require 'sequel'
require 'yaml'

module Libertree
  module DB
    def self.config
      @config
    end

    def self.config=(hash)
      @config = hash
    end

    def self.load_config(filename)
      config_file = filename
      configs ||= YAML.load( IO.read(config_file) )
      env = ENV['LIBERTREE_ENV'] || 'development'
      @config = configs[env]
    end

    def self.dbh
      @dbh ||= Sequel.postgres(host:     config['host'],
                               database: config['database'],
                               user:     config['username'],
                               password: config['password'])
    end
  end
end
