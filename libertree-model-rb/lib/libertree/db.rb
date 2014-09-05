require 'sequel'
require 'yaml'

module Libertree
  module DB
    LAST_MIGRATION = "2014-08-31-2050-search-indices.sql"

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

      # ensure the latest required migration exists
      if @dbh[:schema_migrations].where(filename: LAST_MIGRATION).count == 0
        fail "Libertree::DB: migration `#{LAST_MIGRATION}' not found.  Please update libertree-db."
      end
      @dbh
    end
  end
end
