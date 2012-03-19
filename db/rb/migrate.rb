require 'm4dbi'
require 'rdbi-driver-postgresql'
require 'syck'

module Libertree
  module DB
    def self.config
      if @config
        @config
      else
        configs ||= Syck.load( IO.read("#{ File.dirname( __FILE__ ) }/../database.yaml") )
        env = ENV['LIBERTREE_ENV'] || 'development'
        @config = configs[env]
      end
    end

    def self.dbh
      @dbh ||= M4DBI.connect(
        :PostgreSQL,
        host:     config['host'],
        database: config['database'],
        username: config['username']
      )
    end

    def self.ensure_migration_table_exists
      result = dbh.execute( "SELECT 1 FROM pg_tables WHERE schemaname='public' AND tablename = 'schema_migrations'" )
      return  if result.has_data?

      dbh.execute( %{
        CREATE TABLE schema_migrations (
          filename VARCHAR(1024) NOT NULL UNIQUE
        )
      } )
    end

    def self.migrate
      ensure_migration_table_exists
      migrations_run = dbh.execute( "SELECT filename FROM schema_migrations" ).as(:Struct).fetch(:all).map(&:filename)

      __DIR__ = File.dirname( __FILE__ )
      Dir["#{__DIR__}/migrations/*.sql"].sort.each do |migration|
        filename = File.basename( migration )
        if ! migrations_run.include?( filename )
          dbh.transaction do
            dbh.execute "INSERT INTO schema_migrations ( filename ) VALUES ( ? )", filename
            system "psql -v ON_ERROR_STOP=1 -U #{config['username']} -f '#{migration}' -1 #{config['database']}" or
              dbh.rollback
          end
        end
      end
    end
  end
end

Libertree::DB.migrate
