require 'libertree/db'
require 'irb'

module Libertree
  module Console
    def self.init(db_conf_path='database.yaml')
      Libertree::DB.load_config db_conf_path
      Libertree::DB.dbh  # connect
      require 'libertree/model'
    end

    def self.start
      puts "\n== Libertree console ==\n\n"
      IRB.start
    end
  end
end
