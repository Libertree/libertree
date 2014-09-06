if ENV['LIBERTREE_ENV'] != 'test'
  $stderr.puts "Are you sure you want to run tests in a non-test LIBERTREE_ENV?"
  $stderr.puts "Running tests DESTROYS data in the tested database."
  $stderr.puts "Comment out this check in spec_helper.rb if you know what you're doing."
  exit 1
end

require_relative '../app'
require_relative 'factories'
require 'capybara/rspec'
require 'rack/test'

Capybara.configure do |config|
  config.default_driver = :rack_test
  config.app            = Ramaze
end

Ramaze.setup_dependencies
Ramaze.options.roots << File.expand_path(File.dirname(__FILE__)+'/..')
Ramaze::Log.loggers = [ Ramaze::Logger::RotatingInformer.new(File.dirname(__FILE__)+'/../log', 'test-%d-%m-%Y.log') ]

$dbh.execute "TRUNCATE accounts CASCADE"

$post_login_path = '/test_user_logged_in'

shared_context 'logged in' do
  before do
    @account = Libertree::Model::Account.create( FactoryGirl.attributes_for(:account) )
    @account.password = 'testpass'
    @account.save

    visit '/login'
    fill_in 'username', :with => @account.username
    fill_in 'password', :with => 'testpass'
    click_on 'Login'
  end
end

shared_context 'rack-test' do
  include Rack::Test::Methods
  def app
    Ramaze
  end
end
