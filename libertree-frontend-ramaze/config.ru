#!/usr/bin/env rackup

require 'ramaze'
require 'sass'
require 'yaml'
require 'mini_magick'
require 'fast_gettext'
require 'markdown'
require_relative 'lib/libertree/lang'
require 'libertree/db'

Libertree::DB.load_config("#{ File.dirname( __FILE__ ) }/config/database.yaml")
$dbh = Libertree::DB.dbh
require 'libertree/model'

$conf = YAML.load( File.read("#{ File.dirname( __FILE__ ) }/config/application.yaml") )
$conf['websocket_blacklist'] ||= []
ENV['RACK_ENV'] = $conf['environment'] || 'live'

# -----------------------------

require ::File.expand_path('../app', __FILE__)
require './member-api/member-api'

Ramaze.start  :root => __DIR__, :started => true

class APIRoutingAdapter
  def initialize(app)
    @app = app
  end

  def call(env)
    request = Rack::Request.new(env)
    # Version 1 of the API was served from Ramaze, but the API has since been
    # moved out of Ramaze.
    if request.path =~ %r{/api/(?!v1)}
      # Have the Grape API handle the request
      env_without_api_prefix = env.dup
      ['REQUEST_PATH', 'PATH_INFO', 'REQUEST_URI'].each do |key|
        env_without_api_prefix[key] = env_without_api_prefix[key].gsub(%r{^/api}, '')
      end
      Libertree::MemberAPI.new.call(env_without_api_prefix)
    else
      # Let Ramaze handle the request
      @app.call(env)
    end
  end
end

use APIRoutingAdapter

run Ramaze
