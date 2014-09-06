#!/usr/bin/env rackup

require ::File.expand_path('../app', __FILE__)
Ramaze.start  :root => __DIR__, :started => true
run Ramaze
