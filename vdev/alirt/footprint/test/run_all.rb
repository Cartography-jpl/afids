#! /usr/bin/env ruby
# Run all the tests found in this directory
require 'test/unit'
Dir.glob(File.dirname(__FILE__) + "/*_test.rb").each { |f| require f }
