# Top level include file

require "rubygems"
require "afids"
include Afids

Dir.glob(File.dirname(__FILE__) + "/lib/*.rb").each { |f| require f }
