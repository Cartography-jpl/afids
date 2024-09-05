#! /usr/bin/env ruby
# -*-Ruby-*-
#
# This checks the test results

require "rubygems"
require "afids"
include Afids

t = VicarRasterImage.open("test.img")
puts "Size is not ok" unless(t.number_line == 6000 && t.number_sample == 6000)
r = t.vicar_file.rpc
center_expect = Geodetic.new(36.858227, -116.139394)
puts "RPC is not ok" unless((r.image_coordinate(center_expect).line - 3000).abs < 3 && (r.image_coordinate(center_expect).sample - 3000).abs < 1)

t = VicarRasterImage.open("test2.img")
puts "Size is not ok" unless(t.number_line == 50 && t.number_sample == 5981)
r = t.vicar_file.rpc
center_expect = Geodetic.new(36.858227, -116.139394)
puts "RPC is not ok" unless((r.image_coordinate(center_expect).line - (3000 - 9)).abs < 3 && (r.image_coordinate(center_expect).sample - (3000 - 19)).abs < 1)
