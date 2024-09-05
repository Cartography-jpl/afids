# This determines the high resolution files needed to be processed to 
# cover the data.
# We write this out in a format that can be used by a Makefile.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

navfile, camfile, footfile, output = ARGV

t = Argus.new(navfile, camfile, footfile)

h = t.high_resolution_list

File.open(output, "w") do |f|
  f << "highres_list = "
  f << h.join( " \\\n")
  f << "\n\n"
end
