# This produces an high resolution orthorectified image for a single row 
# and camera.
# Because of the way this is called, it is convenient to embed the row
# and camera number in the output file.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

navfile,camfile, footprint, outputfile = ARGV

t = Argus.new(navfile, camfile, footprint)
unless(outputfile =~ /_row(\d+)_cam(\d+)\.tif/)
  raise "Output file should be named ortho_row<row>_cam<cam>.tif, e.g., ortho_row0003_cam01.tif for row 3 camera 1"
end
row = $1.to_i
camindex = $2.to_i

t.create_high_resolution(row, camindex, outputfile)
