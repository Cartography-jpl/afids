# This produces an orthorectified image for a single row and camera.
# Because of the way this is called, it is convenient to embed the row
# and camera number in the output file.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

navfile,camfile, ortho_file = ARGV

t = Argus.new(navfile, camfile, nil)
unless(ortho_file =~ /_(\d+)_(\d+)\.img/)
  raise "Output file should be named ortho_<row>_<cam>.img, e.g., ortho_3_1.img for row 3 camera 1"
end
row = $1.to_i
camindex = $2.to_i
t.ortho_rect(row,camindex,ortho_file)
