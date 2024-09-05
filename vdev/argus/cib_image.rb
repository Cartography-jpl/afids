# This produces a ERDAS image of the CIB that covers a given set of rows.
# Because of the way this is called, it is convenient to embed the row
# numbers in the output file.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

navfile,camfile, cib_file = ARGV
t = Argus.new(navfile, camfile, nil)
unless(cib_file =~ /_(\d+)_(\d+)\.img/)
  raise "Output file should be named cib_<row1>_<row2>.img, e.g., cib_1_3.img for rows 1 through 3"
end
row1 = $1.to_i
row2 = $2.to_i

fp = []
row1.upto(row2) do |r|
  1.upto(13) do |c|
    od = t.orbit[r,c]
    next unless(t.orbit_good(od))
    od.footprint(t.camera[c],t.dem).each do |pt| 
      fp << pt
    end
  end
end
GdalMapProjectedImage.save_to_erdas(cib_file, t.ref_image.cover(Vector_GroundCoordinate.from_arr(fp),10))

