# Import a JPG/CR2 file and add RPC information. Because of the way this
# is called, it is convenient to embed the row and camera to process in
# the output file name.
# We generate all three bands at the same time.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

navfile,camfile, rpc_file = ARGV

t = Argus.new(navfile, camfile, nil)
unless(rpc_file =~ /(.*_(\d+)_(\d+))_(r|g|b).img/)
  raise "Output file should be named image_<row>_<cam>_r.img, e.g., image_3_1 for row 3 camera 1"
end
bname = $1
row = $2.to_i
camindex = $3.to_i
od = t.orbit[row,camindex]
raise("We don't have good orbit data") unless(t.orbit_good(od))
rpc = t.rpc(row,camindex)
["r","g","b"].each_with_index do |b, bi|
  fin = od.image(bi + 1)
  fout = bname + "_#{b}.img"
  fout += ".generating" if(b == "r")
  VicarRasterImage.open(fout, "w", "BYTE", fin.number_line, 
                        fin.number_sample, VicarFile::BASIC2) do |f|
    f.vicar_file.rpc(rpc)
    copy(fin, f)
  end
end

