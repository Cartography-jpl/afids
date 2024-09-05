# This produces the list of images to do quick processing on.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"
STDOUT.sync = true              # Immediately print out

module Geocal
  class ArgusOrbit
    def row_with_file(file, cam_num)
      (0...(number_row - 1)).to_a.find do |r|
        n = self[r, cam_num]
        n && n.file_name == file
      end
    end
  end
end

t = Argus.new("quick_nav.csv", "../Configuration/camera.yml")
poi = ShapeFile.open("../POI/poi.shp")
pl = poi.first
config = YAML::load(File.read("../Configuration/quick_process_config.yml"))
genname = nil
case
when config["generate_files"].include?("nitf")
  gename = lambda {|row, cam, point_name| "../POI/#{point_name}/rpc_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.ntf" }
when config["generate_files"].include?("kmz")
  gename = lambda {|row, cam, point_name| "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.kmz" }
when config["generate_files"].include?("geotiff")
  gename = lambda {|row, cam, point_name| "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.tif" }
else
  raise "Need to have at least one of nitf, kmz, or geotiff listed"
end

blist = []
poiset = Set.new
1.upto(13) do |cam|
  # Read list of files to process, but also check to see if files are
  # actual available.
  flist = File.read("quick_copy_cam#{sprintf("%02d",cam)}.txt").split
  flist = flist.collect {|f| "../Image/Cam#{sprintf("%02d",cam)}/#{f}"}
  flist = flist.find_all {|f| File.exists?(f)}
  
# Now, find the row number that goes with each item we have found.

  rowlist = flist.collect {|f| t.orbit.row_with_file(f, cam)}
  
# Then determine POIs we will generate  
  rowlist.each do |row|
    if(row)
      od = t.orbit[row, cam]
      fp = od.footprint(t.camera[cam], t.dem).collect {|pt| [pt.longitude, pt.latitude] }
      pl.layer.set_spatial_filter(ShapeFile.polygon_2d(fp))
      poilist = pl.collect {|pf| pf[:Name].gsub(" ", "")}
      poilist.each do |poiname|
        blist << gename.call(row, cam, poiname)
        poiset << poiname
      end
    end
  end
end
blist.sort!
File.open("quick_product_list.in.generating", "w") do |f|
  f << "quick_product_list = " << blist.join(" \\\n") << "\n"
  poiset.each do |poiname|
    f << <<END
../POI/#{poiname}/rpc_%.ntf:
\tmkdir -m g+w -p $(dir $@)
\truby $(script_dir)/quick_process.rb  $@ #{poiname}

../POI/#{poiname}/ortho_%.kmz:
\tmkdir -m g+w -p $(dir $@)
\truby $(script_dir)/quick_process.rb  $@ #{poiname}

../POI/#{poiname}/ortho_%.tif:
\tmkdir -m g+w -p $(dir $@)
\truby $(script_dir)/quick_process.rb  $@ #{poiname}
END
  end
end
