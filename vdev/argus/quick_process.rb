# This generates NITF, KMZ and/or geotiff files. This is similar to 
# high_resolution_image.rb and rpc_generate.rb, but tweaked for how we
# do processing which doing the quick ground processing.
#
# Because it is convenient, we pass in the row and camera to process
# by giving a file name.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

t = Argus.new("quick_nav.csv", "../Configuration/camera.yml",
              "quick_footprint.shp")
fname, point_name = ARGV
fname =~ /row(\d+)_cam(\d+)\./
row = $1.to_i
cam = $2.to_i
config = YAML::load(File.read("../Configuration/quick_process_config.yml"))
if(config["generate_files"].include?("nitf") && 
   config["nitf_resolution"].include?("full_resolution"))
  fout = "../POI/#{point_name}/rpc_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.ntf"
  riarr = GdalRasterImage::read_all(t.orbit[row,cam].file_name).collect {|img| img.deref}
  NitfRpc.write(fout,riarr,t.rpc(row,cam))
end
if(config["generate_files"].include?("nitf") && 
   config["nitf_resolution"].include?("1m"))
  fout = "../POI/#{point_name}/rpc_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}_1m.ntf"
  
  scale_factor = (1.0 / t.orbit[row,cam].resolution_meter(t.camera[cam])).floor
  riarr = GdalRasterImage::read_all(t.orbit[row,cam].file_name).collect do |img|
    RasterAveraged.new(img, scale_factor, scale_factor)
  end
  r = t.rpc(row,cam)
  r.line_offset /= scale_factor
  r.line_scale /= scale_factor
  r.sample_offset /= scale_factor
  r.sample_scale /= scale_factor
  NitfRpc.write(fout,riarr,r)
end
if(config["generate_files"].include?("nitf") && 
   config["nitf_resolution"].include?("2.5m"))
  fout = "../POI/#{point_name}/rpc_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}_2_5m.ntf"
  
  scale_factor = (2.5 / t.orbit[row,cam].resolution_meter(t.camera[cam])).floor
  riarr = GdalRasterImage::read_all(t.orbit[row,cam].file_name).collect do |img|
    RasterAveraged.new(img, scale_factor, scale_factor)
  end
  r = t.rpc(row,cam)
  r.line_offset /= scale_factor
  r.line_scale /= scale_factor
  r.sample_offset /= scale_factor
  r.sample_scale /= scale_factor
  NitfRpc.write(fout,riarr,r)
end
if(config["generate_files"].include?("kmz"))
  fout = "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.kmz"
  begin
    t.create_high_resolution(row,cam,fout + ".tif", "tif", "geodetic")
    KmzFile.generate_kmz(fout + ".tif", fout)
  ensure
    system "rm \"#{fout}.tif\" &> /dev/null"
  end
end
if(config["generate_files"].include?("geotiff"))
  fout = "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}.tif"
  t.create_high_resolution(row,cam,fout, "tif")
  if(config["tiff_resolution"].include?("1m"))
    fout2 = "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}_1m.tif"
    system "rm \"#{fout2}\" \"#{fout2}.temp\"  &> /dev/null"
    system "gdalwarp \"#{fout}\" -tr 1.0 1.0 \"#{fout2}.temp\""
    system "gdal_translate \"#{fout2}.temp\" -co \"COMPRESS=LZW\" \"#{fout2}\""
    system "rm \"#{fout2}.temp\"  &> /dev/null"
  end
  if(config["tiff_resolution"].include?("2.5m"))
    fout2 = "../POI/#{point_name}/ortho_row#{sprintf("%04d",row)}_cam#{sprintf("%02d", cam)}_2_5m.tif"
    system "rm \"#{fout2}\" \"#{fout2}.temp\"  &> /dev/null"
    system "gdalwarp \"#{fout}\" -tr 2.5 2.5 \"#{fout2}.temp\""
    system "gdal_translate \"#{fout2}.temp\" -co \"COMPRESS=LZW\" \"#{fout2}\""
    system "rm \"#{fout2}.temp\"  &> /dev/null"
  end
end

