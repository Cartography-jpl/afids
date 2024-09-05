# Process a data to generate one degree VICAR file, for one band.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

fout = ARGV.shift
unless(fout =~ /mos_(n|s)(\d\d)_(w|e)(\d\d\d)_(r|g|b)/)
  raise "Unrecognized output file, should be mos_<lat>_<lon>_<band>.vic"
end
minlat = $2.to_i * ($1 == "n" ? 1.0 : -1.0)
minlon = $4.to_i * ($3 == "e" ? 1.0 : -1.0)
band = 1 if($5 == "r")
band = 2 if($5 == "g")
band = 3 if($5 == "b")
gc = GeodeticConverter.new
# 5400 is size of 7.5" tiles, and we have 8 per degree
mi = MapInfo.new(gc.to_ptr, minlon, minlat + 1, minlon + 1, minlat, 5400 * 8, 5400 * 8)

# Create first of all using VICAR routines, because VicarLiteMapProjectedImage
# can't create files.

# But then reopen with VicarLiteMapProjectedImage because this uses memory
# mapped I/O which means we don't need to worry about handling caching ourselves.

VicarRasterImage.open(fout, "w", "BYTE", mi.number_y_pixel, mi.number_x_pixel) do |f| 
  f.labels("GEOTIFF")["MODELTIEPOINTTAG"]="(0.0,0.0,0.0,#{mi.ulc_x},#{mi.ulc_y},0)"
  f.labels("GEOTIFF")["MODELPIXELSCALETAG"] = "(#{mi.pixel_x_resolution},#{-mi.pixel_y_resolution},0.0}" 
  f.labels("GEOTIFF")["GTMODELTYPEGEOKEY"]="2(MODELTYPEGEOGRAPHIC)"
  f.labels("GEOTIFF")["GTRASTERTYPEGEOKEY"]="1(RASTERPIXELISAREA)"
  f.labels("GEOTIFF")["GEOGELLIPSOIDGEOKEY"]="7030(ELLIPSE_WGS84)" 
end
f = VicarLiteMapProjectedImage.new(fout, VicarLiteFile::UPDATE)
puts "Have #{ARGV.size} tiles to process"
ARGV.each_with_index do |finname, i|
  puts "Processing tile #{i}"
  fin = GdalMapProjectedImage.open(finname, "r", band)
  ic = f.coordinate(fin.ground_coordinate(0,0).deref)
  f_sub = SubMapProjectedImage.new(f, ic.line.round, ic.sample.round, fin.number_line,fin.number_sample)
  copy(fin, f_sub)
end



