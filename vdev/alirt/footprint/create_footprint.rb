# This generate a shape file describing the footprints of a set of images.

require File.dirname(__FILE__) + "/alirt"

if(ARGV.size != 3)
  print <<END
ruby create_footprint.rb <image to orbit data file> <config file> <output footprint file>

This create a footprint file. It takes the image to orbit data file and
configuration file, and generates a Shapefile.

If the output name has an extension ".kml" then we generate a google Earth
KML file. Otherwise we generate a ESRI shapefile (we could support other 
formats, just let Andrew know that we should add a different format).
END
  exit 1
end

iodfname, cfname, out = ARGV

iod = ImageOrbitData.new(iodfname)
config = YAML::load(File.read(cfname))
maxlen = 0
iod.image_to_orbit_data.keys do |img_name| 
  ln = img_name.length
  maxlen = ln if(ln > maxlen)
end
dem = SrtmDem.new
cam = ArgusCamera.new(config[:yaw].to_f, config[:pitch].to_f, 
                      config[:roll].to_f, 
                      config[:focal_length].to_f)
driver_name = "ESRI Shapefile"
driver_name = "KML" if(out =~ /\.kml$/)
ShapeFile.open(out, "w", driver_name) do |f|
  lay = f.add_layer("footprint", ShapeFile::WKBPOLYGON,
                    [["File", [ShapeFile::OFTSTRING, maxlen]], 
                    ])
  iod.image_to_orbit_data.keys.each do |img_name|
    puts "Processing #{img_name}"
    od = iod.image_to_orbit_data[img_name]
    fp = od.footprint(cam, dem).collect {|pt| [pt.longitude, pt.latitude] }
    lay << {:Geometry => ShapeFile.polygon_2d(fp), :File => img_name}
  end
end
