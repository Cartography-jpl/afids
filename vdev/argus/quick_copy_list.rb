# This generates a list of files we need to copy off of the CF cards to
# process the set of POIs.
# 
# As intermediate files produced in this process, we generate quick_nav.csv and
# a quick_footprint.shp. 
# This is a quick version, we don't do the consistency corrections to
# the aflight data, we simply believe what the file says. This might
# result in bad data, but there really isn't anything else we can do
# until all of the data is available.
#
# This takes the directory name, the default is the current directory. The
# input and output files are then determined by the directory structure.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"
require "lib/ReadFlight"
require "csv"
require "ogr"
include Gdal
STDOUT.sync = true              # Immediately print out

dirbase = (ARGV.size > 0 ? ARGV.first : ".")
dirbase = "." if(dirbase == "")

#==========================================================
# Check for input files, and complain if we don't find them.
#==========================================================

aflight = Dir.glob(dirbase + "/AFlight/*.aflight")
camfile = "#{dirbase}/Configuration/camera.yml"
poifile = "#{dirbase}/Configuration/poi.csv"
unless(aflight && aflight.size > 0)
  puts <<END
No aflight files where found in #{dirbase}/AFlight. Populate
this directory and then try running this program again.
END
  exit 1
end
unless(File.exists?(camfile))
  puts <<END
No camera calibration file found in #{camfile}. Add this file
and then try running this program again.
END
  exit 1
end
unless(File.exists?(poifile))
  puts <<END
No POI file found in #{poifile}. Add this file
and then try running this program again.
END
  exit 1
end

#==========================================================
# Generate nav file.
#==========================================================

puts "Generating the quick_nav.csv file..."
rf = ReadFlight.new(aflight)
puts
puts "************************************************************"
puts <<END
Total number of triggers recorded in the aflight file, for cameras 
   1 through 13:
END
p rf.number_captures
puts
puts "Number of triggers marked as successful"
p rf.number_successful_captures
puts
puts <<END
Number of triggers marked as successful + have image name (the 
  set that is usable in the POI processing)
END
p rf.number_captures_with_image
puts "************************************************************"
puts
system "mkdir -p \"#{dirbase}/GroundProcessed\""
# Change filenames from Windows based names to our local names
rf.cameras.each do |cam|
  if(cam)
    dbase = "../Image/Cam#{sprintf("%02d",cam.camera_index)}/"
    cam.captures_with_image.each do |cap|
      cap.filename = dbase + File.basename(cap.filename.gsub("\\","/"))
    end
  end
end
rf.write_csv("#{dirbase}/GroundProcessed/quick_nav.csv")

#==========================================================
# Generate Shapefile
#==========================================================

puts "Generating the quick_footprint.shp file..."
t = Argus.new("#{dirbase}/GroundProcessed/quick_nav.csv", 
              camfile, nil)
footfile = "#{dirbase}/GroundProcessed/quick_footprint.shp"
footfilekml = "#{dirbase}/GroundProcessed/quick_footprint.kml"
ShapeFile.open(footfile, "w") do |f|
  ShapeFile.open(footfilekml, "w", "KML") do |fkml|
    lay = f.add_layer("footprint", ShapeFile::WKBPOLYGON,
                      [["File", [ShapeFile::OFTSTRING, 100]], 
                       ["Row", [ShapeFile::OFTINTEGER, 4]],
                       ["Camera", [ShapeFile::OFTINTEGER, 2]], 
                       ["Resolution", [ShapeFile::OFTREAL, 3, 2]],
                      ])
    laykml = fkml.add_layer("footprint", ShapeFile::WKBPOLYGON,
                         [["File", [ShapeFile::OFTSTRING, 100]], 
                          ["Row", [ShapeFile::OFTINTEGER, 4]],
                          ["Camera", [ShapeFile::OFTINTEGER, 2]], 
                          ["Resolution", [ShapeFile::OFTREAL, 3, 2]],
                         ])
    puts "Have #{t.orbit.number_row} rows to do"
    t.orbit.number_row.times do |row|
      print "." if(row % 10 ==0)
      1.upto(13) do |cam|
        od = t.orbit[row, cam]
        next unless(t.orbit_good(od))
        fp = od.footprint(t.camera[cam], t.dem).collect {|pt| [pt.longitude, pt.latitude] }
        lay << {:Geometry => ShapeFile.polygon_2d(fp), :Row => row, 
          :Camera => cam, :File => od.file_name,
          :Resolution => od.resolution_meter(t.camera[cam]) }
        laykml << {:Geometry => ShapeFile.polygon_2d(fp), :Row => row, 
          :Camera => cam, :File => od.file_name,
          :Resolution => od.resolution_meter(t.camera[cam]) }
      end
    end
  end
  print "\n"
end

#==========================================================
# Read in the POI data, and generate a shapefile for it
#==========================================================

system "mkdir -p \"#{dirbase}/POI\""
ShapeFile.create_from_poi_csv("#{dirbase}/POI/poi.shp", poifile)
ShapeFile.create_from_poi_csv("#{dirbase}/POI/poi.kml", poifile, "KML")
poi = ShapeFile.open("#{dirbase}/POI/poi.shp")

#========================================================================
# Subset the shapefile by the POI list and print out the resulting files
# for each camera.
#========================================================================

puts "Searching for where we see the list of POIs"
sf = ShapeFile.open(footfile)
sl = sf.first
sl.layer.set_spatial_filter(poi.first.geometry_collection)
count = []
1.upto(13) do |cam|
  sl.set_attribute_filter("Camera = #{cam}")
  flist = sl.collect {|sf| File.basename(sf[:File])}
  count << flist.size
  File.open("#{dirbase}/GroundProcessed/quick_copy_cam#{sprintf("%02d", cam)}.txt", "w") do |of|
    of << flist.join("\n")
  end
end
puts
puts
puts "************************************************************"
puts "Number of files we need to process POIs, for camera 1 through 13"
p count
puts "************************************************************"


