# This generates a shape file describing the footprints of each camera.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"
STDOUT.sync = true              # Immediately print out

navfile,camfile,footfile = ARGV

t = Argus.new(navfile, camfile, nil)
ShapeFile.open(footfile, "w") do |f|
  lay = f.add_layer("footprint", ShapeFile::WKBPOLYGON,
                    [["File", [ShapeFile::OFTSTRING, 12]], 
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
        :Camera => cam, :File => File.basename(od.file_name),
        :Resolution => od.resolution_meter(t.camera[cam]) }
    end
  end
  print "\n"
end
