# Simply script that compares the JPG, footprint file and CSV files together.
require "rubygems"
require "afids"
include Afids
# Requires ruport, which we don't install in AFIDS by default. Add by
# "gem install ruport"
require "ruport"

class Inventory
  attr_reader :image_dir, :orbit, :footprint, :name
  def initialize(idir, orbit, footprint)
    @image_dir = idir
    @orbit = ArgusOrbit.new(orbit, Geocal::Time.new, "") if(File.exists?(orbit))
    @footprint = ShapeFile.open(footprint) if(footprint)
    @name = File.basename(orbit, ".csv")
  end
  def add_to_table(table)
    1.upto(13) do |cam|
      jpeg_count, csv_count, footprint_count = collect_list(cam)
      if(jpeg_count > 0)
        table << [name, cam, jpeg_count, csv_count, 
                  (csv_count.to_f / jpeg_count * 100).round,
                  footprint_count, 
                  (footprint_count.to_f / jpeg_count * 100).round]
      else
        table << [name, cam, 0, 0, 0, 0, 0, 0]
      end
    end
    table
  end
  def image_list
    unless(@image_list)
      @image_list = Dir.glob(image_dir + "/*").collect {|f| File.basename(f)}
    end
    @image_list
  end
  def footprint_list
    unless(@footprint_list)
      if(footprint)
        lay = footprint.layers.first[1]
        @footprint_list =  lay.collect {|f| (f[:File] ? f[:File].sub(/.*\\/,"") : nil)}
      else
        @footprint_list = []
      end
    end
    @footprint_list
  end
  def orbit_list
    unless @orbit_list
      @orbit_list = []
      if(orbit)
        orbit.number_row.times do |r|
          1.upto(13) do |c|
            @orbit_list << File.basename(orbit[r, c].file_name) if(orbit[r,c])
          end
        end
      end
    end
    @orbit_list
  end
  def collect_list(cam)
    s = "^" + sprintf("%04d",cam)
    icount = image_list.grep(/#{s}/).size
    ocount = orbit_list.grep(/#{s}/).size
    fcount = footprint_list.grep(/#{s}/).size
    [icount, ocount, fcount]
  end
  def self.do_all
    table = Table(:column_names => ["Name", "Camera", "JPEG Count", 
                                    "CSV Count", "CSV %", 
                                    "Footprint Count", "Footprint %"])
    [["20090513_153500", "20090513_153500_footprints.shp"],
     ["20090513_183300", "20090513_183300_footprints.shp"],
     ["20090514_133000", "20090514_1330_FootPrints.shp"],
     ["20090531_084101", "20090531_084101_footprints.shp"],
     ["20090531_115522", "20090531_115500_footprints.shp"],
     ["20090531_162949", "20090531_1629_footprints.shp"],
     ["20090531_182543", nil],
     ["20090531_184804", nil],
    ].each do |t|
      puts "Doing #{t[0]}"
      if(t[1])
        shapefile = "/raid1/smyth/mali/ShapeFile/#{t[1]}"
      else
        shapefile = nil
      end
      i = Inventory.new("/raid3/argus/#{t[0]}/Images",
                        "/raid3/argus_csv/#{t[0]}.csv",
                        shapefile)
      i.add_to_table(table)
    end
    table.reorder(0,1,2,3,5,4,6)
    grouping = Grouping(table, :by => "Name", :order => :name)
  end
end
t = Inventory.do_all
puts <<END
The PDF file isn't directly readable by Apple Preview for some reason, only on
the first run shows up. You can use a linux box to open the file and print
it back out as PDF, it then is usable by Preview. Acrobat on Apple can also
read the file, but it won't let you print out to PDF for some obnoxious
reason.
END
t.save_as("inventory.pdf")
t.save_as("inventory.csv")
