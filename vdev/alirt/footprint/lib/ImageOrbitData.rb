require File.dirname(__FILE__) + "/PosExportOrbit"

# This matches up each of the JPEG images with the orbit data that goes with 
# it. 
#
# Note that at least for ARGUS because of the large number of files this was
# actually a reasonably time consuming process. Because of this, we process
# the data and then store it in a Marshal file so we don't need to redo this
# process every time we want to do something with a file. This might end up
# not actually being necessary because the number of image files is small 
# enough. If that turns out to be the case, we remove this step.

class ImageOrbitData
  attr_reader :time_offset, :orbit_file_name, :camera_directory,
  :image_to_orbit_data
  def initialize(fname)
    @time_offset, @orbit_file_name, @camera_directory, 
    @image_to_orbit_data = Marshal.load(File.read(fname))
  end
  # Process through the given data and stash the results for later opening 
  # a file to get the ImageOrbitData.
  def self.save_data(fname, orbit_file_name, camera_directory, 
                               toffset = 0)
    orb = nil
    h = Hash.new
    Dir.glob([camera_directory + "/*.JPG",
              camera_directory + "/*.jpg"]).each do |f|
      # Use first image found to determine start of week 
      unless(orb)
        orb = PosExportOrbit.new(orbit_file_name, 
              PosExportOrbit.start_of_week(image_time(f, toffset)))
      end
      h[File.basename(f)] = orb.aircraft_orbit_data(image_time(f, toffset))
    end
    File.open(fname, "w") do |f|
      f << Marshal.dump([toffset, orbit_file_name, camera_directory,
                         h])
    end
  end
  # Read the EXIF data for the given file to determine the image time 
  # stamp. The time for the camera is nominally synchronized to the Applanix
  # unit, which means it is GPS time rather than UTC. UTC includes leapseconds 
  # while GPS doesn't, the difference as of 2009 is 15 seconds.
  #
  # The camera time is nominally synchronized to the Applanix GPS time, but
  # we allow there to be an offset if needed to correct the image time stamp.
  def self.image_time(fname, toffset = 0)
    ts = `exiv2 #{fname} | grep "Image timestamp"`
    unless(ts =~ /(\d+):(\d+):(\d+) (\d+):(\d+):(\d+)/)
      raise "Unrecognized time stamp #{ts}"
    end
    Time.utc($1,$2,$3,$4,$5,$6) + 15 + toffset
  end
end
