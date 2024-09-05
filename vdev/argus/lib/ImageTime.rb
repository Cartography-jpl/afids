require "yaml"

# This class is used to determine the list of all camera image files
# along with the time stamp given by the EXIF metadata.
#
# Because searching through the directories takes a while, we store the
# results in a Marshal file. This means we can avoid needing to reading
# all the data each time we want to use this class.
#
# Note that the image stamp is not particularly accurate. This is just
# the cameras clock to the nearest second, and the clock is most likely not
# very accurate. However relative times should be somewhat accurate, so
# once we determine a fixed offset from absolute time these times are
# a bit useful.

class ImageTime

  # Read the given Marshal file to create a ImageTime
  def initialize(fname)
    @data, @fname_to_time = Marshal.load(File.open(fname)) if(fname)
  end

  # This goes through the given directories and collects all the information
  # to create a ImageTime object. We store the resulting object in
  # the given marshal file.
  def self.initial_creation(dir_list, mfile)
    i = ImageTime.new(nil)
    puts "Processing Camera image files"
    i.read_data(dir_list)
    print "\nDone\n"
    i.save(mfile)
  end

  # The set of files for a particular camera
  def filenames(cam)
    (@data[cam].collect {|d| d.first}).sort
  end

  # Return number of image files for each camera
  def number_image
    @data[1..13].collect {|d| d.size}
  end

  # Find the image file name that is closest to the given time for the
  # given camera. Return it if the time is within tolerance, otherwise
  # return nil.
  def closest_image(tm, cam_index, tolerance = 1.0)
    i = @data[cam_index].bsearch_first {|x| (x[1] > tm ? 0 : -1)}
    i = i - 1 if(i && i > 0 && (@data[cam_index][i - 1][1] - tm).abs < 
                 (@data[cam_index][i][1] - tm).abs)
    # Handle edge case of time just past the end of our data. -1 is the index
    # used for 1 from  the end
    i = -1 unless(i)
    if((@data[cam_index][i][1] - tm).abs <= tolerance)
      return @data[cam_index][i][0]
    else
      return nil
    end
  end
  
  # Return the time for the given file. We only take the basename of
  # file, stripping off .JPG or .CR2 when looking this up.
  def file_time(fname)
    f = File.basename(File.basename(fname, ".JPG"), ".CR2")
    return @fname_to_time[f]
  end

  # Save as a marshal file
  def save(fname)
    File.open(fname, "w") do |f|
      Marshal.dump([@data, @fname_to_time], f)
    end
  end

  # Go through and populate @data and @fname_to_time
  def read_data(dirlist)
    @data = []                  # This is indexed by camera, and then contains 
                                # file name and time
    14.times {@data << [] }
    @fname_to_time = Hash.new   # This is the file name with directory or 
                                # extension mapped to time
    dirlist.each do |dir|
      puts "  Processing directory #{dir}"
      flist = Dir.entries(dir).find_all do |f| 
        f =~ /((\d\d\d\d)(\d\d\d\d))\.(jpg|cr2)$/i
      end
      puts "    Have #{flist.size} files to process"
      flist.each_with_index do |fbase, i|
        puts "      Processed #{i} files" if((i % 1000) ==0)
        next unless(fbase =~ /((\d\d\d\d)(\d\d\d\d))\.(jpg|cr2)$/i)
        ftrunk = $1
        camindex = $2.to_i
        f = dir + "/" + fbase
        tm = gps_time(f)
        @data[camindex] << [f, tm]
        @fname_to_time[ftrunk] = tm
      end
    end
    # Sort data by time.
    @data = @data.collect do |d|
      d.sort { |x,y| x[1] <=> y[1] }
    end
  end

  # Read the EXIF data for the given file to determine the image time 
  # stamp, and convert that to a GPS time of week.
  def gps_time(fname)
    ts = `exiv2 #{fname} | grep "Image timestamp"`
    unless(ts =~ /(\d+):(\d+):(\d+) (\d+):(\d+):(\d+)/)
      raise "Unrecognized time stamp #{ts}"
    end
    tm = Time.utc($1,$2,$3,$4,$5,$6)
    # GPS time doesn't include leapseconds, but UTC does.  Difference is 15
    # seconds as of 2009
    @tweek = Time.utc(tm.year, tm.month, tm.day) - 
      tm.wday * (24.0 * 60.0 * 60.0) + 15 unless(@tweek)
    tm - @tweek
  end
end
