# This contains the class ReadFlight which is used to read the aflight data
# and correct it for bad information. We ultimately use this to generate
# a CSV file matching image data to exterior orientation which is then
# read by the C++ classes.

require "rubygems"
require "xml"
require "time"
require "lib/bsearch"
require "lib/ImageTime"

# Class to hold camera information.
class CameraRf
  @@attribute_list = [:offset_x, :offset_y, :offset_z,
                     :bias_offset_x, :bias_offset_y, :bias_offset_z,
                     :sigma_offset_x, :sigma_offset_y, :sigma_offset_z,
                     :angle_x, :angle_y, :angle_z,
                     :bias_angle_x, :bias_angle_y, :bias_angle_z,
                     :sigma_angle_x, :sigma_angle_y, :sigma_angle_z,
                     :focal_length, :width, :height,
                     :principal_point_x, :principal_point_y,
                     :k0, :k1, :k2, :k3, :k4, :p1, :p2, :p3]
  attr_reader *@@attribute_list
  # Couple that don't follow camel case
  attr_reader :ccd_width, :ccd_height, :name
  attr_reader :captures
  # Offset in seconds between image time and GPS time.
  attr_accessor :time_offset
  def camel_case(s)
    s.to_s.capitalize.gsub(/_(.)/)  { $1.upcase }
  end
  def camera_index
    name =~ /(\d\d)/
    $1.to_i
  end
  def initialize(cam)
    @name = cam.find_first("Name").content
    @ccd_width = cam.find_first("CCDWidth").content
    @ccd_height = cam.find_first("CCDHeight").content
    @@attribute_list.each do |v|
      instance_variable_set("@" + v.to_s, 
                            cam.find_first(camel_case(v)).content.to_f)
    end
    @captures = []
    cam.find_first("Captures").each do |cap|
      @captures << Capture.new(cap)
    end
    capture_entities = nil
  end
  # Append data from another file. This just adds the capture information
  # in
  def append(cam)
    (@captures << cam.captures).flatten!
  end
  # In mali, there were instance where the trigger lines got swapped around.
  # This means that the capture events were incorrectly labelled, a capture 
  # event for camera 1 might really be for camera 2. This allows us to reset
  # the capture events.
  def set_capture(capturenew)
    @captures = capturenew
    @captures.each do |cap|
      cap.relabel(camera_index)
    end
  end
  # The captures might be missing the file name or have the wrong one 
  # because of a computer glitch. Look through the first few captures to 
  # find the offset in the EXIF time written out by the cameras (which isn't 
  # particular accurate in absolute time, but should be relatively accurate)
  # and the capture event (which is from the GPS and should be very accurate).
  #
  # Then go through all the capture events and reassign images that are close 
  # to the capture time + whatever offset we saw in the first few.
  #
  # The time_offset is calculated, unless this value has already been set.
  #
  # If we can't get consistent data, then print a warning message but 
  # otherwise leave everything alone.
  def assign_image(idata)
    unless(time_offset)
      t = captures_with_image.find_all {|c| idata.file_time(c.file_base)}
      if(t.size < 10)
        print <<END
We don't have 10 successful captures with image data for camera #{camera_index}.
Skipping assignment of image data.
END
        captures_with_image.each do |c|
          c.filename = nil
        end
      return
      end
      maxd = 1e10
      mean_offset = nil
      0.upto(10) do |i|
        diff = t[(i * 10)..((i * 10) + 9)].collect do |c| 
          c.time - idata.file_time(c.file_base)
        end
        mean_offset = (diff.inject { |sum, x| sum + x })/diff.size
        maxd = (diff.collect { |x| (x - mean_offset).abs }).max
        break if(maxd < 1.0)
      end
      if(maxd > 1.0)
        print <<END
The first 10 successful captures have too large a difference in the offsets
between the capture time and the image time. Skipping assignment of image
data for camera #{camera_index}.
END
        p diff
        captures_with_image.each do |c|
          c.filename = nil
        end
        return
      end
      @time_offset = mean_offset
    end
    successful_captures.each do |c|
      f = idata.closest_image(c.time - time_offset,camera_index, 1.5)
      c.filename = f
    end
  end
  
  # Number of captures that are marked as successful.
  def number_successful_captures
    successful_captures.size
  end
  # Total captures, including unsuccessful ones
  def number_captures
    captures.size
  end
  # Captures that are successful
  def successful_captures
    captures.find_all { |c| c.success }
  end
  # Captures that are successful and have image data
  def captures_with_image
    captures.find_all { |c| c.success && c.filename }
  end
  # List of all file base names for succesful captures
  def filenames
    ((successful_captures.find_all {|c| c.file_base}).collect { |c| c.file_base }).sort
  end
end

# Class to hold capture events. 
#
# TriggerTime:: 
#  This is when the computer told the camera to take the picture, The time
#  is the GPS seconds of the week. You can get the actual week by looking
#  at StartDate for the whole flight.
# Time:: 
#  This is when the camera reported it took the picture, 
# Success:: 
#  This indicates if the computer thought the 
#  computer was successful (Ryan Dotson indicated this should be pretty
#  accurate, if it is false then we are 99% sure the camera didn't take a 
#  picture. We need to verify this in the data).
# Lat::
#  Latitude reported by GPS, in degrees. This is a linear interpolation 
#  between the actual measurements, we don't really have an independent
#  measurement for each capture event. See the NavData class for the 
#  actual measurments. This is relative to WGS-84 datum (verified with 
#  Applanix documentation "POS AV V4/V5 Ethernet & Disk Logging ICD 
#  (External Release)" Revison 14.0, Jan 28, 2008
# Lon::
#  Longitude reported by GPS.
# Altitude::
#  Altitude. This is in meters, relative to WGS-84 ellipsoid (verified
#  with applanix document mentioned above
# Roll::
#  In degrees
# Pitch::
#  In degrees
# Yaw::
#  In degrees
# Filename::
#  File name, if present. This isn't always available (which is a problem
#  we need to address.

class Capture
  @@attribute_list = [:trigger_time, :time, :lat, :lon, :alt, :roll,
                      :pitch, :yaw]
  attr_reader *@@attribute_list
  attr_accessor :filename
  attr_accessor :success
  # File name, with the directory portion stripped off.
  def file_base
    (filename ? filename.sub(/.*\\/, "") : nil)
  end
  # In Mali, the trigger lines got jumbled up. So in some instance what
  # is labelled as camera "1" should really be camera "2".  This does 
  # relabeling, if needed.
  def relabel(camera_number)
    if(filename)
      new_label = sprintf("%04d", camera_number)
      filename.sub!(/(.*\\)\d\d\d\d/, "\\1#{new_label}")
    end
  end
  def initialize(cap)
    cap.each_element do |nd|
      nm = nd.name
      @trigger_time = nd.content.to_f if(nm == "TriggerTime")
      @time = nd.content.to_f if(nm == "Time")
      @lat = nd.content.to_f if(nm == "Lat")
      @lon = nd.content.to_f if(nm == "Lon")
      @alt = nd.content.to_f if(nm == "Alt")
      @roll = nd.content.to_f if(nm == "Roll")
      @pitch = nd.content.to_f if(nm == "Pitch")
      @yaw = nd.content.to_f if(nm == "Yaw")
      @filename = nd.content if(nm == "Filename")
      @success = (nd.content == "true") if(nm == "Success")
    end
  end
end

# Class to hold navigation data
class NavData
  attr_reader :time, :lat, :lon, :alt, :roll, :pitch, :yaw,
  :rms_x, :rms_y, :rms_z, :rms_roll, :rms_pitch, :rms_yaw
  def initialize(nav)
    nav.each_element do |nd|
      nm = nd.name
      @time = nd.content.to_f if(nm == "Time")
      @lat = nd.content.to_f if(nm == "Lat")
      @lon = nd.content.to_f if(nm == "Lon")
      @alt = nd.content.to_f if(nm == "Alt")
      @roll = nd.content.to_f if(nm == "Roll")
      @pitch = nd.content.to_f if(nm == "Pitch")
      @yaw = nd.content.to_f if(nm == "Yaw")
      @rms_x = nd.content.to_f if(nm == "RMSX")
      @rms_y = nd.content.to_f if(nm == "RMSY")
      @rms_z = nd.content.to_f if(nm == "RMSZ")
      @rms_roll = nd.content.to_f if(nm == "RMSRoll")
      @rms_pitch = nd.content.to_f if(nm == "RMSPitch")
      @rms_yaw = nd.content.to_f if(nm == "RMSYaw")
    end
  end
end

# Class to read flight data. The first time we process the XML and
# produce a Marshal file, after that we read the Marshaled file.
class ReadFlight
  attr_reader :startdate, :cameras, :nav_list
  # This can take either a single aflight file, or a list of files. Tim
  # seems to separate files into multiple parts for some reason. We put
  # them back together here if needed. The file passed in can include
  # wildcards, e.g ["dir1/*.aflight", "dir2/*.aflight"].
  #
  # In mali, the triggers wires got swapped around. This means that the 
  # capture data was mislabeled, camera data for camera 1 might really be
  # for camera 2. We take a relabel option. If this is nil, nothing is done.
  # Otherwise, this should be an array of two arrays. The first array is the
  # list of cameras to relabel, and the second is the new label to give it.
  # For example [[1,2,3],[2,3,1]] says that what is labelled as for camera 1
  # should really be for camera 2, camera 2 label is for camera 3, and 
  # camera 3 label is for 1.
  #
  # If a ImageTime object is passed in, we use that reassign file names
  # to all of the capture events. This corrects for missing and/or incorrect
  # file name assignments
  #
  # If mark_all_successful is true, we mark all of the capture events as
  # successful. For some reason a significant number of captures are
  # marked as unsuccessful even though they did trigger and image.
  def initialize(file_list, relabel = nil , idata = nil, time_offset = nil,
                 mark_all_successful = false)
    @cameras = []
    @nav_list = []
    Dir.glob(file_list).each do |f|
      parser = XML::Parser.file(f)
      doc = parser.parse
# Can look at this file to see content. Original file is pretty difficult to
# read.
#doc.save("20090531_084101easyread.xml")

# Get the starting date/time of the flight. Despite the name, this
# actually contains both the date and the time.
      @startdate = doc.find_first("//StartDate").content unless(@startdate)

# File has a list of CameraEntity. This contains the information about
# the nominal camera configuration, and each of the capture events (when
# we try to trigger a picture to be taken). Not ever capture event
# actually records an image, we look at the success flag to see if this
# occurred.

      camera_entities = doc.find("//CameraEntity")
      camera_entities.each_with_index do |cam,camind|
        c = CameraRf.new(cam)
        if(@cameras[c.camera_index])
          @cameras[c.camera_index].append(c)
        else
          @cameras[c.camera_index] = c
        end
      end
      nav_data_entities = doc.find("//NavDataEntity")
      nav_data_entities.each do |nav|
        @nav_list << NavData.new(nav)
      end
# See the libxml documentation on the garbage collection limitation that
# requires us to manually free the returned lists.
      camera_entities = nil
      nav_data_entities = nil
      GC.start
      first = false
    end

# Relabel captures if needed.
      
    if(relabel)
      t = relabel[0].collect {|i| @cameras[i].captures}
      relabel[1].each_with_index {|i, j| @cameras[i].set_capture(t[j])}
    end
    
# Assign time offsets, if given
    if(time_offset)
      @cameras.each_with_index do |cam, c|
        cam.time_offset = time_offset[c] if(cam && time_offset[c])
      end
    end

# Mark all captures as successful if requested
    if(mark_all_successful)
      @cameras.each do |cam|
        if(cam)
          cam.captures.each do |cap|
            if(cap.time && cap.lat && cap.lon && cap.alt &&
               cap.roll && cap.pitch && cap.yaw)
              cap.success = true
            end
          end
        end
      end
    end
    
# Reassign image data if needed.
      
    if(idata)
      @cameras.each { |cam| cam.assign_image(idata) if cam }
    end
      
  end
  
  # Translate CR2 files to PPM if we have any
  def translate_cr2(ppm_dir)
    cnt = 0
    @cameras.each do |cam|
      next unless cam
      cam.captures_with_image.each do |c|
        cnt += 1 if(c.filename =~ /\.CR2$/)
      end
    end
    return unless(cnt > 0)
    puts "Have #{cnt} CR2 files to translate"
    system "mkdir -p #{ppm_dir}; chmod g+w #{ppm_dir}"
    cnt = 0
    @cameras.each do |cam|
      next unless cam
      cam.captures_with_image.each do |c|
        next unless(c.filename =~ /\.CR2$/)
        puts "Doing #{cnt}" if(cnt % 10 ==0)
        flink = ppm_dir + "/" + File.basename(c.filename, ".CR2")
        fnew = ppm_dir + "/" + File.basename(c.filename, ".CR2") + ".ppm"
        unless(File.exists?(fnew))
          system "ln -s #{c.filename} #{flink}"
          system "dcraw #{flink}"
          system "rm #{flink}"
        end
        c.filename = fnew
        cnt += 1
      end
    end
  end

  # Write out data as a CSV file
  def write_csv(fname)
    File.open(fname, "w") do |f|
      f << "File,Camera,Time,Lat,Lon,Alt,Roll,Pitch,Heading\n"
      t = Time.parse(@startdate)
      f << t.year << "-" << t.month << "-" << t.day << " 00:00:00\n"
      @cameras.each do |cam|
        next unless cam
        cam.captures_with_image.each do |c|
          f << c.filename << "," << cam.name << "," << c.time << ","
          f << c.lat << "," << c.lon << "," << c.alt << ","
          f << c.roll << "," << c.pitch << "," << c.yaw << "\n"
        end
      end
    end
  end

  # List of focal lengths
  def focal_lengths
    cameras[1..13].collect {|cam| cam.focal_length}
  end
  # Time offsets for each of the cameras
  def time_offsets
    cameras[1..13].collect {|cam| cam.time_offset}
  end
  # Number of successful captures for each camera
  def number_successful_captures
    cameras[1..13].collect {|cam| cam.number_successful_captures}
  end
  # Number of successful captures for each camera
  def number_captures_with_image
    cameras[1..13].collect {|cam| cam.captures_with_image.size }
  end
  # Total captures, including unsuccessful ones
  def number_captures
    cameras[1..13].collect {|cam| cam.number_captures}
  end
  # It turns out over mali that there are 3 different camera arrangements.
  # It also turns out for now that we can seperate the camera arrangements
  # by looking at camera 3 only. We'll need to modify this logic in
  # the future as we get more data, but for now this is a good way to separate
  # the data.
  def camera_arrangement
    case cameras[3].focal_length
    when 393.418317921218
      return 1
    when 200.0
      return 2
    when 85.0
      return 3
    else
      raise "Unrecognized camera arrangement"
    end
  end
end

