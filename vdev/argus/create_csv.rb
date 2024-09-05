# This program is used to read the aflight and camera image metadata 
# information. We correct errors in the aflight data and write this out
# as a CSV file. The CSV file is easier to parse in C++ code than the
# original XML, plus it has fixes.

$: << File.expand_path(File.dirname(__FILE__))
require "lib/ReadFlight"

# Read in configuration information.
config_file, idata, csv_out = ARGV
config = YAML::load(File.read(config_file))

id = ImageTime.new(idata)
rf = ReadFlight.new(config["aflight_files"], config["relabel_camera"], id,
                    config["time_offset"], false)

rf.translate_cr2(File.dirname(config_file) + "/ppm_dir")
rf.write_csv(csv_out) if(csv_out)

def format_output(d)
  print "   "
  d.each do |n| 
    printf("%-5d", n)
    print " " 
  end
  print "\n"
end

def format_output2(d)
  print "   "
  d.each do |n|
    printf("%-7.2f", (n ? n : -9999.0))
    print " " 
  end
  print "\n"
end

# Print out some diagnostic information
puts "  Camera arrangement: #{rf.camera_arrangement}"
puts "  Focal lengths:"
format_output2(rf.focal_lengths)
puts "  Time offsets:"
format_output2(rf.time_offsets)
puts "  Number succesful captures:"
format_output(rf.number_successful_captures)
puts "  Number succesful captures + image:"
format_output(rf.number_captures_with_image)
puts "  Number captures:"
format_output(rf.number_captures)
puts "  Number images:"
format_output(id.number_image)
puts "  % data covered:"
t = []
tnc = 0
tni = 0
rf.number_captures_with_image.each_with_index do |nc, i|
  if(id.number_image[i] == 0)
    t << 0
  else
    t << (nc * 100.0 / id.number_image[i]).floor
    tnc += nc
    tni += id.number_image[i]
  end
end
format_output(t)
puts "Total = #{(tnc * 100.0/ tni).floor} %"
1.upto(13) do |camind|
  puts "    #{camind}:"
  filenames1 = rf.cameras[camind].filenames
  filenames2 = id.filenames(camind).collect do |f|
    if(f =~ /\.CR2/)
      File.dirname(config_file) + "/ppm_dir/" + 
        File.basename(f, ".CR2") + ".ppm"
    else
      f
    end
  end
  puts "      In aflight, but no image file:"
  p (filenames1 - filenames2).collect {|f| File.basename(f)}
  puts "      Image file, but not in aflight:"
  p (filenames2 - filenames1).collect {|f| File.basename(f)}
end      
