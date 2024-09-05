$: << File.expand_path(File.dirname(__FILE__))
require "argus"

STDOUT.sync = true              # Immediately out to log file
navfile,calfile,camfile = ARGV
camcal = YAML::load(File.read(calfile))
ref_img = nil                   # Default is CIB-01
if(camcal["reference_image"])
  ref_img = GdalMapProjectedImage.open(camcal["reference_image"]) 
end
t = Argus.new(navfile, camcal["initial_camera"], nil, ref_img)
1.upto(13) do |index|
  cc = camcal[index]
  puts "Doing #{index}"
  if(cc["do_calibration"])
    lineoffset = cc["line_offset"] || 0
    sampleoffset = cc["sample_offset"] || 0
    if(cc["do_manual"])
      puts "Starting manual calibration"
      row = cc["manual_row"]
      od = t.orbit[row,index]
      cpt = []
      cc["tiepoint"].each do |tp|
        pt = t.dem.surface_point(Geodetic.new(tp[1],tp[0]))
        fc = od.frame_coordinate(pt.deref, t.camera[index])
        ref = t.dem.surface_point(Geodetic.new(tp[3],tp[2]))
        cpt << [fc, ref, od]
      end
      t.camera[index] = t.camera[index].fit_camera(cpt)
    end
    puts "Starting automatic calibration"
    puts "Doing coarse calibration"
    res = t.camera_calibrate((cc["row_start"].to_i)..(cc["row_end"].to_i),
                             index, lineoffset, sampleoffset, 10)
    
    unless res           # Try slightly finer if previous failed. For 
                         # 400mm lens, often the 10x10 averaging makes 
                         # too small an image
      res = t.camera_calibrate((cc["row_start"].to_i)..(cc["row_end"].to_i),
                               index, lineoffset, sampleoffset, 5)
    end
    if(res)
      lineoffset = 0
      sampleoffset = 0
    end  
    puts "Doing middle level calibration"
    res = t.camera_calibrate((cc["row_start"].to_i)..(cc["row_end"].to_i),
                             index, lineoffset, sampleoffset, 3)
    if(res)
      lineoffset = 0
      sampleoffset = 0
    end  
    puts "Doing fine level calibration"
    res = t.camera_calibrate((cc["row_start"].to_i)..(cc["row_end"].to_i),
                             index, lineoffset, sampleoffset, 1)
    t.save_camera(camfile)
  end
end
t.save_camera(camfile)
if(camcal["overlap_rows"] && camcal["overlap_rows"].size > 0 &&
   camcal["overlap_cameras"] && camcal["overlap_cameras"].size > 0)
  camlist = camcal["overlap_cameras"]
  conjpoint = t.conjugate_point(camcal["overlap_rows"], camlist)
  camset = ArgusCameraSet.new(t.camera, t.dem)
  camset.fit_camera_set(camlist, conjpoint)
  camset.save_camera(camfile)
end


