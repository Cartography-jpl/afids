$: << File.expand_path(File.dirname(__FILE__))
require "rubygems"
require "afids"
include Afids
require "lib/CamCalibration"
require "lib/MosaicFile"
require "forkoff"

# This just collects together some stuff that we are using in one place,
# so we don't need to write this code in every routine we are working
# with.
class Argus
  attr_reader :dem, :orbit, :ref_image, :border, :footprint, :grid_spacing,
  :ref_map_info
  # The list of cameras. By convention, we use the 1 based index used
  # by ARGUS, so the first camera is camera[1], *not* camera[0]
  attr_reader :camera

  # Create an Argus object. We pass in the file names for the orbit, camera
  # and footprint. If the camera or footprint file are not found or are
  # passed in as nil we skip initializing them.
  #
  # The reference image to use can be passed in. The default is to use
  # the CIB 1 data found at ${AFIDS_DATA}/cib1
  #
  def initialize(oname = "nav_data.csv", camfile = "camera.yml", 
                 ffile = "footprint.shp", ref_img = nil)
    @orbit = ArgusOrbit.new(oname)
    @dem = SrtmDem.new("", "", false)
    # Allow this to fail, it just means the CIB isn't available. This will
    # cause some routines to fail, but not others, so we don't immediately
    # trigger an error if the CIB isn't available.
    begin
      if(ref_img)
        @ref_image = ref_img
      else
        @ref_image = 
          VicarMultiFile.new(ENV["AFIDS_DATA"] + "/cib1/cib01_db.int",
                             ENV["AFIDS_DATA"] + "/cib1",
                             ".img", 1000)
      end
      @ref_map_info = @ref_image.map_info
    rescue
      # Set default map info. We hardwire this to the CIB, for use when
      # the CIB isn't available.
      @ref_map_info = MapInfo.new(GeodeticConverter.new.to_ptr,
                                  -12.0016574604822,
                                  25.0112593701802,
                                  4.51246298365644,
                                  10.9933333816365,
                                  1783525, 1513936)
    end
    @footprint = ShapeFile.open(ffile) if(ffile && File.exists?(ffile))
    @border = 10
    @camera = []
    @grid_spacing = 10
    load_camera(camfile) if(camfile && File.exists?(camfile))
  end

  # Return the layer of the footprint file that contains the footprint.
  def footprint_layer
    footprint.layers.first[1]
  end

  # Determine row and camera that covers a given mapinfo. This is based
  # on the footprint file, we don't filter this by checking that we have
  # good orbit data. We order the data by row, and then by the given camera 
  # list
  def touch(mi, camlist)
    ulc = mi.ground_coordinate(0, 0)
    lrc = mi.ground_coordinate(mi.number_y_pixel - 1, mi.number_x_pixel - 1)
    min_lat, min_lon = ulc.latitude, ulc.longitude
    max_lat, max_lon = lrc.latitude, lrc.longitude
    min_lat, max_lat = max_lat, min_lat if(max_lat < min_lat)
    min_lon, max_lon = max_lon, min_lon if(max_lon < min_lon)
    cond = (camlist.collect {|c| "Camera=#{c}"}).join(" OR ")
    footprint_layer.set_attribute_filter(cond)
    footprint_layer.set_rect_filter(min_lon, min_lat, max_lon, max_lat)
    res = footprint_layer.collect {|fl| [fl[:Row], fl[:Camera]] }
    footprint_layer.clear_filter
    camhash = {}
    camlist.each_with_index {|c, i| camhash[c] = i}
    res.sort {|a, b| (a[0] != b[0] ? a[0] <=> b[0] : camhash[a[1]] <=> camhash[b[1]])}
  end

  # Determine overlap area between two row and/or camera. This overlap is
  # just approximate, we find the footprint using just the four corner points. 
  # This is referenced to the ref_image, simply because this is a reasonable 
  # base to use. We return the rectangular region in the reference image
  # map projection that encloses the overlap, or nil if there is no
  # overlap.
  def overlap(row1, row2, index1, index2)
    od1 = orbit[row1, index1]
    od2 = orbit[row2, index2]
    return nil unless(od1 && od2)
    fp1 = od1.footprint(camera[index1], dem).collect do |pt| 
      [pt.longitude, pt.latitude]
    end
    fp2 = od2.footprint(camera[index2], dem).collect do |pt| 
      [pt.longitude, pt.latitude]
    end
    footprint1 = ShapeFile.polygon_2d(fp1)
    footprint2 = ShapeFile.polygon_2d(fp2)
    return nil unless(footprint1.intersect(footprint2))
    env = footprint1.intersection(footprint2).get_envelope
    vg = Vector_GroundCoordinate.new(2)
    vg.set_value([SharedPtr_GroundCoordinate.to_ptr(Geodetic.new(env[2], 
                                                                 env[0])),
                  SharedPtr_GroundCoordinate.to_ptr(Geodetic.new(env[3], 
                                                                 env[1]))])
    ref_map_info.cover(vg)
  end

  # Given a set of rows and camera indexes, find all overlaps between pairs
  # and look for conjugate points between the pairs.
  def conjugate_point(rowlist, camindexlist)
    rowcam = []
    rowlist.each do |row|
      camindexlist.each do |cam|
        rowcam << [row, cam]
      end
    end
    res = []
    rowcam.size.times do |i|
      row1 = rowcam[i][0]
      cam1 = rowcam[i][1]
      puts "Looking at row #{row1} camera #{cam1}"
      (i + 1).upto(rowcam.size - 1) do |j|
        row2 = rowcam[j][0]
        cam2 = rowcam[j][1]
        conjugate_point_pair(row1, row2, cam1, cam2).each do |cp|
          res << cp
        end
      end
    end
    res
  end
  
  # Determine conjugate points between two rows and/or cameras. This searches
  # in the area given by overlap.
  def conjugate_point_pair(row1, row2, index1, index2, lineoffset = 0, 
                           sampleoffset = 0)
    mi = overlap(row1, row2, index1, index2)
    return [] unless(mi)
    od1 = orbit[row1, index1]
    od2 = orbit[row2, index2]
    odp1 = SharedPtr_OrbitData.to_ptr(od1.deref)
    timg1 = OrbitMapProjected.new(mi, odp1, od1.image(1).to_ptr, 
                                  camera[index1].to_ptr, dem.to_ptr)
    oimg1 = MemoryMapProjectedImage.new(timg1)
    odp2 = SharedPtr_OrbitData.to_ptr(od2.deref)
    timg2 = OrbitMapProjected.new(mi, odp2, od2.image(1).to_ptr, 
                                  camera[index2].to_ptr, dem.to_ptr)
    oimg2 = MemoryMapProjectedImage.new(timg2)
    fd = ForstnerFeatureDetector.new
    iclist = fd.interest_point_grid(oimg1, CombinedMask.new, 7, 7)
    match = CcorrLsmMatcher.new
    conjpoint = []
    fact = 1
    iclist.each do |ic1|
      ic2 = ImageCoordinate.new(ic1.line + lineoffset / fact,
                                ic1.sample + sampleoffset / fact)
      if(ic2.line > 0 && ic2.line < oimg2.number_line &&
         ic2.sample > 0 && ic2.sample < oimg2.number_sample)
        t = match.match(oimg1, oimg2, ic1, ic2).first
        if(t)
          gc1 = oimg1.ground_coordinate(ic1, dem)
          gc2 = oimg2.ground_coordinate(t, dem)
          fc1 = od1.frame_coordinate(gc1.deref, camera[index1])
          fc2 = od2.frame_coordinate(gc2.deref, camera[index2])
          imgc1 = ImageCoordinate.new(fc1.line, fc1.sample)
          imgc2 = ImageCoordinate.new(fc2.line, fc2.sample) 
          # Finest image matching, not sure if this is useful or not
          if(false)
            t = match.match(od1.image(1), od1.image(2), imgc1, imgc2).first
            if(t)
              fc2 = FrameCoordinate.new(t.line, t.sample)
              conjpoint << [index1, fc1, od1, index2, fc2, od2]
            end
          else                  # Stop with 1 meter match
            conjpoint << [index1, fc1, od1, index2, fc2, od2]
          end
        end
      end
    end
    conjpoint
  end

  # This uses the current camera, orbit, etc. to generate the RPC for the given
  # row and camera index. It returns nil if we don't have data for the row and
  # index.
 
  def rpc(row,index)
    od = orbit[row,index]
    return nil unless(orbit_good(od))
    cam = camera[index]
    footprint = od.footprint(cam, dem)
    mi = ref_map_info.cover(footprint, border)
    ulc = mi.ground_coordinate(0,0)
    lrc = mi.ground_coordinate(mi.number_x_pixel - 1, mi.number_y_pixel - 1)
    lat_min = lrc.latitude
    lat_max = ulc.latitude
    lat_min, lat_max = lat_max, lat_min if(lat_min > lat_max)
    lon_min = ulc.longitude
    lon_max = lrc.longitude
    lon_min, lon_max = lon_max, lon_min if(lon_min > lon_max)
    h_min, h_max = approx_height_range(mi)
    Rpc.generate_rpc(od.deref, camera[index], lat_min, lat_max, lon_min, 
                     lon_max, h_min, h_max)
    
  end
  
  # Determine approximate height range for an area. We make this range a
  # multiple of an 500 meters.
  
  def approx_height_range(mi)
    hmin = nil
    hmax = nil
    0.step(mi.number_x_pixel, mi.number_x_pixel / 100.0) do |xind|
      0.step(mi.number_y_pixel, mi.number_y_pixel / 100.0) do |yind|
        h = dem.height_reference_surface(mi.ground_coordinate(xind, 
                                                              yind).deref)
        hmin = h if(!hmin || hmin > h)
        hmax = h if(!hmax || hmax < h)
      end
    end
    mid = ((hmin + hmax) / 2).round
    # 100 is pad here, so we leave some slop in case we are near the boundary.
    range = ((hmax - hmin + 100.0) / 500).ceil * 500
    [mid - range / 2, mid + range / 2]
  end

  # Use the results from rpc_run and orthorectify. This is just for testing,
  # normally AFIDS does this.
  def rpc_project(row, index, inp, out)
    VicarRasterImage.open(inp) do |vin|
      od = orbit[row,index]
      cam = camera[index]
      footprint = od.footprint(cam, dem)
      mi = ref_map_info.cover(footprint, border)
      ri = RpcImage.new(vin.to_ptr, vin.vicar_file.rpc, dem.to_ptr, mi)
      ri.copy_vicar(out)
    end
  end

  # Write all of the camera data to the given yaml file.
  def save_camera(fname)
    ArgusCamera.save_camera(fname, camera)
  end

  # Load camera data from the given yaml file
  def load_camera(fname)
    @camera = ArgusCamera.load_camera(fname)
  end
  
  # File name for high resolution image. We break this into multiple 
  # subdirectories to keep too many files from ending up in one 
  # directory.
  def high_resolution_name(row, index)
    "high_resolution/" + sprintf("%02d", row / 100) + "/" + "ortho_row" +
      sprintf("%04d", row) + "_cam" + sprintf("%02d", index) + ".tif"
  end
  
  # Full list of high resolution data to produce.
  def high_resolution_list
    res = []
    orbit.number_row.times do |r|
      1.upto(13) do |c|
        res << high_resolution_name(r, c) if(orbit_good(orbit[r,c]))
      end
    end
    res
  end

  # Generate a full resolution UTM or geodetic image for the given row 
  # and index.
  def create_high_resolution(row, index, fname = nil, format = "tifjpeg",
                             projection = "utm")
    od = orbit[row, index]
    if(orbit_good(od))
      # Determine UTM coordinates and resolution of center pixel. Use to
      # create MapInfo of output.
      fc = FrameCoordinate.new(camera[index].number_line(0) / 2,
                               camera[index].number_sample(0) / 2)
      mi = nil
      case projection
      when "utm"
        utm_coor = OgrCoordinate.to_utm(
         Geodetic.new(od.surface_intersect(camera[index], fc, dem).deref))
        psize = od.resolution_meter(camera[index])
        mi = MapInfo.new(OgrCoordinateConverter.new(utm_coor.ogr_ptr).to_ptr, 
                         utm_coor.x, utm_coor.y, utm_coor.x + psize, 
                         utm_coor.y - psize, 1, 1)
      when "geodetic"
        geod_coor1 = Geodetic.new(od.surface_intersect(camera[index], fc, 
                                                       dem).deref)
        fc.line += 1
        geod_coor2 = Geodetic.new(od.surface_intersect(camera[index], fc, 
                                                       dem).deref)
        fc.line -= 1
        fc.sample += 1
        geod_coor3 = Geodetic.new(od.surface_intersect(camera[index], fc, 
                                                       dem).deref)
        psize = [(geod_coor2.latitude - geod_coor1.latitude).abs,
                 (geod_coor3.latitude - geod_coor1.latitude).abs,
                 (geod_coor2.longitude - geod_coor1.longitude).abs,
                 (geod_coor3.longitude - geod_coor1.longitude).abs].max
        mi = MapInfo.new(GeodeticConverter.new.to_ptr, 
                         geod_coor1.longitude, geod_coor1.latitude, 
                         geod_coor1.longitude + psize, 
                         geod_coor1.latitude - psize, 1, 1)
      else
        raise "unrecognized projection #{projection}"
      end
      # Now save output.
      fname = high_resolution_name(row,index) unless(fname)
      od.save_ortho(mi, camera[index], dem, fname, format, border, 
                    grid_spacing)
    end
  end

  # Create an orthorectified image for a single row and camera. The
  # output file can be selected as "img", "tiff" or "vicar"

  def ortho_rect(row, index, fname=nil, type = "img")
    od = orbit[row, index]
    if(orbit_good(od))
      unless(fname)
        fname = File.basename(od.file_name, ".JPG") + "proj"
        fname = fname + ".img" if(type =="img")
        fname = fname + ".tif" if(type =="tif")
      end
      if(od.resolution_meter(camera[index]) > 1.7)
        # Nothing important about particular file used here, we just want
        # a file to get the 2.5 meter resolution projection
        mi = MosaicFile.fname_to_mapinfo("q_n12_00_00_w007_30_00.tif")
      else
        # Otherwise, use CIB 1 meter resolution
        mi = ref_map_info
      end
      od.save_ortho(mi, camera[index], dem, fname, type, border)
    else
      puts "No data for #{row} #{index}"
    end
  end

  # Find the first image for each camera, and print out the focal length
  # found in the metadata of the image file.
  def print_focal_length
    1.upto(13) { |index| puts "#{index}: #{orbit.focal_length(index)}" }
  end
  
  # Total number of good orbit data entries.
  def total_good_orbit_data
    cnt1 = 0
    cnt2 = 0
    orbit.number_row.times do |r|
      1.upto(13) do |c|
        cnt1 += 1 if(orbit_good(orbit[r,c]))
      end
      3.upto(10) do |c|
        cnt2 += 1 if(orbit_good(orbit[r,c]))
      end
    end
    [cnt1, cnt2]
  end
  
  # Test if orbit data is good. We check that it is present, and that
  # the attitude isn't a very large value. Large attitude values might
  # be real (the plane is banking) or might be bad data, but in any
  # case we aren't going to get very good data. We also require the
  # orbit data to have imagery (some CSV files don't have file names
  # for the orbit data).
  def orbit_good(od)
    return false unless(od)
    return false if(od.roll.abs > 10 || od.pitch.abs > 10)
    # We have the occasional fill value that is all 0's. This will filter
    # out data that happens to legitimately have attitude and lat/lon of
    # 0, but this doesn't seem real likely
    return false if(od.roll == 0.0 && od.pitch == 0.0 && od.heading == 0.0 &&
                    od.position_geodetic.latitude == 0.0 &&
                    od.position_geodetic.longitude == 0.0)
    return false unless(od.file_name =~ /\w/)
    true
  end

  # We have various bits of code that use the disk, which is a bit flaky (old NFS?),
  # so this tries a block up to 5 times, with a brief sleep in between. If it still
  # fails, we print an error message but otherwise continue
  def retry_or_fail_if_needed
    max_try = 5
    numretry = 0
    begin
      yield
    rescue Exception => e
      # Sometimes the disk is flaky, so if this fails retry up to 5 times
      # with a brief sleep in between.
      if(numretry < 5)
        numretry += 1
        sleep 10
        puts "Task failed. Retry #{numretry}."
        retry
      end
      puts "Failed with error " + e
      puts "Skipping to next"
    end
  end
  
  # Calibrate the camera by doing image matching with the reference
  # image for the given rows and index. We update our copy of the camera.
  #
  # For initial images, it is not uncommon for there to be a large
  # nearly static offset between the reference and new image. In that
  # case, you can pass in a fixed offset to use for the initial guess.

  def camera_calibrate(rowlist, index, lineoffset = 0, sampleoffset = 0, 
                       fact = 1)
    conjpoint = []
    rowlist.each do |row|
      od = orbit[row, index]
      if(orbit_good(od))
        mr = imagematch(row, index, lineoffset, sampleoffset, fact)
#      mr = picmatch(row, index)
        puts "For row #{row} found #{mr.size} points"
        mr.each do |f|
          f << od
          conjpoint << f
        end
      else
        puts "Skipping row #{row} camera #{index} because no orbit data"
      end
    end
    if(conjpoint.size < 10)
      puts "Too few points to fit camera"
      return false
    else
      @camera[index] = camera[index].fit_camera(conjpoint)
      camera[index].print_diff(conjpoint)
      return true
    end
  end

  # This is like picmatch, but I use my own matcher. 
  #
  # For initial images, it is not uncommon for there to be a large
  # nearly static offset between the reference and new image. In that
  # case, you can pass in a fixed offset to use for the initial guess.
  #
  # We also allow a fact to passed in. This allow coarser calibration
  # to be done, which can be useful when we have larger errors.

  def imagematch(row, index, lineoffset = 0, sampleoffset = 0, fact = 1)
    od = orbit[row, index]
    cam = camera[index]
    footprint = od.footprint(cam, dem)
    # For very oblique angles, the argus data may be coarser than the
    # reference image. In that case, average the reference image down.
    fact2 = (od.resolution_meter(cam) / ref_image.resolution_meter).round
    fact = fact2 if(fact2 > fact)
    if(fact > 1)
      ref2 = MemoryMapProjectedImage.new(MapProjectedAveraged.new(ref_image.cover(footprint, border), fact, fact))
    else
      ref2 = MemoryMapProjectedImage.new(ref_image.cover(footprint, border))
      fact = 1
    end
    odp = SharedPtr_OrbitData.to_ptr(od.deref)
    timg = OrbitMapProjected.new(ref2.map_info, odp, od.image(1).to_ptr, 
                                 cam.to_ptr, dem.to_ptr)
    oimg = MemoryMapProjectedImage.new(timg)
    fd = ForstnerFeatureDetector.new
    iclist = fd.interest_point_grid(ref2, CombinedMask.new, 20, 40)
# This works ok if Argus and reference image are similar, and if initial
# guess is close.    
    match = CcorrLsmMatcher.new
# Other matches to try if data is very different
#    match = CcorrMatcher.new(45, 45, 25, 25, 0.5)
#    match = CcorrMatcher.new(65, 65, 25, 25, 0.4)
    conjpoint = []
    iclist.each do |ic|
      ic2 = ImageCoordinate.new(ic.line + lineoffset / fact,
                                ic.sample + sampleoffset / fact)
      if(ic2.line > 0 && ic2.line < oimg.number_line &&
         ic2.sample > 0 && ic2.sample < oimg.number_sample)
        t = match.match(ref2, oimg, ic, ic2).first
        if(t)
          gc = ref2.ground_coordinate(ic, dem)
          gc2 = oimg.ground_coordinate(t, dem)
          fc = od.frame_coordinate(gc2.deref, cam)
          conjpoint << [fc, gc]
        end
      end
    end
    conjpoint
  end

  # This uses picmtch4 to match the reference image to the argus image,
  # and determines conjugate point. This is a list of FrameCoordinate and 
  # GroundCoordinate that the point is seen.
  def picmatch(row, index)
    od = orbit[row, index]
    cam = camera[index]
    footprint = od.footprint(cam, dem)
    ref2 = ref_image.cover(footprint, border)
    odp = SharedPtr_OrbitData.to_ptr(od.deref)
    oimg = OrbitMapProjected.new(ref2.map_info, odp, od.image(1).to_ptr, 
                                 cam.to_ptr, dem.to_ptr)
    ndiv = 10
    edge_offset = 50
    pnt = IbisFile.temp_file
    IbisFile.open(pnt, "w", ndiv * ndiv, ["DOUB"] * 11) do |f|
      i = 0
      edge_offset.step(ref2.number_line - edge_offset, 
                     (ref2.number_line - 2 * edge_offset) / (ndiv - 1)) do |ln|
        edge_offset.step(ref2.number_sample - edge_offset, 
                 (ref2.number_sample - 2 * edge_offset) / (ndiv - 1)) do |smp|
          if(i < ndiv * ndiv)
            f[i,0] = ln.round
            f[i,1] = smp.round
          end
          i += 1
        end
      end
    end
    ref2v = ref2.for_vicar
    oimgv = oimg.for_vicar
#    vicar_command("picmtch4 (#{vicname(ref2v)},#{vicname(oimgv)},#{vicname(pnt)}) SEARCH=32 fftsize=32 redo=0")
    vicar_command("picmtch4 (#{vicname(ref2v)},#{vicname(oimgv)},#{vicname(pnt)}) SEARCH=96 fftsize=32 redo=40")
    oimgv.close!
    ref2v.close!
    r = []
    IbisFile.open(pnt) do |f|
      ref_ic = IbisImageCoordinate.new(f, 2, 3)
      oimg_ic = IbisImageCoordinate.new(f, 5, 6)
      f.number_row.times do |i|
        if(f[i,8] > 0)          # This is the correlation, set to -9999 
                                # if point failed
          gc = ref2.ground_coordinate(ref_ic[i], dem)
          fc = od.frame_coordinate(oimg.ground_coordinate(oimg_ic[i],dem).deref,
                                   cam)
          r << [fc, gc]
        end
      end
    end
    pnt.close!
    r
  end
end
