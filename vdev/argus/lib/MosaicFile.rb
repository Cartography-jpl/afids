# This does a mosaic for a given map area, storing the results in
# the given file.

class Mosaic
  attr_reader :fname, :mapinfo, :argus, :type, :camlist
  def initialize(fname, mapinfo, argus, camlist, type = "img")
    @fname, @mapinfo, @argus, @camlist, @type = 
      fname, mapinfo, argus, camlist, type
  end
  def process
    od = []
    argus.touch(mapinfo, camlist).each do |t|
      odv = argus.orbit[*t]
      if(Mosaic.will_process(argus, odv))
        od << odv
      end
    end
    cam = argus.camera[1..13]
    puts "Have #{od.size} JPEGS to process"
    ArgusOrbitData.mosaic(Vector_ArgusOrbitData.from_arr(od),
                          Vector_Camera.from_arr(cam),
                          argus.dem,
                          mapinfo, fname, type)
  end
  def Mosaic.will_process(argus, odv)
    # We have some short jpeg files. It appears to be bad data, so skip 
    # if we find this
    return argus.orbit_good(odv) && File.size(odv.file_name) > 6e6
  end
end
# This contains various bits of code related to generating mosaics.
# To standardize the process, we generate data in quads, or 7.5
# minute x 7.5 minute files. We have a standard naming convention for
# these files q45_07_30N_050_07_30W.ext is the file at 45 7.5' North latitude,
# 50 7.5' West longitude.

class MosaicFile
  @@geodetic_converter = GeodeticConverter.new 
  # Return the list of files needed to process a given ShapeFile layer.
  # We return this as a hash mapping to 1x1 degree tile, which is
  # used to generate 1 degree vicar mosaics of the 7.5 minutes quads
  def MosaicFile.tile_list(argus, camlist)
    footprint_layer = argus.footprint_layer
    cond = (camlist.collect {|c| "Camera=#{c}"}).join(" OR ")
    footprint_layer.set_attribute_filter(cond)
    lon_min, lon_max, lat_min, lat_max = footprint_layer.layer.get_extent

# Multiple by 8 (there are 8 7.5 minute divisions in a degree). We then
# work with these integer tile number    
    lon_min = (lon_min * 8).floor
    lon_max = (lon_max * 8).ceil
    lat_min = (lat_min * 8).floor
    lat_max = (lat_max * 8).ceil
    flist = []
    puts "Have #{(lon_max - lon_min) * (lat_max - lat_min)} to check"
    i = 0
    res = Hash.new {|h, k| h[k] = []}
    lon_min.upto(lon_max - 1) do |lonscaled|
      lat_min.upto(lat_max - 1) do |latscaled|
        puts "Doing #{i}" if(i % 10 ==0)
        i += 1
        footprint_layer.set_rect_filter(lonscaled / 8.0,
                                        latscaled / 8.0,
                                        (lonscaled + 1) / 8.0,
                                        (latscaled + 1) / 8.0)
        do_tile = footprint_layer.any? do |fl|
          odv = argus.orbit[fl[:Row], fl[:Camera]]
          Mosaic.will_process(argus, odv)
        end
        if(do_tile)
          lat = (latscaled / 8.0).floor
          lon = (lonscaled / 8.0).floor
          f = "mos_#{lat >= 0 ? "n" : "s"}#{sprintf("%02d",lat.abs)}_#{lon >= 0 ? "e" : "w"}#{sprintf("%03d",lon.abs)}_r.vic"
          gp = Geodetic.new((latscaled + 0.5) / 8.0, (lonscaled + 0.5) / 8.0)
          res[f] << loc_to_fname(gp)
        end
      end
    end
    footprint_layer.clear_filter
    res
  end
  # Find the file that contains a given location.
  def MosaicFile.loc_to_fname(gp)
    latname, lat_min, lat_max = lat_to_name(gp.latitude)
    lonname, lon_min, lon_max = lon_to_name(gp.longitude)
    fname = "q_" + latname + "_" + lonname
    fname
  end
  def MosaicFile.fname_to_mapinfo(fname)
    fname =~ /^q_(n|s)(\d\d)_(\d\d)_(\d\d)_(w|e)(\d\d\d)_(\d\d)_(\d\d)/
    lat_min = ($1 == "n" ? 1 : -1) * ($2.to_f + ($3.to_f + $4.to_f / 60.0) / 60.0)
    lon_min = ($5 == "e" ? 1 : -1) * ($6.to_f + ($7.to_f + $8.to_f / 60.0) / 60.0)
    lat_max = lat_min + 7.5 / 60
    lon_max = lon_min + 7.5 / 60
    MapInfo.new(@@geodetic_converter.to_ptr, lon_min, lat_max, lon_max,
                lat_min, 5400, 5400)    
  end
  private
  def MosaicFile.lat_to_name(lat)
    deg, min = to_deg_min(lat)
    if(lat > 0)
      m = (min / 7.5).floor * 7.5
    else
      m = (min / 7.5).ceil * 7.5
      if(m ==60)
         deg = deg - 1
         m = 0
      end
    end
    r = ""
    if(lat >= 0)
      r << "n"
    else
      r << "s"
    end
    r << sprintf("%02d", deg.abs) << "_"
    r << sprintf("%02d", m.floor) << "_"
    if(m - m.floor > 0.4)
      r << "30"
    else
      r << "00"
    end
    if(lat > 0)
      lat_min = deg + m / 60.0
    else
      lat_min = -(deg.abs + m / 60.0)
    end
    lat_max = lat_min + (7.5 / 60.0)
    [r, lat_min, lat_max]
  end
  def MosaicFile.lon_to_name(lon)
    deg, min = to_deg_min(lon)
    if(lon > 0)
      m = (min / 7.5).floor * 7.5
    else
      m = (min / 7.5).ceil * 7.5
      if(m ==60)
         deg = deg - 1
         m = 0
      end
    end
    r = ""
    if(lon >= 0)
      r << "e"
    else
      r << "w"
    end
    r << sprintf("%03d", deg.abs) << "_"
    r << sprintf("%02d", m.floor) << "_"
    if(m - m.floor > 0.4)
      r << "30"
    else
      r << "00"
    end
    if(lon > 0)
      lon_min = deg + m / 60.0
    else
      lon_min = -(deg.abs + m / 60.0)
    end
    lon_max = lon_min + (7.5 / 60.0)
    [r, lon_min, lon_max]
  end
  # Convert value to integer degree and fractional minutes.
  def MosaicFile.to_deg_min(v)
    deg = (v > 0 ? v.floor : v.ceil)
    min = (v - deg).abs * 60.0
    [deg, min]
  end
end
