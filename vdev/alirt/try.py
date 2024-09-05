from datetime import *
from geocal import *
from subprocess import *
import re

# Note that we need to have the epoch that the Applanix time is relative to. 
# The Applanix can be configured in different modes, but normally the times
# reported are the "GPS seconds of the week", which is just the seconds from
# Sunday 0:00:00 UTC (i.e., just between Saturday and Sunday). As an aid, 
# there is the routine PosExportOrbit.start_of_week that converts a time
# somewhere in the week to the starting time. So, for example, you can get
# the time for one of the acquired images and use it to determine the start
# of the week.
#
# Note that the GPS time does not include leapseconds, while UTC time does. 
# The difference between these two times is 15 seconds as of 2009. In practice 
# this doesn't matter much, because we are really just using time here as
# an index number to look up the Applanix data. But we'll go ahead and
# include this to avoid confusion at some point in the future when this 
# might matter. We hardwire the number of leapseconds.
#
# For right now, the conversion from python time to GeoCal time doesn't fully
# work. We'll sort that out later, but for now we'll use SPICE for doing the
# conversion.
def start_of_week(tm):
    y = tm.year
    m = tm.month
    d = tm.day - int(tm.strftime("%w"))
    return Time.parse_time("%d-%02d-%02dT00:00:%02dZ" %(y,m,d,15))

# The camera time is nominally synchronized to the Applanix GPS time, but
# we allow there to be an offset if needed to correct the image time stamp.
def time_stamp(f):
    t = Popen(["exiv2", f], stdout=PIPE).communicate()[0]
    t = re.search('Image timestamp : (.*)', t).group(1)
    tm = datetime.strptime(t, "%Y:%m:%d %H:%M:%S")
    # The clock on the camera appears to be set for the wrong day. As far
    # as I can tell, it is exactly 1 day off. We'll need to sort this out for
    # future data.
    tm = tm - timedelta(days = 1)
    return Time.parse_time("%d-%02d-%02dT%02d:%02d:%02dZ" %(tm.year,tm.month,tm.day,tm.hour,tm.minute,tm.second))

t = start_of_week(datetime.strptime("2010-05-19T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"))
#p = PosExportOrbit("/Users/smyth/Desktop/Las Vegas/s250_sbet.txt", t)
#f = "/Users/smyth/Desktop/Las Vegas/NTS_2010_0520_Las_Vegas/2010_05_21/20100521_EOS5DIIM_0454.JPG"
p = PosExportOrbit("/Users/smyth/Desktop/Las Vegas/s251_sbet.txt", t)
f = "/Users/smyth/Desktop/Las Vegas/NTS_2010_0520_Las_Vegas/2010_05_21/20100521_EOS5DIIM_2403.JPG"
tm = time_stamp(f)

# We'll probably want to clean up the underlying C++, which hasn't been
# updated for the new GdalMultiBand. But for now, do the same things we
# did for Argus.
img = GdalRasterImage.read_all(f)
od = p.orbit_data(tm)
# Not sure if we will use CIB, but start with this for getting 1m map info
ref_img = VicarMultiFile("/usr/local/afids/data/cib1/cib01_db.int", "/usr/local/afids/data/cib1", ".img", 1000)
ref_map_info = ref_img.map_info()
border = 10
datum = DatumGeoid96("/usr/local/afids/data/vdev/EGM96_20_x100.HLF")
dem = SrtmDem("/usr/local/afids/data/srtmL2_filled/L2_dem_db.int", "/usr/local/afids/data/srtmL2_filled", True, datum)
yaw = 0
pitch = 20
roll = 0
focal_length = 85
cam = ArgusCamera(yaw, pitch, roll, focal_length)
misub = ref_map_info.cover(od.footprint(cam, dem), border)
grid_spacing = 10
mp = OrbitMapProjected(misub, od, img, cam, dem)
mres = Vector_RasterImage()
mres.push_back(MemoryRasterImage(misub))
mres.push_back(MemoryRasterImage(misub))
mres.push_back(MemoryRasterImage(misub))
mp.write_multiple(mres,grid_spacing)
GdalRasterImage.save("temp.tif", "GTiff", mres[0], mres[1], mres[2],
                     GdalRasterImage.Byte,
                     "PHOTOMETRIC=RGB TILED=YES COMPRESS=JPEG", True);
                     
