# Process a data to generate tile of data.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

config_file, navfile, camfile, footfile, output = ARGV

config = YAML::load(File.read(config_file))
t = Argus.new(navfile, camfile, footfile)

mi = MosaicFile.fname_to_mapinfo(File.basename(output))
mos = Mosaic.new(output, mi, t, config["camera_list"], "tif")
mos.process
