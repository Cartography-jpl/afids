# This determines the tiles needed to be processed to cover the data.
# We write this out in a format that can be used by a Makefile.

$: << File.expand_path(File.dirname(__FILE__))
require "argus"

config_file, navfile, camfile, footfile, output = ARGV

config = YAML::load(File.read(config_file))
t = Argus.new(navfile, camfile, footfile)

h = MosaicFile.tile_list(t, config["camera_list"])

File.open(output, "w") do |f|
  f << "tile_list = "
  f << (h.values.flatten.sort.collect {|v| "mosaic/" + v + ".tif"}).join(" \\\n     ")
  f << "\n\n"
  f << "one_deg_list = "
  f << (h.keys.sort.collect {|v| "mosaic/" + v}).join(" \\\n     ")
  f << " \\\n     "
  f << (h.keys.sort.collect {|v| "mosaic/" + v.sub("_r.vic", "_g.vic")}).join(" \\\n     ")
  f << " \\\n     "
  f << (h.keys.sort.collect {|v| "mosaic/" + v.sub("_r.vic", "_b.vic")}).join(" \\\n     ")
  f << "\n\n"
  h.keys.sort.each do |k|
    f << "mosaic/" << k << ": "
    f << (h[k].sort.collect {|v| "mosaic/" + v + ".tif"}).join(" \\\n     ")
    f << "\n\n"
    f << "mosaic/" << k.sub("_r.vic", "_g.vic") << ": "
    f << (h[k].sort.collect {|v| "mosaic/" + v + ".tif"}).join(" \\\n     ")
    f << "\n\n"
    f << "mosaic/" << k.sub("_r.vic", "_b.vic") << ": "
    f << (h[k].sort.collect {|v| "mosaic/" + v + ".tif"}).join(" \\\n     ")
    f << "\n\n"
  end
end

