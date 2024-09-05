# Short script to look through all the image files and get the times for
# them. We store the results in the given marshal file.

$: << File.expand_path(File.dirname(__FILE__))
require "lib/ImageTime"
STDOUT.sync = true              # Immediately print out, so we can show status
                                # as we run

# Read in configuration information.
config_file, tm_out = ARGV
config = YAML::load(File.read(config_file))

ImageTime.initial_creation(config["image_dirs"], tm_out)
