# This sets up the directory for doing JPL Processsing. This takes
# the base directory name.

basedir = ARGV[0]
system "mkdir -p #{basedir}/JplProcessed"
dirname = File.dirname(__FILE__)
system "cp #{dirname}/Makefile.template #{basedir}/JplProcessed/Makefile"
system "cp #{dirname}/jpl_processing_config.yml.template #{basedir}/JplProcessed/jpl_processing_config.yml"
system "cp #{dirname}/camera_calibration_config.yml.template #{basedir}/JplProcessed/camera_calibration_config.yml"
system "cp #{basedir}/Configuration/camera.yml #{basedir}/JplProcessed/camera_initial.yml"


