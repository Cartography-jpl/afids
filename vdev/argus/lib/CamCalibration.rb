require "rubygems"
require "gsl"
include GSL::MultiMin
require "xmlsimple"

class ArgusCamera
  # Fit a camera based on conjugate points. The conjugate point should be 
  # given as an array of FrameCoordinate, GroundCoordinate and OrbitData
  #
  # Note that this does *not* update this camera, instead it returns a new
  # ArgusCamera. We just don't happen to have routines in the C++ code to
  # update an existing camera, and it isn't worth adding them since a new 
  # camera is easy enough to work with.
  #
  # In addition to adjusting the yaw, pitch and roll we can either hold
  # the focal length fixed or adjust it also. I haven't decided if one way
  # is better than the other, so we just implement the ability to do both.
  def fit_camera(conjpoint, fit_focal_length = true)
    if(fit_focal_length)
      xval = [yaw, pitch, roll, focal_length]
    else
      xval = [yaw, pitch, roll]
    end
    # Function we are fitting
    cam_f = Proc.new do |v|
      if(fit_focal_length)
        cam =  ArgusCamera.new(v[0], v[1], v[2], v[3])
      else
        cam =  ArgusCamera.new(v[0], v[1], v[2], focal_length)
      end
      res = 0
      conjpoint.each do |p|
        fccalc = p[2].frame_coordinate(p[1].deref, cam)
        res += sqr(fccalc.line - p[0].line) + sqr(fccalc.sample - p[0].sample)
      end
      res
    end
    
    # Boiler plate code for minimizing. We may want to move this into its 
    # own utility class, but for now it isn't worth doing that.

    np = xval.size
    cam_gsl_f = Function.alloc(cam_f, np)
    x = GSL::Vector.alloc(*xval)
    ss = GSL::Vector.alloc(np)
    ss.set_all(1.0)
    minimizer = FMinimizer.alloc("nmsimplex", np)
    minimizer.set(cam_gsl_f, x, ss)
    iter = 0
    begin
      iter += 1
      status = minimizer.iterate()
      status = minimizer.test_size(1e-2)
      if status == GSL::SUCCESS
        puts("converged to minimum at")
      end
      x = minimizer.x
      printf("%5d ", iter);
      for i in 0...np do
        printf("%10.3e ", x[i])
      end
      printf("f() = %7.3f size = %.3f\n", minimizer.fval, minimizer.size);
    end while status == GSL::CONTINUE and iter < 1000
    if(fit_focal_length)
      ArgusCamera.new(x[0], x[1], x[2], x[3])
    else
      ArgusCamera.new(x[0], x[1], x[2], focal_length)
    end
  end

  # Print out the difference between conjugate points and the prediction
  # by the camera.
  def print_diff(conjpoint)
    conjpoint.each do |v|
      fccalc = v[2].frame_coordinate(v[1].deref, self)
      puts "#{v[0]} #{v[0].line - fccalc.line} #{v[0].sample - fccalc.sample}"
    end
    nil
  end
  
  # Save an array of 13 cameras to a yml file. By convention, we 
  # use 1 based index instead of 0 because that is what ARGUS does.
  def self.save_camera(fname, cam_arr)
    open(fname, "w") do |f|
      f << "# All of the camera data\n"
      1.upto(13) do |i|
        c = cam_arr[i]
        f << i << ":\n"
        f << "   yaw:          " << c.yaw << "\n"
        f << "   pitch:        " << c.pitch << "\n"
        f << "   roll:         " << c.roll << "\n"
        f << "   focal_length: " << c.focal_length << "\n"
        f << "\n"
      end
    end
  end
  
  # Load an array of 13 cameras from a yml file. By convention, we 
  # use 1 based index instead of 0 because that is what ARGUS does.
  def self.load_camera(fname)
    h = YAML::load(File.read(fname))
    res = [nil]
    1.upto(13) do |i|
      unless (h[i])
        raise "Problems reading camera file #{fname}, didn't find camera #{i}"
      end
      y,p,r,f = h[i]["yaw"].to_f, h[i]["pitch"].to_f, h[i]["roll"].to_f,
                h[i]["focal_length"].to_f
      unless(y && p && r && f)
        raise "Problems reading camera file #{fname}, didn't find yaw, pitch, roll, or focal length for camera #{i}"
      end
      res << ArgusCamera.new(y,p,r,f)
    end
    res
  end
  
  # Read in original CPM files delivered from Fireball. This was only
  # done in the original set up of the camera, we normally use
  # a YAML file as given by save_camera and load_camera.
  def self.fireball_argus_camera(num)
    fname = "/raid1/smyth/mali/common_data/CAM#{sprintf("%02d",num.to_i)}.cpm"
    cdata = XmlSimple.xml_in(File.open(fname), {'ForceArray' => false})
    roll = cdata["Angles"]["X"].to_f + cdata["BiasAngles"]["X"].to_f
    pitch = cdata["Angles"]["Y"].to_f + cdata["BiasAngles"]["Y"].to_f
    yaw = cdata["Angles"]["Z"].to_f + cdata["BiasAngles"]["Z"].to_f
    focal = cdata["FocalLength"].to_f * 1000
    ArgusCamera.new(yaw, pitch, roll, focal)
  end
  private
  def sqr(x)
    x * x
  end
end

class ArgusCameraSet
  attr_accessor :camera
  attr_reader :dem

  def save_camera(fname)
    ArgusCamera.save_camera(fname, camera)
  end
  
  def initialize(camset, d)
    @camera = camset
    @dem = d
  end
  
  # Fit a set of cameras based on conjugate points. The conjugate points 
  # are of two types - between two cameras or with a reference image. 
  # With the reference image this takes an array of Camera index,
  # FrameCoordinate, GroundCoordinate, OrbitData
  # For 2 cameras, this should be an array of Camera index 1, 
  # FrameCoordinate 1, OrbitData 1, Camera index 2, FrameCoordinate 2,
  # OrbitData 2.
  def fit_camera_set(cam_index_list, conjpoint)
    xval = []
    cam_index_list.each do |ind|
      xval << camera[ind].yaw
      xval << camera[ind].pitch
      xval << camera[ind].roll
      xval << camera[ind].focal_length
    end
    # Function we are fitting
    cam_f = Proc.new do |v|
      vind = 0
      cam_index_list.each do |ind|
        camera[ind] = ArgusCamera.new(v[vind + 0], v[vind + 1], v[vind + 2],
                                      v[vind + 3])
        vind += 4
      end
      res = 0
      conjpoint.each do |p|
        if(p.size == 4)
          fccalc = p[3].frame_coordinate(p[2].deref, camera[p[0]])
          res += sqr(fccalc.line - p[1].line) + sqr(fccalc.sample - 
                                                    p[1].sample)
        else
          fg = p[2].surface_intersect(camera[p[0]], p[1], dem)
          fccalc = p[5].frame_coordinate(fg.deref, camera[p[3]])
          res += sqr(fccalc.line - p[4].line) + sqr(fccalc.sample - 
                                                    p[4].sample)
        end
      end
      res
    end
    
    # Boiler plate code for minimizing. We may want to move this into its 
    # own utility class, but for now it isn't worth doing that.

    np = xval.size
    cam_gsl_f = Function.alloc(cam_f, np)
    x = GSL::Vector.alloc(*xval)
    ss = GSL::Vector.alloc(np)
    ss.set_all(1.0)
    minimizer = FMinimizer.alloc("nmsimplex", np)
    minimizer.set(cam_gsl_f, x, ss)
    iter = 0
    begin
      iter += 1
      status = minimizer.iterate()
      status = minimizer.test_size(1e-2)
      if status == GSL::SUCCESS
        puts("converged to minimum at")
      end
      x = minimizer.x
      printf("%5d ", iter);
      for i in 0...np do
        printf("%10.3e ", x[i])
      end
      printf("f() = %7.3f size = %.3f\n", minimizer.fval, minimizer.size);
    end while status == GSL::CONTINUE and iter < 1000
    xind = 0
    cam_index_list.each do |ind|
      camera[ind] = ArgusCamera.new(x[xind + 0], x[xind + 1], x[xind + 2],
                                    x[xind + 3])
      xind += 4
    end
  end
  def print_diff(cam_index_list, conjpoint)
    conjpoint.each do |p|
      if(p.size == 4)
        fccalc = p[3].frame_coordinate(p[2].deref, camera[p[0]])
        puts "Ground: #{p[1]} #{p[1].line - fccalc.line} #{p[1].sample - fccalc.sample}"
      else
        fg = p[2].surface_intersect(camera[p[0]], p[1], dem)
        fccalc = p[5].frame_coordinate(fg.deref, camera[p[3]])
        puts "Conj: #{p[4]} #{p[4].line - fccalc.line} #{p[4].sample - fccalc.sample}"
      end
    end
  end
  private
  def sqr(x)
    x * x
  end
end
