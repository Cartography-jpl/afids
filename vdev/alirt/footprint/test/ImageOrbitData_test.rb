require File.dirname(__FILE__) + "/TestClass.rb"

class ImageOrbitTest < TestClass
  def test_image_time
    assert_equal ImageOrbitData.image_time(data_dir + 
        "20100521_EOS5DIIM_2401.JPG", -24 * 60 * 60),
    Time::parse("2010-05-20 09:40:27 UTC")
    
  end
  def test_save_data
    ImageOrbitData.save_data(out_dir + "image_orbit_data_test", 
                             data_dir + "s251_sbet_subset.txt",
                             data_dir, -24 * 60 * 60)
    iod = ImageOrbitData.new(out_dir + "image_orbit_data_test")
    assert_equal iod.time_offset, -24 * 60 * 60
    assert_equal iod.orbit_file_name, data_dir + "s251_sbet_subset.txt"
    assert_equal iod.camera_directory, data_dir
    assert_equal iod.image_to_orbit_data["20100521_EOS5DIIM_2401.JPG"].heading, 271.104
  end
end
