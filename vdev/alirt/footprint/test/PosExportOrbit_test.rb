require File.dirname(__FILE__) + "/TestClass.rb"

class PostExportOrbitTest < TestClass
  def test_start_of_week
    t = Time::parse("2010-05-21 09:40:12 UTC")
    assert_equal PosExportOrbit.start_of_week(t), 
    Time::parse("2010-05-16 00:00:15 UTC")
  end
  def test_read
    orb = PosExportOrbit.new(data_dir + "s251_sbet_subset.txt", 
                             Time::parse("2010-05-16 00:00:15 UTC"))
    od = orb.aircraft_orbit_data(Time::parse("2010-05-20 09:40:27 UTC"))
    assert distance(od.position_geodetic, 
                    Geodetic.new(35.97443327, -115.13749157, 711.513)) < 10
  end
end
