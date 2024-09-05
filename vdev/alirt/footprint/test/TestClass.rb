# Whatever common test routines we want.

require File.dirname(__FILE__) + "/../alirt"

class TestClass < Test::Unit::TestCase
  def default_test
  end
  def data_dir
    File.dirname(__FILE__) + "/data/"
  end
  def out_dir
    File.dirname(__FILE__) + "/test_out/"
  end
end
