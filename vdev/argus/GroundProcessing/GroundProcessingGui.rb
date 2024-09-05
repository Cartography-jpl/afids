# Because GroundProcessingGlade is automatically generated, it is overwritten
# each time we update the glade file. So we derive from this class and 
# override various hook routines here.

$: << File.expand_path(File.dirname(__FILE__))
require "GroundProcessingGlade"
require "set"
require "csv"

class GroundProcessingGui < GroundProcessingGlade
# Use newer version on devilrat.
  @@spreadsheet = "openoffice.org-2.0" 
#  @@spreadsheet = "/usr/bin/openoffice" 
#  @@editor = "gedit"
  # Andrew prefers using openoffice to gedit.
  @@editor = "openoffice.org-2.0 -writer" 
  @@base_data_dir = "/home/smyth"
  def initialize
    super(File.dirname(__FILE__) + "/ground_processing.glade", nil, 
          "ground_processing")
    @help_about_widget = @glade.get_widget("help_about")
    @poi_copy_widget = @glade.get_widget("poi_file_copy")
    @top_widget = @glade.get_widget("top_window")
    @create_dir_widget = @glade.get_widget("create_dir")
    @open_dir_widget = @glade.get_widget("open_dir")
    @aflight_copy_widget = @glade.get_widget("aflight_copy")
    @pagename = []
    @quick_copy_list_buffer = 
      @glade.get_widget("quick_copy_list_text").buffer
    @aflight_buffer = @glade.get_widget("aflight_text").buffer
    @poi_buffer = @glade.get_widget("poi_text").buffer
    @quick_copy_list_widget = @glade.get_widget("quick_copy_list")
    @quick_copy_widget = @glade.get_widget("quick_copy")
    @quick_copy_buffer = @glade.get_widget("quick_copy_text").buffer
    @generate_image_widget = @glade.get_widget("generate_image")
    @generate_image_buffer = @glade.get_widget("generate_image_text").buffer
    @quick_copy_cam_buffer = []
    1.upto(13) do |i|
      @quick_copy_cam_buffer[i] = 
        @glade.get_widget("quick_copy_cam_#{i}").buffer
    end
    @copy_cam_buffer = []
    1.upto(13) do |i|
      @copy_cam_buffer[i] = 
        @glade.get_widget("copy_cam_#{i}").buffer
    end
    @generate_image_widget = @glade.get_widget("generate_image")
    @tabset_widget = @glade.get_widget("tabset")
    @tabset_widget.children.each do |wid|
      @pagename << wid.name
    end
    @nitf_check = @glade.get_widget("nitf_check")
    @nitf_fr_check = @glade.get_widget("nitf_full_res_check")
    @nitf_1m_check = @glade.get_widget("nitf_1m_check")
    @nitf_2_5m_check = @glade.get_widget("nitf_2_5m_check")
    @kmz_check = @glade.get_widget("kmz_check")
    @geotiff_check = @glade.get_widget("geotiff_check")
    @geotiff_1m_check = @glade.get_widget("geotiff_1m_check")
    @geotiff_2_5m_check = @glade.get_widget("geotiff_2_5m_check")
    dir_default = File.expand_path("~/Argus_Data")
    system "mkdir -p \"#{dir_default}\""
    @create_dir_widget.current_folder = dir_default
    @open_dir_widget.current_folder = dir_default
  end
  def data_directory
    @data_directory
  end
  def data_directory=(v)
    @data_directory = v
    @top_widget.title = "#{File.basename(v)} - ARGUS Ground Processing"
    config_to_gui
  end
  # Return the page number for a particular tab. We do this to avoid
  # hardwiring this
  def pagenum(tabname)
    @pagename.index(tabname)
  end
  def quit
    Gtk.main_quit
  end
  def help_about
    @help_about_widget.show
  end
  def help_about_close
    @help_about_widget.hide
  end
  def next_page
    @tabset_widget.next_page
  end
  def edit_poi_file
    system("#{@@spreadsheet} \"#{poi_file}\" &")
  end
  def copy_poi_file
    @poi_copy_widget.show
  end
  def poi_file_cancel(widget)
    @poi_copy_widget.hide
  end
  def poi_file_selected(widget)
    @poi_copy_widget.hide
    fname = @poi_copy_widget.filename
    system "cp \"#{fname}\" \"#{poi_file}\""
    poi_update
  end
  def poi_file
    "#{data_directory}/Configuration/poi.csv"
  end
  def poi_data
    poi = Hash.new
    CSV.open(poi_file,"r") do |row|
      name, lon, lat = row
      if(lon && lat && lon =~ /\A[+-]?\d+?(\.\d+)?\Z/ &&
         lat =~ /\A[+-]?\d+?(\.\d+)?\Z/ && name && name =~ /\w/ &&
         !poi[name])
        poi[name] = [lon.to_f, lat.to_f]
      end
    end
    poi.keys.sort.collect {|k| [k, poi[k][0], poi[k][1]]}
  end
  def aflight_dir
    "#{data_directory}/AFlight"
  end
  def ground_processed_dir
    "#{data_directory}/GroundProcessed"
  end
  def create_directory_select(widget)
    fname = @create_dir_widget.filename
    return unless(fname)        # Handle case of button pushed but nothing
                                # selected.
    if(File.exists?(fname))
      dialog = Gtk::MessageDialog.new(@initialdir_widget,
                                      Gtk::Dialog::DESTROY_WITH_PARENT,
                                      Gtk::MessageDialog::ERROR,
                                      Gtk::MessageDialog::BUTTONS_OK,
             "Directory already exists. Select a different directory")
      dialog.run
      dialog.destroy
      return
    end
    unless(system "mkdir \"#{fname}\" && cd \"#{fname}\" && tar -xzf \"#{directory_template_tar}\" && exit 0; exit 1")
      dialog = Gtk::MessageDialog.new(@initialdir_widget,
                                      Gtk::Dialog::DESTROY_WITH_PARENT,
                                      Gtk::MessageDialog::ERROR,
                                      Gtk::MessageDialog::BUTTONS_OK,
             "Error creating and populating directory. Check file system permissions")
      dialog.run
      dialog.destroy
      return
    end
    self.data_directory = fname
    unless(system "cp \"#{@@base_data_dir}/CameraFiles/camera.yml\" \"#{data_directory}/Configuration\"")
      dialog = Gtk::MessageDialog.new(@initialdir_widget,
                                      Gtk::Dialog::DESTROY_WITH_PARENT,
                                      Gtk::MessageDialog::ERROR,
                                      Gtk::MessageDialog::BUTTONS_OK,
             "Error copying camera calibration file. Make sure #{@@base_data_dir}/CameraFiles/camera.yml exists and is readable.")
      dialog.run
      dialog.destroy
      return
    end
    @create_dir_widget.hide
    @tabset_widget.page = 0
    @top_widget.show
  end
  def open_directory(widget)
    @create_dir_widget.hide
    @open_dir_widget.show
  end
  def create_directory(widget)
    @open_dir_widget.hide
    @create_dir_widget.show
  end
  def open_directory_select(widget)
    self.data_directory = @open_dir_widget.filename
    # Need to add checks to make sure directory is valid
    @open_dir_widget.hide
    @tabset_widget.page = 0
    @top_widget.show
  end
  def not_implemented
    dialog = Gtk::MessageDialog.new(@top_widget,
                                    Gtk::Dialog::DESTROY_WITH_PARENT,
                                    Gtk::MessageDialog::ERROR,
                                    Gtk::MessageDialog::BUTTONS_OK,
                                    "Not Implemented yet")
    dialog.run
    dialog.destroy
  end
  def directory_template_tar
    File.expand_path(File.dirname(__FILE__)) + "/DirectoryTemplate.tar.gz"
  end
  def edit_debrief
    system "#{@@editor} \"#{data_directory}/Debrief/debrief.odt\" &"
  end
  def aflight_copy
    @aflight_copy_widget.show
  end
  def aflight_copy_cancel(widget)
    @aflight_copy_widget.hide
  end
  def aflight_copy_selected(widget)
    @aflight_copy_widget.filenames.each do |fname|
      system "cp -f \"#{fname}\" \"#{aflight_dir}\""
    end
    @aflight_copy_widget.hide
    aflight_update
  end
  # Current displayed page has changed. If the page contains dynamic 
  # information, we update this.
  def change_page(widget,page,pagen)
    case pagen
    when pagenum('aflight')
      aflight_update
    when pagenum('poi')
      poi_update
    when pagenum('quick_copy_list')
      quick_copy_list_update
    when pagenum('quick_copy')
      quick_copy_update
    when pagenum('generate_image')
      generate_image_update
    when pagenum('copy')
      copy_update
    end
  end
  def aflight_update
    text = <<END
The "aflight" files are created by  the Fireball software running on the flight computer. These are generated during flight. We need to copy these into our local directory.  

It is not uncommon for there to be more than one aflight file for a given flight (e.g., the data acquisition was interrupted, or mutliple runs were done). You should copy the full set of aflight files.

List of Aflight files:

END
    text << (Dir.glob("#{aflight_dir}/*.aflight").collect {|i| File.basename(i)}).join("\n")
    @aflight_buffer.text = text
  end
  def config_file
    "#{data_directory}/Configuration/quick_process_config.yml"
  end
  def config_to_gui
    config = YAML::load(File.read(config_file))
    @nitf_check.active = config["generate_files"].include?("nitf")
    @nitf_fr_check.active = 
      config["nitf_resolution"].include?("full_resolution")
    @nitf_1m_check.active = config["nitf_resolution"].include?("1m")
    @nitf_2_5m_check.active = config["nitf_resolution"].include?("2.5m")
    @kmz_check.active = config["generate_files"].include?("kmz")
    @geotiff_check.active = config["generate_files"].include?("geotiff")
    @geotiff_1m_check.active = config["tiff_resolution"].include?("1m")
    @geotiff_2_5m_check.active = config["tiff_resolution"].include?("2.5m")
    if(@geotiff_check.active?)
      @geotiff_1m_check.sensitive = true
      @geotiff_2_5m_check.sensitive = true
    else
      @geotiff_1m_check.sensitive = false
      @geotiff_2_5m_check.sensitive = false
    end
    if(@nitf_check.active?)
      @nitf_fr_check.sensitive = true
      @nitf_1m_check.sensitive = true
      @nitf_2_5m_check.sensitive = true
    else
      @nitf_fr_check.sensitive = false
      @nitf_1m_check.sensitive = false
      @nitf_2_5m_check.sensitive = false
    end
  end
  def gui_to_config
    config = Hash.new
    config["generate_files"] = Array.new
    config["tiff_resolution"] = Array.new
    config["nitf_resolution"] = Array.new
    config["generate_files"] << "nitf" if(@nitf_check.active?)
    config["generate_files"] << "kmz" if(@kmz_check.active?)
    config["generate_files"] << "geotiff" if(@geotiff_check.active?)
    config["tiff_resolution"] << "1m" if(@geotiff_1m_check.active?)
    config["tiff_resolution"] << "2.5m" if(@geotiff_2_5m_check.active?)
    config["nitf_resolution"] << "full_resolution" if(@nitf_fr_check.active?)
    config["nitf_resolution"] << "1m" if(@nitf_1m_check.active?)
    config["nitf_resolution"] << "2.5m" if(@nitf_2_5m_check.active?)
    if(@geotiff_check.active?)
      @geotiff_1m_check.sensitive = true
      @geotiff_2_5m_check.sensitive = true
    else
      @geotiff_1m_check.sensitive = false
      @geotiff_2_5m_check.sensitive = false
    end
    if(@nitf_check.active?)
      @nitf_fr_check.sensitive = true
      @nitf_1m_check.sensitive = true
      @nitf_2_5m_check.sensitive = true
    else
      @nitf_fr_check.sensitive = false
      @nitf_1m_check.sensitive = false
      @nitf_2_5m_check.sensitive = false
    end
    File.open(config_file, 'w') do |f|
      YAML.dump(config, f)
    end
  end
  def poi_update
    text = <<END
To run, we need to have a list of points of interest (POIs). This is supplied by a CSV (comma separated value) file. This can be generated by a number of standard tools, including a spreadsheet (just select CSV as the format that you store to).

The data should be ordered as Point Name, Longitude, Latitude. The name can be any text, but needs to be unique in the file. The Longitude and Latitude should be in decimal degrees, with negative values for west longitude or south latitude. Top row and columns past the third are ignored, so you can use this for labels or comments if desired.

We need to copy a POI file generated by the customer and/or edit an existing POI file. If you edit the file, make sure to save it before proceeding to the next step.

List of POIs:

END
    poi_data.each do |t|
      text << "#{t[0]}: #{t[1]}, #{t[2]}\n"
    end
    @poi_buffer.text = text
  end
  def have_aflight_file?
    t = Dir.glob("#{aflight_dir}/*.aflight")
    return t && t.size >0
  end
  def have_quick_list_file?
    File.exists?("#{ground_processed_dir}/quick_copy_cam13.txt")
  end
  def quick_copy_list_update
    unless(have_aflight_file?)
      @quick_copy_list_buffer.text = "Need to copy aflight files before generating a quick copy list"
      @quick_copy_list_widget.sensitive = false
      return
    end
    unless(have_quick_list_file?)
      @quick_copy_list_buffer.text = <<END
We need to create a list of files to copy over for the to cover the POIs. Press button to generate the quick file list. 

This also generates both a shapefile and a KML file for the POIs and footprint of the run coverage. You can view the KML in google earth if desired. These files are generated in #{data_directory}/POI and #{data_directory}/GroundProcessed.
END
      @quick_copy_list_widget.sensitive = true
      return
    end
    text = <<END
We have already generated the quick list files. We have also generated both a shapefile and a KML file for the POIs and footprint of the run coverage. You can view the KML in google earth if desired. These files are generated in #{data_directory}/POI and #{data_directory}/GroundProcessed.

You can regenerate the quick list if needed (e.g., you have updated the POI list).

The file counts for each camera:

END
    1.upto(13) do |i|
      text << "#{i} - #{quick_copy_list_count(i)}\n"
    end
    @quick_copy_list_buffer.text = text
    @quick_copy_list_widget.sensitive = true
  end
  # File name for quick copy list
  def quick_copy_list_file(index)
    "#{ground_processed_dir}/quick_copy_cam#{sprintf("%02d", index)}.txt"
  end
  # Return the count of quick copy items for the given camera index.
  def quick_copy_list_count(index)
    return 0 unless(have_quick_list_file?)
    cnt = `wc -w \"#{quick_copy_list_file(index)}\"`
    cnt.split.first
  end
  def poi_dir
    data_directory + "/POI"
  end
  def image_dir(index)
    data_directory + "/Image/Cam" + sprintf("%02d", index)
  end
  # Return the number of images for the given camera. If "quick_list" is
  # true, then return the number intersected with the quick_copy_list.
  def image_count(index, quick_list = false)
    unless(quick_list)
      return Dir.glob(image_dir(index) + "/*.JPG").size
    end
    t = Dir.entries(image_dir(index))
    t2 = Set.new(File.read(quick_copy_list_file(index)).split)
    t2.intersection(t).size
  end
  def quick_copy_update
    unless(have_quick_list_file?)
      @quick_copy_buffer.text = "You need to generate the quick copy list before doing a quick copy."
      @quick_copy_widget.sensitive = false
      return
    end
    @quick_copy_buffer.text = <<END
We need to copy data from the camera cards. You can select the cameras to copy, and start the copying. Note if you have more than one card loaded you can start multiple copy jobs at once.

This only copies the files needed to generate the POIs, you'll copy the remaining data later (in the "Copy All" step).

There is no requirement that you actually copy data from all the cameras. If the customer is only interested in a subset of the cameras (e.g., 6 and 7), we can just copy and process for those cameras. The copy is done using rsync so that it is restartable.
END
    @quick_copy_widget.sensitive = true
    1.upto(13) do |i|
      @quick_copy_cam_buffer[i].text = 
        "#{image_count(i,  true)} out of #{quick_copy_list_count(i)} copied"
    end
  end
  def generate_image_update
    unless(have_quick_list_file?)
      @generate_image_buffer.text = "You need to generate the quick copy list before doing generating images."
      @generate_image_widget.sensitive = false
      return
    end
    @generate_image_buffer.text = <<END
We can generate map projected images. This works with whatever data has been copied over by the "Quick Copy" step, it is ok if some of the cameras haven't been copied over or if some of the images are missing. This only generates images that haven't been already created, so you can rerun the generate image command after copying more data over without reprocessing all the existing data.

Note that in general this will take a while to run, and will load the system. We use all 4 processors, starting multiple jobs.

You can choose to browse images or files that have been generated. Browsing images is useful for viewing the GeoTIFF files, and browsing files is useful to looking at the NITF and Google Earth KMZ files.
END
    @generate_image_widget.sensitive = true
  end
  def copy_update
    1.upto(13) do |i|
      @copy_cam_buffer[i].text = 
        "#{image_count(i,  false)} copied"
    end
  end
  def generate_quick_list
    system "xterm  -title 'Generate Quick Copy List' -e \"#{File.expand_path(File.dirname(__FILE__))}/quick_copy_list.script\" \"#{data_directory}\""
    quick_copy_list_update
  end
  def copy_card(widget)
    widget.name =~ /(quick_)?copy_(\d+)/
    index = $2.to_i
    quick_copy = ($1 ? true : false)
    dialog = Gtk::FileChooserDialog.new("Copy From Card",
                       @top_widget,
                       Gtk::FileChooser::ACTION_SELECT_FOLDER,
                       nil,
                       [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL],
                       [Gtk::Stock::COPY, Gtk::Dialog::RESPONSE_ACCEPT])
    dialog.filename = camera_card_dir(index)
    do_copy = (dialog.run == Gtk::Dialog::RESPONSE_ACCEPT)
    fname = dialog.filename
    dialog.destroy
    return unless(do_copy)
    if(quick_copy)
      system "xterm -title 'Quick Copy Camera #{index}' -e \"#{File.expand_path(File.dirname(__FILE__))}/quick_rsync.script\" \"#{quick_copy_list_file(index)}\" \"#{fname}\" \"#{image_dir(index)}\" &"
    else
      system "xterm -title 'Full Copy Camera #{index}' -e \"#{File.expand_path(File.dirname(__FILE__))}/rsync.script\" \"#{fname}\" \"#{image_dir(index)}\" &"
    end
  end
  def camera_card_dir(index)
    "#{@@base_data_dir}/CameraCard/Cam#{sprintf("%02d",index)}"
  end
  def generate_image(widget)
    num_job = 4                    # Hardcoded for now
    system "xterm -title 'Generate Images' -e \"#{File.expand_path(File.dirname(__FILE__))}/generate_image.script\" \"#{ground_processed_dir}\" #{num_job}"
  end
  def browse_image(widget)
    system "gthumb \"#{poi_dir}\" &"
  end
  def browse_file(widget)
    system "konqueror \"#{poi_dir}\" &"
  end
  def export_data(widget)
    dialog = Gtk::FileChooserDialog.new("Export data to",
                       @top_widget,
                       Gtk::FileChooser::ACTION_SELECT_FOLDER,
                       nil,
                       [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL],
                       [Gtk::Stock::COPY, Gtk::Dialog::RESPONSE_ACCEPT])
    dialog.filename = @@base_data_dir
    do_copy = (dialog.run == Gtk::Dialog::RESPONSE_ACCEPT)
    fname = dialog.filename
    dialog.destroy
    return unless(do_copy)
    system "xterm -title 'Export data' -e \"#{File.expand_path(File.dirname(__FILE__))}/rsync2.script\" \"#{data_directory}\" \"#{fname}\""
  end
end

if __FILE__ == $0
  GroundProcessingGui.new
  Gtk.main
end
