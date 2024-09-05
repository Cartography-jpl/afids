#!/usr/bin/env ruby
#
# This file is gererated by ruby-glade-create-template 1.1.4.
#
require 'libglade2'

class GroundProcessingGlade
  include GetText

  attr :glade
  
  def initialize(path_or_data, root = nil, domain = nil, localedir = nil, flag = GladeXML::FILE)
    bindtextdomain(domain, localedir, nil, "UTF-8")
    @glade = GladeXML.new(path_or_data, root, domain, localedir, flag) {|handler| method(handler)}
    
  end
  
  def poi_file_selected(widget)
    puts "poi_file_selected() is not implemented yet."
  end
  def quick_copy_update(widget)
    puts "quick_copy_update() is not implemented yet."
  end
  def create_directory(widget)
    puts "create_directory() is not implemented yet."
  end
  def aflight_copy_selected(widget)
    puts "aflight_copy_selected() is not implemented yet."
  end
  def export_data(widget)
    puts "export_data() is not implemented yet."
  end
  def open_directory(widget)
    puts "open_directory() is not implemented yet."
  end
  def aflight_copy_cancel(widget)
    puts "aflight_copy_cancel() is not implemented yet."
  end
  def gui_to_config(widget)
    puts "gui_to_config() is not implemented yet."
  end
  def edit_debrief(widget)
    puts "edit_debrief() is not implemented yet."
  end
  def next_page(widget)
    puts "next_page() is not implemented yet."
  end
  def open_directory_select(widget)
    puts "open_directory_select() is not implemented yet."
  end
  def copy_poi_file(widget)
    puts "copy_poi_file() is not implemented yet."
  end
  def aflight_copy(widget)
    puts "aflight_copy() is not implemented yet."
  end
  def create_directory_select(widget)
    puts "create_directory_select() is not implemented yet."
  end
  def generate_quick_list(widget)
    puts "generate_quick_list() is not implemented yet."
  end
  def generate_image(widget)
    puts "generate_image() is not implemented yet."
  end
  def copy_card(widget)
    puts "copy_card() is not implemented yet."
  end
  def quit(widget)
    puts "quit() is not implemented yet."
  end
  def help_about(widget)
    puts "help_about() is not implemented yet."
  end
  def browse_file(widget)
    puts "browse_file() is not implemented yet."
  end
  def browse_image(widget)
    puts "browse_image() is not implemented yet."
  end
  def poi_file_cancel(widget)
    puts "poi_file_cancel() is not implemented yet."
  end
  def copy_update(widget)
    puts "copy_update() is not implemented yet."
  end
  def help_about_close(widget, arg0)
    puts "help_about_close() is not implemented yet."
  end
  def change_page(widget, arg0, arg1)
    puts "change_page() is not implemented yet."
  end
  def edit_poi_file(widget)
    puts "edit_poi_file() is not implemented yet."
  end
  def poi_update(widget)
    puts "poi_update() is not implemented yet."
  end
end

# Main program
if __FILE__ == $0
  # Set values as your own application. 
  PROG_PATH = "ground_processing.glade"
  PROG_NAME = "YOUR_APPLICATION_NAME"
  GroundProcessingGlade.new(PROG_PATH, nil, PROG_NAME)
  Gtk.main
end
