package provide TMASRV 1.0

## ***************************************************************************
## Simple support routines.
## ***************************************************************************
proc _GuiSelectWeatherOK {} {
  global gui_WeatherSelection _guiSelectWeatherState
  set gui_WeatherSelection $_guiSelectWeatherState(index)
  set _guiSelectWeatherState(done) 1
}

## ***************************************************************************
## Get the weather.
## ***************************************************************************
proc _GuiSelectWeatherApply {} {
  global gui_WeatherSelection _guiSelectWeatherState
  set gui_WeatherSelection $_guiSelectWeatherState(index)
}

## ***************************************************************************
## Get the weather.
## ***************************************************************************
proc _GuiSelectWeatherCancel {} {
  global gui_WeatherSelection _guiSelectWeatherState
  set gui_WeatherSelection $_guiSelectWeatherState(original)
  set _guiSelectWeatherState(done) 1
}

## ***************************************************************************
## Get the weather.
## ***************************************************************************
proc _GuiSelectWeatherHelp { w } {
  Gui_Dialog {$w.help "Help" "Not implemented" {} 0 OK}
}

## ***************************************************************************
## Get the weather.
## ***************************************************************************
proc _GuiSelectWeatherClick { lb y } {
global _guiSelectWeatherState
  set _guiSelectWeatherState(index) [$lb nearest $y]
}

## ***************************************************************************
## Get the weather.
## ***************************************************************************
proc _GuiSelectWeatherDoubleClick { lb y } {
global _guiSelectWeatherState
  set _guiSelectWeatherState(index) [$lb nearest $y]
  _GuiSelectWeatherOK
}

## ***************************************************************************
## Select a weather from the list.
## ***************************************************************************
proc Gui_SelectWeather {w title weatherList} {
  global gui_WeatherSelection
  global _guiSelectWeatherState

set _guiSelectWeatherState(original) $gui_WeatherSelection

# Create the top-level window and divide into top and bottom parts.

toplevel $w -class toplevel
wm title $w $title
wm minsize $w 15 10
wm iconname $w toplevel
frame $w.top -relief raised -bd 1
pack $w.top -side top -fill both -expand true
frame $w.bottom -relief raised -bd 1
pack $w.bottom -side bottom -fill x

# Fill the top part with a scrolled listbox.

set weatherListBox [Gui_VScrolledListBox $w.top.list -width 15 -height 9 \
			-selectmode single -setgrid true]
pack $w.top.list -side top -fill both -expand true

foreach weatherName $weatherList {
  $weatherListBox insert end $weatherName
  }

# Create a row of buttons at the bottom.

set buttonText {Ok Apply Cancel}
set buttonCmds {_GuiSelectWeatherOK _GuiSelectWeatherApply _GuiSelectWeatherCancel }
set default 0

set i 0
foreach callbackProc $buttonCmds {
  button $w.bottom.button$i -text [lindex $buttonText $i] \
		 -command $callbackProc
  if {$i == $default} {
    # Put a sunken frame around the button.
    frame $w.bottom.default -relief sunken -bd 1
    raise $w.bottom.button$i
    pack $w.bottom.default -side left -expand 1 -padx 3m -pady 2m
    pack $w.bottom.button$i -in $w.bottom.default \
	-side left -padx 2m -pady 2m -ipadx 2m -ipady 1m
    } else {
    pack $w.bottom.button$i -side left -expand 1 \
	-padx 3m -pady 3m -ipadx 2m -ipady 1m
    }
  incr i
  }

# Set up click and return bindings.
bind $weatherListBox <Button-1> {
  _GuiSelectWeatherClick %W %y
  }
bind $weatherListBox <Double-Button-1> {
  _GuiSelectWeatherDoubleClick %W %y
  }
bind $weatherListBox <Return> {
  $w.bottom.button$default flash; _GuiSelectWeatherOK
}

set oldFocus [focus]
focus $w

# Wait for the user to respond, then restore the focus.

tkwait variable _guiSelectWeatherState(done)
destroy $w
focus $oldFocus
}
