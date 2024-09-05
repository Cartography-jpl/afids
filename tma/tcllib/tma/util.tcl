package provide TMASRV 1.0

proc string2Coord {string} {
    if {[regexp {^([N|S])([0-9]?[0-9]) ([0-9]?[0-9])' ([0-9]?[0-9])'' ([W|E])([0-9]?[0-9]?[0-9]) ([0-9]?[0-9])' ([0-9]?[0-9])''$} ${string} match \
        nsprefix latdegrees latminutes latseconds \
        ewprefix londegrees lonminutes lonseconds] == 0} {
        return -1
    }

#puts "$latdegrees $latminutes $latseconds"
#puts "$londegrees $lonminutes $lonseconds"

    set lat [expr $latdegrees + $latminutes / 60.0 + $latseconds / 3600.0]
    if {$nsprefix == "S"} {
        set lat [expr -1.0 * $lat]
    }
    set lon [expr $londegrees + $lonminutes / 60.0 + $lonseconds / 3600.0]
    if {$ewprefix == "W"} {
        set lon [expr -1.0 * $lon]
    }

    list $lat $lon
}

# returns rnf file name
proc findOrComputeAoiData {} {
    global aoi
    global vectorDataSetType
    global env

    set aoiId [lindex $aoi 4]

    # find existing rnf file name
    set results [Data_Select tma_aoiData {RNF_FILE} [list [list AOI_ID $aoiId] [list DATA_SET $vectorDataSetType]]]
    if {[llength $results] > 0} {
	return [lindex $results 0]
    }

    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set lines [lindex $aoi 2]
    set samples [lindex $aoi 3]

    set n [lindex $nw 0]
    set w [lindex $nw 1]
    set s [lindex $se 0]
    set e [lindex $se 1]

    set mapRegionName [getMapHandle]
    puts "mapRegionName = $mapRegionName"
    set region [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "region = $region"

    set rnfFile ${region}_${aoiId}_${vectorDataSetType}.rnf

    # create rnf file
    # usage: rnf TVFroads TVFrivers RIF RNF x0 y0 x1 y1 width height regionName
    foreach themeType {Roads Rivers Intersection} {
  	set sel_column {TEXT_FILENAME}
  	set criteria [list [list REGION_NAME $region] [list THEME_TYPE $themeType]]
	set $themeType [lindex [lindex [Data_Select tma_regionThemes $sel_column $criteria] 0] 0]
    }
    set cmd "exec -- rnf $env(TMA_DATA)/${Roads} $env(TMA_DATA)/${Rivers} $env(TMA_DATA)/${Intersection} $env(TMA_DATA)/$rnfFile $w $s $e $n $samples $lines $region"

    # log the command before executing
    if [file exists /tmp/tmacmd.log] {
  	set file [open /tmp/tmacmd.log a]
    } else {
  	set file [open /tmp/tmacmd.log w]
    }
    puts $file $cmd
    close $file

    puts $cmd

    # execute the rnf command
    catch {eval $cmd}

    # update tma_aoiData
    Data_Insert tma_aoiData [list [list AOI_ID $aoiId] [list DATA_SET $vectorDataSetType] [list RNF_FILE $rnfFile]]

    return $rnfFile
}

proc PickStringFromMap {} {
  return [Map_CoordToString [Map_PickCoord [getMapHandle]]]
}

## ******************************************************************************
#  Creates a dialog box with message and buttons.
#  Displays message and waits for user to select one of buttons,
#  returns the number of the button (counting from zero).
#  The arguments are :
#  1. name of dialog's top-level window,
#  2. title of the window,
#  3. text to display,
#  4. name of biguip to display ({} for no biguip)
#  5. the index of the default button (-1 for no default),
#	<Return> selects the default.
#  6 and additional. Strings for the buttons.
## ******************************************************************************
proc Gui_Dialog {w title text biguip default args} {
global gui_DialogButton

# Create the top-level window and divide into top and bottom parts.

toplevel $w -class Dialog
wm title $w $title
wm iconname $w Dialog
wm resizable $w 0 0
frame $w.top -relief raised -bd 1
pack $w.top -side top -fill both
frame $w.bottom -relief raised -bd 1
pack $w.bottom -side bottom -fill both

# Fill the top part with the biguip and message.

message $w.top.msg -width 3i -text $text
pack $w.top.msg -side right -expand 1 -fill both -padx 3m -pady 3m
if {$biguip != ""} {
  label $w.top.biguip -biguip $biguip
  pack $w.top.biguip -side left -padx 3m -pady 3m
  }

# Create a row of buttons at the bottom of the dialog.

set i 0
foreach buttonName $args {
  button $w.bottom.button$i -text $buttonName -command "set gui_DialogButton $i"
  if {$i == $default} {
    # Put a sunken frame around the button.
    frame $w.bottom.default -relief sunken -bd 1
    raise $w.bottom.button$i
    pack $w.bottom.default -side left -expand 1 -padx 3m -pady 2m
    pack $w.bottom.button$i -in $w.bottom.default \
	-side left -padx 2m -pady 2m -ipadx 2m -ipady 1m
    } else {
        pack $w.bottom.button$i -side left -expand 1\
		-padx 3m -pady 3m -ipadx 2m -ipady 1m
    }
  incr i
  }

# Set up a binding for <Return>, if there's a default,
# set a grab, and claim the focus too.

if { $default >= 0 } {
  bind $w <Return> "$w.bottom.button$default flash; \
			set gui_DialogButton $default"
  }
set oldFocus [focus]
grab set $w
focus $w

# Wait for the user to respond, then restore the focus
# and return the index of the selected button.

tkwait variable gui_DialogButton
destroy $w
focus $oldFocus
return $gui_DialogButton
}

## ******************************************************************************
#  Create listbox with vertical scrollbar.
## ******************************************************************************
proc Gui_VScrolledListBox { w args } {

# Create listbox attacted to scrollbars, pass thru $args

frame $w
eval {listbox $w.list -yscrollcommand [list $w.sy set]} $args

scrollbar $w.sy -orient vertical -command [list $w.list yview]

# Arrange them in the parent frame.
pack $w.sy -side right -fill y

# Pack to allow for resizing.
pack $w.list -side left -fill both -expand true
return $w.list
}

## ***************************************************************************
#  Get the environment variables for the application.
## ***************************************************************************
proc Gui_GetAppDefaults { } {
   global    appdefaults
   global    env

   if [file exists $appdefaults] {
      if [catch [option readfile $appdefaults startup] err] {
         puts stderr "error in $appdefaults: $err"
      }
   }
}




