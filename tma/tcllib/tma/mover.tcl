package provide TMASRV 1.0

## ***************************************************************************
## There are two lists containing mover information.
## They are codes and descriptions.
## There are functions that access these two lists.
## ***************************************************************************
proc getMoverCodes {} {
  global _guiMoverCodes
  return $_guiMoverCodes
}

proc setMoverCodes { codes } {
  global _guiMoverCodes
  set _guiMoverCodes $codes
}

proc getMoverDescriptions {} {
  global _guiMoverDescriptions
  return $_guiMoverDescriptions
}

proc setMoverDescriptions { desc } {
  global _guiMoverDescriptions
  set _guiMoverDescriptions $desc
}

## ***************************************************************************
## Read mover meta data from file.
## ***************************************************************************
proc readMoverMetaData {} {
    global _guiMoverCodes _guiMoverDescriptions
    global env
    set appId $env(TMA_APPLICATION_ID)
    
    # Database access.
    set columns [list MOVER_ID MOVER_NAME]
    set criteria [list [list APPLICATION_ID $appId]]
    set moverdata [Data_Select tma_moverName $columns $criteria]

    set _guiMoverCodes ""
    set _guiMoverDescriptions ""

    foreach mover $moverdata {
	lappend _guiMoverCodes "[lindex $mover 0]"
	lappend _guiMoverDescriptions "[lindex $mover 1]"
    }
}

## ***************************************************************************
## OK.
## ***************************************************************************
proc _GuiSelectMoverOK {} {
  global currentMover

    catch {
	set currentMover(index) $currentMover(tempIndex)
	set currentMover(code) [lindex [getMoverCodes] $currentMover(index)]
	set currentMover(description) \
		[lindex [getMoverDescriptions] $currentMover(index)]
	#set currentMover(location) [lindex [getMoverLocations] $currentMover(index)]
	set currentMover(set) TRUE
    }
}

## ***************************************************************************
## Apply.
## ***************************************************************************
proc _GuiSelectMoverApply {} {
  global currentMover

    catch {
	set currentMover(code) [lindex [getMoverCodes] $currentMover(tempIndex)]
	set currentMover(description) \
		[lindex [getMoverDescriptions] $currentMover(tempIndex)]
	#set currentMover(location) [lindex [getMoverLocations] $currentMover(tempIndex)]
    }
}

## ***************************************************************************
## Cancel.
## ***************************************************************************
proc _GuiSelectMoverCancel {} {
  global currentMover

    catch {
	# set currentMover values back to original state
	set currentMover(code) [lindex [getMoverCodes] $currentMover(index)]
	set currentMover(description) \
		[lindex [getMoverDescriptions] $currentMover(index)]
	#set currentMover(location) [lindex [getMoverLocations] $currentMover(index)]
	set currentMover(set) TRUE
    }
}

## ***************************************************************************
## Get the mover.
## ***************************************************************************
proc _GuiSelectMoverHelp { w } {
  Gui_Dialog {$w.help "Help" "Not implemented" {} 0 OK}
}

## ***************************************************************************
## Get the mover.
## ***************************************************************************
proc _GuiSelectMoverClick { lb y } {
  global currentMover
  set currentMover(tempIndex) [$lb nearest $y]
}

## ***************************************************************************
## Get the mover.
## ***************************************************************************
proc _GuiSelectMoverDoubleClick { lb y } {
  global currentMover
  set currentMover(tempIndex) [$lb nearest $y]
  _GuiSelectMoverOK
}

## ***************************************************************************
## Select a mover from the list.
## ***************************************************************************
proc Gui_SelectMover {w title moverList} {

# Create the top-level window and divide into top and bottom parts.

toplevel $w -class toplevel
wm title $w $title
wm minsize $w 15 11
wm iconname $w toplevel
frame $w.top -relief raised -bd 1
pack $w.top -side top -fill both -expand true
frame $w.bottom -relief raised -bd 1
pack $w.bottom -side bottom -fill x

# Fill the top part with a scrolled listbox.

set moverListBox [Gui_VScrolledListBox $w.top.list -width 15 -height 10 \
			-selectmode single -setgrid true]
pack $w.top.list -side top -fill both -expand true

foreach moverName $moverList {
  $moverListBox insert end $moverName
  }

# Create a row of buttons at the bottom.

set buttonText {Ok Apply Cancel}
set buttonCmds {_GuiSelectMoverOK _GuiSelectMoverApply _GuiSelectMoverCancel }
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
bind $moverListBox <Button-1> {
  _GuiSelectMoverClick %W %y
  }
bind $moverListBox <Double-Button-1> {
  _GuiSelectMoverDoubleClick %W %y
  }
bind $moverListBox <Return> {
  $w.bottom.button$default flash; _GuiSelectMoverOK
}

set oldFocus [focus]
focus $w
raise $w

# Wait for the user to respond, then restore the focus.

tkwait variable currentMover(set)
    destroy $w
catch {
    focus $oldFocus
}
}

## ***************************************************************************
## Get the mover's data from db.
## ***************************************************************************
proc Gui_GetMoverAttributes { moverType } {
  global _guiMoverCosts

# OLD CODE !!!

  set columns { \
SPEED_SECONDARY_ROADS \
SPEED_PRIMARY_ROADS \
SPEED_MAJOR_ROADS \
SPEED_SPUR_TRACK \
SPEED_SINGLE_TRACK \
SPEED_DOUBLE_TRACK \
SPEED_OPEN_FIELDS \
SPEED_SCATTERED_TREES \
SPEED_LIGHT_FOREST \
SPEED_DENSE_FOREST \
SPEED_VILLAGES \
SPEED_TOWNS \
SPEED_CITIES \
FACTOR_SLOPE_0_TO_2 \
FACTOR_SLOPE_11_TO_20 \
FACTOR_SLOPE_21_TO_30 \
FACTOR_SLOPE_31_TO_45 \
FACTOR_SLOPE_3_TO_5 \
FACTOR_SLOPE_46_PLUS \
FACTOR_SLOPE_6_TO_10 \
TIME_LARGE_RIVER \
TIME_MEDIUM_RIVER \
TIME_SMALL_RIVER \
TIME_SECONDARY_BRIDGE \
TIME_PRIMARY_BRIDGE \
TIME_MAJOR_BRIDGE \
TIME_SPUR_BRIDGE \
TIME_SINGLE_BRIDGE \
TIME_DOUBLE_BRIDGE \
FACTOR_SLEET_OR_HAIL \
FACTOR_DUST_OR_SMOKE \
FACTOR_FOG_OR_HAZE \
FACTOR_LIGHT_RAIN \
FACTOR_MODERATE_RAIN \
FACTOR_HEAVY_RAIN \
FACTOR_LIGHT_SNOW \
FACTOR_MODERATE_SNOW \
FACTOR_HEAVY_SNOW \
FACTOR_FROZEN_GROUND \
}

  set criteria [list [list MOVER_TYPE ${moverType}]]
  set costList [Data_Select AMD_MOVER_TYPE ${columns} ${criteria}]
  set i 0
  set costListStrip [lindex $costList 0]
  foreach cost $costListStrip {
    set _guiMoverCosts($i) $cost
    incr i
  }
}

############################################################################
# Create labelled entries in canvas.
############################################################################
proc labeledEntries { canvas labels } {
  global _guiMoverCosts _guiMoverCodes

  # Create one frame to hold everything
  # and position it on the canvas.
  set f [frame $canvas.f -bd 0]
  $canvas create window 0 0 -anchor nw -window $f

  # Find out how big the labels are.
  set max 0
  foreach label $labels {
    set len [string length $label]
    if {$len > $max} {
      set max $len
    }
  }

  # Create and pack the labeled scales.
  set i 0
  foreach label $labels {
    frame $f.$i
    label $f.$i.label -text $label -justify left -anchor w -width $max
    entry $f.$i.entry -width 10 \
      -textvariable _guiMoverCosts($i)
    pack $f.$i.label -side left
    pack $f.$i.entry -side right
    pack $f.$i -side top -fill x
    incr i
  }

  set child [lindex [pack slaves $f] 0]

  # Wait for the window to become visible and then
  # set up the scroll region and increment based on
  # the size of the frame and the subframes.

  tkwait visibility $child
  set incr [winfo height $child]
  set width [winfo width $f]
  set height [winfo height $f]
  $canvas config -scrollregion "0 0 $width $height"
  $canvas config -yscrollincrement $incr
  if {$height > 20 * $incr} {
    set height [expr 20 * $incr]
  }
  $canvas config -width $width -height $height
}


## ***************************************************************************
## Present mover's data to user.
## ***************************************************************************
proc Gui_ShowMoverAttributes { w title moverType } {

# Create the top-level window.

toplevel $w -class toplevel
wm title $w $title
wm iconname $w toplevel

frame $w.menubar -relief raised -borderwidth 2

# File

menubutton $w.menubar.file \
  -text "File" \
  -menu $w.menubar.file.menu

menu $w.menubar.file.menu

$w.menubar.file.menu add command \
  -label "Exit" \
  -command "destroy $w"

# Help

menubutton $w.menubar.help \
  -text "Help" \
  -menu $w.menubar.help.menu

menu $w.menubar.help.menu

$w.menubar.help.menu add command \
  -label "Read me"

# Pack main menu bar items.
pack $w.menubar.file -side left
pack $w.menubar.help -side right

pack $w.menubar -side top -fill x -expand true

wm minsize $w 200 100

# Create a scrollable canvas
frame $w.c
canvas $w.c.canvas -width 10 -height 10 -yscrollcommand [list $w.c.yscroll set]
scrollbar $w.c.yscroll -orient vertical -command [list $w.c.canvas yview]
pack $w.c.yscroll -side right -fill y
pack $w.c.canvas -side left -fill both -expand true
pack $w.c -side top -fill both -expand true

# Get the terrain feature names.
set featureNames {"secondary road" "primary road" "major road" "spur-track rail" \
	"single-track rail" "double-track rail" "open field" "scattered trees" \
	"light forest" "dense forest" "village" "town" "city" "0-2% slope multiplier" \
	"3-5% slope multiplier" "6-10% slope multiplier" "11-20% slope multiplier" \
	"21-30% slope multiplier" "31-44% slope multiplier" "45+% slope multiplier"
	"large river" "medium river" "small river" "secondary road bridge crossing" \
	"primary road bridge crossing" "major road bridge crossing" "spur track rail crossing" \
	"single track rail crossing" "double track rail crossing" "sleet or hail" \
	"dust or smoke" "fog or haze" "light rain" "moderate rain" "heavy rain" "light snow" \
	"moderate snow"	"heavy snow" "frozen ground"}

labeledEntries $w.c.canvas $featureNames

# Get the data.
Gui_GetMoverAttributes $moverType
}

## ***************************************************************************
## Present mover's data to user.
## ***************************************************************************
proc Gui_ShowMover {} {
  global currentMover
  set saveIt $currentMover(index)
  Gui_SelectMover .d "Select Mover" [getMoverDescriptions]
  Gui_ShowMoverAttributes .showMoverAttributes "Show Mover Attributes" \
		$currentMover(code)
  # reset to old values.
  set currentMover(tempIndex) $saveIt
  _GuiSelectMoverOK
}
