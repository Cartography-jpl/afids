package provide TMASRV 1.0

# Naming conventions:
#	Public procedures: Gui_DoIt
#	Public variables: gui_PublicVariable
#	Private procedures: _GuiDoIt
#	Private variables: _guiPrivateVariable
#	Tcl commands: gui_doIt
#	Tcl command procedure (C procedure): Gui_DoItCmd 

#GetAppDefaults

## ***************************************************************************
## Simple support routines.
## ***************************************************************************
proc _GuiSelectObstaclesOK {} {
  global gui_ObstaclesSelection _guiSelectObstaclesState
  set gui_ObstaclesSelection $_guiSelectObstaclesState(index)
  set _guiSelectObstaclesState(done) 1
}

## ***************************************************************************
## Get the obstacles.
## ***************************************************************************
proc _GuiSelectObstaclesApply {} {
  global gui_ObstaclesSelection _guiSelectObstaclesState
  set gui_ObstaclesSelection $_guiSelectObstaclesState(index)
}

## ***************************************************************************
## Get the obstacles.
## ***************************************************************************
proc _GuiSelectObstaclesCancel {} {
  global gui_ObstaclesSelection _guiSelectObstaclesState
  set gui_ObstaclesSelection $_guiSelectObstaclesState(original)
  set _guiSelectObstaclesState(done) 1
}

## ***************************************************************************
## Get the obstacles.
## ***************************************************************************
proc _GuiSelectObstaclesHelp { w } {
  Gui_Dialog {$w.help "Help" "Not implemented" {} 0 OK}
}

## ***************************************************************************
## Get the obstacles.
## ***************************************************************************
proc _GuiSelectObstaclesClick { lb y } {
global _guiSelectObstaclesState
  set _guiSelectObstaclesState(index) [$lb nearest $y]
}

## ***************************************************************************
## Get the obstacles.
## ***************************************************************************
proc _GuiSelectObstaclesDoubleClick { lb y } {
global _guiSelectObstaclesState
  set _guiSelectObstaclesState(index) [$lb nearest $y]
  _GuiSelectObstaclesOK
}

## ***************************************************************************
## Select a obstacles from the list.
## ***************************************************************************
proc Gui_SelectObstacles {w title obstaclesList} {
  global gui_ObstaclesSelection
  global _guiSelectObstaclesState

set _guiSelectObstaclesState(original) $gui_ObstaclesSelection

# Create the top-level window and divide into top and bottom parts.

toplevel $w -class toplevel
wm title $w $title
wm minsize $w 15 3
wm iconname $w toplevel
frame $w.top -relief raised -bd 1
pack $w.top -side top -fill both -expand true
frame $w.bottom -relief raised -bd 1
pack $w.bottom -side bottom -fill x

# Fill the top part with a scrolled listbox.

set obstaclesListBox [Gui_VScrolledListBox $w.top.list -width 15 -height 2 \
			-selectmode single -setgrid true]
pack $w.top.list -side top -fill both -expand true

foreach obstaclesName $obstaclesList {
  $obstaclesListBox insert end $obstaclesName
  }

# Create a row of buttons at the bottom.

set buttonText {Ok Apply Cancel}
set buttonCmds {_GuiSelectObstaclesOK _GuiSelectObstaclesApply _GuiSelectObstaclesCancel }
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
bind $obstaclesListBox <Button-1> {
  _GuiSelectObstaclesClick %W %y
  }
bind $obstaclesListBox <Double-Button-1> {
  _GuiSelectObstaclesDoubleClick %W %y
  }
bind $obstaclesListBox <Return> {
  $w.bottom.button$default flash; _GuiSelectObstaclesOK
}

set oldFocus [focus]
focus $w

# Wait for the user to respond, then restore the focus.

tkwait variable _guiSelectObstaclesState(done)
destroy $w
focus $oldFocus
}
