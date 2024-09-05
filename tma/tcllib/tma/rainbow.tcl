#   DESCRIPTION     : This module provides an array "rainbow" indexed
#                     by 0 to (rainbowWidth - 1) of Tk colors. The editRainbow
#                     proc is used to allow the user to edit the rainbow. Must
#                     call initRainbow before using.
#
#       PUBLIC      : proc initRainbow
#                     Needs to be called once at startup
#
#                     proc editRainbow
#                     Creates dialog for editing rainbow.

package provide TMASRV 1.0

# Initialize rainbow
proc initRainbow {} {
    global rainbowWidth rainbow

    set colors {#ff0000 #ff7f00 #ffff00 #00ff00 #00ffff #007fff #0000ff #7f00ff #ff00ff}
    set rainbowWidth [llength $colors]
    set index 0
    foreach color $colors {
	set rainbow($index) $color
	incr index
    }
}

proc editRainbow {doneProc} {
    global rainbow

    set w .rainbow

    if {[info commands $w] != ""} {
	destroy $w
    }

    toplevel .rainbow
    
    for {set color 0} {$color < 6} {incr color} {
	set frame $w.color$color
	frame $frame
	pack $frame
	label $frame.label -text "               " -bg $rainbow($color)
	pack $frame.label -side left
	button $frame.button -text "Change" -command "chooseRainbowColor $frame.label $color"
	pack $frame.button -side left
    }

    button $w.done -text "Done" -command "destroy $w; $doneProc"
    pack $w.done
}

proc editOneColor {color} {
    global rainbow

    set chosenColor [chooseColor]
    if {$chosenColor == ""} {
	return
    }

    set rainbow($color) $chosenColor

#    $doneProc
}

set singleColor(MinPath) #ff0000
set singleColor(AnalystPath) #ff0000
set singleColor(Isochronal) #ff0000
set singleColor(Corridor) #ff0000
set singleColor(MultiSource) #ff0000

proc editSingleColor {w type} {
    global singleColor

    set chosenColor [chooseColor]
    if {$chosenColor == ""} {
	return
    }

    set singleColor($type) $chosenColor
    $w configure -bg $chosenColor
}

proc chooseRainbowColor {label color} {
    global rainbow

    set chosenColor [chooseColor]
    if {$chosenColor == ""} {
	return
    }

    set rainbow($color) $chosenColor
    $label configure -bg $chosenColor
}

proc chooseColor {} {
    set w .colorChooser

    if {[info commands $w] != ""} {
	return
    }

    toplevel $w -class Dialog
    wm title $w "Select Color"

    set image chooseColorImage

    if {[lsearch [image names] $image] < 0} {
	global env
	image create photo $image -file $env(TMA_DATA)/colorwheel.gif
    }

    canvas $w.c -bg black -width [image width $image] -height [image height $image]
    pack $w.c
    bind $w.c <Button-1> "set chosenColor \[$image get %x %y\]; set chooseColorOK 1"
    $w.c create image 0 0 -anchor nw -image $image

    set oldFocus [focus]
    update
    grab set $w
    focus $w

    # Wait for the user to respond, then restore the focus and return the index of the selected button.

    tkwait variable chooseColorOK
    destroy $w
    focus $oldFocus
    global chosenColor

    eval format "#%02x%02x%02x" $chosenColor
}