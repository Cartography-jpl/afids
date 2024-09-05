proc xorColor {win color} {
    set rgb [winfo rgb $win $color]
    format "#%03x%03x%03x" [expr 0xffff & ~[lindex $rgb 0]] [expr 0xffff & ~[lindex $rgb 1]] [expr 0xffff & ~[lindex $rgb 2]]
}

proc lremove {list item} {
    set index [lsearch $list $item]

    while {$index >= 0} {
	set list [lreplace $list $index $index]
	set index [lsearch $list $item]
    }

    return $list
}

proc min {args} {
    set min [lindex $args 0]
    set list [lrange $args 1 end]
    foreach x $list {
	if {$x < $min} {
	    set min $x
	}
    }
    return $min
}

proc max {args} {
    set max [lindex $args 0]
    set list [lrange $args 1 end]
    foreach x $list {
	if {$x > $max} {
	    set max $x
	}
    }
    return $max
}

set promptUserCounter 0
proc promptUser {title prompt choices} {
    set w .promptUserWin

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w; set promptUserVar {}"
    wm title $w $title
    wm iconname $w $title
    
    label $w.label -text $prompt
    pack $w.label -fill both -expand true -side top

    frame $w.buttons
    pack $w.buttons -fill both -side top

    set bNum 0
    foreach choice $choices {
	set label [lindex $choice 0]
	set retval [lindex $choice 1]
	if {$retval == ""} {
	    set retval $label
	}
	button $w.buttons.b_${bNum} -text $label -command "set promptUserVar $retval; destroy $w"
	pack $w.buttons.b_${bNum} -fill both -side left -expand true
	incr bNum
    }

    # promptUseCounter is used to deal with reentrant calls to
    # promptUser. If it changes while tkwaiting, another call to
    # promptUser must have been made, so return nothing, rather than
    # the user value, allowing the later call to preempt this one.
    global promptUserCounter
    incr promptUserCounter
    set localCounter $promptUserCounter

    # promptUserVar is set by each command button created above
    global promptUserVar
    tkwait variable promptUserVar

    if {$promptUserCounter == $localCounter} {
	return $promptUserVar
    } else {
	return ""
    }
}