package provide TMASRV 1.0

# analysisHistory is a list of records containing:
# type (one of Path, Contour, Corridor)
# name (entered by the user, used to distinguish history items in the history dialog)
# items (line, dot, image, etc. canvas item ids of a given analysis)

set typeIndex 0
set nameIndex 1
set itemsIndex 2

# Adds an analysis item to the history
proc addAnalysisHistoryItem {type name items} {
    global analysisHistory
    
    lappend analysisHistory [list $type $name $items]
}

# Returns a list of {name index} from the history for the given type. Used to generate the history dialog.
proc getHistoryByType {type} {
    global analysisHistory typeIndex nameIndex

    set history {}
    set index 0
    foreach item $analysisHistory {
	if {! [string compare $type [lindex $item $typeIndex]]} {
	    lappend history [list [lindex $item $nameIndex] $index]
	}
	incr index
    }

    return $history
}

# Remove nth item from history
proc removeHistoryItem {index} {
    global analysisHistory itemsIndex

    set item [lindex $analysisHistory $index]
    set analysisHistory [lreplace $analysisHistory $index $index]
    foreach objId [lindex $item $itemsIndex] {
	if {[llength $objId] == 1} {
	    Map_EraseObject [getMapHandle] $objId
	} else {
	    # this is a photo image; erase the canvas item
	    Map_EraseObject [getMapHandle] [lindex $objId 0]
	    # then delete the image
	    image delete [lindex $objId 1]
	}
    }
}

proc editAnalysisHistory {type} {
    set w .analysisHistory

    if {[info commands $w] != ""} {
	destroy $w
    }

    toplevel $w

    listbox $w.listbox -selectmode multiple
    pack $w.listbox -fill both -expand 1
    
    global editedHistories

    set editedHistories [getHistoryByType $type]
    foreach analysis $editedHistories {
	$w.listbox insert end [lindex $analysis 0]
    }

    # Delete and Cancel buttons
    frame $w.bottom
    pack $w.bottom

    button $w.bottom.selectAll -text "Select All" -command "$w.listbox selection set 0 end"
    button $w.bottom.delete -text Delete -command "deleteAnalysisHistory $w.listbox $type"
    button $w.bottom.cancel -text Done -command "destroy $w"
    pack $w.bottom.selectAll $w.bottom.delete $w.bottom.cancel -anchor center -side left -pady 4 
}

proc deleteAnalysisHistory {listbox type} {
    set curselection [$listbox curselection]

    if {$curselection == ""} {
	tk_messageBox -message "Select an analysis item to delete"
	return
    }

    global editedHistories
    for {set i [expr [llength $curselection] - 1]} {$i >= 0} {incr i -1} {
	set selection [lindex $curselection $i]

	$listbox delete $selection

	removeHistoryItem [lindex [lindex $editedHistories $selection] 1]
    }

    set editedHistories [getHistoryByType $type]
}

proc initAnalysisHistory {} {
    global analysisHistory

    set analysisHistory {}
}

proc overlayDialog {type} {
    set w .overlay

    if {[info commands $w] != ""} {
	destroy $w
    }

    switch $type {
	Weather -
	Obstacle {
	    set toggleType polygon
	}
	Path -
	Contour -
	Corridor {
	    set toggleType analysisHistory
	}
	default {
	    tk_messageBox -message "Unknown analysis type $type"
	    return
	}
    }

    toplevel $w

    listbox $w.listbox -selectmode multiple
    pack $w.listbox -fill both -expand 1
    
    # Toggle and Done buttons
    frame $w.bottom
    pack $w.bottom

    button $w.bottom.selectAll -text "Select All" -command "$w.listbox selection set 0 end"
    button $w.bottom.toggle -text Toggle -command "toggleAnalysisHistoryOrPolygon $w.listbox $toggleType"
    button $w.bottom.done -text Done -command "destroy $w"
    pack $w.bottom.selectAll $w.bottom.toggle $w.bottom.done -anchor center -side left -pady 4 

    switch $type {
	Weather {
	    global aoiPolygons toggledPolygons
	    set toggledPolygons {}
	    foreach polygon $aoiPolygons {
		switch [lindex $polygon 1] {
		    Obstacle {}
		    RegionObstacle {}
		    Weather {
			set type [lindex $polygon 1]
			set subtype [lindex $polygon 2]
			set name [lindex $polygon 3]
			lappend toggledPolygons $polygon
			if {$subtype == ""} {
			    $w.listbox insert end "$name ($type)"
			} else {
			    $w.listbox insert end "$name ($type - $subtype)"
			}
		    }
		}
	    }
	}
	Obstacle {
	    global aoiPolygons toggledPolygons
	    set toggledPolygons {}
	    foreach polygon $aoiPolygons {
		switch [lindex $polygon 1] {
		    Obstacle -
		    RegionObstacle {
			set type [lindex $polygon 1]
			set subtype [lindex $polygon 2]
			set name [lindex $polygon 3]
			lappend toggledPolygons $polygon
			if {$subtype == ""} {
			    $w.listbox insert end "$name ($type)"
			} else {
			    $w.listbox insert end "$name ($type - $subtype)"
			}
		    }
		    Weather {}
		}
	    }
	}

	Path -
	Contour -
	Corridor {
	    global toggledAnalysisHistories
	    set toggledAnalysisHistories [getHistoryByType $type]
	    foreach analysis $toggledAnalysisHistories {
		$w.listbox insert end [lindex $analysis 0]
	    }
	}
    }
}

proc toggleAnalysisHistoryOrPolygon {listbox type} {
    set curselection [$listbox curselection]

    global toggledAnalysisHistories toggledPolygons analysisHistory
    
    set map [getMapHandle]
    # Note: this is not going through the Map Layer API.
    # It is using the canvas capabilities directly.
    # This should be fixed during integration.
    set canvas .map${map}.display
    set scrollRegion [$canvas cget -scrollregion]
    set canvasWidth [lindex $scrollRegion 2]
    set canvasHeight [lindex $scrollRegion 3]

    foreach selection $curselection {
	switch $type {
	    analysisHistory {
		set toggledHistory [lindex $toggledAnalysisHistories $selection]
		set historyIndex [lindex $toggledHistory 1]
		set history [lindex $analysisHistory $historyIndex]
		set canvasIds [lindex $history 2]
	    }
	    polygon {
		set toggledPolygon [lindex $toggledPolygons $selection]
		set canvasIds [lindex $toggledPolygon 0]
	    }
	}
	foreach canvasId $canvasIds {
	    set coords [$canvas coords $canvasId]
	    set newCoords {}
	    foreach {x y} $coords {
		if {$x < $canvasWidth} {
		    set x [expr $x + $canvasWidth]
		    set y [expr $y + $canvasHeight]
		} else {
		    set x [expr $x - $canvasWidth]
		    set y [expr $y - $canvasHeight]
		}
		lappend newCoords $x $y
	    }
	    eval [concat $canvas coords $canvasId $newCoords]
	}
    }
}