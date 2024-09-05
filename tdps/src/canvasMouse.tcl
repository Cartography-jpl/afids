proc setupCanvasMouseBindings {} {
    global c

    bind $c <Enter> "focus $c ; enterCanvas %x %y"
    bind $c <Leave> "leaveCanvas"
    bind $c <Motion> "focus $c ; buttonNoneMotion %x %y"
    bind $c <ButtonPress-1> "button1Press %x %y none"
    bind $c <Shift-ButtonPress-1> "button1Press %x %y shift"
    bind $c <Control-ButtonPress-1> "button1Press %x %y control"
    bind $c <B1-Motion> "button1Motion %x %y"
    bind $c <ButtonRelease-1> "button1Release %x %y none"
    bind $c <Shift-ButtonRelease-1> "button1Release %x %y shift"
    bind $c <ButtonPress-2> "button2Press %x %y none"
    bind $c <ButtonPress-3> "button3Press %x %y none"
    bind $c <Double-Button-1> "doubleButton1 %x %y none"
    bind $c <Up> "nudgeVector north"
    bind $c <Down> "nudgeVector south"
    bind $c <Left> "nudgeVector west"
    bind $c <Right> "nudgeVector east"
    bind $c <Control-A> "selectAll" 			; bind $c <Control-a> "selectAll"
    bind $c <Control-B> "splitVector"			; bind $c <Control-b> "splitVector"
    bind $c <Control-C> "copySelectedVectors" 		; bind $c <Control-c> "copySelectedVectors"
    bind $c <Control-D> "debugTdps"	 		; bind $c <Control-d> "debugTdps"
    bind $c <Control-E> "displayShownElevation" 	; bind $c <Control-e> "displayShownElevation"
    bind $c <Control-F> "zoomToFit vec" 		; bind $c <Control-f> "zoomToFit vec"
    bind $c <Control-G> "toggleGridCell" 		; bind $c <Control-g> "toggleGridCell"
    bind $c <Control-I> "zoominCenter" 			; bind $c <Control-i> "zoominCenter"
    bind $c <Control-M> "toggleMap" 			; bind $c <Control-m> "toggleMap"
    bind $c <Control-O> "zoomoutCenter" 		; bind $c <Control-o> "zoomoutCenter"
    bind $c <Control-P> "insertVertices" 		; bind $c <Control-p> "insertVertices"
    bind $c <Control-S> "saveOneOrMoreChangedFiles"	; bind $c <Control-s> "saveOneOrMoreChangedFiles"
    bind $c <Control-U> "undo" 				; bind $c <Control-u> "undo"
    bind $c <Control-V> "pasteVectorPasteBuffer" 	; bind $c <Control-v> "pasteVectorPasteBuffer"
    bind $c <Control-X> "cutSelectedVectors" 		; bind $c <Control-x> "cutSelectedVectors"
    bind $c <Delete> "deleteBackspace"
    bind $c <BackSpace> "deleteBackspace"
    bind $c <comma> "gotoPreviousVertex"
    bind $c <less> "gotoPreviousVertex"
    bind $c <period> "gotoNextVertex"
    bind $c <greater> "gotoNextVertex"
}

proc deleteBackspace {} {
    global editingVertices

    if {$editingVertices} {
	deleteVertices
    } else {
	deleteSelectedVectors
    }
}

proc doubleButton1 {x y key} {
    global cursorMode editingVertices

    switch $cursorMode {
	left_ptr {
	    if {$editingVertices} {
		unselectAll
		clearVertices
		return
	    }
	    editVertices    
	}
    }
}

# bound to <Enter>
proc enterCanvas {x y} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    updateMouseDoc $x $y
}

# bound to <Leave>
proc leaveCanvas {} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    clearMouseDoc
    clearObjInfo
}

proc toggleGridCell {} {
    global lastMouseX lastMouseY

    global gridIsOn
    if {! $gridIsOn} {
	gridOn
    }

    button2Press $lastMouseX $lastMouseY "none"
}

# bound to <ButtonPress-2>
proc button2Press {x y key} {
    # toggle grid cell
    global gridIsOn

    if {! $gridIsOn} {
	return
    }

    set lat [yToLat $y]
    set lon [xToLon $x]
    
    global gridSize

    set lat [expr $gridSize * floor($lat / $gridSize)]
    set lon [expr $gridSize * floor($lon / $gridSize)]

    toggleGrid $lat $lon
}

proc checkGridCell {lat lon} {
    global c gridColor scalePower gridSize

    set x1 [expr $lon * pow (2, $scalePower)]
    set y1 [expr - $lat * pow (2, $scalePower)]
    set x2 [expr ($lon + $gridSize) * pow (2, $scalePower)]
    set y2 [expr (- $lat - $gridSize) * pow (2, $scalePower)]
    $c create line $x1 $y1 $x2 $y2 -fill $gridColor -tags grid -width 3
    $c create line $x1 $y2 $x2 $y1 -fill $gridColor -tags grid -width 3

    set halfSize [expr $gridSize / 2.0]
    set xc [expr ($lon + $halfSize) * pow (2, $scalePower)]
    set yc [expr (- $lat - $halfSize) * pow (2, $scalePower)]

    $c create line $x1 $yc $xc $y1 -fill $gridColor -tags grid -width 3
    $c create line $xc $y2 $x2 $yc -fill $gridColor -tags grid -width 3
    $c create line $x1 $yc $xc $y2 -fill $gridColor -tags grid -width 3
    $c create line $xc $y1 $x2 $yc -fill $gridColor -tags grid -width 3
}

proc toggleGrid {lat lon} {
    global filledGridCells
    
    set index [lsearch $filledGridCells [list $lat $lon]]

    if {$index < 0} {
	lappend filledGridCells [list $lat $lon]

	checkGridCell $lat $lon
    } else {
	gridOff
	set filledGridCells [lreplace $filledGridCells $index $index]
	gridOn
    }

    writeDatabase
}

# bound to <ButtonPress-3>
proc button3Press {x y key} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global cursorMode editingVertices
    switch $cursorMode {
	left_ptr {
	    if {$editingVertices} {
		placeAndSelectNextVertex $x $y
	    }
	}
    }
}

proc placeAndSelectNextVertex {x y} {
    prepareForMoveUndo

    global c scalePower

    set x [expr round ([xToLon $x] * pow (2, $scalePower))]
    set y [expr - round ([yToLat $y] * pow (2, $scalePower))]
    set selectedItems [$c find withtag selected]

    if {[llength $selectedItems] == 0} {
	tk_messageBox -message "Select a vertex to move"

	return

	# pick one of the edited items' first vertex
	global editedVectorCanvasIds

	set vectorCanvasId [lindex $editedVectorCanvasIds 0]

	global vectorVertexItems

	set selectedItems [lindex $vectorVertexItems($vectorCanvasId) 0]
    }

    # for each selected vertex
    foreach item $selectedItems {
	# move the vertex dot's four coords
	set coords [$c coords $item]
	set x1 [lindex $coords 0]
	set x2 [lindex $coords 2]

	set radius [expr abs ($x2 - $x1) / 2.0]

	set x1 [expr $x - $radius]
	set x2 [expr $x + $radius]
	set y1 [expr $y - $radius]
	set y2 [expr $y + $radius]

	$c coords $item $x1 $y1 $x2 $y2

	set vectorCanvasId [getVectorFromVertex $item]

	# move the vector vertices that the vertex dots represent
	set vertexIndex [getVertexOffset $item $vectorCanvasId]
	set coords [$c coords $vectorCanvasId]
	set newX $x
	set newY $y

	set newCoords [concat [lrange $coords 0 [expr 2 * $vertexIndex - 1]] $newX $newY [lrange $coords [expr 2 * ($vertexIndex + 1)] end]]
	eval [concat $c coords $vectorCanvasId $newCoords]

	# unselect vertex
	$c dtag $item selected
	$c itemconfigure $item -fill [getNormalVertexColor]

	# determine vector type
	set vectorType [getLoadedFileVectorType [canvasItemToFilename $vectorCanvasId]]

	if {$vectorType == "POLYGON"} {
	    global vectorVertexItems

	    set otherItem -1
	    set vertexItems $vectorVertexItems($vectorCanvasId)
	    if {$item == [lindex $vertexItems 0]} {
		set otherItem [lindex $vertexItems [expr [llength $vertexItems] - 1]]
	    } elseif {$item == [lindex $vertexItems [expr [llength $vertexItems] - 1]]} {
		set otherItem [lindex $vertexItems 0]
	    }

	    # move matching vertex
	    if {$otherItem > 0} {
		set item $otherItem

		# move the vertex dot's four coords
		set coords [$c coords $item]
		set x1 [lindex $coords 0]
		set x2 [lindex $coords 2]

		set radius [expr abs ($x2 - $x1) / 2.0]

		set x1 [expr $x - $radius]
		set x2 [expr $x + $radius]
		set y1 [expr $y - $radius]
		set y2 [expr $y + $radius]

		$c coords $item $x1 $y1 $x2 $y2
		
		# move the vector vertices that the vertex dots represent
		set vertexIndex [getVertexOffset $item $vectorCanvasId]
		set coords [$c coords $vectorCanvasId]
		set newX $x
		set newY $y

		set newCoords [concat [lrange $coords 0 [expr 2 * $vertexIndex - 1]] $newX $newY [lrange $coords [expr 2 * ($vertexIndex + 1)] end]]
		eval [concat $c coords $vectorCanvasId $newCoords]

		# unselect vertex
		$c dtag $item selected
		$c itemconfigure $item -fill [getNormalVertexColor]
	    }
	}
    }

    saveModifiedSelection

    selectOneVertex $item
    gotoNextVertex 1
}

proc nudgeVector {direction} {
    global c editingVertices

    set selected [$c find withtag selected]

    if {$selected == {}} {
	if {$editingVertices} {
	    tk_messageBox -message "No vertices are selected."
	} else {
	    tk_messageBox -message "No vectors are selected."
	}
	return
    }

    initMoveSelectedNudge 10 10
    switch $direction {
	north {
	    moveSelected 10 9
	}
	south {
	    moveSelected 10 11
	}
	east {
	    moveSelected 11 10
	}
	west {
	    moveSelected 9 10
	}
    }
    saveModifiedSelection
}

set draggingCanvas 0
# bound to <ButtonPress-1> and <Shift-ButtonPress-1>
proc button1Press {x y key} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    if {$key == "control"} {
	set lat [yToLat $y]

	if {$lat < -90.0} {
	    set lat -90.0
	}
	if {$lat > 90.0} {
	    set lat 90.0
	}

	set lon [xToLon $x]

	if {$lon < -180.0} {
	    set lon -180.0
	}
	if {$lon > 180.0} {
	    set lon 180.0
	}

	displayElevation $lat $lon

	return
    }

    global cursorMode
    switch $cursorMode {
	left_ptr {
	    if {$key == "none" && [overSelectedItems $x $y]} {
		initMoveSelected $x $y
	    } else {
		initRubberBand $x $y $key
	    }
	}
	zoomin {
	    zoominXY $x $y
	}
	zoomout {
	    zoomoutXY $x $y
	}
	hand {
	    global c
	    global draggingCanvas
	    set draggingCanvas 1
	    $c scan mark $x $y
	}
	addLine {
	    unselectAll
	    addLineAt $x $y
	}
	addPolygon {
	    unselectAll
	    addPolygonAt $x $y
	}
    }
}

proc displayShownElevation {} {
    global w
    set lat [string trim [lindex [$w.grid.mouseLatDec config -text] 4]]
    set lon [string trim [lindex [$w.grid.mouseLonDec config -text] 4]]

    displayElevation $lat $lon
}

proc displayElevation {lat lon} {
    global srtmRoot
    set cmd "get_elev_pixel lat=$lat lon=$lon adir=$srtmRoot"
    global argv
    if {[lsearch $argv "expert"] >= 0} {
	puts $cmd
    }
    if {[catch {exec taetm -s "${cmd}"} erVar]} {
	puts "get_elev_pixel error: $erVar"
	tk_messageBox -message "Altitude calculation failed"
	return
    }
    set index [string first "Elevation is" $erVar]
    set msg "[string range $erVar $index [string length $erVar]]\nat lat ${lat}, lon $lon"
    tk_messageBox -message $msg -title "Elevation At Point In Meters"
}

# bound to <B1-Motion>
proc button1Motion {x y} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global cursorMode

    switch $cursorMode {
	rubberBand {
	    rubberBandSelection $x $y
	}
	move {
	    moveSelected $x $y
	}
	hand {
	    global c
	    $c scan dragto $x $y 1
	}
    }

    updateMouseDoc $x $y
}

# bound to <ButtonRelease-1> and <Shift-ButtonRelease-1>
proc button1Release {x y key} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global cursorMode

    switch $cursorMode {
	rubberBand {
	    acceptSelection $x $y $key
	    set cursorMode left_ptr
	}
	move {
	    saveModifiedSelection
	    set cursorMode left_ptr
	}
	hand {
	    global draggingCanvas
	    set draggingCanvas 0
	    global mapIsOn
	    if {$mapIsOn} {
		genLoadMapImage
	    }
	}
    }

    buttonNoneMotion $x $y
}

# bound to <Motion>
proc buttonNoneMotion {x y} {
    global lastMouseX lastMouseY
    set lastMouseX $x
    set lastMouseY $y

    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global cursorMode

    switch $cursorMode {
	left_ptr {
	    if {[overSelectedItems $x $y]} {
		setAppCursor fleur
	    } else {
		setAppCursor left_ptr
	    }
	}
    }

    global dotDownX
    set dotDownX {}
    updateMouseDoc $x $y
}

proc overSelectedItems {x y} {
    global c scalePower selectionRadius

    set top [expr $y - $selectionRadius]
    set bottom [expr $y + $selectionRadius]
    set left [expr $x - $selectionRadius]
    set right [expr $x + $selectionRadius]

    set x [expr round ([xToLon $x] * pow (2, $scalePower))]
    set y [expr - round ([yToLat $y] * pow (2, $scalePower))]
    set left [expr round ([xToLon $left] * pow (2, $scalePower))]
    set top [expr - round ([yToLat $top] * pow (2, $scalePower))]
    set right [expr round ([xToLon $right] * pow (2, $scalePower))]
    set bottom [expr - round ([yToLat $bottom] * pow (2, $scalePower))]

    set selectedItems [$c find withtag selected]

    set enclosedItems [$c find overlapping $left $top $right $bottom]

    foreach item $enclosedItems {
	if {[lsearch -exact $selectedItems $item] >= 0} {
	    return true
	}
    }

    return false
}

proc prepareForMoveUndo {} {
    global c editingVertices
    set selectedItems [$c find withtag selected]
    if {$editingVertices} {
	set actions {}
	set doneVectorCanvasIds {}
	foreach item $selectedItems {
	    set vectorCanvasId [getVectorFromVertex $item]

	    if {! [partOfMultiPartVector $vectorCanvasId] && [lsearch $doneVectorCanvasIds $vectorCanvasId] < 0} {
		lappend actions [captureUndoAction $vectorCanvasId moveVector]
		lappend doneVectorCanvasIds $vectorCanvasId
	    }
	}
	if {$actions != {}} {
	    addUndoActions $actions moveVertex
	}
    } else {
	set actions {}
	foreach item $selectedItems {
	    if {! [partOfMultiPartVector $item]} {
		lappend actions [captureUndoAction $item moveVector]
	    }
	}
	if {$actions != {}} {
	    addUndoActions $actions moveVector
	}
    }
}

proc initMoveSelected {x y} {
    initMoveSelectedNudge $x $y

    global cursorMode
    set cursorMode move    
}

proc initMoveSelectedNudge {x y} {
    prepareForMoveUndo

    global lastCoordX lastCoordY scalePower
    set lastCoordX [expr round ([xToLon $x] * pow (2, $scalePower))]
    set lastCoordY [expr - round ([yToLat $y] * pow (2, $scalePower))]
}

proc moveSelected {x y} {
    global lastCoordX lastCoordY
    global c selectionBox selectionBoxColor scalePower

    set x [expr round ([xToLon $x] * pow (2, $scalePower))]
    set y [expr - round ([yToLat $y] * pow (2, $scalePower))]

    set selectedItems [$c find withtag selected]

    global editingVertices
    if {$editingVertices} {
	# for each selected vertex
	foreach item $selectedItems {
	    # move the vertex dot's four coords
	    set coords {}
	    set count 0
	    foreach {vx vy} [$c coords $item] {
		lappend coords [expr $vx + $x - $lastCoordX] [expr $vy + $y - $lastCoordY]
		incr count
	    }
	    eval [concat $c coords $item $coords]
	    
	    # move the vector vertices that the vertex dots represent
	    set vectorCanvasId [getVectorFromVertex $item]
	    set vertexIndex [getVertexOffset $item $vectorCanvasId]
	    set coords [$c coords $vectorCanvasId]
	    set newX [expr [lindex $coords [expr 2 * $vertexIndex]] + $x - $lastCoordX]
	    set newY [expr [lindex $coords [expr 2 * $vertexIndex + 1]] + $y - $lastCoordY]

	    set newCoords [concat [lrange $coords 0 [expr 2 * $vertexIndex - 1]] $newX $newY [lrange $coords [expr 2 * ($vertexIndex + 1)] end]]
	    eval [concat $c coords $vectorCanvasId $newCoords]
	}
    } else {
	foreach item $selectedItems {
	    if {! [partOfMultiPartVector $item]} {
		set coords {}
		foreach {vx vy} [$c coords $item] {
		    lappend coords [expr $vx + $x - $lastCoordX] [expr $vy + $y - $lastCoordY]
		}
		eval [concat $c coords $item $coords]
	    }
	}
    }

    set lastCoordX $x
    set lastCoordY $y
}

proc partOfMultiPartVector {item} {
    set parts [canvasItemToCanvasParts $item]
    if {[llength $parts] > 1} {
	return true
    } else {
	return false
    }
}