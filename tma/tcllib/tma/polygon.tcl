#   DESCRIPTION     : This module contains the functions for manipulating AOI
#                     weather and obstacle polygons. There are two types of
#                     closed polygon: Weather and RegionObstacle. There is 
#                     one type of open polygon: Obstacle. Closed polygons
#                     are drawn with a stipple fill.
#
#   COMMANDS        :
#
#       PUBLIC      : polygonPathDialog {type operation}
#                     type is one of {Weather Obstacle RegionObstacle}
#                     operation is one of {Create Delete}
#                     Used to add/remove weather/obstacles to/from current AOI.
#
#                     loadAoiPolygons {aoiId}
#                     Draws polygons for given AOI.
#
#                     eraseAllPolygons {}
#                     Erases all polygons from map. Used when changing AOIs
#
#                     editPolygonColors {}
#                     Used to view/change the colors used to draw polygons.
#
#                     initPolygonColors {}
#                     Must be called before calling other polygon functions
##############################################################################

package provide TMASRV 1.0

proc polygonPathDialog {type operation} {
    global locations env
    global polyPoints
    global polygonObject
    global newPolygonName newPolygonSubtype
    global aoi

    set w .polyPath

    while {! [info exists aoi] || $aoi == ""} {
	TMASRV_OpenAOI
    }

    set aoiId [lindex $aoi 4]

    set polyPoints {}

    if {$type != "Obstacle" && $type != "RegionObstacle" && $type != "Weather"} {
	tk_messageBox -message "Unknown polygon type \"$type\""
	return
    }

    if {[info commands $w] != {} } {
	destroy $w
    }

    toplevel $w -class Dialog
    wm title $w "$operation $type Constraint"
    wm resizable $w 0 0

    if {$operation == "Delete"} {
	# Delete existing polygons
	global existingPolygons
	set existingPolygons [Data_Select tma_polygon {ID NAME} [list [list AOI_ID $aoiId] [list TYPE $type]]]
	if {[llength $existingPolygons]} {
	    frame $w.top
	    pack $w.top -side top
	    
	    label $w.top.label1 -text "Existing $type"
	    pack $w.top.label1 -side top
	    
	    listbox $w.top.listbox
	    pack $w.top.listbox -side top
	    
	    foreach polygon $existingPolygons {
		$w.top.listbox insert end [lindex $polygon 1]
	    }
	} else {
	    tk_messageBox -message "There are no $type constraints to delete in this AOI"
	    destroy $w
	    return
	}

	# Delete and Cancel buttons
	frame $w.bottom
	pack $w.bottom -side top
	button $w.bottom.delete -text Delete -command "deletePolygon $w.top.listbox"
	button $w.bottom.cancel -text Done -command "destroy $w"
	pack $w.bottom.delete $w.bottom.cancel -anchor center -side left -pady 4 
    } else {
	
	set frame [frame $w.f]
	pack $frame

	set row 0
	
	# Polygon subtype
	if {$type == "Obstacle"} {
	    global newPolygonSubtype
	    set newPolygonSubtype ""
	} else {
	    if {$type == "RegionObstacle"} {
		set label [label $frame.subtypeLabel -text "Speed Multiplier"]
		grid $label -row $row -column 0
		global obstacleTypes
		set menu [eval tk_optionMenu $frame.subtype newPolygonSubtype $obstacleTypes]
		set newPolygonSubtype [lindex $obstacleTypes 0]
		grid $frame.subtype -row $row -column 1
	    } else {
		# must be weather
		set label [label $frame.subtypeLabel -text "Type"]
		grid $label -row $row -column 0
		global weatherTypes
		set menu [eval tk_optionMenu $frame.subtype newPolygonSubtype $weatherTypes]
		set newPolygonSubtype [lindex $weatherTypes 0]
		grid $frame.subtype -row $row -column 1
	    }

	    incr row
	}

	# Name
	set label [label $frame.polygonNameLabel -text "Name"]
	grid $label -row $row -column 0
	set entry [entry $frame.polygonNameEntry -textvariable newPolygonName]
	grid $entry -row $row -column 1 -sticky ew
	incr row

	# Corners
	set button $frame.multiLocationsButton
	set listFrame $frame.multiLocationsListbox
	set listbox $listFrame.list

	button $button -text "$type Corners" -command "getPolygon $type $listbox"
	grid $button -row $row -column 0  -sticky ew

	frame $listFrame
	set listScroll $listFrame.sy
	listbox $listbox -height 5 -width 25 -yscrollcommand [list $listScroll set]
	scrollbar $listScroll -orient vertical -command [list $listbox yview]
	pack $listScroll -side right -fill y
	pack $listbox -side left -fill both -expand true
	grid $listFrame -row $row -column 1 -sticky nesw

	incr row

	# Create and Cancel buttons
	set button [button $frame.create -text Create -command "createPolygon $type ; destroy $w"]
	grid $button -row $row -column 0
	set button [button $frame.cancel -text Cancel -command "eraseTempObject ; destroy $w"]
	grid $button -row $row -column 1
    }
}

proc eraseTempObject {} {
    global polygonObject

    if {$polygonObject != ""} {
	Map_EraseObject [getMapHandle] $polygonObject
    }
    set polygonObject ""
}

proc polygonTypeToColor {type subtype} {
    switch $type {
	Weather {
	    global weatherRainbow
	    return $weatherRainbow($subtype)
	}

	RegionObstacle {
	    global obstacleRainbow
	    return $obstacleRainbow($subtype)
	}
	
	Obstacle {
	    global lineObstacleColor
	    return $lineObstacleColor
	}
    }
}

# Used to get a list of points defining the shape of a polygon.
# type is the parameter passed to polygonPathDialog
proc getPolygon {type listbox} {
    global polyPoints
    global polygonObject
    global newPolygonSubtype

    set color [polygonTypeToColor $type $newPolygonSubtype]

    set polyPoints {}

    eraseTempObject

    $listbox delete 0 end

    set location "not null"

    set firstPoint "none yet"

    set map [getMapHandle]
    while {$location != ""} {
	set location [Map_PickCoord $map]
	if {($type == "Weather" || $type == "RegionObstacle") && $firstPoint == "none yet"} {
	    set firstPoint $location
	}

	if {$location != ""} {
	    appendPolygonLocation $listbox $location $color
	}
    }

    # Close the weather/obstacle region
    if {$type == "Weather" || $type == "RegionObstacle"} {
	appendPolygonLocation $listbox $firstPoint $color fill
    }
}

# A support function for getPolygon
proc appendPolygonLocation {listbox latLon color {fill ""}} {
    global polyPoints
    global polygonObject

    lappend polyPoints $latLon

    eraseTempObject
    set map [getMapHandle]
    if {[llength $polyPoints] == 1} {
	set polygonObject [Map_DrawFilledCircle $map $latLon 10 $color]
    } else {
	if {$fill == ""} {
	    set polygonObject \
		    [Map_DrawPolyline $map $polyPoints $color 2]
	} else {
	    set polygonObject \
		    [Map_DrawPolygon $map $polyPoints \
		    $color gray25.bmp $color]
	}
    }

    $listbox insert end [Map_CoordToString $latLon]
    $listbox see end
}

# Used to delete a selected polygon
proc deletePolygon {listbox} {
    # list of {ID NAME} ordered as in listbox
    global existingPolygons

    set curselection [$listbox curselection]

    if {$curselection == ""} {
	tk_messageBox -message "Select an existing item to delete"
	return
    }

    set polygonId  [lindex [lindex $existingPolygons $curselection] 0]

    Data_Delete tma_polygon [list [list ID $polygonId]]

    $listbox delete $curselection
    
    global aoiPolygons aoiPolygonIds

    set index [lsearch $aoiPolygonIds $polygonId]

    set map [getMapHandle]
    foreach canvasId [lindex [lindex $aoiPolygons $index] 0] {
	Map_EraseObject $map $canvasId
    }

    set aoiPolygons [lreplace $aoiPolygons $index $index]
    set aoiPolygonIds [lreplace $aoiPolygonIds $index $index]
    set existingPolygons [lreplace $existingPolygons $curselection $curselection]
}

# Used to create a polygon as described in the polygon dialog
# type is the parameter passed to polygonPathDialog
proc createPolygon {type} {
    global polyPoints
    global polygonObject
    global newPolygonName newPolygonSubtype
    global aoi
    global aoiPolygons aoiPolygonIds

    set aoiId [lindex $aoi 4]

    if {[llength $polyPoints] < 2} {
	tk_messageBox -message "Select at least two points to define the polygon"
	return
    }

    if {$newPolygonName == ""} {
	tk_messageBox -message "Enter a name for the new $newPolygonSubtype $type"
	return
    }

    set newPolygonId [findNewId tma_polygon ID]
    set record [list \
	    [list ID [findNewId tma_polygon ID]] \
	    [list AOI_ID $aoiId] \
	    [list TYPE $type] \
	    [list SUBTYPE $newPolygonSubtype] \
	    [list POINTS $polyPoints] \
	    [list NAME $newPolygonName]]

    Data_Insert tma_polygon $record

    # Draw polygon name
    set map [getMapHandle]
    set color [polygonTypeToColor $type $newPolygonSubtype]
    set textAndLeash \
	    [Map_Text2 $map $newPolygonName [lindex $polyPoints 0] \
	    $color polygonLabel]
    lappend aoiPolygonIds $newPolygonId
    lappend aoiPolygons [list [concat $polygonObject $textAndLeash] \
	    $type $newPolygonSubtype $newPolygonName]

    set polygonObject ""
}

proc eraseAllPolygons {} {
    global aoiPolygons aoiPolygonIds
    set map [getMapHandle]

    foreach aoiPolygon $aoiPolygons {
	foreach canvasId [lindex $aoiPolygon 0] {
	    Map_EraseObject $map $canvasId
	}
    }
    
    set aoiPolygons {}
    set aoiPolygonIds {}
}

proc loadAoiPolygons {aoiId} {
    global aoiPolygons aoiPolygonIds
    set map [getMapHandle]

    set tmaPolygons [Data_Select tma_polygon {ID POINTS TYPE SUBTYPE NAME} [list [list AOI_ID $aoiId]]]
    foreach aoiPolygon $tmaPolygons {
	destructure $aoiPolygon id points type subtype name
	set color [polygonTypeToColor $type $subtype]
	lappend aoiPolygonIds $id
	if {$type == "Weather" || $type == "RegionObstacle"} {
	    set canvasId \
		    [Map_DrawPolygon $map $points $color gray25.bmp $color]
	    set textAndLeash \
		    [Map_Text2 $map $name [lindex $points 0] \
		    $color polygonLabel]
	    lappend aoiPolygons [list [concat $canvasId $textAndLeash] \
		    $type $subtype $name]
	} else {
	    set canvasId \
		    [Map_DrawPolyline $map $points $color 2]
	    set textAndLeash \
		    [Map_Text2 $map $name [lindex $points 0] \
		    $color polygonLabel]
	    lappend aoiPolygons [list [concat $canvasId $textAndLeash] \
		    $type $subtype $name]
	}
    }
}

proc editPolygonColors {polygonType} {
    set w .polygonColors

    if {[info commands $w] != ""} {
	destroy $w
    }

    toplevel $w
    set frame [frame $w.f]
    pack $frame

    set row 0

    global rainbowWidth rainbow

    switch $polygonType {
	Weather {
	    global weatherTypes weatherRainbow

	    set label [label $frame.weatherLabel -text "Weather Colors"]
	    grid $label -row $row -column 0 -columnspan 2
	    incr row

	    set index 0
	    foreach weatherType $weatherTypes {
		set label [label $frame.weatherColor${index} -text $weatherType]
		grid $label -row $row -column 0
		set label [label $frame.weatherColor${index}color -text " " -bg $weatherRainbow($weatherType)]
		grid $label -row $row -column 1
		bind $label <Button-1> [list choosePolygonColor $label weatherRainbow($weatherType)]

		incr index
		incr row
	    }
	}

	RegionObstacle {
	    global obstacleTypes obstacleRainbow

	    set label [label $frame.obstacleLabel -text "Region Obstacle Colors"]
	    grid $label -row $row -column 0 -columnspan 2
	    incr row

	    set index 0
	    foreach obstacleType $obstacleTypes {
		set label [label $frame.obstacleColor${index} -text $obstacleType]
		grid $label -row $row -column 0
		set label [label $frame.obstacleColor${index}color -text " " -bg $obstacleRainbow($obstacleType)]
		grid $label -row $row -column 1
		bind $label <Button-1> [list choosePolygonColor $label obstacleRainbow($obstacleType)]

		incr index
		incr row
	    }
	}
	
	Obstacle {
	    global lineObstacleColor

	    set label [label $frame.lineObstacleLabel -text "Line Obstacle Color"]
	    grid $label -row $row -column 0 -columnspan 2
	    incr row

	    set label [label $frame.lineObstacleColorColor -text "Color"]
	    grid $label -row $row -column 0
	    set label [label $frame.lineObstacleColor -text " " -bg $lineObstacleColor]
	    grid $label -row $row -column 1
	    bind $label <Button-1> [list choosePolygonColor $label lineObstacleColor]

	    incr row
	}
    }

    set button [button $frame.done -text "Done" -command "destroy $w"]
    grid $button -row $row -column 0 -sticky ew -columnspan 2
}

proc choosePolygonColor {label color} {
    global weatherRainbow obstacleRainbow

    set chosenColor [chooseColor]
    if {$chosenColor == ""} {
	return
    }

    set $color $chosenColor
    $label configure -bg $chosenColor
}

proc initPolygons {} {
    global weatherTypes obstacleTypes
    global weatherRainbow obstacleRainbow
    global rainbowWidth rainbow
    global lineObstacleColor

    set selectedWeatherTypes [Data_Select tma_terrain {TERRAIN_ID} [list [list APPLICATION_ID [applicationId]] {TYPE WEATHER}]]
    set weatherTypes {}
    set index 0
    foreach weatherType $selectedWeatherTypes {
	if {[lsearch $weatherTypes $weatherType] < 0} {
	    lappend weatherTypes $weatherType
	    set weatherRainbow($weatherType) $rainbow([expr $index % $rainbowWidth])
	    incr index
	}
    }

    set obstacleTypes  {"0.0" "0.25" "0.50" "0.75"}
    set index 0
    foreach obstacleType $obstacleTypes {
	set obstacleRainbow($obstacleType) $rainbow([expr $index % $rainbowWidth])
	incr index
    }

    set lineObstacleColor $rainbow(0)

    global aoiPolygons aoiPolygonIds polygonObject
    set aoiPolygons {}
    set aoiPolygonIds {}
    set polygonObject ""
}

