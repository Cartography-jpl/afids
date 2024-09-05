#   DESCRIPTION     : This module provides the functions for creating the
#                     minimum path dialog and performing the minimum path
#                     analysis.
#
#       PUBLIC      : minPathDialog - creates the dialog
#
#       PRIVATE     : minPath - performs the analysis

package provide TMASRV 1.0
package require helpers

proc minPathDialog {} {
    global currentMover startLocation endLocation env

    set w .minPath

    if {[info commands $w] != ""} {
	destroy $w
    }

    removeAllDots

    toplevel $w -class Dialog

    wm title $w "Minimum Path"
    wm resizable $w 0 0

    set fEntries $w.entries
    frame $fEntries
    pack $fEntries

    set gridRow 0

    # Mover Id
    set button $fEntries.moverIdButton
    button $button -text "Mover" -command {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]}
    grid $button -row $gridRow -column 0 -sticky nesw

    set entry $fEntries.moverIdEntry
    entry $entry -textvariable currentMover(code)
    grid $entry -row $gridRow -column 1

    incr gridRow

    # Start Location
    # Single Start Location
    set button $fEntries.startLocationButton
    button $button -text "Start Location" -command "getQueryLocation start \$singleColor(MinPath)"
    grid $button -row $gridRow -column 0  -sticky nesw

    set entry $fEntries.startLocationEntry
    entry $entry -textvariable startLocation
    grid $entry -row $gridRow -column 1
    
    incr gridRow

    # End Location
    set button $fEntries.endLocationButton
    button $button -text "End Location" -command "getQueryLocation end \$singleColor(MinPath)"
    grid $button -row $gridRow -column 0  -sticky nesw

    set entry $fEntries.endLocationEntry
    entry $entry -textvariable endLocation
    grid $entry -row $gridRow -column 1
    
    incr gridRow

    # Single color
    set button $fEntries.colorButton
    set label $fEntries.singleColor
    button $button -text "Path Color" -command "editSingleColor $label MinPath"
    grid $button -row $gridRow -column 0

    global singleColor
    label $label -text " " -bg $singleColor(MinPath)
    grid $label -row $gridRow -column 1 -sticky ew
    bind $label <ButtonPress> "editSingleColor $label MinPath"

    incr gridRow

    # Analysis name
    set label $fEntries.analysisNameLabel
    label $label -text "Analysis Name"
    grid $label -row $gridRow -column 0

    set entry $fEntries.analysisNameEntry
    entry $entry -textvariable analysisName
    grid $entry -row $gridRow -column 1

    incr gridRow

    # Analysis results
    set label $fEntries.timeResultLabel
    label $label -text "Time"
    grid $label -row $gridRow -column 0

    set label $fEntries.timeResultValue
    label $label -textvariable minPathTimeResult
    grid $label -row $gridRow -column 1

    incr gridRow

    set label $fEntries.lengthResultLabel
    label $label -text "Length"
    grid $label -row $gridRow -column 0

    set label $fEntries.lengthResultValue
    label $label -textvariable minPathLengthResult
    grid $label -row $gridRow -column 1

    # The command buttons at the bottom
    frame $w.commands
    pack $w.commands
    set button  $w.commands.execute
    button $button -text Execute -command "minPath 0"
    pack $button -side left
    set button $w.commands.cancel
    button $button -text Cancel -command "removeAllDots; destroy $w"
    pack $button -side left

    # Auto forward to mover selection
    update
    Gui_SelectMover .d "Select Mover" [getMoverDescriptions]

    # Auto forward to map for points
    global singleColor
    preloadTwoLocations $singleColor(MinPath)
}

proc preloadTwoLocations {color} {
    global multiSourceLocations
    global workingMultiDots

    set locationCount [getMultiLocation noListboxNeeded $color 2]

    # got start location
    if {$locationCount >= 1} {
	global startLocationCoord startLocation workingStartDot

	set startLocationCoord [lindex $multiSourceLocations 0]
	removeDots start
	set startLocation [Map_CoordToString $startLocationCoord]
	set workingStartDot [lindex $workingMultiDots 0]
    }

    # got end location
    if {$locationCount == 2} {
	global endLocationCoord endLocation workingEndDot

	set endLocationCoord [lindex $multiSourceLocations 1]
	removeDots end
	set endLocation [Map_CoordToString $endLocationCoord]
	set workingEndDot [lindex $workingMultiDots 1]
    }
}

# legNumber is used by analystPath
proc minPath {legNumber} {
    global currentMover _guiMoverCosts aoi startLocation endLocation
    global minPathTimeResult minPathLengthResult
    global env
    global analysisName 

    if {$currentMover(code) == "" || $currentMover(code) < 0} {
	tk_messageBox -message "Please select a mover"
	return
    }

    if {$startLocation == ""} {
	tk_messageBox -message "Please select a start location"
	return
    }

    if {$endLocation == ""} {
	tk_messageBox -message "Please select an end location"
	return
    }

    if {$analysisName == ""} {
	tk_messageBox -message "Please enter a name for this analysis"
	return
    }

    set rnfName [findOrComputeAoiData]

    puts "aoi = $aoi"

    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set lines [lindex $aoi 2]
    set samples [lindex $aoi 3]

    set n [lindex $nw 0]
    set s [lindex $se 0]
    set e [lindex $se 1]
    set w [lindex $nw 1]

    set mapRegionName [getMapHandle]
    puts "mapRegionName = $mapRegionName"
    set regionName [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "regionName = $regionName"

    puts "Calling MinPathCmd $regionName [list $n $w] [list $s $e]"
    puts "$currentMover(code) [string2Coord $startLocation] [string2Coord $endLocation] $rnfName"

    set aoiId [lindex $aoi 4]

    global vectorDataSetType
    puts "MinPathCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) [string2Coord $startLocation] [string2Coord $endLocation] $rnfName $vectorDataSetType $aoiId"

    MinPathCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) [string2Coord $startLocation] [string2Coord $endLocation] $rnfName $vectorDataSetType $aoiId

    set pathFile [open $env(TMA_DATA)/minPath.dat]
    # time.
    gets $pathFile line

    if {[catch {set minutes [expr int($line)]}]} {
	tk_messageBox -message "Infinite path time indicates that no path is possible between the specified Start and End Locations within this AOI. Excursions outside the AOI may provide possible paths."
	return
    }
    set hours [expr int($minutes/60.0)]
    set minutes [expr int($minutes - (60.0 * $hours))]
    set minPathTimeResult "$hours hr $minutes min"
    # length.
    gets $pathFile line
    set minPathLengthResult [format "%.1f km" [expr $line / 1000.0]]
    # number of points.
    gets $pathFile line
    set minPathGraphics ""
    while { [gets $pathFile line] >= 0 } {
	lappend minPathGraphics [Map_StringToCoord $line]
    }
    close $pathFile

    #puts "Path data :"
    #puts "*************************************************************************"
    #puts $minPathGraphics

    global singleColor
    if {$legNumber > 0} {
	set pathId [Map_DrawPolyline [getMapHandle] $minPathGraphics $singleColor(AnalystPath) 3]

	set startLatLon [lindex $minPathGraphics 0]

	set textAndLeash [Map_Text2 [getMapHandle] "Leg ${legNumber}\n${minPathTimeResult}\n${minPathLengthResult}" $startLatLon $singleColor(AnalystPath) minPathLabel]
    } else {
	set pathId [Map_DrawPolyline [getMapHandle] $minPathGraphics $singleColor(MinPath) 3]

	set startLatLon [lindex $minPathGraphics 0]

	set textAndLeash [Map_Text2 [getMapHandle] "${minPathTimeResult}\n${minPathLengthResult}" $startLatLon $singleColor(MinPath) minPathLabel]
    }

    global workingStartDot workingEndDot

    if {$legNumber == 0} {
	# if the start point is not on the map, create it
	if {$workingStartDot != ""} {
	    Map_EraseObject [getMapHandle] $workingStartDot
	}

	global startLocationCoord
	set circleId [Map_DrawFilledCircle $mapRegionName $startLocationCoord 10 $singleColor(MinPath)]
	set workingStartDot $circleId

	# if the end point is not on the map, create it
	if {$workingEndDot != ""} {
	    Map_EraseObject [getMapHandle] $workingEndDot
	}

	global endLocationCoord
	set circleId [Map_DrawFilledCircle $mapRegionName $endLocationCoord 10 $singleColor(MinPath)]
	set workingEndDot $circleId
    }

    set canvasIds [concat $workingStartDot $workingEndDot $textAndLeash $pathId]
    set workingStartDot {}
    set workingEndDot {}
    set workingMultiDots {}

    if {$legNumber > 0} {
	return $canvasIds
    } else {
	addAnalysisHistoryItem Path $analysisName $canvasIds
    }
}
