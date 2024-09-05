#   DESCRIPTION     : This module provides the functions for creating the
#                     analyst path dialog and performing the analyst path
#                     analysis.
#
#       PUBLIC      : analystPathDialog - creates the dialog
#
#       PRIVATE     : analystPath - performs the analysis

package provide TMASRV 1.0


proc analystPathDialog {} {
    global currentMover

    set w .analystPath

    if {[info commands $w] != ""} {
	destroy $w
    }

    removeAllDots

    toplevel $w -class Dialog

    wm title $w "Analyst Path"
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
    grid $entry -row $gridRow -column 1 -sticky nesw

    incr gridRow

    # Multiple Locations
    set button $fEntries.multiLocationsButton
    set listFrame $fEntries.multiLocationsListbox
    set listbox $listFrame.list

    button $button -text "Locations" -command "getMultiLocation $listbox \$singleColor(AnalystPath)"
    grid $button -row $gridRow -column 0  -sticky ew

    frame $listFrame
    set listScroll $listFrame.sy
    listbox $listbox -height 5 -width 25 -yscrollcommand [list $listScroll set]
    scrollbar $listScroll -orient vertical -command [list $listbox yview]
    pack $listScroll -side right -fill y
    pack $listbox -side left -fill both -expand true
    grid $listFrame -row $gridRow -column 1 -sticky nesw

    incr gridRow

    # Single color
    set button $fEntries.colorButton
    set label $fEntries.singleColor
    button $button -text "Path Color" -command "editSingleColor $label AnalystPath"
    grid $button -row $gridRow -column 0 -sticky nesw

    global singleColor
    label $label -text " " -bg $singleColor(AnalystPath)
    grid $label -row $gridRow -column 1 -sticky ew
    bind $label <ButtonPress> "editSingleColor $label AnalystPath"

    incr gridRow

    # Analysis name
    set label $fEntries.analysisNameLabel
    label $label -text "Analysis Name"
    grid $label -row $gridRow -column 0 -sticky nesw

    set entry $fEntries.analysisNameEntry
    entry $entry -textvariable analysisName
    grid $entry -row $gridRow -column 1 -sticky nesw

    incr gridRow

    # Analysis results
    set label $fEntries.timeResultLabel
    label $label -text "Time"
    grid $label -row $gridRow -column 0 -sticky nesw

    set label $fEntries.timeResultValue
    label $label -textvariable analystPathTimeResult
    grid $label -row $gridRow -column 1 -sticky nesw

    incr gridRow

    set label $fEntries.lengthResultLabel
    label $label -text "Length"
    grid $label -row $gridRow -column 0 -sticky nesw

    set label $fEntries.lengthResultValue
    label $label -textvariable analystPathLengthResult
    grid $label -row $gridRow -column 1 -sticky nesw

    # The command buttons at the bottom
    frame $w.commands
    pack $w.commands
    set button  $w.commands.execute
    button $button -text Execute -command "analystPath $listbox"
    pack $button -side left
    set button $w.commands.cancel
    button $button -text Cancel -command "removeAllDots; destroy $w"
    pack $button -side left

    # Auto forward to mover selection
    update
    Gui_SelectMover .d "Select Mover" [getMoverDescriptions]

    # Auto forward to map for points
    getMultiLocation $listbox $singleColor(AnalystPath)
}

proc analystPath {listbox} {
    global startLocation endLocation env
    global analystPathTimeResult analystPathLengthResult
    global currentMover

    if {$currentMover(code) < 0} {
	tk_messageBox -message "Please select a mover"
	return
    }

    set analystPathTimeResult 0
    set analystPathLengthResult 0
    set numPoints 0

    # Open analyst.dat - truncate for writing.
    set outFile [open "$env(TMA_DATA)/analystPath.dat" w]
    set locations [$listbox get 0 end]
    set canvasIds {}
    set legNumber 1

    # Go through locations in listbox in analyst dialog.
    set startLocation [lindex $locations 0]
    foreach location [lrange $locations 1 end] {
	set endLocation $location

	set canvasIds [concat $canvasIds [minPath $legNumber]]
	incr legNumber

	# Append minPath result to analystPath result.

	set pathFile [open "$env(TMA_DATA)/minPath.dat" r]
	# time.
	gets $pathFile line
	set analystPathTimeResult [expr $analystPathTimeResult + $line]
	# length.
	gets $pathFile line
	set analystPathLengthResult [expr $analystPathLengthResult + $line]

	# number of points.
	gets $pathFile line
	set numPoints [expr $numPoints + $line]
	while { [gets $pathFile line] >= 0 } {
	    puts $outFile $line
	}
	close $pathFile
	set startLocation $endLocation
    }

    close $outFile

    # Prepend time, length, number points to outFile.
    set outFile [open "$env(TMA_DATA)/minPath.dat" w]
    set inFile [open "$env(TMA_DATA)/analystPath.dat" r]
    puts $outFile $analystPathTimeResult
    puts $outFile $analystPathLengthResult
    puts $outFile $numPoints
    while { [gets $inFile line] >= 0 } {
	puts $outFile $line
    }

    # Convert to nice format for gui display.
    # time.
    set minutes [expr int($analystPathTimeResult)]
    set hours [expr int($minutes/60.0)]
    set minutes [expr int($minutes - (60.0 * $hours))]
    set analystPathTimeResult "$hours hr $minutes min"
    # length.
    set analystPathLengthResult [format "%.1f km" [expr $analystPathLengthResult / 1000.0]]

    global multiSourceLocations

    # if the start points are not on the map, create them
    set map [getMapHandle]
    global workingMultiDots singleColor
    if {$workingMultiDots != ""} {
	foreach dot $workingMultiDots {
	    Map_EraseObject [getMapHandle] $dot
	}
    }

    foreach locationCoord $multiSourceLocations {
	set circleId [Map_DrawFilledCircle $map $locationCoord 10 $singleColor(AnalystPath)]
	lappend workingMultiDots $circleId
    }

    # annotate total time/length
    set textAndLeash [Map_Text2 [getMapHandle] "Total\n${analystPathTimeResult}\n${analystPathLengthResult}" [lindex $multiSourceLocations end] $singleColor(AnalystPath) minPathLabel]

    global analysisName
    addAnalysisHistoryItem Path $analysisName [concat $canvasIds $workingMultiDots $textAndLeash]
    set workingMultiDots {}
}

proc getAnalystPath {list} {
    set location "not null"

    while {$location != ""} {
	set location [Map_PickCoord [getMapHandle]]

	if {$location != ""} {
	    appendAnalystLocation [Map_CoordToString $location] $list
	}
    }
}