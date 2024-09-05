#   DESCRIPTION     : This module contains the procs used to create the three
#                     contour analysis input dialogs and execute the analysis.
#
#       PUBLIC      : contoursDialog {type}
#                     Creates contour dialog in one of three forms.
#                     type is one of Isochronal, Corridor or MultiSource.
#
#       PRIVATE     : These three procs perform the contour analysis:
#                     isoContours
#                     corridorContour
#                     multiSourceContours

package provide TMASRV 1.0

proc contoursDialog {type} {
    global currentMover startLocation deltaTime numContours env
    global endLocation maxTime

    set w .contours

    if {[info commands $w] != ""} {
	destroy $w
    }

    removeAllDots

    toplevel $w -class Dialog

    switch $type {
	Isochronal {
	    set title "Isochronal Contours"
	}
	Corridor {
	    set title "Corridor Contours"
	}
	MultiSource {
	    set title "Multi Source Contours"
	}
	default {
	    # fail silently
	    return
	}
    }

    wm title $w $title
    wm resizable $w 0 0

    # Everything except the command buttons at the bottom
    frame $w.main
    pack $w.main

    # Entries on the left
    set fEntries $w.main.entries
    frame $fEntries
    pack $fEntries -side left

    set gridRow 0

    # Mover Id
    set button $fEntries.moverIdButton
    button $button -text "Mover" -command {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]}
    grid $button -row $gridRow -column 0 -sticky nesw

    set entry $fEntries.moverIdEntry
    entry $entry -textvariable currentMover(code)
    grid $entry -row $gridRow -column 1 -sticky nesw

    incr gridRow

    # Start Location
    if {[string compare $type MultiSource]} {
	# Single Start Location
	set button $fEntries.startLocationButton
	button $button -text "Start Location" -command "getQueryLocation start \$singleColor($type)"
	grid $button -row $gridRow -column 0  -sticky nesw

	set entry $fEntries.startLocationEntry
	entry $entry -textvariable startLocation
	grid $entry -row $gridRow -column 1 -sticky nesw
    } else {
	# Multiple Start Locations
	set button $fEntries.multiLocationsButton
	set listFrame $fEntries.multiLocationsListbox
	set listbox $listFrame.list

	button $button -text "Locations" -command "getMultiLocation $listbox \$singleColor($type)"
	grid $button -row $gridRow -column 0  -sticky ew

	frame $listFrame
	set listScroll $listFrame.sy
	listbox $listbox -height 5 -width 25 -yscrollcommand [list $listScroll set]
	scrollbar $listScroll -orient vertical -command [list $listbox yview]
	pack $listScroll -side right -fill y
	pack $listbox -side left -fill both -expand true
	grid $listFrame -row $gridRow -column 1 -sticky nesw
    }
    
    incr gridRow

    # Only Corridor has End Location and Max Time
    # All but Corridor have Delta Time and Num Contours
    if {! [string compare $type Corridor]} {
	# End Location
	set button $fEntries.endLocationButton
	button $button -text "End Location" -command "getQueryLocation end \$singleColor($type)"
	grid $button -row $gridRow -column 0  -sticky nesw

	set entry $fEntries.endLocationEntry
	entry $entry -textvariable endLocation
	grid $entry -row $gridRow -column 1 -sticky nesw
    
	incr gridRow

	# Max Time
	set label $fEntries.maxTimeLabel
	label $label -text "Max Time (min)"
	grid $label -row $gridRow -column 0 -sticky nesw

	set entry $fEntries.maxTimeEntry
	entry $entry -textvariable maxTime
	grid $entry -row $gridRow -column 1 -sticky nesw

	incr gridRow

    } else {
	# Delta Time
	set label $fEntries.deltaTimeLabel
	label $label -text "Time Interval (min)"
	grid $label -row $gridRow -column 0 -sticky nesw

	set entry $fEntries.deltaTimeEntry
	entry $entry -textvariable deltaTime
	grid $entry -row $gridRow -column 1 -sticky nesw

	incr gridRow

	# Num Contours
	set label $fEntries.numContoursLabel
	label $label -text "Number"
	grid $label -row $gridRow -column 0 -sticky nesw

	set entry $fEntries.numContoursEntry
	entry $entry -textvariable numContours
	grid $entry -row $gridRow -column 1 -sticky nesw

	incr gridRow
    }

    # Analysis name
    set label $fEntries.analysisNameLabel
    label $label -text "Analysis Name"
    grid $label -row $gridRow -column 0 -sticky nesw

    set entry $fEntries.analysisNameEntry
    entry $entry -textvariable analysisName
    grid $entry -row $gridRow -column 1 -sticky nesw

    # The color choices at the right
    set fColorChoices $w.main.colorChoices
    frame $fColorChoices
    pack $fColorChoices -side left

    # Multi colors
    set fRainbow $fColorChoices.rainbowColors
    frame $fRainbow
    pack $fRainbow -anchor w

    set label $fRainbow.label
    label $label -text "Multicolors"
    pack $label -side left

    global rainbow rainbowWidth
    for {set index 0} {$index < $rainbowWidth} {incr index} {
	set label $fRainbow.l$index
	label $label -text " " -bg $rainbow($index)
	pack $label -side left
	bind $label <ButtonPress> "editOneColor $index; $label configure -bg \$rainbow($index)"
    }

    # Single color
    set fSingle $fColorChoices.singleColor
    frame $fSingle
    pack $fSingle -anchor w

    set label $fSingle.label
    label $label -text "Single Color"
    pack $label -side left

    global singleColor
    set label $fSingle.color
    label $label -text " " -bg $singleColor($type)
    pack $label -side left -pady 5
    bind $label <ButtonPress> "editSingleColor $label $type"

    set radiobutton $fColorChoices.fillContours
    radiobutton $radiobutton -text "Color Coded Filled Contours" -variable contourMode -value filled
    pack $radiobutton -anchor w
    set radiobutton $fColorChoices.codedContours
    radiobutton $radiobutton -text "Color Coded Lined Contours" -variable contourMode -value coded
    pack $radiobutton -anchor w
    set radiobutton $fColorChoices.monochromeContours
    radiobutton $radiobutton -text "Monochrome Lined Contours" -variable contourMode -value monochrome
    pack $radiobutton -anchor w
#      set radiobutton $fColorChoices.legacyContours
#      radiobutton $radiobutton -text "Legacy Lined Contours" -variable contourMode -value legacy
#      pack $radiobutton -anchor w

    # The command buttons at the bottom
    frame $w.commands
    pack $w.commands

    switch $type {
	Isochronal {
	    set command isoContours
	}
	Corridor {
	    set command corridorContour
	}
	MultiSource {
	    set command multiSourceContours
	}
    }

    set button  $w.commands.execute
    button $button -text Execute -command $command
    pack $button -side left
    set button $w.commands.cancel
    button $button -text Cancel -command "removeAllDots; destroy $w"
    pack $button -side left

    # Auto forward to mover selection
    update
    Gui_SelectMover .d "Select Mover" [getMoverDescriptions]

    # Auto forward to get locations
    switch $type {
	Isochronal {
	    getQueryLocation start $singleColor($type)
	}
	Corridor {
	    global singleColor
	    preloadTwoLocations $singleColor($type)
	}
	MultiSource {
	    getMultiLocation $listbox $singleColor($type)
	}
    }
}
#------------------------------------------------------------------------------
proc isoContours {} {
    global _guiMoverCosts startLocation aoi currentMover deltaTime numContours env analysisName

    if {$currentMover(code) == "" || $currentMover(code) < 0} {
	tk_messageBox -message "Please select a mover"
	return
    }

    if {$startLocation == ""} {
	tk_messageBox -message "Please select a start location"
	return
    }

    if {$deltaTime == ""} {
	tk_messageBox -message "Please enter the time interval between contours"
	return
    }

    if {$numContours == ""} {
	tk_messageBox -message "Please enter the number of contours"
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
    set aoiId [lindex $aoi 4]

    set height $lines
    set width $samples
    set n [lindex $nw 0]
    set s [lindex $se 0]
    set e [lindex $se 1]
    set w [lindex $nw 1]

    set mapRegionName [getMapHandle]
    set regionName \
	    [lindex [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]

    global vectorDataSetType
    ContoursCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) [string2Coord $startLocation] \
	    $rnfName $vectorDataSetType $aoiId

    set contourDisplayList {}    
    global contourMode
    global rainbow rainbowWidth
    global env singleColor

    set levels {}
    for {set contour 0} {$contour < $numContours} {incr contour} {
	lappend levels [expr $deltaTime * ($contour + 1)]
    }
    set cmd [concat [list exec mintime2contours $lines $samples \
	    "$env(TMA_DATA)/minTimeSoFar.dat" \
	    "$env(TMA_DATA)/contourOverlay.pgm"] $levels]
    puts $cmd
    eval $cmd
    set cmd [list exec overlay2polygons "$env(TMA_DATA)/contourOverlay.pgm" \
	    "$env(TMA_DATA)/contours.dat" $numContours]
    puts $cmd
    eval $cmd

    # options: monochrome coded filled
    if {$contourMode != "monochrome"} {
	set colors {}
	for {set contour 0} {$contour < $numContours} {incr contour} {
	    lappend colors [lindex [split \
		    $rainbow([expr $contour % $rainbowWidth])] 0]
	}
    }

    # remove the start point from map and then create it so color is right
    if {$contourMode == "monochrome"} {
	set color $singleColor(Isochronal)
    } else {
	set color [lindex $colors 0]
    }
    removeDots start
    global workingStartDot
    global startLocationCoord
    set circleId [Map_DrawFilledCircle $mapRegionName \
	    $startLocationCoord 10 $color]
    
    # This is for the labels on the contours
    # Put a label on the largest contour for each level
    # These 2 variables will be used to keep track of the
    # largest number of point in each level and the point where the
    # text will be put for each level. 
    set maxPointsByLevel {}
    set labelPointByLevel {}
    foreach level $levels {
	lappend maxPointsByLevel 0
	lappend labelPointByLevel {}
    }
    set contoursFile [open "$env(TMA_DATA)/contours.dat"]
    while { [gets $contoursFile line] >= 0 } {
	set level [lindex $line 0]
	set numPoints [lindex $line 1]
	set contoursGraphics ""
	for {set pointNum 0} {$pointNum < $numPoints} {incr pointNum} {
	    gets $contoursFile line
	    # Convert line sample coord in aoi space to lat lon
	    set linePixel   [lindex $line 0]
	    set samplePixel [lindex $line 1]
	    set coords [list \
		    [expr (((( $s - $n ) / $height ) * $linePixel   ) + $n )] \
		    [expr (((( $e - $w ) / $width  ) * $samplePixel ) + $w )]]
	    lappend contoursGraphics $coords
	    # Keep track of where leash should go
	    if {$pointNum == 0} {
		if {$numPoints > [lindex $maxPointsByLevel $level]} {
		    set maxPointsByLevel \
			    [lreplace $maxPointsByLevel $level $level \
			    $numPoints]
		    set labelPointByLevel \
			    [lreplace $labelPointByLevel $level $level \
			    $coords]
		}
	    }
	}
	#puts $contoursGraphics
	if {$contourMode == "monochrome"} {
	    set color $singleColor(Isochronal)
	} else {
	    set color [lindex $colors $level]
	}
	lappend contourDisplayList \
		[Map_DrawPolyline $mapRegionName \
		$contoursGraphics \
		$color 3]
	set contoursGraphics ""
    }
    close $contoursFile

    set textAndLeashList {}
    for {set levelNum 0} {$levelNum < $numContours} {incr levelNum} {
	if {$contourMode == "monochrome"} {
	    set color $singleColor(Isochronal)
	} else {
	    set color [lindex $colors $levelNum]
	}

	set minutes [expr $deltaTime * ($levelNum + 1)]
	set hours [expr int($minutes/60.0)]
	set minutes [expr int($minutes - (60.0 * $hours))]
	set timeLabel "$hours hr $minutes min"
	set textAndLeash [Map_Text2 $mapRegionName \
		$timeLabel \
		[lindex $labelPointByLevel $levelNum] \
		$color contoursLabel]
	set textAndLeashList [concat $textAndLeashList $textAndLeash]
    }

    # filled option. Uses overlay.
    set image ""
    if {$contourMode == "filled"} {
	set colors2 {}
	foreach color $colors {
	    # color2 is hex color. example = 0xff0000 (red).
	    set color2 "0x[string range $color 1 end]"
	    lappend colors2 $color2 $color2
	}
	#set x0 [MapSRV_LonToPix $mapRegionName $w]
	set y1 [MapSRV_LatToPix $mapRegionName $s]
	#set x1 [MapSRV_LonToPix $mapRegionName $e]
	set y0 [MapSRV_LatToPix $mapRegionName $n]
	set scale [expr abs($y0 - $y1)/${lines}.0]
	set cmd [concat [list exec pgm2ppm \
		"$env(TMA_DATA)/contourOverlay.pgm" \
		"$env(TMA_DATA)/contourOverlay.ppm" $scale] $colors2]
	puts $cmd
	eval $cmd
	set cmd [list exec ppmtogif -transparent rgb:0/0/0 \
		"$env(TMA_DATA)/contourOverlay.ppm" ">" \
		"$env(TMA_DATA)/contourOverlay.gif"]
	puts $cmd
	catch {eval $cmd}
	if {[catch {
	    set image [Map_OverlayPhoto $mapRegionName \
		    "$env(TMA_DATA)/contourOverlay.gif" $w $n]
	}]} {
	    tk_messageBox -message \
		    "Failed to overlay image. Tk may be out of memory."
	}
    }
    global analysisName
    if {$image == ""} {
	addAnalysisHistoryItem Contour $analysisName \
		[concat $circleId $textAndLeashList $contourDisplayList]
    } else {
	addAnalysisHistoryItem Contour $analysisName \
		[concat $circleId $textAndLeashList $contourDisplayList $image]
    }
}
#-----------------------------------------------------------------------------
proc corridorContour {} {
    global _guiMoverCosts startLocation endLocation aoi
    global currentMover maxTime env

    if {$maxTime == ""} {
	tk_messageBox -message "Please select a max time"
	return
    }
    
    set rnfName [findOrComputeAoiData]
    puts "aoi = $aoi"

    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set lines [lindex $aoi 2]
    set samples [lindex $aoi 3]
    set aoiId [lindex $aoi 4]

    set height $lines
    set width $samples
    set n [lindex $nw 0]
    set s [lindex $se 0]
    set e [lindex $se 1]
    set w [lindex $nw 1]

    set mapRegionName [getMapHandle]
    set regionName [lindex \
	    [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]

    global vectorDataSetType
    CorridorCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) \
	    [string2Coord $startLocation] \
	    [string2Coord $endLocation] \
	    $rnfName $vectorDataSetType $aoiId
    
    set contourDisplayList {}
    global contourMode
    global env singleColor

    set numCorridor 1
    set cmd [concat [list exec mintime2contours $lines $samples \
	    "$env(TMA_DATA)/minTimeSoFar.dat" \
	    "$env(TMA_DATA)/corridorOverlay.pgm"] $maxTime]
    puts $cmd
    eval $cmd
    set cmd [list exec overlay2polygons "$env(TMA_DATA)/corridorOverlay.pgm" \
	    "$env(TMA_DATA)/corridor.dat" 1]
    puts $cmd
    eval $cmd

    # remove the start and end point from map and
    # then create it so color is right
    set color $singleColor(Corridor)
    #removeDots start
    #removeDots end
    removeDots multi
    global workingStartDot
    global startLocationCoord endLocationCoord
    set startCircleId [Map_DrawFilledCircle $mapRegionName \
	    $startLocationCoord 10 $color]
    set endCircleId [Map_DrawFilledCircle $mapRegionName \
	    $endLocationCoord 10 $color]

    # Check for max time too long (check if corridorOverlay.dat is empty)
    set file [open "$env(TMA_DATA)/corridor.dat" r]
    set line ""
    gets $file line
    close $file
    if {$line == ""} {
	tk_messageBox -message "Max Time was less than the min path between Start and End Locations; No contour possible."
	return
    }
    set levels [list $maxTime]

    # This is for the labels on the contours
    # Put a label on the largest contour for each level
    # These 2 variables will be used to keep track of the
    # largest number of point in each level and the point where the
    # text will be put for each level. 
    set maxPointsByLevel {}
    set labelPointByLevel {}
    foreach level $levels {
	lappend maxPointsByLevel 0
	lappend labelPointByLevel {}
    }
    set corridorFile [open "$env(TMA_DATA)/corridor.dat"]
    while { [gets $corridorFile line] >= 0 } {
	set level [lindex $line 0]
	set numPoints [lindex $line 1]
	set corridorGraphics ""
	for {set pointNum 0} {$pointNum < $numPoints} {incr pointNum} {
	    gets $corridorFile line
	    # Convert line sample coord in aoi space to lat lon
	    set linePixel   [lindex $line 0]
	    set samplePixel [lindex $line 1]
	    set coords [list \
		    [expr (((( $s - $n ) / $height ) * $linePixel   ) + $n )] \
		    [expr (((( $e - $w ) / $width  ) * $samplePixel ) + $w )]]
	    lappend corridorGraphics $coords
	    # Keep track of where leash should go
	    if {$pointNum == 0} {
		if {$numPoints > [lindex $maxPointsByLevel $level]} {
		    set maxPointsByLevel \
			    [lreplace $maxPointsByLevel $level $level \
			    $numPoints]
		    set labelPointByLevel \
			    [lreplace $labelPointByLevel $level $level \
			    $coords]
		}
	    }
	}
	lappend corridorDisplayList \
		[Map_DrawPolyline $mapRegionName \
		$corridorGraphics \
		$color 3]
	set corridorGraphics ""
    }
    close $corridorFile
    
    set textAndLeashList {}
    for {set levelNum 0} {$levelNum < $numCorridor} {incr levelNum} {
	set hours [expr int($maxTime/60.0)]
	set minutes [expr int($maxTime - (60.0 * $hours))]
	set timeLabel "$hours hr $minutes min"
	set textAndLeash [Map_Text2 $mapRegionName \
		$timeLabel \
		[lindex $labelPointByLevel $levelNum] \
		$color corridorLabel]
	set textAndLeashList [concat $textAndLeashList $textAndLeash]
    }
    
    # filled option. Uses overlay.
    set colors [list $color]
    set image ""
    if {$contourMode == "filled"} {
	set colors2 {}
	foreach color $colors {
	    # color2 is hex color. example = 0xff0000 (red).
	    set color2 "0x[string range $color 1 end]"
	    lappend colors2 $color2 $color2
	}
	#set x0 [MapSRV_LonToPix $mapRegionName $w]
	set y1 [MapSRV_LatToPix $mapRegionName $s]
	#set x1 [MapSRV_LonToPix $mapRegionName $e]
	set y0 [MapSRV_LatToPix $mapRegionName $n]
	set scale [expr abs($y0 - $y1)/${lines}.0]
	set cmd [concat [list exec pgm2ppm \
		"$env(TMA_DATA)/corridorOverlay.pgm" \
		"$env(TMA_DATA)/corridorOverlay.ppm" $scale] $colors2]
	puts $cmd
	eval $cmd
	set cmd [list exec ppmtogif -transparent rgb:0/0/0 \
		"$env(TMA_DATA)/corridorOverlay.ppm" ">" \
		"$env(TMA_DATA)/corridorOverlay.gif"]
	puts $cmd
	catch {eval $cmd}
	if {[catch {
	    set image [Map_OverlayPhoto $mapRegionName \
		    "$env(TMA_DATA)/corridorOverlay.gif" $w $n]
	}]} {
	    tk_messageBox -message \
		    "Failed to overlay image. Tk may be out of memory."
	}
    }

    global analysisName
    if {$image == ""} {
	addAnalysisHistoryItem Corridor $analysisName \
		[concat $startCircleId $endCircleId $textAndLeashList \
		$corridorDisplayList]
    } else {
	addAnalysisHistoryItem Corridor $analysisName \
		[concat $startCircleId $endCircleId $textAndLeashList \
		$corridorDisplayList $image]
    }
}
#------------------------------------------------------------------------------
proc multiSourceContours {} {
    global workingMultiDots
    global currentMover
    global multiSourceLocations
    global aoi
    global deltaTime numContours 
    global env

    if {$currentMover(code) < 0} {
	tk_messageBox -message "Please select a mover"
	return
    }

    if {[llength $multiSourceLocations] < 2} {
	tk_messageBox -message "Please select at least two source locations"
	return
    }

    set rnfName [findOrComputeAoiData]

    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set lines [lindex $aoi 2]
    set samples [lindex $aoi 3]
    set aoiId [lindex $aoi 4]

    set height $lines
    set width $samples
    set n [lindex $nw 0]
    set s [lindex $se 0]
    set e [lindex $se 1]
    set w [lindex $nw 1]

    set mapRegionName [getMapHandle]
    set regionName [lindex [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]

    global vectorDataSetType
    MultiSourceContoursCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) $multiSourceLocations \
	    $rnfName $vectorDataSetType $aoiId
 
    set contourDisplayList {}   
    global contourMode
    global rainbow rainbowWidth
    global env singleColor

    set levels {}
    for {set contour 0} {$contour < $numContours} {incr contour} {
	lappend levels [expr $deltaTime * ($contour + 1)]
    }
    set cmd [concat [list exec mintime2contours $lines $samples \
	    "$env(TMA_DATA)/minTimeSoFar.dat" \
	    "$env(TMA_DATA)/contourOverlay.pgm"] $levels]
    puts $cmd
    eval $cmd
    set cmd [list exec overlay2polygons "$env(TMA_DATA)/contourOverlay.pgm" \
	    "$env(TMA_DATA)/contours.dat" $numContours]
    puts $cmd
    eval $cmd

    # monochrome coded filled
    if {$contourMode != "monochrome"} {
	set colors {}
	for {set contour 0} {$contour < $numContours} {incr contour} {
	    lappend colors [lindex [split \
		    $rainbow([expr $contour % $rainbowWidth])] 0]
	}
    }

    # remove the source points from map and then create it so color is right
    if {$contourMode == "monochrome"} {
	set color $singleColor(MultiSource)
    } else {
	set color [lindex $colors 0]
    }

    removeDots multi
    set circleIdList {}
    global multiSourceLocations
    foreach locationCoord $multiSourceLocations {
	set circleId [Map_DrawFilledCircle $mapRegionName \
		$locationCoord 10 $color]
	lappend circleIdList $circleId
    }

    # This is for the labels on the contours
    # Put a label on the largest contour for each level
    # These 2 variables will be used to keep track of the
    # largest number of point in each level and the point where the
    # text will be put for each level. 
    set maxPointsByLevel {}
    set labelPointByLevel {}
    foreach level $levels {
	lappend maxPointsByLevel 0
	lappend labelPointByLevel {}
    }
    set contoursFile [open "$env(TMA_DATA)/contours.dat"]
    while { [gets $contoursFile line] >= 0 } {
	set level [lindex $line 0]
	set numPoints [lindex $line 1]
	set contoursGraphics ""
	for {set pointNum 0} {$pointNum < $numPoints} {incr pointNum} {
	    gets $contoursFile line
	    # Convert line sample coord in aoi space to lat lon
	    set linePixel   [lindex $line 0]
	    set samplePixel [lindex $line 1]
	    set coords [list \
		    [expr (((( $s - $n ) / $height ) * $linePixel   ) + $n )] \
		    [expr (((( $e - $w ) / $width  ) * $samplePixel ) + $w )]]
	    lappend contoursGraphics $coords
	    # Keep track of where leash should go
	    if {$pointNum == 0} {
		if {$numPoints > [lindex $maxPointsByLevel $level]} {
		    set maxPointsByLevel \
			    [lreplace $maxPointsByLevel $level $level \
			    $numPoints]
		    set labelPointByLevel \
			    [lreplace $labelPointByLevel $level $level \
			    $coords]
		}
	    }
	}
	#puts $contoursGraphics
	if {$contourMode == "monochrome"} {
	    set color $singleColor(MultiSource)
	} else {
	    set color [lindex $colors $level]
	}
	lappend contourDisplayList \
		[Map_DrawPolyline $mapRegionName \
		$contoursGraphics \
		$color 3]
	set contoursGraphics ""
    }
    close $contoursFile

    set textAndLeashList {}
    for {set levelNum 0} {$levelNum < $numContours} {incr levelNum} {
	if {$contourMode == "monochrome"} {
	    set color $singleColor(MultiSource)
	} else {
	    set color [lindex $colors $levelNum]
	}
	set minutes [expr $deltaTime * ($levelNum + 1)]
	set hours [expr int($minutes/60.0)]
	set minutes [expr int($minutes - (60.0 * $hours))]
	set timeLabel "$hours hr $minutes min"
	set textAndLeash [Map_Text2 $mapRegionName \
		$timeLabel \
		[lindex $labelPointByLevel $levelNum] \
		$color contoursLabel]
	set textAndLeashList [concat $textAndLeashList $textAndLeash]
    }

    # filled option. Uses overlay.
    set image ""
    if {$contourMode == "filled"} {
	set colors2 {}
	foreach color $colors {
	    # color2 is hex color. example = 0xff0000 (red).
	    set color2 "0x[string range $color 1 end]"
	    lappend colors2 $color2 $color2
	}
	#set x0 [MapSRV_LonToPix $mapRegionName $w]
	set y1 [MapSRV_LatToPix $mapRegionName $s]
	#set x1 [MapSRV_LonToPix $mapRegionName $e]
	set y0 [MapSRV_LatToPix $mapRegionName $n]
	set scale [expr abs($y0 - $y1)/${lines}.0]
	set cmd [concat [list exec pgm2ppm \
		"$env(TMA_DATA)/contourOverlay.pgm" \
		"$env(TMA_DATA)/contourOverlay.ppm" $scale] $colors2]
	puts $cmd
	eval $cmd
	set cmd [list exec ppmtogif -transparent rgb:0/0/0 \
		"$env(TMA_DATA)/contourOverlay.ppm" ">" \
		"$env(TMA_DATA)/contourOverlay.gif"]
	puts $cmd
	catch {eval $cmd}
	if {[catch {
	    set image [Map_OverlayPhoto $mapRegionName \
		    "$env(TMA_DATA)/contourOverlay.gif" $w $n]
	}]} {
	    tk_messageBox -message \
		    "Failed to overlay image. Tk may be out of memory."
	}
    }

    global analysisName
    if {$image == ""} {
	addAnalysisHistoryItem Contour $analysisName \
		[concat $circleIdList $textAndLeashList $contourDisplayList]
    } else {
	addAnalysisHistoryItem Contour $analysisName \
		[concat $circleIdList $textAndLeashList \
		$contourDisplayList $image]
    }
}

#------------------------------------------------------------------------------
proc tmpnam {dir prefix suffix} {
    for {set i 0} {[file exists ${dir}/${prefix}_${i}${suffix}]} {incr i} {
    }
    return ${dir}/${prefix}_${i}${suffix}
}
#------------------------------------------------------------------------------
proc getMultiLocation {listbox color {max 0}} {
    global workingMultiDots
    global multiSourceLocations

    set map [getMapHandle]
    Map_Grab $map

    set multiSourceLocations {}

    set location "not null"

    if {! $max} {
	$listbox delete 0 end
    }

    foreach dot $workingMultiDots {
	Map_EraseObject [getMapHandle] $dot
    }
    set workingMultiDots {}

    set locationCount 0
    while {$location != "" && (! $max || $locationCount < $max)} {
	set location [Map_PickCoord [getMapHandle]]

	if {$location != ""} {
	    lappend multiSourceLocations $location
	    lappend workingMultiDots [Map_DrawFilledCircle [getMapHandle] $location 10 $color]

	    if {! $max} {
		$listbox insert end [Map_CoordToString $location]
		$listbox see end
	    }

	    incr locationCount
	}
    }

    Map_Release $map

    return $locationCount
}

# Supporting procs
#------------------------------------------------------------------------------
proc removeAllDots {} {
    foreach dotType {start end multi} {
	removeDots $dotType
    }
}
#------------------------------------------------------------------------------
proc removeDots {type} {
    set map [getMapHandle]

    switch $type {
	start {
	    global workingStartDot
	    Map_EraseObject $map $workingStartDot
	    set workingStartDot {}
	}
	end {
	    global workingEndDot
	    Map_EraseObject $map $workingEndDot
	    set workingEndDot {}
	}
	multi {
	    global workingMultiDots

	    foreach dot $workingMultiDots {
		Map_EraseObject $map $dot
	    }

	    set workingMultiDots {}
	}
    }
}
#------------------------------------------------------------------------------
proc updateDialogColors {} {
    global rainbow rainbowWidth

#      if {[info commands .contours] != ""} {
#  	for {set index 0} {$index < $rainbowWidth} {incr index} {
#  	    .contours.top.contourColor.rainbow.l$index configure -bg $rainbow($index)
#  	}
#      }

    if {[info commands .corridor] != ""} {
	for {set index 0} {$index < $rainbowWidth} {incr index} {
	    .corridor.top.contourColor.rainbow.l$index configure -bg $rainbow($index)
	}
    }

    if {[info commands .multiContour] != ""} {
	for {set index 0} {$index < $rainbowWidth} {incr index} {
	    .multiContour.top.contourColor.rainbow.l$index configure -bg $rainbow($index)
	}
    }
}
#------------------------------------------------------------------------------
proc getQueryLocation {type color} {
    set map [getMapHandle]

    switch $type {
	start {
	    global startLocationCoord startLocation

	    set startLocationCoord [Map_PickCoord $map]

	    if {$startLocationCoord != ""} {
		removeDots start

		set circleId [Map_DrawFilledCircle $map $startLocationCoord 10 $color]
		set startLocation [Map_CoordToString $startLocationCoord]

		global workingStartDot
		set workingStartDot $circleId
	    }
	}
	end {
	    global endLocationCoord endLocation

	    set endLocationCoord [Map_PickCoord $map]

	    if {$endLocationCoord != ""} {
		removeDots end

		set circleId [Map_DrawFilledCircle $map $endLocationCoord 10 $color]
		set endLocation [Map_CoordToString $endLocationCoord]

		global workingEndDot
		set workingEndDot $circleId
	    }
	}
    }
}

set contourMode monochrome
set workingMultiDots {}
set workingStartDot {}
set workingEndDot {}




