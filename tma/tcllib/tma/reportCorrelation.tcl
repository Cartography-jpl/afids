package provide TMASRV 1.0
package require helpers

proc reportCorrelation {} {
    global currentMover aoi startLocation endLocation deltaTime
    global env
    
    if {$currentMover(code) < 0} {
	tk_messageBox -message "Please select a mover"
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
    set regionName \
	    [lindex \
	    [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]
    puts "regionName = $regionName"
    
    puts "Calling MinPathCmd $regionName [list $n $w] [list $s $e]"
    puts "$currentMover(code) [string2Coord $startLocation] [string2Coord $endLocation] $rnfName"

    set aoiId [lindex $aoi 4]

    global vectorDataSetType
    MinPathCmd $regionName \
	    [list $n $w] \
	    [list $s $e] \
	    $lines $samples \
	    $currentMover(code) \
	    [string2Coord $startLocation] \
	    [string2Coord $endLocation] \
	    $rnfName \
	    $vectorDataSetType \
	    $aoiId

    set pathFile [open $env(TMA_DATA)/minPath.dat]
    # time.
    gets $pathFile line
    set rawMinutes $line
    set minutes [expr int($line)]
    set hours [expr int($minutes/60.0)]
    set minutes [expr int($minutes - (60.0 * $hours))]
    set minPathTimeResult "$hours hr $minutes min"
    # length.
    #gets $pathFile line
    #set minPathLengthResult "[expr $line / 1000.0] km"

    ###############################
    toplevel .reportCorrelationResult -class Dialog
    wm title .reportCorrelationResult "Report Correlation Result"
    
    # Create frames.
    frame .reportCorrelationResult.top
    frame .reportCorrelationResult.bottom
    
    # Text results area.
    text .reportCorrelationResult.top.text -height 3 -width 40 -wrap word
    pack .reportCorrelationResult.top.text -anchor center
    
    # Ok button.
    button .reportCorrelationResult.bottom.ok -text Ok -width 7  \
	    -command { destroy .reportCorrelationResult } 
    pack .reportCorrelationResult.bottom.ok -anchor center -side left -pady 4 
    pack .reportCorrelationResult.top .reportCorrelationResult.bottom -side top
    wm resizable .reportCorrelationResult 0 0
    
    if { $rawMinutes < $deltaTime } {
	.reportCorrelationResult.top.text insert 1.0 \
		"The entities are distinct.\nMinimal travel time between points :\n$hours hours $minutes minutes"
    } else {
	.reportCorrelationResult.top.text insert 1.0 \
		"The entities may correlate.\nMinimal travel time between points :\n$hours hours $minutes minutes"
    }
}

############################################################################
#  Create dialog widget for minimum path parameters.
############################################################################
proc reportCorrelationDialog {} {
  global currentMover startLocation endLocation deltaTime env

set dotColor #ff0000

toplevel .reportCorrelation -class Dialog
wm title .reportCorrelation "Report Correlation"

removeAllDots

# Create frames.

frame .reportCorrelation.top
frame .reportCorrelation.bottom

# User entry area.

frame .reportCorrelation.top.entityCode
frame .reportCorrelation.top.startLocation
frame .reportCorrelation.top.endLocation
frame .reportCorrelation.top.deltaTime

# Create buttons.

button .reportCorrelation.top.entityCode.button -text "Mover" \
	-width 7 \
	-command {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]}
button .reportCorrelation.top.startLocation.button -text "Location 1" \
	-width 21 \
	-command "getQueryLocation start $dotColor"
#  button .reportCorrelation.top.startLocation.button -text "Location 1" \
#  	-width 21 \
#  	-command {set startLocation [PickStringFromMap]}
button .reportCorrelation.top.endLocation.button -text "Location 2" \
	-width 21 \
	-command "getQueryLocation end $dotColor"
#  button .reportCorrelation.top.endLocation.button -text "Location 2" \
#  	-width 21 \
#  	-command {set endLocation [PickStringFromMap]}
button .reportCorrelation.top.deltaTime.button -text "Delta Time (minutes)" \
	-width 21

# Create entry widgets.

entry .reportCorrelation.top.entityCode.entry \
	-textvariable currentMover(code) -width 9
# NOTE: the start and end locations have been reversed here.
# This is how it should be given the logic of the report correlation dialog.
# The reason this is at all necessary is that the travel time from
# point A to point B may be different from point B to point A.
# This is because of slope factor non-symmetries.
entry .reportCorrelation.top.startLocation.entry \
	-textvariable startLocation -width 22
entry .reportCorrelation.top.endLocation.entry \
	-textvariable endLocation -width 22
entry .reportCorrelation.top.deltaTime.entry \
	-textvariable deltaTime -width 13 -justify right

# Pack them.

pack .reportCorrelation.top.entityCode.button \
	.reportCorrelation.top.entityCode.entry -side top
pack .reportCorrelation.top.startLocation.button \
	.reportCorrelation.top.startLocation.entry -side top
pack .reportCorrelation.top.endLocation.button \
	.reportCorrelation.top.endLocation.entry -side top
pack .reportCorrelation.top.deltaTime.button \
	.reportCorrelation.top.deltaTime.entry -side top

pack .reportCorrelation.top.entityCode \
	.reportCorrelation.top.startLocation \
	.reportCorrelation.top.endLocation \
	.reportCorrelation.top.deltaTime -side left

# Execute and cancel buttons.

button .reportCorrelation.bottom.execute -text Execute -width 7 \
	-command { reportCorrelation }
button .reportCorrelation.bottom.cancel -text Cancel -width 7  \
	-command { removeAllDots; destroy .reportCorrelation } 
pack .reportCorrelation.bottom.execute .reportCorrelation.bottom.cancel \
	-anchor center -side left -pady 4 

pack .reportCorrelation.top \
	.reportCorrelation.bottom -side top

wm resizable .reportCorrelation 0 0

    # Auto forward to mover selection
    update
    Gui_SelectMover .d "Select Mover" [getMoverDescriptions]

preloadTwoLocations $dotColor
}








