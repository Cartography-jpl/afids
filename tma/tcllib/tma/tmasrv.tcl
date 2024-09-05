package provide TMASRV 1.0

set TMASRV_map {}

proc findAOI {toleranceInSeconds region n s e w} {
    package require dbtcl

    global env

    set delta [expr $toleranceInSeconds / 3600.0]
    set results [Data_Select tma_aoi {ID NAME NWLAT NWLON SELAT SELON LINES SAMPLES} [list [list REGION $region]]]

    foreach result $results {
	set id [lindex $result 0]
	set name [lindex $result 1]
	set nr [lindex $result 2]
	set wr [lindex $result 3]
	set sr [lindex $result 4]
	set er [lindex $result 5]
	set linesr [lindex $result 6]
	set samplesr [lindex $result 7]

	if [expr abs($n - $nr) <= $delta && abs($s - $sr) <= $delta && abs($e - $er) <= $delta && abs($w - $wr) <= $delta] {
	    puts "found similar AOI \"$name\""
	    return [list [list $nr $wr] [list $sr $er] $linesr $samplesr $id]
	}
    }

    puts "no matching AOI found, creating ..."

    set latDelta [expr $n - $s]
    set lonDelta [expr $e - $w]
    if {$latDelta < $lonDelta} {
	set lines 128
	set samples [expr int(128.0 * $lonDelta / $latDelta)]
    } else {
	set samples 128
	set lines [expr int(128.0 * $latDelta / $lonDelta)]
    }

    # delete temp aoi, if any
    deleteAOI 1

    # create new temp aoi
    Data_Insert tma_aoi [list \
  	    [list ID 1]     \
  	    [list NAME temp] \
  	    [list REGION $region] \
  	    [list NWLAT $n] \
  	    [list NWLON $w] \
  	    [list SELAT $s] \
  	    [list SELON $e] \
  	    [list LINES $lines] \
  	    [list SAMPLES $samples]]

    return [list [list $n $w] [list $s $e] $lines $samples 1]
}

proc deleteAOI {aoiId} {
    global env aoi

    set answer [tk_messageBox \
	    -message "Are you sure?" \
	    -type yesno -icon question ]
    if {($answer == "no")} {
	return {}
    }

    set rnfFiles [Data_Select tma_aoiData {RNF_FILE} [list [list AOI_ID $aoiId]]]

    if {[llength $rnfFiles] > 0} {
	foreach rnfFile $rnfFiles {
	    file delete $env(TMA_DATA)/$rnfFile
	}
	Data_Delete tma_aoiData [list [list AOI_ID $aoiId]]
    }

    Data_Delete tma_aoi [list [list ID $aoiId]]

    if {$aoiId == [lindex $aoi 4]} {
	set aoi {}
    }
}

proc TMASRV_thingSelect {aoiList title} {
    global TMASRV_currentThing

    set aois [toplevel .aois -class Dialog]
    wm title ${aois} $title

    set upper [frame ${aois}.upper -relief raised -bd 2]
    pack ${upper} -fill both -side top
    set lower [frame ${aois}.lower -relief raised -bd 2]
    pack ${lower} -fill both -side bottom

    set scroll [scrollbar ${upper}.scroll -command "${upper}.list yview"]
    pack ${scroll} -side right -fill y
    set list [listbox ${upper}.list -yscrollcommand "${upper}.scroll set"]
    pack ${list} -fill both -expand true

    foreach aoi ${aoiList} {
        ${list} insert end ${aoi}
    }

    bind ${list} <Button-1> {
        set index [%W nearest %y]
        set TMASRV_currentThing [%W get ${index}]
    }

    set oldfocus [focus]
    grab ${aois}
    focus ${aois}
    tkwait variable TMASRV_currentThing
    focus ${oldfocus}
    grab release ${aois}
    destroy ${aois}

    set TMASRV_currentThing
}

proc TMASRV_thingEnter {title} {
    global TMASRV_currentThing

    set aois [toplevel .aois -class Dialog]
    wm title ${aois} $title

    label $aois.label -text $title
    pack $aois.label
    entry $aois.entry
    pack $aois.entry
    button $aois.ok -text OK
    pack $aois.ok

    bind $aois.ok <Button-1> {
        set TMASRV_currentThing [.aois.entry get]
    }

    set oldfocus [focus]
    grab ${aois}
    focus ${aois}
    tkwait variable TMASRV_currentThing
    focus ${oldfocus}
    grab release ${aois}
    destroy ${aois}

    set TMASRV_currentThing
}

proc deleteAnAOI {} {
    set anAOI [TMASRV_getAOI " to delete"]
    set aoiId [lindex $anAOI 4]
    deleteAOI $aoiId
}

proc TMASRV_getAOI {prompt} {
    set mapRegionName [getMapHandle]
    puts "mapRegionName = $mapRegionName"
    set region [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "region = $region"

    set column {NAME}
    set criteria [list [list REGION $region]]
    puts "Data_Select tma_aoi $column $criteria"
    set aoiList [Data_Select tma_aoi $column $criteria]
    set aoiName [TMASRV_thingSelect ${aoiList} "Select AOI $prompt"]
    set column {NWLAT NWLON SELAT SELON LINES SAMPLES ID}
    set criteria [list [list NAME $aoiName]]
    set coords [lindex [Data_Select tma_aoi $column $criteria] 0]
    set n [lindex $coords 0]
    set w [lindex $coords 1]
    set s [lindex $coords 2]
    set e [lindex $coords 3]
    set lines [lindex $coords 4]
    set samples [lindex $coords 5]
    set id [lindex $coords 6]

    return [list [list $n $w] [list $s $e] $lines $samples $id]
}

proc setMapHandle {map} {
  global TMASRV_map
  set TMASRV_map $map
}

proc getMapHandle {} {
  global TMASRV_map
  return $TMASRV_map
}

proc iconDisplay {} {
global env

toplevel .icons -class Dialog
wm title .icons "TMA"

# Create image icons.

image create photo minPathIcon -file $env(PUB)/toolkit/data/minpath.gif
image create photo analPathIcon -file $env(PUB)/toolkit/data/anapath.gif
image create photo contourIcon -file $env(PUB)/toolkit/data/contour.gif
image create photo multiContourIcon -file $env(PUB)/toolkit/data/multicontour.gif
image create photo corridorIcon -file $env(PUB)/toolkit/data/corridor.gif
image create photo reportIcon -file $env(PUB)/toolkit/data/report.gif

# Create analysis icons.

button .icons.minPath -image minPathIcon -command "verifyDataSetAndShowDialog minPathDialog"
button .icons.analPath -image analPathIcon -command "verifyDataSetAndShowDialog analystPathDialog"
button .icons.contour -image contourIcon -command {verifyDataSetAndShowDialog "contoursDialog Isochronal"}
button .icons.multiContour -image multiContourIcon -command {verifyDataSetAndShowDialog "contoursDialog MultiSource"}
button .icons.corridor -image corridorIcon -command {verifyDataSetAndShowDialog "contoursDialog Corridor"}
button .icons.report -image reportIcon -command "verifyDataSetAndShowDialog reportCorrelationDialog"

# Pack them.
pack .icons.minPath .icons.analPath .icons.contour .icons.multiContour .icons.corridor .icons.report -side top

wm resizable .icons 0 0
}

proc TMASRV_GetVectorDataSet {mapRegionName} {
    global vectorDataSetType

    puts "mapRegionName = $mapRegionName"
    set region [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "region = $region"
    set column {DATA_SET}
    set criteria [list [list REGION_NAME $region] [list THEME_TYPE Terrain]]
    set existingDataSets [Data_Select tma_regionThemes $column $criteria]
    if {[llength $existingDataSets] < 1} {
	tk_messageBox -message "No terrain data sets for region $region" -title "No Terrain Data"
	return 1
    }

    if {[llength $existingDataSets] > 1} {
	set dataSetList {}
	foreach ds $existingDataSets {
	    lappend dataSetList [lindex $ds 0]
	}
	set vectorDataSetType [TMASRV_thingSelect $dataSetList "Vector Data Sets"]
    } else {
	set vectorDataSetType [lindex $existingDataSets 0]
    }

    return 0
}

proc TMASRV_PickBoxAndFindOrCreateAOI {} {
    global aoi

    set mapRegionName [getMapHandle]

    set aoi [Map_PickBox $mapRegionName red]
    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set n [lindex $nw 0]
    set w [lindex $nw 1]
    set s [lindex $se 0]
    set e [lindex $se 1]

    set toleranceInSeconds "3.0"

    puts "mapRegionName = $mapRegionName"
    set region [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "region = $region"
    set aoi [findAOI $toleranceInSeconds $region $n $s $e $w]

    puts $aoi

    eraseAllPolygons
}

proc verifyDataSetAndShowDialog {dialog} {
    global debuggingWithoutMap
    if {$debuggingWithoutMap} {
	eval $dialog
	return
    }

    set mapRegion [getMapHandle]

    global aoi

    while {! [info exists aoi] || $aoi == ""} {
	TMASRV_OpenAOI
    }

    if [TMASRV_GetVectorDataSet $mapRegion] {
	# no terrain data
	return
    }

    global vectorDataSetType
    puts "data set is $vectorDataSetType"

    eval $dialog
}

proc destructure {list args} {
    for {set i 0} {$i < [llength $args]} {incr i} {
	eval uplevel [list set [lindex $args $i] [list [lindex $list $i]]]
    }
}

proc TMASRV_OpenAOI {} {
    global aoi

    set mapRegion [getMapHandle]

    set aoi [TMASRV_getAOI " to open"]

    set nw [lindex $aoi 0]
    set se [lindex $aoi 1]
    set n [lindex $nw 0]
    set w [lindex $nw 1]
    set s [lindex $se 0]
    set e [lindex $se 1]

    MAPSRV_ReCoordBox $mapRegion $n $e $s $w red

    set aoiId [lindex $aoi 4]

    eraseAllPolygons

    loadAoiPolygons $aoiId

    puts $aoi
}


proc TMASRV_SaveAOI {} {
    set mapRegionName [getMapHandle]
    puts "mapRegionName = $mapRegionName"
    set region [lindex [Data_Select tma_mapRegion REGION [list [list NAME $mapRegionName]]] 0]
    puts "region = $region"

    set columns {NWLAT NWLON SELAT SELON LINES SAMPLES}
    set criteria [list [list ID 1] [list REGION $region]]
    set data [lindex [Data_Select tma_aoi $columns $criteria] 0]

    if {[llength $data] == 0} {
	tk_messageBox -message "Nothing to save"
	return
    }

    set name [TMASRV_thingEnter "Enter an AOI name"]

    set columns {NAME}
    set criteria [list [list NAME $name] [list REGION $region]]
    if {[llength [Data_Select tma_aoi $columns $criteria]] > 0} {
	tk_messageBox -message "AOI $name already exists in region $region"
	return
    }

    # find new id
    set columns {ID}
    set criteria {}
    set ids [Data_Select tma_aoi $columns $criteria]
    for {set id 2} {[lsearch $ids $id] >= 0} {incr id} {
    }

    set nwlat [lindex $data 0]
    set nwlon [lindex $data 1]
    set selat [lindex $data 2]
    set selon [lindex $data 3]
    set lines [lindex $data 4]
    set samples [lindex $data 5]

    Data_Insert tma_aoi [list \
	    [list ID      $id]     \
	    [list NAME    $name] \
	    [list REGION  $region] \
	    [list NWLAT   $nwlat] \
	    [list NWLON   $nwlon] \
	    [list SELAT   $selat] \
	    [list SELON   $selon] \
	    [list LINES   $lines] \
	    [list SAMPLES $samples]]

    global aoi
    set aoi [list [list $nwlat $nwlon] [list $selat $selon] $lines $samples $id]
}


set debuggingWithoutMap 0
proc TMASRV_Init {} {

    #package require Comm
    package require dbtcl
    package require maptcl


#    MinPathCmd maui  {20.81985804 -156.47717970}  {20.72024997 -156.43055900}  273 128  0 {20.802500000000002 -156.44749999999999} {20.803333333333335 -156.435} kihei.rnf mauiShape 25
#return

#    MUSESRV_Init

    #Comm_ServerRegister TMA

    global debuggingWithoutMap
    if {! $debuggingWithoutMap} {
	setMapHandle [Map_Connect]
    }
    frame .menubar -relief raised -borderwidth 2

    # AOI

    menubutton .menubar.file \
	    -text "AOI" \
	    -underline 0 \
	    -menu .menubar.file.menu

    menu .menubar.file.menu

    .menubar.file.menu add command \
	    -label "New" \
	    -underline 0 \
	    -command TMASRV_PickBoxAndFindOrCreateAOI

    .menubar.file.menu add command \
	    -label "Open" \
	    -underline 0 \
	    -command TMASRV_OpenAOI

    .menubar.file.menu add separator

    .menubar.file.menu add command \
	    -label "Save" \
	    -command "TMASRV_SaveAOI" \
	    -underline 0

    .menubar.file.menu add command \
	    -label "Delete" \
	    -command deleteAnAOI \
	    -underline 0

    .menubar.file.menu add separator

    .menubar.file.menu add command \
	    -label "Exit" \
	    -underline 0 \
	    -command { exit }

    # Edit menu

    menubutton .menubar.edit \
	    -text "Edit" \
	    -underline 0 \
	    -menu .menubar.edit.menu

    menu .menubar.edit.menu

    # Edit -> Analysis History menu
    .menubar.edit.menu add cascade \
	      -label "Analysis History" \
	      -underline 0 \
	      -menu .menubar.edit.menu.analysisHistory

    menu .menubar.edit.menu.analysisHistory

    # Edit -> Analysis History -> Path command

    .menubar.edit.menu.analysisHistory add command \
	    -label "Path ..." \
	    -underline 0 \
	    -command "editAnalysisHistory Path"

    # Edit -> Analysis History -> Contour Set command

    .menubar.edit.menu.analysisHistory add command \
	    -label "Contour Set ..." \
	    -underline 0 \
	    -command "editAnalysisHistory Contour"

    # Edit -> Analysis History -> Corridor command

    .menubar.edit.menu.analysisHistory add command \
	    -label "Corridor ..." \
	    -underline 0 \
	    -command "editAnalysisHistory Corridor"

    # Edit -> Constraint menu

    .menubar.edit.menu add cascade \
	    -label "Constraint" \
	    -underline 0 \
	    -menu .menubar.edit.menu.constraint

    menu .menubar.edit.menu.constraint

    # Edit -> Constraint -> Weather menu

    .menubar.edit.menu.constraint add cascade \
	    -label "Weather" \
	    -underline 0 \
	    -menu .menubar.edit.menu.constraint.weather

    menu .menubar.edit.menu.constraint.weather

    # Edit -> Constraint -> Weather -> Colors

    .menubar.edit.menu.constraint.weather add command \
	    -label "Colors ..." \
	    -underline 0 \
	    -command "editPolygonColors Weather"

    # Edit -> Constraint -> Weather -> Create

    .menubar.edit.menu.constraint.weather add command \
	    -label "Create ..." \
	    -underline 0 \
	    -command "polygonPathDialog Weather Create"

    # Edit -> Constraint -> Weather -> Delete

    .menubar.edit.menu.constraint.weather add command \
	    -label "Delete ..." \
	    -underline 0 \
	    -command "polygonPathDialog Weather Delete"

    # Edit -> Constraint -> Line Obstacle menu

    .menubar.edit.menu.constraint add cascade \
	    -label "Line Obstacle" \
	    -underline 0 \
	    -menu .menubar.edit.menu.constraint.obstacle

    menu .menubar.edit.menu.constraint.obstacle

    # Edit -> Constraint -> Line Obstacle -> Colors

    .menubar.edit.menu.constraint.obstacle add command \
	    -label "Colors ..." \
	    -underline 0 \
	    -command "editPolygonColors Obstacle"

    # Edit -> Constraint -> Line Obstacle -> Create

    .menubar.edit.menu.constraint.obstacle add command \
	    -label "Create ..." \
	    -underline 0 \
	    -command "polygonPathDialog Obstacle Create"

    # Edit -> Constraint -> Line Obstacle -> Delete

    .menubar.edit.menu.constraint.obstacle add command \
	    -label "Delete ..." \
	    -underline 0 \
	    -command "polygonPathDialog Obstacle Delete"

    # Edit -> Constraint -> Region Obstacle menu

    .menubar.edit.menu.constraint add cascade \
	    -label "Region Obstacle" \
	    -underline 0 \
	    -menu .menubar.edit.menu.constraint.regionObstacle

    menu .menubar.edit.menu.constraint.regionObstacle

    # Edit -> Constraint -> Region Obstacle -> Colors

    .menubar.edit.menu.constraint.regionObstacle add command \
	    -label "Colors ..." \
	    -underline 0 \
	    -command "editPolygonColors RegionObstacle"

    # Edit -> Constraint -> Region Obstacle -> Create

    .menubar.edit.menu.constraint.regionObstacle add command \
	    -label "Create ..." \
	    -underline 0 \
	    -command "polygonPathDialog RegionObstacle Create"

    # Edit -> Constraint -> Region Obstacle -> Delete

    .menubar.edit.menu.constraint.regionObstacle add command \
	    -label "Delete ..." \
	    -underline 0 \
	    -command "polygonPathDialog RegionObstacle Delete"

    # End constraints cascade

    .menubar.edit.menu add cascade \
	    -label "Entity" \
	    -underline 0 \
	    -menu .menubar.edit.menu.entity

    # Begin entity cascade

    menu .menubar.edit.menu.entity

    .menubar.edit.menu.entity add command \
	    -label "New" \
	    -underline 0 \
	    -command { moverEditor::newMover $env(TMA_APPLICATION_ID) }

    .menubar.edit.menu.entity add command \
	    -label "Modify" \
	    -underline 0 \
	    -command { moverEditor::editMovers $env(TMA_APPLICATION_ID) }

    .menubar.edit.menu.entity add command \
	    -label "Delete" \
	    -underline 0 \
	    -command { moverEditor::deleteMover $env(TMA_APPLICATION_ID) }

    # End entity cascade

    # View

    menubutton .menubar.view \
	    -text "View" \
	    -underline 0 \
	    -menu .menubar.view.menu

    # Begin overlay cascade

    menu .menubar.view.menu
    .menubar.view.menu add cascade \
	    -label "Overlay..." \
	    -underline 0 \
	    -menu .menubar.view.menu.overlay

    menu .menubar.view.menu.overlay

    foreach overlay {Weather Obstacle Path Contour Corridor} {
      .menubar.view.menu.overlay add command \
  	    -label "$overlay ..." \
	    -command "overlayDialog $overlay"
    }

    # End overlay cascade

    .menubar.view.menu add command \
	    -label "Query Terrain" \
	    -underline 0 \
	    -command "queryTerrainWithClicks"

#      .menubar.view.menu add command \
#  	    -label "Path Info" \
#  	    -underline 0

    .menubar.view.menu add command \
	    -label "Path Graph" \
	    -underline 5 \
	    -command "queryTerrainFromPathFile"

#      # Options

#      menubutton .menubar.options \
#  	    -text "Options" \
#  	    -underline 0 \
#  	    -menu .menubar.options.menu

#      menu .menubar.options.menu

#      .menubar.options.menu add command \
#  	    -label "Select Database" \
#  	    -underline 0

#      .menubar.options.menu add command \
#  	    -label "Color Chart" \
#  	    -underline 0

    # Analysis

    menubutton .menubar.analysis \
	    -text "Analysis" \
	    -underline 0 \
	    -menu .menubar.analysis.menu

    menu .menubar.analysis.menu

    .menubar.analysis.menu add command \
	    -label "Minimal Path" \
	    -underline 8 \
	    -command "verifyDataSetAndShowDialog minPathDialog"

    .menubar.analysis.menu add command \
	    -label "Analyst Path" \
	    -underline 0 \
	    -command "verifyDataSetAndShowDialog analystPathDialog"

    .menubar.analysis.menu add command \
	    -label "Isochronal Contours" \
	    -underline 0 \
	    -command {verifyDataSetAndShowDialog "contoursDialog Isochronal"}

    .menubar.analysis.menu add command \
	    -label "Multi Source Contours" \
	    -underline 0 \
	    -command {verifyDataSetAndShowDialog "contoursDialog MultiSource"}

    .menubar.analysis.menu add command \
	    -label "Corridor" \
	    -underline 0 \
	    -command {verifyDataSetAndShowDialog "contoursDialog Corridor"}

    .menubar.analysis.menu add command \
	    -label "Report Correlation" \
	    -underline 0 \
	    -command "verifyDataSetAndShowDialog reportCorrelationDialog"

    .menubar.analysis.menu add separator

    .menubar.analysis.menu add command \
	    -label "Show Icons" \
	    -underline 0 \
	    -command iconDisplay

    # Help

    menubutton .menubar.help \
	    -text "Help" \
	    -underline 0 \
	    -menu .menubar.help.menu

    menu .menubar.help.menu

    .menubar.help.menu add command \
	    -label "Contents" \
	    -underline 0

    .menubar.help.menu add separator

    .menubar.help.menu add command \
	    -label "About TMA" \
	    -underline 0

    # Pack main menu bar items.
    pack .menubar.file .menubar.edit .menubar.view \
	    .menubar.analysis -side left
    pack .menubar.help -side right

    pack .menubar -side top -fill x -expand true

    wm title . "TMA"
    #wm minsize . 20 20
    #wm maxsize . 200 200

    set gui_WeatherSelection 1
    set gui_ObstacleSelection 1

    set gui_ColorWheelSelection {255 0 0}
    set startTime 0800

    # Need also for black box server with no gui.
    readMoverMetaData

    initRainbow
    initAnalysisHistory
    initPolygons
}

