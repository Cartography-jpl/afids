package provide TMASRV 1.0
package require helpers

set tcl_precision 17

proc queryTerrainDialog {} {
    set top .queryTerrain
    if {[info commands $top] != ""} {
	destroy $top
    }

    toplevel $top -class Dialog
    wm title $top "Query Terrain"
    
    # Create frames.
    frame .queryTerrain.top
    frame .queryTerrain.bottom
    
    # Text results area.
    text .queryTerrain.top.text -height 4 -width 80 -wrap word
    pack .queryTerrain.top.text -anchor center
    
    # Cancel button.
    button .queryTerrain.bottom.cancel -text Cancel -width 7  \
	    -command { destroy .queryTerrain } 
    pack .queryTerrain.bottom.cancel -anchor center -side left -pady 4 
    pack .queryTerrain.top .queryTerrain.bottom -side top
    .queryTerrain.top.text insert 1.0 \
	    "Select locations on map.\Click right mouse button to quit"
    wm resizable .queryTerrain 0 0
}

###############################################################################
proc queryTerrainWithClicks {} {
    global env

    # Build dialog for results.
    queryTerrainDialog

    set mapRegionName [getMapHandle]
    #puts "mapRegionName = $mapRegionName"
    set regionName \
	    [lindex \
	    [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]
    #puts "regionName = $regionName"

    set latLon [Map_PickCoord $mapRegionName]

    # Get region corners.
    set result [lindex [Data_Select tma_regionThemes \
	    [list DATA_SET COUNT_HEIGHT COUNT_WIDTH \
	    TEXT_FILENAME NWLAT NWLON SELAT SELON] \
	    [list \
	    [list REGION_NAME $regionName] \
	    [list THEME_TYPE Terrain]]] 0]
    #puts $result
    set dataSet  [lindex $result 0]
    set lines    [lindex $result 1]
    set samples  [lindex $result 2]
    set filename [lindex $result 3]
    set nwLat [lindex $result 4]
    set nwLon [lindex $result 5]
    set seLat [lindex $result 6]
    set seLon [lindex $result 7]

    set result [lindex [Data_Select tma_regionThemes \
	    [list TEXT_FILENAME] \
	    [list \
	    [list REGION_NAME $regionName] \
	    [list THEME_TYPE Elevation]]] 0]
    set elevationFilename [lindex $result 0]

    while {$latLon != ""} {
	set lat [lindex $latLon 0]
	set lon [lindex $latLon 1]
	
	set line   [expr $lines * ($nwLat - $lat) / ($nwLat - $seLat)]
	set sample [expr $samples * \
		(1.0 - (($seLon - $lon) / ($seLon - $nwLon)))]

	# Get pixel value in terrain file using line, sample.
	set terrainPixel [GetTerrainPixel \
		$env(TMA_DATA)/$filename $samples $line $sample]
		
	#puts "terrainPixel = $terrainPixel"
		
	# Get terrain name using pixel value.
	set terrainName [join \
		[lindex \
		[Data_Select tma_terrainMapping {TERRAIN_ID} \
		[list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
		[list DATA_SET $dataSet] \
		[list PGM_VALUE $terrainPixel]]] 0] " "]
	
	# Get pixel value in elevation file for line, sample.
	# Elevation data is stored as 16-bit meters.
	# Assumes Elevation and Terrain covers the same area,
	# and is same size.
	
	set line   [expr $lines * ($nwLat - $lat) / ($nwLat - $seLat)]
	set sample [expr $samples * \
		(1.0 - (($seLon - $lon) / ($seLon - $nwLon)))]
	set elevationPixel [GetElevationPixel \
		$env(TMA_DATA)/$elevationFilename $samples $line $sample]
	
	# Roads and rivers are gotten with respect to aoi space 
	global aoi
	if ![info exists aoi] {
	    tk_messageBox -type ok -default ok \
		    -title "Open or create an AOI first" \
		    -message "Open or create an AOI first"
	    destroy .queryTerrain
	    return
	}
	set nw [lindex $aoi 0]
	set se [lindex $aoi 1]
	set lines [lindex $aoi 2]
	set samples [lindex $aoi 3]
	
	set nwLat [lindex $nw 0]
	set nwLon [lindex $nw 1]
	set seLat [lindex $se 0]
	set seLon [lindex $se 1]

	set line   [expr $lines * ($nwLat - $lat) / ($nwLat - $seLat)]
	set sample [expr $samples * \
		(1.0 - (($seLon - $lon) / ($seLon - $nwLon)))]
	
	if {[catch {set roadAndRiverInfo [GetRoadsAndRiversInCell $line $sample]}] != 0} {
	    tk_messageBox -message "Run a minimum path query to load data."
	    destroy .queryTerrain
	    return
	}
	
	puts "RoadandRiverInfo = $roadAndRiverInfo"

	set roadCodes  [lindex $roadAndRiverInfo 0]
	puts "roadCodes = $roadCodes"
	set riverCodes [lindex $roadAndRiverInfo 1]
	puts "riverCodes = $riverCodes"
	set roadInfo {}
	foreach roadCode $roadCodes {
	    # Get road name using road code.

	    puts "calling Data_Select tma_terrainMapping {TERRAIN_ID} \
		    [list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
		    [list DATA_SET $dataSet] \
		    [list FEATURE_VALUE $roadCode]]"
	    
	    set roadName [lindex \
		    [Data_Select tma_terrainMapping {TERRAIN_ID} \
		    [list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
		    [list DATA_SET $dataSet] \
		    [list FEATURE_VALUE $roadCode]]] 0]
	    puts "roadCode = $roadCode"
	    puts "roadName = $roadName"
	    lappend roadInfo $roadName

	}
	set riverInfo {}
	foreach riverCode $riverCodes {
	    # Get river name using river code.

	    puts "calling Data_Select tma_terrainMapping {TERRAIN_ID} \
		    [list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
		    [list DATA_SET $dataSet] \
		    [list FEATURE_VALUE $riverCode]]"
	    
	    set riverName [lindex \
		    [Data_Select tma_terrainMapping {TERRAIN_ID} \
		    [list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
		    [list DATA_SET $dataSet] \
		    [list FEATURE_VALUE $riverCode]]] 0]
	    puts "riverCode = $riverCode"
	    puts "riverName = $riverName"
	    lappend riverInfo $riverName

	}

	.queryTerrain.top.text delete 1.0 end
	.queryTerrain.top.text insert 1.0 \
		"${terrainName}\nElevation : $elevationPixel meters.\n"
	if {$roadInfo == {}} {
	    .queryTerrain.top.text insert 3.0 \
		    "There are no roads in the cell.\n"
	} else {
	    .queryTerrain.top.text insert 3.0 \
		    "Roads : $roadInfo\n"
	}
	if {$riverInfo == {}} {
	    .queryTerrain.top.text insert 4.0 \
		    "There are no rivers in the cell.\n"
	} else {
	    .queryTerrain.top.text insert 4.0 \
		    "Rivers : $riverInfo\n"
	}
	set latLon [Map_PickCoord $mapRegionName]
    }
    destroy .queryTerrain
}

###############################################################################
proc queryTerrainFromPathFile {} {
    global env

    if {[catch {set file [open $env(TMA_DATA)/minPathN.dat r]}] != 0} {
	tk_messageBox -message "Run a minimum path query."
	return
    }
    gets $file timeValue
    gets $file lengthValue
    gets $file numPoints
    set pathPixels {}
    if {[catch {set dnetNodeFile [open $env(TMA_DATA)/minPathI.dat r]}] != 0} {
	tk_messageBox -message "Run a minimum path query."
	return
    }
    gets $dnetNodeFile dummy
    gets $dnetNodeFile dummy
    gets $dnetNodeFile dummy

    set mapRegionName [getMapHandle]
    #puts "mapRegionName = $mapRegionName"
    set regionName \
	    [lindex \
	    [Data_Select tma_mapRegion REGION \
	    [list [list NAME $mapRegionName]]] 0]
    #puts "regionName = $regionName"

    gets $file latLon
    gets $dnetNodeFile line
    set nodeIndex         [lindex $line 0]
    set neighborNodeIndex [lindex $line 1]
    set linkCost          [lindex $line 2]

    incr numPoints -1

    # Get region corners.
    set result [lindex [Data_Select tma_regionThemes \
	    [list DATA_SET COUNT_HEIGHT COUNT_WIDTH \
	    TEXT_FILENAME NWLAT NWLON SELAT SELON] \
	    [list \
	    [list REGION_NAME $regionName] \
	    [list THEME_TYPE Terrain]]] 0]
    #puts $result
    set dataSet  [lindex $result 0]
    set lines    [lindex $result 1]
    set samples  [lindex $result 2]
    set filename [lindex $result 3]
    set nwLat [lindex $result 4]
    set nwLon [lindex $result 5]
    set seLat [lindex $result 6]
    set seLon [lindex $result 7]
    set linesTimesSamples [expr $lines * $samples]

    set result [lindex [Data_Select tma_regionThemes \
	    [list TEXT_FILENAME] \
	    [list \
	    [list REGION_NAME $regionName] \
	    [list THEME_TYPE Elevation]]] 0]
    set elevationFilename [lindex $result 0]

    while {$numPoints >= 0} {
	set lat [lindex $latLon 0]
	set lon [lindex $latLon 1]
	
	set line   [expr $lines * ($nwLat - $lat) / ($nwLat - $seLat)]
	set sample [expr $samples * \
		(1.0 - (($seLon - $lon) / ($seLon - $nwLon)))]

	# Get pixel value in elevation file for line, sample.
	# Elevation data is stored as 16-bit meters.
	# Assumes Elevation and Terrain covers the same area,
	# and is same size.

	set elevationPixel [GetElevationPixel \
		$env(TMA_DATA)/$elevationFilename $samples $line $sample]

	# Check if the link is a road.
	set roadCode [GetRoadOnLink $nodeIndex $neighborNodeIndex]
	if {$roadCode == "Not a road link"} {
	    # Get pixel value in terrain file using line, sample.
	    set terrainCode [GetTerrainPixel \
		    $env(TMA_DATA)/$filename $samples $line $sample]
	    set linkInfo [list Terrain $terrainCode $elevationPixel $linkCost]
	} else {
	    set linkInfo [list Road $roadCode $elevationPixel $linkCost]
	}
	
	lappend pathPixels $linkInfo
	gets $file latLon
	gets $dnetNodeFile line
	set nodeIndex         [lindex $line 0]
	set neighborNodeIndex [lindex $line 1]
	set linkCost          [lindex $line 2]
	incr numPoints -1
    }
    close $file
    displayPathGraph $dataSet $timeValue $pathPixels
}

###############################################################################
# timeValue is total time for path.
proc displayPathGraph {dataSet timeValue pathPixels} {
    global env
    set maxElevation -9000
    set minElevation 9000

    # Set up pgm value to terrain id mapping.
    set pgms {}
    set terrIds {}
    set terrainMapping [Data_Select tma_terrainMapping \
	    {PGM_VALUE TERRAIN_ID} \
	    [list [list APPLICATION_ID $env(TMA_APPLICATION_ID)] \
	    [list DATA_SET $dataSet]]]
    foreach pair $terrainMapping {
	set pgm    [lindex $pair 0]
	set terrId [lindex $pair 1]
	lappend terrIds $terrId
	lappend pgms $pgm
    }

    # If the type is Road then just label it ROAD.
    # i.e. there is no distinction *HERE* between the different road types.
    
    # There are two types: Road and Terrain
    # if Road then just call it ROAD.
    # if Terrain then look up terrain id using pgm value
    # gotten from the terrain image.
    # The time is took is the last element in linkInfo
    # linkInfo will be:
    # Road 0 elevation(m) time,
    # or
    # Terrain (PGM_VALUE from tma_terrainMapping) elevation time

    # First get a list of all the terrain types in path.
    # Reserve a place for ROAD, this will make color of ROAD be black.
    set pixelSet {ROAD}
    foreach linkInfo $pathPixels {
	set type [lindex $linkInfo 0]
	if {$type == "Terrain"} {
	    # Get terrain name using pixel value.
	    set pgmValue [lindex $linkInfo 1]
	    set terrainCode \
		    [lindex $terrIds [lsearch -exact $pgms $pgmValue]]
	    if {[lsearch -exact $pixelSet $terrainCode] == -1} {
		if {$terrainCode != "WATER"} {
		    lappend pixelSet $terrainCode
		}
	    }
	}
	set elevPixel [lindex $linkInfo 2]
	if {$elevPixel > $maxElevation} { set maxElevation $elevPixel }
	if {$elevPixel < $minElevation} { set minElevation $elevPixel }
    }

    set cList {00 ff}
    foreach r $cList {
    	foreach g $cList {
	    foreach b $cList {
		lappend colors "#${r}${g}${b}"
	    }
	}
    }

    toplevel .pathGraph -class Dialog
    wm title .pathGraph "Path Graph"
    
    # Create frames.
    frame .pathGraph.top
    frame .pathGraph.middle
    frame .pathGraph.bottom
    
    # Graph results area.
    set c [canvas .pathGraph.top.canvas -height 140 -width 440]
    pack .pathGraph.top.canvas -anchor center

    # Legend
    set colorIndex 0
    foreach p $pixelSet {
	label .pathGraph.middle.[string tolower $p] -text "$p\n" \
		-pady 0 -anchor w \
		-foreground [lindex $colors $colorIndex]
	incr colorIndex
	pack .pathGraph.middle.[string tolower $p] -side bottom
    }
    
    # Cancel button.
    button .pathGraph.bottom.cancel -text Cancel -width 7  \
	    -command { destroy .pathGraph }
    pack .pathGraph.bottom.cancel -anchor center -side left -pady 4 
    pack .pathGraph.top .pathGraph.middle .pathGraph.bottom -side top
    wm resizable .pathGraph 0 0

    $c create line 20 120 420 120 -fill black -arrow last
    $c create line 20 120  20 20  -fill black -arrow last
    $c create text  10   5 -anchor nw -fill black -text "Elevation"
    $c create text 385 120 -anchor nw -fill black -text "Time"
    
    set x1 20
    set y1 0
    if {$timeValue != 0} {
	set xFactor [expr 400.0 / $timeValue]
    } else {
	# timeValue should never be 0, but if it is handle it.
	set xFactor 1
    }
    set index 0
    foreach pathPixel $pathPixels {
	set x0 $x1
	set y0 $y1
	set type [lindex $pathPixel 0]
	if {$type == "Terrain"} {
	    set pgmValue [lindex $pathPixel 1]
	    set code \
		    [lindex $terrIds [lsearch -exact $pgms $pgmValue]]
	    if {$code == "WATER"} { set code ROAD }
	} else {
	    set code ROAD
	}
	set elevPixel [lindex $pathPixel 2]
	set linkCost  [lindex $pathPixel 3]
	set y1 \
		[expr int(20.0 + 100.0 * \
		($elevPixel - $minElevation) / ($maxElevation - $minElevation))]
	if {$index == 0} {
	    incr index
	    continue
	}
	set x1 [expr $x0 + ($xFactor * $linkCost)]
	set colorIndex [lsearch -exact $pixelSet $code]
	$c create line $x0 $y0 $x1 $y1 -fill [lindex $colors $colorIndex]
    }
}



