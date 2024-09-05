#load ossimTcl.so

# returns {ll_lat ll_lon ur_lat ur_lon} given absolute path to cib a.toc file
proc cibaoi {absPathToToc} {

    set imageCount [ossim::getEntryListSize $absPathToToc]

    set ll_lat 90.0
    set ur_lat -90.0
    set ll_lon 180.0
    set ur_lon -180.0

    for {set index 0} {$index < $imageCount} {incr index} {
	set geom [ossim::getEntryGeometry $absPathToToc $index]

	set geom [split $geom \n]

	foreach line $geom {
	    set line [split $line ": "]
	    if {[lindex $line 0] == "ll_lat"} {
		set lat [lindex $line 2]
		if {$lat < $ll_lat} {
		    set ll_lat $lat
		}
	    }
	    if {[lindex $line 0] == "ll_lon"} {
		set lon [lindex $line 2]
		if {$lon < $ll_lon} {
		    set ll_lon $lon
		}
	    }
	    if {[lindex $line 0] == "ur_lat"} {
		set lat [lindex $line 2]
		if {$lat > $ur_lat} {
		    set ur_lat $lat
		}
	    }
	    if {[lindex $line 0] == "ur_lon"} {
		set lon [lindex $line 2]
		if {$lon > $ur_lon} {
		    set ur_lon $lon
		}
	    }
	}
    }

    return [list $ll_lat $ll_lon $ur_lat $ur_lon]
}
