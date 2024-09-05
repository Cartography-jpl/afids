proc registerDatabaseVar {var val doc} {
    global databaseVars

    global $var
    set $var $val

    if {[lsearch [array names databaseVars] $var] < 0} {
	set databaseVars($var) $doc
    }
}

set databaseWritten false
set currentColoredFiles {}
proc writeDatabase {} {
    global databaseWritten
    set databaseWritten true

    global tdpsDatabase
    
    if {$tdpsDatabase == ""} {
	return
    }

    global currentColoredFiles fileColorMap
    set fileColorMap {}
    foreach file $currentColoredFiles {
	global $file
	lappend fileColorMap $file [set $file]
    }

    set file [open $tdpsDatabase "w"]

    global databaseVars
    foreach var [array names databaseVars] {
	puts $file $var
	puts $file $databaseVars($var)
	global $var
	puts $file [set $var]
    }

    close $file
}

set databaseRead false
proc readDatabase {} {
    global databaseRead
    set databaseRead true

    global tdpsDatabase
    
    set file [open $tdpsDatabase "r"]

    global databaseVars
    foreach var [array names databaseVars] {
	gets $file var
	gets $file doc
	global $var
	gets $file $var
    }

    close $file

    global playbox_aoiMinLat playbox_aoiMaxLat playbox_aoiMinLon playbox_aoiMaxLon
    global playbox_aoiMinLat_last playbox_aoiMaxLat_last playbox_aoiMinLon_last playbox_aoiMaxLon_last
    set playbox_aoiMinLat_last $playbox_aoiMinLat
    set playbox_aoiMaxLat_last $playbox_aoiMaxLat
    set playbox_aoiMinLon_last $playbox_aoiMinLon
    set playbox_aoiMaxLon_last $playbox_aoiMaxLon
    revertLatLon

    createColorPalette2

    global fileColorMap currentColoredFiles
    set currentColoredFiles {}
    foreach {coloredFile colorIndex} $fileColorMap {
	global $coloredFile
	set $coloredFile $colorIndex
	lappend currentColoredFiles $coloredFile
    }
}
