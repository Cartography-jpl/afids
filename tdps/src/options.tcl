set defaultOptions { \
	selectionRadius "Selection Radius" 5 \
	fgColor "Foreground Color" white \
	bgColor "Background Color" black \
    }

# called by tdps.tcl:loadHistory
proc loadOptions {} {
    global defaultOptions

    loadParams "no options file here" $defaultOptions
}

proc loadParams {path defaultParams} {
    if {[catch {glob $path}]} {
	set params $defaultParams
    } else {
	set file [open $path r]
	set params [read $file]
	close $file
    }
    
    foreach {var label value} $params {
	global $var
	set $var $value
    }
}

# called by aoi.tcl:setAoi
proc saveHistory {} {
#    global defaultHistory
#    saveParams history $defaultHistory
    puts "not saving history"
}

