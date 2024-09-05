proc debugTdps {} {
    puts "xyzzx"

    puts "Loaded files:"
    foreach file [getLoadedFilenames] {
	puts "$file : [getLoadedFileData $file]"
    }

    global editedFile
    puts "editedFile: $editedFile"
}

proc dumpStack {} {
    for {set x [expr [info level] - 1]} {$x > 0} {incr x -1} {
	puts "$x: [info level $x]"
    }
}
