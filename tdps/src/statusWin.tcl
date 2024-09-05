proc openStatus {heading logFile} {
    global statusLogFile
    global tdpsDatabaseDir
    
    set statusLogFile ${tdpsDatabaseDir}/${logFile}.log
    file delete -force $statusLogFile
    exec touch $statusLogFile

    set w .status
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "TDPS Processing Status"
    wm iconname $w "TDPS Processing Status"

    frame $w.buttons
    pack $w.buttons -side bottom -fill x -pady 2m
    button $w.buttons.close -text "Close" -command "destroy $w"
    pack $w.buttons.close $w.buttons.close -side left -expand 1

    text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set" -setgrid 1 -height 20
    scrollbar $w.scroll -command "$w.text yview"
    pack $w.scroll -side right -fill y
    pack $w.text -expand yes -fill both

    addStatus $heading
}

proc addStatus {status {nonewline false}} {
    global statusLogFile
    global tdpsDatabaseDir

    set file [open $statusLogFile "a"]

    set text .status.text

    if {$nonewline} {
	$text insert end "${status}"
	puts -nonewline $file $status
    } else {
	$text insert end "${status}\n"
	puts $file $status
    }
    $text yview moveto 1.0

    update idletasks
    
    close $file
}