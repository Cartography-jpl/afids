proc viewAttributeSummary {{file ""}} {
    if {$file == ""} {
	set types {{"Shape files" {.shp}} {"All files" *}}

	global w
	set file [tk_getOpenFile -filetypes $types -parent $w]

	if {$file == ""} {
	    return
	}
    }

    if {[catch {getAttributeValueSummary $file 25} avList]} {
  	tk_messageBox -message "Error reading shapefile:\n$avList" -title "Read Error"
  	return
    }

    set win .avSummary

    catch {destroy $win}
    toplevel $win
    wm protocol $win WM_DELETE_WINDOW "destroy $win"
    wm title $win "$file Attribute Summary"
    wm iconname $win "$file Attribute Summary"

    set grid $win.grid
    frame $grid
    pack $grid -fill both -expand true
    grid columnconfig $grid 0 -weight 1
    grid rowconfig $grid 0 -weight 1

    set text $grid.text
    set scroll $grid.scroll
    text $text -relief sunken -bd 2 -yscrollcommand "$scroll set" -setgrid 1 -height 20 -width 30 -wrap none
    grid $text -row 0 -column 0 -sticky news
    scrollbar $scroll -command "$text yview"
    grid $scroll -row 0 -column 1 -sticky news

    set ok $grid.ok
    button $ok -text "OK" -command "destroy $win"
    grid $ok -row 2 -column 0
    grid configure $ok -columnspan 2

    set insert ""
    foreach avPair $avList { ; # [lsort -index 0 $avList]
	set attribute [lindex $avPair 0]
	set valueList [lindex $avPair 1]

	append insert "${attribute}\n"

	foreach value [lsort $valueList] {
	    append insert "    ${value}\n"
	}
    }
    
    $text insert 0.1 $insert
}

