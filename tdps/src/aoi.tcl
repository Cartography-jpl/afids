proc presentSetAOIDialog {} {
    global aoiN aoiS aoiE aoiW

    set w .setAOI
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Set AOI"
    wm iconname $w "Set AOI"

    set g $w.grid
    frame $g
    pack $g -expand true -fill both

    set l $g.aoiLabel
    label $l -text "AOI"
    grid $l -row 1 -column 1 -padx 1 -pady 1
    set e $g.aoiN
    entry $e -width 8
    $e insert 0 [format "%4.2f" $aoiN] 
    grid $e -row 0 -column 1 -padx 1 -pady 1
    set e $g.aoiS
    entry $e -width 8
    $e insert 0 [format "%4.2f" $aoiS] 
    grid $e -row 2 -column 1 -padx 1 -pady 1
    set e $g.aoiW
    entry $e -width 9
    $e insert 0 [format "%4.2f" $aoiW] 
    grid $e -row 1 -column 0 -padx 1 -pady 1
    set e $g.aoiE
    entry $e -width 9
    $e insert 0 [format "%4.2f" $aoiE] 
    grid $e -row 1 -column 2 -padx 1 -pady 1
    
    set f $w.buttons
    frame $f
    pack $f -fill both

    set b $f.ok
    button $b -text "OK" -command "setAOI"
    pack $b -expand true -fill both -side left
    bind $b <Enter> {setObjInfo "Update AOI with these values"}
    bind $b <Leave> clearObjInfo

    set b $f.reset
    button $b -text "Reset" -command "resetAOIEntries"
    pack $b -expand true -fill both -side left
    bind $b <Enter> {setObjInfo "Restore previous AOI values"}
    bind $b <Leave> clearObjInfo

    set b $f.view
    button $b -text "View" -command "viewAOIEntries"
    pack $b -expand true -fill both -side left
    bind $b <Enter> {setObjInfo "Set to current view extent"}
    bind $b <Leave> clearObjInfo

    set b $f.world
    button $b -text "World" -command "worldAOIEntries"
    pack $b -expand true -fill both -side left
    bind $b <Enter> {setObjInfo "Set to world extent"}
    bind $b <Leave> clearObjInfo

    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -expand true -fill both -side left
    bind $b <Enter> {setObjInfo "Close dialog without changing AOI"}
    bind $b <Leave> clearObjInfo
}

proc resetAOIEntries {} {
    global aoiN aoiS aoiE aoiW

    set w .setAOI
    set g $w.grid

    $g.aoiN delete 0 end
    $g.aoiN insert 0 $aoiN

    $g.aoiS delete 0 end
    $g.aoiS insert 0 $aoiS

    $g.aoiE delete 0 end
    $g.aoiE insert 0 $aoiE

    $g.aoiW delete 0 end
    $g.aoiW insert 0 $aoiW
}

proc worldAOIEntries {} {
    set w .setAOI
    set g $w.grid

    $g.aoiN delete 0 end
    $g.aoiN insert 0 90.0

    $g.aoiS delete 0 end
    $g.aoiS insert 0 -90.0

    $g.aoiE delete 0 end
    $g.aoiE insert 0 180.0

    $g.aoiW delete 0 end
    $g.aoiW insert 0 -180.0
}

proc viewAOIEntries {} {
    global w

    set g .setAOI.grid

    $g.aoiN delete 0 end
    $g.aoiN insert 0 [lindex [$w.grid.viewN config -text] 4]

    $g.aoiS delete 0 end
    $g.aoiS insert 0 [lindex [$w.grid.viewS config -text] 4]

    $g.aoiE delete 0 end
    $g.aoiE insert 0 [lindex [$w.grid.viewE config -text] 4]

    $g.aoiW delete 0 end
    $g.aoiW insert 0 [lindex [$w.grid.viewW config -text] 4]
}

set aoiN 90
set aoiE 180
set aoiW -180
set aoiS -90
proc setAOI {} {
    global aoiN aoiS aoiE aoiW

    set w .setAOI
    set g $w.grid

    set e $g.aoiN
    set north [string trim [$e get]]

    set e $g.aoiS
    set south [string trim [$e get]]

    set e $g.aoiW
    set west [string trim [$e get]]

    set e $g.aoiE
    set east [string trim [$e get]]

    if {$north < $south} {
	tk_messageBox -message "The northern limit is south of the southern limit"
	return
    }

    if {$east < $west} {
	tk_messageBox -message "Note: this AOI straddles 180th meridian"
    }

    set aoiN $north
    set aoiS $south
    set aoiW $west
    set aoiE $east

    destroy $w

    saveHistory
}

