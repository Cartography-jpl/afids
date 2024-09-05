# load shapeUtils.so
# source html_library.tcl
# source globals.tcl
# source canvasMouse.tcl
# source loadUnload.tcl
# source view.tcl
# source aoi.tcl
# source selection.tcl
# source propEd.tcl
# source attribute.tcl
# source colors.tcl
# source vecEdit.tcl
# source options.tcl
# source misc.tcl

# This proc computes the width and height added by the window manager's border to the created widgetry
proc measureWindowFrame {} {
    global w winxgap winygap winsized

    update idletasks

    set geom [wm geometry $w]
    set geom [split $geom "x+"]
    set winx [lindex $geom 0]
    set winy [lindex $geom 1]

    global canvasHscroll canvasVscroll
    global c

    set cV [winfo height $c]
    set cH [winfo width $c]

    set winxgap [expr $winx - $cH]
    set winygap [expr $winy - $cV]

    if {$winxgap > 0} {
	set winsized 1
    } else {
	measureWindowFrame
    }
}

# if the window is too big for the current zoom, the zoom can be increased, or the window size can be reduced
proc checkCanvasSize {{preferWindowResize false}} {
    global w winsized winxgap winygap scalePower
    if {! $winsized} {return}
    set geom [wm geometry $w]
    set geom [split $geom "x+"]
    set winx [lindex $geom 0]
    set winy [lindex $geom 1]

    set scaleAdjust -1
    set xgap 1
    while {$xgap > 0 || $ygap > 0} {
	incr scaleAdjust

	set xgap [expr $winx - $winxgap - 1 - (360.0 * pow (2, $scalePower + $scaleAdjust))]
	set ygap [expr $winy - $winygap - 1 - (180.0 * pow (2, $scalePower + $scaleAdjust))]
	if {$xgap <= 0 && $ygap <= 0} {
	    if {$scaleAdjust > 0} {
		zoomTo [expr $scalePower + $scaleAdjust]
	    }

	    return

	} elseif {$preferWindowResize} {
  	    if {$xgap > 0} {
  		set winx [expr int (ceil ($winxgap + 1 + (360.0 * pow (2, $scalePower))))]
  	    }
  	    if {$ygap > 0} {
  		set winy [expr int (ceil ($winygap + 1 + (180.0 * pow (2, $scalePower))))]
  	    }
  	    wm geometry $w "${winx}x${winy}"

	    return
	}
    }
}

proc yToLat {y} {
    global c scalePower

    expr - [$c canvasy $y] / pow (2, $scalePower)
}

proc xToLon {x} {
    global c scalePower

    expr [$c canvasx $x] / pow (2, $scalePower)
}

proc setupSelectionCursor {} {
    global cursorMode
    set cursorMode left_ptr

    setAppCursor left_ptr

    global radiotool
    set radiotool select
}

proc setupZoominCursor {} {
    global cursorMode
    set cursorMode zoomin

    global fgColor tdpsGraphics
    setAppCursor "@${tdpsGraphics}/zoomin.xbm $fgColor"

    global radiotool
    set radiotool zoomin
}

proc setupZoomoutCursor {} {
    global cursorMode
    set cursorMode zoomout

    global fgColor tdpsGraphics
    setAppCursor "@${tdpsGraphics}/zoomout.xbm $fgColor"

    global radiotool
    set radiotool zoomout
}

proc setupHandCursor {} {
    global cursorMode
    set cursorMode hand

    global fgColor tdpsGraphics
    setAppCursor "@${tdpsGraphics}/hand.xbm $fgColor"

    global radiotool
    set radiotool hand
}

proc setupLineCursor {} {
    global cursorMode
    set cursorMode addLine

    global fgColor tdpsGraphics
    setAppCursor "@${tdpsGraphics}/line.xbm $fgColor"

    global radiotool
    set radiotool line
}

proc setupPolygonCursor {} {
    global cursorMode
    set cursorMode addPolygon

    global fgColor tdpsGraphics
    setAppCursor "@${tdpsGraphics}/polygon.xbm $fgColor"

    global radiotool
    set radiotool polygon
}

proc createVectorEditorWindow {} {
    global w creatingMainWindow
    global editingVertices

    set editingVertices false

    set creatingMainWindow true

    set w .vectorEditor
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "wm iconify $w"
    # Terrain Database Preparation System
    wm title $w "TDPS Vector Editor"
    wm iconname $w "TDPS Vector Editor"

    addMenuBarToMainWindow
    addToolBoxToMainWindow
    addCanvasToMainWindow
    addStatusAreaToMainWindow
    measureWindowFrame

    set creatingMainWindow false

    updateViewDoc
}

proc unloadAllCmd {} {
    set touched 0
    foreach file [getLoadedFilenames] {
	if {[getLoadedFileChangedFlag $file]} {
	    set touched 1
	    break
	}
    }

    if {$touched} {
	if {[promptUser "Unload without saving?" "Some files have changed. Unload without saving?" {Yes No}] == "Yes"} {
	    unloadAll
	    return 1
	}
    } else {
	unloadAll
	return 1
    }
    return 0
}

proc addMenuBarToMainWindow {} {
    global w argv

    # create menu bar
    menu $w.menu -tearoff 0

    # file menu
    set m $w.menu.file
    menu $m -tearoff 0
    $w.menu add cascade -label "File" -menu $m -underline 0
    if {[lsearch $argv "expert"] >= 0} {
	$m add command -label "Set AOI ..." -command "presentSetAOIDialog"
    }
    $m add command -label "Select file receiving pastes/new vectors ..." -command {selectEditedFile "" 1} -underline 12
    $m add separator
    if {[lsearch $argv "expert"] >= 0} {
	$m add command -label "Load one file ..." -command "loadAFile"
    }
    $m add command -label "Load files ..." -command "loadSeveralFiles" -underline 0

    $m add cascade -label "Import" -menu $m.import -underline 0
    menu $m.import -tearoff 0
    $m.import add command -label "Line vectors ..." -command importLineVectors -underline 0
    $m.import add command -label "Polygon vectors ..." -command importPolygonVectors -underline 0

    $m add command -label "Unload file ..." -command "unloadAFile" -underline 0
    $m add command -label "Unload all files" -command "unloadAllCmd"
    $m add command -label "Save" -command "saveOneOrMoreChangedFiles" -underline 0
    if {[lsearch $argv "expert"] >= 0} {
	$m add command -label "Save as ..." -command "selectDataToSave" -underline 5
    }
    $m add separator
    $m add command -label "Exit TDPS" -command "exitTdps" -underline 1

    # edit menu
    set m $w.menu.edit
    menu $m -tearoff 0
    $w.menu add cascade -label "Edit" -menu $m -underline 0
    $m add command -label "Copy (Ctl-C)" -command copySelectedVectors -underline 0
    $m add command -label "Cut (Ctl-X)" -command cutSelectedVectors -underline 2
    $m add command -label "Paste (Ctl-V)" -command pasteVectorPasteBuffer -underline 0
    $m add separator
    $m add command -label "Delete (Del)" -command deleteBackspace -underline 0
    $m add separator
    global editMenuWidget
    set editMenuWidget $m
    $m add command -label "Undo (Ctl-U)" -command undo -underline 0

    # zoom
    set m $w.menu.zoom
    menu $m -tearoff 0
    $w.menu add cascade -label "Zoom" -menu $m -underline 0
    $m add command -label "In (Ctl-I)" -command zoominCenter -underline 0
    $m add command -label "Out (Ctl-O)" -command zoomoutCenter -underline 0
    $m add command -label "To Fit All (Ctl-F)" -command "zoomToFit vec" -underline 3
    $m add command -label "To Fit Selection" -command "zoomToFit selected" -underline 0
    $m add command -label "To Playbox" -command "zoomToAoi" -underline 0
    $m add command -label "Center On Coord ..." -command "centerOnCoord" -underline 0

    # select
    set m $w.menu.select
    menu $m -tearoff 0
    $w.menu add cascade -label "Select" -menu $m -underline 0
    $m add command -label "None" -command unselectAll -underline 0
    $m add command -label "All (Ctl-A)" -command selectAll -underline 0
    $m add command -label "Next Vertex (>)" -command gotoNextVertex -underline 0
    $m add command -label "Previous Vertex (<)" -command gotoPreviousVertex -underline 0
    $m add command -label "By File ..." -command selectByFile -underline 3
    $m add cascade -label "By CBS Record ID" -underline 14 -menu $w.menu.select.byCbsIds
    $m add command -label "Unselect by Vector ID ..." -underline 0 -command unselectById
    set m $w.menu.select.byCbsIds
    menu $m -tearoff 0
    global cbsOutput
    foreach class [lsort [array names cbsOutput]] {
	set file $cbsOutput($class)
	$m add command -label $file -command "selectByCbsRecordId $file"
    }

    # vector menu
    set m $w.menu.vector
    global vectorMenu
    set vectorMenu $m
    menu $m -tearoff 0
    $w.menu add cascade -label "Vector" -menu $m -underline 0
    $m add cascade -label "Color" -menu $m.color -underline 0
    menu $m.color -tearoff 0
    set widget [makeColorMenu $m.color selectedVectorColor setSelectedVectorColor]
    $m.color add cascade -label "Selected" -menu $widget -underline 0
    $m.color add command -label "Options ..." -command setVectorColorOptions -underline 0
    $m add command -label "Edit Vertices" -command editVertices -underline 0
    $m add command -label "Insert Vertices (Ctl-P)" -command insertVertices -state disabled -underline 0
    $m add command -label "Delete Vertices" -command deleteVertices -state disabled -underline 0
    $m add command -label "Properties ..." -command vectorProperties
    $m add command -label "Break Object (Ctl-B)" -command splitVector -underline 0
    $m add cascade -label "Nudge" -menu ${m}.nudge -underline 0
    set m ${m}.nudge
    menu $m -tearoff 1
    $m add command -label "Nudge North (Up Arrow)" -command "nudgeVector north"
    $m add command -label "Nudge South (Down Arrow)" -command "nudgeVector south"
    $m add command -label "Nudge East (Right Arrow)" -command "nudgeVector east"
    $m add command -label "Nudge West (Left Arrow)" -command "nudgeVector west"

    # map menu
    set m $w.menu.overlay
    $w.menu add cascade -label "Overlay" -menu $m -underline 0
    menu $m -tearoff 0

    set m $w.menu.overlay.map
    $w.menu.overlay add cascade -label "Map" -menu $m -underline 0
    menu $m -tearoff 0
    $m add command -label "Toggle (Ctl-M)" -command toggleMap -underline 0
    $m add cascade -label "Nudge" -menu $w.menu.overlay.map.nudge -underline 0
    set m $w.menu.overlay.map.nudge
    menu $m -tearoff 1
    $m add command -label "Nudge North" -command "nudgeMap north"
    $m add command -label "Nudge South" -command "nudgeMap south"
    $m add command -label "Nudge East" -command "nudgeMap east"
    $m add command -label "Nudge West" -command "nudgeMap west"

    set m $w.menu.overlay.grid
    $w.menu.overlay add cascade -label "Grid" -menu $m -underline 0
    menu $m -tearoff 0
    $m add command -label "On" -command gridOn -underline 1
    $m add command -label "Off" -command gridOff -underline 1
    $m add command -label "Clear" -command gridClear -underline 0
    $m add command -label "Set Size ..." -command gridSetSize
    set widget [makeColorMenu $m gridColor setGridColor]
    $m add cascade -label "Color" -menu $widget -underline 0
    $m add check -label "Crop to playbox" -variable cropGridToPlaybox -command flashGrid
    global cropGridToPlaybox
    set cropGridToPlaybox 1

    set m $w.menu.overlay.aoi
    $w.menu.overlay add cascade -label "Playbox" -menu $m -underline 0
    menu $m -tearoff 0
    $m add command -label "On" -command aoiOn -underline 1
    $m add command -label "Off" -command aoiOff -underline 1
    set widget [makeColorMenu $m gridColor setAoiColor]
    $m add cascade -label "Color" -menu $widget -underline 0

    # tools menu
    if {[lsearch $argv "expert"] >= 0} {
	set m $w.menu.tools
	menu $m -tearoff 0
	$w.menu add cascade -label "Tools" -menu $m -underline 0
	$m add command -label "View Attribute Sample ..." -command viewAttributeSummary
	$m add command -label "Reclassify Polygon Holes ..." -command reclassifyPolygonHoles
	$m add command -label "Crop Vectors ..." -command cropVectors
    }

    # help menu
    set m $w.menu.help
    menu $m -tearoff 0
    $w.menu add cascade -label "Help" -menu $m -underline 0
    global env
    $m add command -label "About Vector Editor ..." -command "help $env(AFIDSTOP)/share/doc/tdps/help.html #about" -underline 0

    # associate menu with main window
    $w configure -menu $w.menu
}

proc exitTdps {} {
    set touched 0
    foreach file [getLoadedFilenames] {
	if {[getLoadedFileChangedFlag $file]} {
	    set touched 1
	    break
	}
    }

    if {$touched} {
	if {[promptUser "Exit without saving?" "Some files have changed. Exit without saving?" {Yes No}] == "Yes"} {
	    exit
	}
    } else {
	exit
    }
}

set aoiColor ""
proc setAoiColor {color} {
    global colorPaletteValues
    set color [lindex $colorPaletteValues $color]

    global aoiColor

    set aoiColor $color

    aoiOn

    global c
    $c itemconfigure aoi -outline $aoiColor
}

proc aoiOn {} {
    aoiOff

    global c scalePower
    global aoiColor

    if {$aoiColor == ""} {
	global fgColor
	set aoiColor $fgColor
    }

    global playbox_aoiMinLat playbox_aoiMinLon playbox_aoiMaxLat playbox_aoiMaxLon

    set x1 [expr $playbox_aoiMinLon * pow (2, $scalePower)]
    set x2 [expr $playbox_aoiMaxLon * pow (2, $scalePower)]
    set y1 [expr - $playbox_aoiMinLat * pow (2, $scalePower)]
    set y2 [expr - $playbox_aoiMaxLat * pow (2, $scalePower)]
    $c create rect $x1 $y1 $x2 $y2 -outline $aoiColor -tags aoi
}

proc aoiOff {} {
    global c

    $c delete aoi
}

set gridColor ""
proc setGridColor {color} {
    global colorPaletteValues
    set color [lindex $colorPaletteValues $color]

    global gridColor

    set gridColor $color

    gridOn

    global c
    $c itemconfigure grid -fill $gridColor
}

proc max {a b} {
    if {$a > $b} {
	return $a
    } else {
	return $b
    }
}

proc min {a b} {
    if {$a < $b} {
	return $a
    } else {
	return $b
    }
}

set gridIsOn false

proc flashGrid {} {
    global gridIsOn
    if {$gridIsOn} {
	gridOff
	gridOn
    } else {
	gridOn
	gridOff
    }
}

proc gridOn {} {
    gridOff

    global gridSize
    global c scalePower
    global gridIsOn
    global gridColor

    if {$gridColor == ""} {
	global fgColor
	set gridColor $fgColor
    }

    set gridIsOn true

    global playbox_aoiMinLat playbox_aoiMaxLat playbox_aoiMinLon playbox_aoiMaxLon
    global cropGridToPlaybox

    set minX [expr $playbox_aoiMinLon * pow (2, $scalePower)]
    set maxX [expr $playbox_aoiMaxLon * pow (2, $scalePower)]
    set maxY [expr - $playbox_aoiMinLat * pow (2, $scalePower)]
    set minY [expr - $playbox_aoiMaxLat * pow (2, $scalePower)]

    for {set lat -90.0} {$lat <= 90.0} {set lat [expr $lat + $gridSize]} {
	set y [expr $lat * pow (2, $scalePower)]

	if {! $cropGridToPlaybox || ($y >= $minY && $y <= $maxY)} {
	    set x1 [expr -180.0 * pow (2, $scalePower)]
	    set x2 [expr 180.0 * pow (2, $scalePower)]
	    if {$cropGridToPlaybox} {
		set x1 [max $x1 $minX]
		set x2 [min $x2 $maxX]
	    }
	    $c create line $x1 $y $x2 $y -fill $gridColor -tags grid -width 3
	}
    }

    for {set lon -180.0} {$lon <= 180.0} {set lon [expr $lon + $gridSize]} {
	set x [expr $lon * pow (2, $scalePower)]

	if {! $cropGridToPlaybox || ($x >= $minX && $x <= $maxX)} {
	    set y1 [expr -90.0 * pow (2, $scalePower)]
	    set y2 [expr 90.0 * pow (2, $scalePower)]
	    if {$cropGridToPlaybox} {
		set y1 [max $y1 $minY]
		set y2 [min $y2 $maxY]
	    }
	    $c create line $x $y1 $x $y2 -fill $gridColor -tags grid -width 3
	}
    }

    global filledGridCells
    foreach latLon $filledGridCells {
	checkGridCell [lindex $latLon 0] [lindex $latLon 1]
    }
}

proc gridOff {} {
    global c
    global gridIsOn

    set gridIsOn false

    $c delete grid
}

proc gridClear {} {
    global filledGridCells

    if {[llength $filledGridCells] > 10 && [promptUser "Clear Grid?" "Grid has [llength $filledGridCells] cells checked. Confirm clear?" {Yes No}] != "Yes"} {
	return
    }

    gridOff
    set filledGridCells {}
    writeDatabase
    gridOn
}

registerDatabaseVar gridSize 1.0 "Grid spacing in decimal degrees"
proc gridSetSize {} {
    set w .gridSize
    set title "Enter Grid Size"
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set l $w.label
    label $l -text "Enter Grid Spacing In Decimal Degrees:" -anchor w
    pack $l -side top -anchor w

    set e $w.entry
    entry $e
    pack $e -side top -fill x
    focus $e

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.ok
    set command "acceptGridSize $w $e"
    button $b -text "OK" -command $command
    bind $e <Return> $command
    pack $b -side left -fill both -expand true
    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -fill both -expand true
}

registerDatabaseVar filledGridCells {} "Lower left lat/lon of filled grid cells"
proc acceptGridSize {w e} {
    global gridSize
    global filledGridCells

    if {$filledGridCells != {} && [promptUser "Clear Grid?" "Changing grid size requires clearing checked cells. Continue?" {Yes No}] != "Yes"} {
	return
    }

    gridOff

    set filledGridCells {}
    set gridSize [$e get]

    writeDatabase

    gridOn

    destroy $w
}

proc help {{helpDoc ""} {sublink "#"}} {
    global env

    if { $helpDoc == "" } {
	set helpDoc $env(AFIDSTOP)/share/doc/tdps/help.html
    }

    global HM_text

    set w .help
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Help"
    wm iconname $w "Help"

    set g $w.grid
    frame $g
    pack $g -expand true -fill both
    grid rowconfigure $g 0 -weight 1

    set t $g.text
    text $t -relief sunken -xscrollcommand "$g.xscroll set" -yscrollcommand "$g.yscroll set" \
	    -height 30 -width 60 -setgrid 1
    scrollbar $g.xscroll -command "$t xview" -orient horiz
    scrollbar $g.yscroll -command "$t yview"

    grid $t -row 0 -column 0 -sticky news
    grid $g.xscroll -row 1 -column 0 -sticky news
    grid $g.yscroll -row 0 -column 1 -sticky news

    set b $g.dismiss
    button $b -text "Close" -command "destroy $w"
    grid $b -row 2 -column 0

    grid columnconfigure $g 0 -weight 1
    set HM_text $t
    
    set file [open $helpDoc r]
    set html [read $file]
    close $file

    HMinit_win $HM_text

    HMparse_html $html "HMrender $HM_text"
    HMlink_callback "" $sublink
}

# Override the library link-callback routine for the sample app.
# It only handles the simple cases.

proc HMlink_callback {win href} {
	global Url

	if {[string match #* $href]} {
		render $href
		return
	}
	if {[string match /* $href]} {
		set Url $href
	} else {
		set Url [file dirname $Url]/$href
	}

	update
	render $Url
}

# Go render a page.  We have to make sure we don't render one page while
# still rendering the previous one.  If we get here from a recursive 
# invocation of the event loop, cancel whatever we were rendering when
# we were called.
# If we have a fragment name, try to go there.

proc render {file} {
    global HM_text Url
    global Running message

    set fragment ""
    regexp {([^#]*)#(.+)} $file dummy file fragment
    if {$file == "" && $fragment != ""} {
	HMgoto $HM_text $fragment
	return
    }

    return

    HMreset_win $HM_text
    set Running busy
    set message "Displaying $file"
    update idletasks
    if {$fragment != ""} {
	HMgoto $HM_text $fragment
    }
    set Url $file

    HMparse_html [get_html $file] {HMrender $HM_text}
    set Running ready
    HMset_state $HM_text -stop 1	;# stop rendering previous page if busy
    set message ""
}

proc addToolBoxToMainWindow {} {
    global w radiotool tdpsGraphics

    # create tool box
    set toolbox $w.tb
    frame $toolbox
    pack $toolbox -fill both

    # selection tool
    set b $toolbox.select
    image create photo select -file ${tdpsGraphics}/select.gif
    radiobutton $b -image select -indicatoron 0 -variable radiotool -value select -command setupSelectionCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Selection Tool"}
    bind $b <Leave> clearObjInfo
    set radiotool select

    # magnification tools
    set b $toolbox.zoomin
    image create photo zoomin.gif -file ${tdpsGraphics}/zoomin.gif
    radiobutton $b -image zoomin.gif -indicatoron 0 -variable radiotool -value zoomin -command setupZoominCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Zoomin Tool"}
    bind $b <Leave> clearObjInfo
    set b $toolbox.zoomout
    image create photo zoomout.gif -file ${tdpsGraphics}/zoomout.gif
    radiobutton $b -image zoomout.gif -indicatoron 0 -variable radiotool -value zoomout -command setupZoomoutCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Zoomout Tool"}
    bind $b <Leave> clearObjInfo

    # zoom to fit tool
    set b $toolbox.zoomToFit
    image create photo zoomfit -file ${tdpsGraphics}/zoomfit.gif
    button $b -image zoomfit -command "zoomToFit vec"
    pack $b -side left
    bind $b <Enter> {setObjInfo "Zoom To Fit All"}
    bind $b <Leave> clearObjInfo

    # hand tool
    set b $toolbox.hand
    image create photo hand -file ${tdpsGraphics}/hand.gif
    radiobutton $b -image hand -indicatoron 0 -variable radiotool -value hand -command setupHandCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Drag Tool (Scroll Display)"}
    bind $b <Leave> clearObjInfo

    # line tool
    set b $toolbox.line
    image create photo line -file ${tdpsGraphics}/line.gif
    radiobutton $b -image line -indicatoron 0 -variable radiotool -value line -command setupLineCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Add Line Tool"}
    bind $b <Leave> clearObjInfo

    # polygon tool
    set b $toolbox.polygon
    image create photo polygon -file ${tdpsGraphics}/polygon.gif
    radiobutton $b -image polygon -indicatoron 0 -variable radiotool -value polygon -command setupPolygonCursor
    pack $b -side left
    bind $b <Enter> {setObjInfo "Add Polygon Tool"}
    bind $b <Leave> clearObjInfo

    # toggle map tool
    set b $toolbox.map
    image create photo mapicon -file ${tdpsGraphics}/map.gif
    button $b -image mapicon -command "toggleMap"
    pack $b -side left
    bind $b <Enter> {setObjInfo "Toggle Map"}
    bind $b <Leave> clearObjInfo

    # remove vertex tool
    set b $toolbox.remove
    global deleteVertexButton
    set deleteVertexButton $b
    image create photo remove -file ${tdpsGraphics}/remove.gif
    button $b -image remove -command "deleteVertices" -state disabled
    pack $b -side right
    bind $b <Enter> {setObjInfo "Remove Selected Vertices"}
    bind $b <Leave> clearObjInfo

    # add vertex tool
    set b $toolbox.add
    global addVertexButton
    set addVertexButton $b
    image create photo add -file ${tdpsGraphics}/add.gif
    button $b -image add -command "insertVertices" -state disabled
    pack $b -side right
    bind $b <Enter> {setObjInfo "Insert Vertices between Selected Vertices"}
    bind $b <Leave> clearObjInfo

    # edit vertices tool
    set b $toolbox.editVertices
    global editVerticesButton
    set editVerticesButton $b
    image create photo move -file ${tdpsGraphics}/move.gif
    button $b -image move -command "editVertices"
    pack $b -side right
    bind $b <Enter> {setObjInfo "Edit Vertices"}
    bind $b <Leave> clearObjInfo

    set radiotool select
}

proc addCanvasToMainWindow {} {
    global w c bgColor

    set g $w.cg
    frame $g
    pack $g -expand true -fill both
    grid columnconfigure $g 0 -weight 1
    grid rowconfigure $g 0 -weight 1

    set c $g.c
    canvas $c -bd 0 -bg $bgColor -relief sunken -borderwidth 2 -scrollregion {-179.5 -89.5 179.5 89.5} -width 360 -height 180 \
	    -yscrollcommand "canvasYScrollCmd $g.vscroll" \
	    -xscrollcommand "canvasXScrollCmd $g.hscroll" 
    global canvasHscroll canvasVscroll
    set canvasHscroll $g.hscroll
    set canvasVscroll $g.vscroll
    scrollbar $g.vscroll -command "yScrollCmd"
    scrollbar $g.hscroll -orient horiz -command "xScrollCmd"

    grid $c -row 0 -column 0  -sticky news
    grid $g.vscroll -row 0 -column 1 -sticky news
    grid $g.hscroll -row 1 -column 0 -sticky news

    update idletasks

    $c configure -scrollregion {-180 -90 180 90}

    bind $c <Configure> checkCanvasSize

    setupCanvasMouseBindings
    setupSelectionCursor
}

# called when canvas xScrolls
set lastCanvasXScrollCmdArgs {}
proc canvasXScrollCmd {sb args} {
    global lastCanvasXScrollCmdArgs
    if {$lastCanvasXScrollCmdArgs == $args} {
	return
    }

    eval "$sb set $args"

    global mapIsOn
    global draggingCanvas
    if {$mapIsOn && ! $draggingCanvas} {
	genLoadMapImage
    }

    updateViewDoc
}

# called when canvas yScrolls
set lastCanvasYScrollCmdArgs {}
proc canvasYScrollCmd {sb args} {
    global lastCanvasYScrollCmdArgs
    if {$lastCanvasYScrollCmdArgs == $args} {
	return
    }

    eval "$sb set $args"

    global mapIsOn
    global draggingCanvas
    if {$mapIsOn && ! $draggingCanvas} {
	genLoadMapImage
    }

    updateViewDoc
}

# called when xscrollbar scrolls
proc xScrollCmd {args} {
    global c

    eval "$c xview $args"
}

# called when yscrollbar scrolls
proc yScrollCmd {args} {
    global c

    eval "$c yview $args"
}

proc setAppCursor {cursor} {
    global w c

    $c config -cursor $cursor
    if {$cursor == "watch"} {
	$w config -cursor $cursor
	$w.menu config -cursor $cursor
    }
}

proc restoreCursor {} {
    global radiotool

    switch $radiotool {
	select {
	    setupSelectionCursor
	}
	zoomin {
	    setupZoominCursor
	}
	zoomout {
	    setupZoomoutCursor
	}
	hand {
	    setupHandCursor
	}
	line {
	    setupLineCursor
	}
	polygon {
	    setupPolygonCursor
	}
    }	

    global w

    $w config -cursor left_ptr
    $w.menu config -cursor left_ptr
}

# mouse lat/lon deg.deg and lat/log deg min sec.sec, view extent, obj info
proc addStatusAreaToMainWindow {} {
    global w bgColor fgColor

    set g $w.grid
    frame $g -bd 1 -relief solid
    pack $g -fill both -side top
    
    set l $g.viewLabel
    label $l -text "View"
    grid $l -row 1 -column 1 -padx 1 -pady 1
    set l $g.viewN
    label $l -text "north" -bg $bgColor -fg $fgColor -width 6
    grid $l -row 0 -column 1 -padx 1 -pady 1
    set l $g.viewS
    label $l -text "south" -bg $bgColor -fg $fgColor -width 6
    grid $l -row 2 -column 1 -padx 1 -pady 1
    set l $g.viewW
    label $l -text "west" -bg $bgColor -fg $fgColor -width 7
    grid $l -row 1 -column 0 -padx 1 -pady 1
    set l $g.viewE
    label $l -text "east" -bg $bgColor -fg $fgColor -width 7
    grid $l -row 1 -column 2 -padx 1 -pady 1

    grid columnconfigure $g 3 -weight 1

    set subf $g.subf
    frame $subf -bd 1 -relief solid
    grid $subf -row 0 -column 3 -padx 5 -pady 0 -sticky ns
    grid configure $subf -rowspan 3

    set l $subf.zoomLabel
    label $l -text "Zoom"
    pack $l -side top -fill both -expand true
    set l $subf.zoomValue
    label $l -text "X" -bg $bgColor -fg $fgColor
    pack $l -side top -fill both -expand true

    set l $g.mouseLabel
    label $l -text "Cursor"
    grid $l -row 0 -column 5 -columnspan 2 -padx 1 -pady 1
    set l $g.lonLabel
    label $l -text "Lon"
    grid $l -row 1 -column 4 -padx 1 -pady 1
    set l $g.mouseLonDec
    label $l -text "" -bg $bgColor -fg $fgColor -width 10
    grid $l -row 1 -column 5 -padx 1 -pady 1
    set l $g.mouseLonDeg
    label $l -text "" -bg $bgColor -fg $fgColor -width 14
    grid $l -row 1 -column 6 -padx 1 -pady 1
    set l $g.latLabel
    label $l -text "Lat"
    grid $l -row 2 -column 4 -padx 1 -pady 1
    set l $g.mouseLatDec
    label $l -text "" -bg $bgColor -fg $fgColor -width 10
    grid $l -row 2 -column 5 -padx 1 -pady 1
    set l $g.mouseLatDeg
    label $l -text "" -bg $bgColor -fg $fgColor -width 14
    grid $l -row 2 -column 6 -padx 1 -pady 1

    set l $w.objInfo
    label $l -text ""
    pack $l -fill both -side top
}

set mapIsOn 0
proc mapOn {} {
    global tdpsDatabase tdpsDatabaseDir
    if {$tdpsDatabase == "" || [llength [glob -nocomplain $tdpsDatabaseDir/*]] == 0} {
	tk_messageBox -message "No database selected"
	return
    }

    if {[genLoadMapImage] == 0} {
	global mapIsOn
	set mapIsOn 1
    }
}

proc mapOff {} {
    global c mapImage

    $c delete $mapImage
    set mapImage -1

    global mapIsOn
    set mapIsOn 0
}

proc toggleMap {} {
    global mapIsOn

    if {$mapIsOn} {
	mapOff
    } else {
	mapOn
    }
}

set mapImage -1
set loadingMapImage 0
set lastViewport {}
proc genLoadMapImage {} {
    global argv
    global loadingMapImage
    if {$loadingMapImage} {
	return 0
    } else {
	set loadingMapImage 1
    }

    global scalePower
    global adrgN adrgE adrgW adrgS

    if {[catch {
	set imgWest $adrgW
	set imgEast $adrgE
	set imgSouth $adrgS
	set imgNorth $adrgN

	set pixPerDeg 2048.0

	set imgHeight [expr ($imgNorth - $imgSouth) * $pixPerDeg]
	set imgWidth [expr ($imgEast - $imgWest) * $pixPerDeg]
    }]} {
	tk_messageBox -message "ADRG background not yet extracted"
	set loadingMapImage 0
	return 1
    }



    global tdpsDatabaseDir
    if {! [file exists ${tdpsDatabaseDir}/adrg_red.img] || ! [file exists ${tdpsDatabaseDir}/adrg_grn.img] || ! [file exists ${tdpsDatabaseDir}/adrg_blu.img]} {
	tk_messageBox -message "ADRG background not yet extracted"
	set loadingMapImage 0
	return 1
    }

    global w winxgap winygap
    set geom [wm geometry $w]
    set canvasNorth [yToLat 0]
    set canvasWest [xToLon 0]
    set thisViewport [list $geom $canvasNorth $canvasWest]

    global lastViewport
    global mapIsOn
    if {$lastViewport == $thisViewport && $mapIsOn} {
	if {[lsearch $argv "expert"] >= 0} {
	    puts "map already on and viewport hasn't changed"
	}
	set loadingMapImage 0
  	return 0
    } else {
  	set lastViewport $thisViewport
    }

    setAppCursor watch
    update idletasks

    set zoomOut [expr 11 - $scalePower]
    set skip [expr pow(2, $zoomOut)]

    if {$skip < 1} {
	set interp [expr - int(pow(2, - $zoomOut))]
	set skipParm $interp
    } else {
	set skipParm [expr int($skip)]
    }

    set geom [split $geom "x+"]
    set winx [lindex $geom 0]
    set winy [lindex $geom 1]

    # viewport size in image space
    set viewHeight [expr ($winy - $winygap) * $skip]
    set viewWidth [expr ($winx - $winxgap) * $skip]

    # viewport origin offset in image space
    set latOffset [expr $canvasNorth - $imgNorth]
    set lonOffset [expr $canvasWest - $imgWest]
    set pixOffsetX [expr int($lonOffset * $pixPerDeg)]
    set pixOffsetY [expr int($latOffset * $pixPerDeg)]

    if {$canvasNorth <= $imgNorth} {
	if {abs($pixOffsetY) > $imgHeight} {
	    if {[lsearch $argv "expert"] >= 0} {
		puts "map outside of view"
	    }
	    restoreCursor
	    set loadingMapImage 0
	    return 0
	}

	set y [expr - $canvasNorth * pow (2, $scalePower)]

	set imgSubHeight [expr int($imgHeight - ($imgNorth - $canvasNorth) * $pixPerDeg)]
	if {$imgSubHeight > $viewHeight} {
	    set imgSubHeight $viewHeight
	}
    } else {
	if {$pixOffsetY > $viewHeight} {
	    if {[lsearch $argv "expert"] >= 0} {
		puts "map outside of view"
	    }
	    restoreCursor
	    set loadingMapImage 0
	    return 0
	}

	set y [expr - $imgNorth * pow (2, $scalePower)]

	set imgSubHeight [expr int($viewHeight - ($canvasNorth - $imgNorth) * $pixPerDeg)]
    }
    if {$imgSubHeight > $imgHeight} {
	set imgSubHeight $imgHeight
    }

    if {$canvasWest > $imgWest} {
	if {$pixOffsetX > $imgWidth} {
	    if {[lsearch $argv "expert"] >= 0} {
		puts "map outside of view"
	    }
	    restoreCursor
	    set loadingMapImage 0
	    return 0
	}

	set x [expr $canvasWest * pow (2, $scalePower)]

	set imgSubWidth [expr int($imgWidth - ($canvasWest - $imgWest) * $pixPerDeg)]
	if {$imgSubWidth > $viewWidth} {
	    set imgSubWidth $viewWidth
	}
    } else {
	if {abs($pixOffsetX) > $viewWidth} {
	    if {[lsearch $argv "expert"] >= 0} {
		puts "map outside of view"
	    }
	    restoreCursor
	    set loadingMapImage 0
	    return 0
	}
	
	set x [expr $imgWest * pow (2, $scalePower)]

	set imgSubWidth [expr int($viewWidth - ($imgWest - $canvasWest) * $pixPerDeg)]
    }
    if {$imgSubWidth > $imgWidth} {
	set imgSubWidth $imgWidth
    }

    if {$pixOffsetX < 0} {
	set pixOffsetX 0
    }
	
    if {$pixOffsetY < 0} {
	set pixOffsetY [expr - $pixOffsetY]
    } else {
	set pixOffsetY 0
    }

    global tdpsDatabaseDir
    global tdpsRoot
    set cmd "${tdpsRoot}/vdev/rgb2ppm ${tdpsDatabaseDir}/adrg_red.img ${tdpsDatabaseDir}/adrg_grn.img ${tdpsDatabaseDir}/adrg_blu.img $imgHeight $imgWidth mapImage.pgm $pixOffsetY $pixOffsetX [expr ceil($imgSubHeight)] [expr ceil($imgSubWidth)] $skipParm"
    if {[lsearch $argv "expert"] >= 0} {
	puts $cmd
    }

    eval [concat exec $cmd]

    image create photo map -format ppm -file mapImage.pgm

    global c mapImage

    if {$mapImage >= 0} {
	$c delete $mapImage
    }

    set mapImage [$c create image $x $y -anchor nw -image map -tags map]
    $c lower $mapImage all

    restoreCursor

    set loadingMapImage 0

    return 0
}

proc nudgeMap {direction} {
    global c mapImage
    global scalePower
    set coords [$c coords $mapImage]
    set x [lindex $coords 0]
    set y [lindex $coords 1]

    switch $direction {
	north {
	    set y [expr $y - pow (2, $scalePower)*3.0/3600]
	}
	south {
	    set y [expr $y + pow (2, $scalePower)*3.0/3600]
	}
	east {
	    set x [expr $x + pow (2, $scalePower)*3.0/3600]
	}
	west {
	    set x [expr $x - pow (2, $scalePower)*3.0/3600]
	}
    }

    $c coords $mapImage [list $x $y]
}

proc centerOnCoord {} {
    set w .centerOnCoord
    catch {destroy $w}
    toplevel $w
    set title "Center On Coord"
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set grid $w.frame
    frame $grid
    pack $grid -expand true -fill both

    set row 0

    set rb $grid.rb_${row}
    radiobutton $rb -text "MGRS" -variable coordType -value mgrs -anchor w
    grid $rb -row $row -column 0 -sticky w
    global coordType
    set coordType mgrs

    set lab $grid.lab_${row}_1
    label $lab -text "Coordinate:"
    grid $lab -row $row -column 1 -sticky e
    entry $grid.mgrsCoordinate
    grid $grid.mgrsCoordinate -row $row -column 2 -sticky w
    focus $grid.mgrsCoordinate
    bind $grid.mgrsCoordinate <Return> "focus $w.buttons.jump"
    bind $grid.mgrsCoordinate <FocusIn> "set coordType mgrs"
#    bind $grid.mgrsCoordinate <Leave> "convertFrom mgrs $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    incr row

    set lab $grid.spacer_${row}
    label $lab -text " "
    grid $lab -column 0 -row $row

    incr row

    set rb $grid.rb_${row}
    radiobutton $rb -text "UTM" -variable coordType -value utm -anchor w
    grid $rb -row $row -column 0 -sticky w

    set lab $grid.lab_${row}_1
    label $lab -text "Northing (meters):"
    grid $lab -row $row -column 1 -sticky e
    entry $grid.eNorthing
    grid $grid.eNorthing -row $row -column 2 -sticky w
    bind $grid.eNorthing <Return> "focus $grid.eEasting"
    bind $grid.eNorthing <FocusIn> "set coordType utm"
#    bind $grid.eNorthing <Leave> "convertFrom utm $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    incr row

    set lab $grid.lab_${row}_2
    label $lab -text "Easting (meters):"
    grid $lab -row $row -column 1 -sticky e
    entry $grid.eEasting
    grid $grid.eEasting -row $row -column 2 -sticky w
    bind $grid.eEasting <Return> "focus $grid.eZone"
    bind $grid.eEasting <FocusIn> "set coordType utm"
#    bind $grid.eEasting <Leave> "convertFrom utm $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    incr row

    set lab $grid.lab_${row}_3
    label $grid.lZone -text "Zone (e.g. 42N):"
    grid $grid.lZone -row $row -column 1 -sticky e
    entry $grid.eZone
    grid $grid.eZone -row $row -column 2 -sticky w
    bind $grid.eZone <Return> "focus $w.buttons.jump"
    bind $grid.eZone <FocusIn> "set coordType utm"
#    bind $grid.eZone <Leave> "convertFrom utm $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    incr row

    set lab $grid.spacer_${row}
    label $lab -text " "
    grid $lab -column 0 -row $row

    incr row

    set rb $grid.rb_${row}
    radiobutton $rb -text "Lat/Lon" -variable coordType -value latLon -anchor w
    grid $rb -row $row -column 0 -sticky w

    set lab $grid.lab_${row}_1
    label $grid.lLat -text "Lat (deg):"
    grid $grid.lLat -row $row -column 1 -sticky e
    entry $grid.eLat
    grid $grid.eLat -row $row -column 2 -sticky w
    bind $grid.eLat <Return> "focus $grid.eLon"
    bind $grid.eLat <FocusIn> "set coordType latLon"
#    bind $grid.eLat <Leave> "convertFrom latLon $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    incr row

    label $grid.lLon -text "Lon (deg):"
    grid $grid.lLon -row $row -column 1 -sticky e
    entry $grid.eLon
    grid $grid.eLon -row $row -column 2 -sticky w
    bind $grid.eLon <Return> "focus $w.buttons.jump"
    bind $grid.eLon <FocusIn> "set coordType latLon"
#    bind $grid.eLon <Leave> "convertFrom latLon $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.convert
    button $b -text "Convert" -command "convertFrom \$coordType $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"
    bind $b <Return> "convertFrom \$coordType $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"
    pack $b -side left -fill both -expand true
    set b $f.jump
    set command "convertFrom \$coordType $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"
    set command "${command} ; centerOnCoord_2 $w $grid.mgrsCoordinate $grid.eNorthing $grid.eEasting $grid.eZone $grid.eLat $grid.eLon"
    button $b -text "Center" -command $command
    bind $b <Return> $command
    bind $b <ButtonPress> "addCenterCrossHairs"
    bind $b <ButtonRelease> "removeCenterCrossHairs"
    pack $b -side left -fill both -expand true
    set b $f.cancel
    button $b -text "Close" -command "destroy $w"
    bind $b <Return> "destroy $w"
    pack $b -side left -fill both -expand true

}

set lastCrossHairCenterX 0
set lastCrossHairCenterY 0
proc addCenterCrossHairs {} {
    global c
    global lastCrossHairCenterX
    global lastCrossHairCenterY

    set coords [list [expr $lastCrossHairCenterX - 10] $lastCrossHairCenterY [expr $lastCrossHairCenterX + 10] $lastCrossHairCenterY]
    $c create line $coords -fill red -tags centerCrossHairs

    set coords [list $lastCrossHairCenterX [expr $lastCrossHairCenterY - 10] $lastCrossHairCenterX [expr $lastCrossHairCenterY + 10]]
    $c create line $coords -fill red -tags centerCrossHairs

    set coords [list [expr $lastCrossHairCenterX - 10] [expr $lastCrossHairCenterY - 10] [expr $lastCrossHairCenterX + 10] [expr $lastCrossHairCenterY + 10]]
    $c create oval $coords -outline red -tags centerCrossHairs
}

proc removeCenterCrossHairs {} {
    global c

    $c delete centerCrossHairs
}

proc convertFrom {coordType mgrsEntry utmNorthingEntry utmEastingEntry utmZoneEntry latEntry lonEntry} {
    switch $coordType {
	mgrs {
	    set coord [$mgrsEntry get]

	    if {! [catch {set latLonZoneXY [mgrsToLatLonZoneXY $coord]} msg]} {
		set lat [lindex $latLonZoneXY 0]
		set lon [lindex $latLonZoneXY 1]
		set zone [lindex $latLonZoneXY 2]
		set easting [lindex $latLonZoneXY 3]
		set northing [lindex $latLonZoneXY 4]

		$utmNorthingEntry delete 0 end
		$utmNorthingEntry insert 0 $northing
		$utmEastingEntry delete 0 end
		$utmEastingEntry insert 0 $easting
		$utmZoneEntry delete 0 end
		if {$lat >= 0.0} {
		    $utmZoneEntry insert 0 "${zone}N"
		} else {
		    $utmZoneEntry insert 0 "${zone}S"
		}
		$latEntry delete 0 end
		$latEntry insert 0 $lat
		$lonEntry delete 0 end
		$lonEntry insert 0 $lon
	    } else {
		tk_messageBox -title "Bad MGRS Coord?" -message "Conversion from MGRS failed. Example format: 11SLT9400981602"
	    }
	}
	latLon {
	    set lat [$latEntry get]
	    set lon [$lonEntry get]

	    if {! [catch {set mgrsZoneXY [latLonToMgrsZoneXY $lat $lon]} msg]} {
		set mgrs [lindex $mgrsZoneXY 0]
		set zone [lindex $mgrsZoneXY 1]
		set easting [lindex $mgrsZoneXY 2]
		set northing [lindex $mgrsZoneXY 3]

		$mgrsEntry delete 0 end
		$mgrsEntry insert 0 $mgrs
		$utmNorthingEntry delete 0 end
		$utmNorthingEntry insert 0 $northing
		$utmEastingEntry delete 0 end
		$utmEastingEntry insert 0 $easting
		$utmZoneEntry delete 0 end
		if {$lat >= 0.0} {
		    $utmZoneEntry insert 0 "${zone}N"
		} else {
		    $utmZoneEntry insert 0 "${zone}S"
		}
	    } else {
		tk_messageBox -title "Bad Lat/Lon?" -message "Conversion from Lat/Lon failed. Example format: Lat 34.0 Lon -118.0"
	    }
	}
	utm {
	    set zone [$utmZoneEntry get]
	    set easting [$utmEastingEntry get]
	    set northing [$utmNorthingEntry get]

	    set len [string length $zone]
	    set last [string index $zone [expr $len - 1]]
	    set num [string range $zone 0 [expr $len - 2]]

	    set last [string toupper $last]
	    if {! [string is integer $num] || $num < 1 || $num > 60 || ($last != "N" && $last != "S")} {
		tk_messageBox -message "Zone must be of the form XXH, where XX is an\ninteger >= 1 and <= 60, and H is either N or S"
		return
	    }

	    if {[catch {set latLon [utmToLatLon $northing $easting $num $last]}]} {
		tk_messageBox -title "Bad UTM Coords?" -message "Conversion from UTM failed. Bad coords?"
		return
	    }

	    set lat [lindex $latLon 0]
	    set lon [lindex $latLon 1]

	    if {! [catch {set mgrsZoneXY [latLonToMgrsZoneXY $lat $lon]} msg]} {
		$mgrsEntry delete 0 end
		$mgrsEntry insert 0 [lindex $mgrsZoneXY 0]
		$latEntry delete 0 end
		$latEntry insert 0 $lat
		$lonEntry delete 0 end
		$lonEntry insert 0 $lon
	    } else {
		tk_messageBox -title "Bad UTM Coords?" -message "Conversion from UTM failed. Bad coords?"
	    }
	}
    }
}

proc centerOnCoord_2 {w eM eN eE eZ cY cX} {
    global coordType

    switch $coordType {
	mgrs {
	    set coord [$eM get]

	    if {[catch {set latLon [mgrsToLatLonZoneXY $coord]}]} {
		tk_messageBox -title "Bad MGRS Coord?" -message "MGRS to Geodetic conversion failed; bad coords? Example: 11SLT9400981602"
		return
	    }

	    centerOnCoord_3 $w [lindex $latLon 0] [lindex $latLon 1]
	}
	utm {
	    set northing [$eN get]
	    set easting [$eE get]
	    set zone [$eZ get]

	    set len [string length $zone]
	    set last [string index $zone [expr $len - 1]]
	    set num [string range $zone 0 [expr $len - 2]]

	    set last [string toupper $last]
	    if {! [string is integer $num] || $num < 1 || $num > 60 || ($last != "N" && $last != "S")} {
		tk_messageBox -message "Zone must be of the form XXH, where XX is an\ninteger >= 1 and <= 60, and H is either N or S"
		return
	    }

	    if {[catch {set latLon [utmToLatLon $northing $easting $num $last]}]} {
		tk_messageBox -title "Bad UTM Coords?" -message "UTM to Geodetic conversion failed; bad coords?"
		return
	    }

	    centerOnCoord_3 $w [lindex $latLon 0] [lindex $latLon 1]
	}
	latLon {
	    set lat [$cY get]
	    set lon [$cX get]

	    if {$lat < -90 || $lat > 90 || $lon < -180 || $lon > 180} {
		tk_messageBox -title "Lat/Lon Out Of Range" -message "Lat must be in range -90 to 90\nLon must be in range -180 to 180"
		return
	    }

	    centerOnCoord_3 $w $lat $lon
	}
    }
}

proc centerOnCoord_3 {w cY cX} {
    global lastCrossHairCenterX
    global lastCrossHairCenterY

    global scalePower
    set lastCrossHairCenterX [expr $cX * pow( 2, $scalePower )]
    set lastCrossHairCenterY [expr - $cY * pow( 2, $scalePower )]

    global playbox_aoiMinLat playbox_aoiMaxLat playbox_aoiMinLon playbox_aoiMaxLon
    if {$cX < $playbox_aoiMinLon || $cX > $playbox_aoiMaxLon || $cY < $playbox_aoiMinLat || $cY > $playbox_aoiMaxLat} {
	if {[promptUser "Leave Playbox?" "Requested center point is outside playbox. Continue?" {Yes No}] != "Yes"} {
	    return
	}
    }

    global c

    set xview [$c xview]
    set yview [$c yview]

    set leftPercent [lindex $xview 0]
    set rightPercent [lindex $xview 1]
    set topPercent [lindex $yview 0]
    set bottomPercent [lindex $yview 1]

    set width [expr 360.0 * ($rightPercent - $leftPercent)]
    set height [expr 180.0 * ($bottomPercent - $topPercent)]

    set left [expr $cX - $width / 2.0]
    set right [expr $cX + $width / 2.0]
    set bottom [expr $cY - $height / 2.0]
    set top [expr $cY + $height / 2.0]

    if {$left < -180.0} {
	set gap [expr -180.0 - $left]
	set left [expr $left + $gap]
	set right [expr $right + $gap]
    } elseif {$right > 180.0} {
	set gap [expr 180.0 - $right]
	set left [expr $left + $gap]
	set right [expr $right + $gap]
    }

    if {$bottom < -90.0} {
	set gap [expr -90.0 - $bottom]
	set bottom [expr $bottom + $gap]
	set top [expr $top + $gap]
    } elseif {$top > 90.0} {
	set gap [expr 90.0 - $top]
	set bottom [expr $bottom + $gap]
	set top [expr $top + $gap]
    }

    set leftPercent [expr ($left + 180.0) / 360.0]
    set rightPercent [expr ($right + 180.0) / 360.0]
    set bottomPercent [expr ($bottom + 90.0) / 180.0]
    set topPercent [expr (90.0 - $top) / 180.0]

    $c xview moveto $leftPercent
    $c yview moveto $topPercent
}

proc updateViewDoc {} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global w c scalePower
    set xview [$c xview]
    set yview [$c yview]

    set leftPercent [lindex $xview 0]
    set rightPercent [lindex $xview 1]
    set topPercent [lindex $yview 0]
    set bottomPercent [lindex $yview 1]

    set leftLon [expr $leftPercent * 360.0 - 180.0]
    set rightLon [expr $rightPercent * 360.0 - 180.0]

    set topLat [expr 90.0 - $topPercent * 180.0]
    set bottomLat [expr 90.0 - $bottomPercent * 180.0]

    $w.grid.subf.zoomValue config -text $scalePower
    $w.grid.viewN config -text [format "%6.2f" $topLat]
    $w.grid.viewW config -text [format "%7.2f" $leftLon]
    $w.grid.viewS config -text [format "%6.2f" $bottomLat]
    $w.grid.viewE config -text [format "%7.2f" $rightLon]
}

proc updateMouseDoc {x y} {
    global w c winsized scalePower

    if {! $winsized} {
	return
    }

    set lat [yToLat $y]

    if {$lat < -90.0} {
	set lat -90.0
    }
    if {$lat > 90.0} {
	set lat 90.0
    }

    if {$lat < 0} {
	set func ceil
    } else {
	set func floor
    }
    set latDeg [expr round ($func ($lat))]
    set rest [expr abs ($lat - $func ($lat))] ; # abs() because we don't want the sign in front of min, sec
    set latMin [expr round (floor (60.0 * $rest))]
    set rest [expr $rest - $latMin / 60.0]
    set latSec [expr $rest * 3600.0]

    set lon [xToLon $x]

    if {$lon < -180.0} {
	set lon -180.0
    }
    if {$lon > 180.0} {
	set lon 180.0
    }

    if {$lon < 0} {
	set func ceil
    } else {
	set func floor
    }
    set lonDeg [expr round ($func ($lon))]
    set rest [expr abs ($lon - $func ($lon))] ; # abs() because we don't want the sign in front of min, sec
    set lonMin [expr round (floor (60.0 * $rest))]
    set rest [expr $rest - $lonMin / 60.0]
    set lonSec [expr $rest * 3600.0]

    $w.grid.mouseLonDec config -text [format "%10.6f" $lon]
    $w.grid.mouseLonDeg config -text "[format {%4d} ${lonDeg}] [format {%02d} ${lonMin}]' [format {%05.2f} ${lonSec}]\""
    $w.grid.mouseLatDec config -text [format "%10.6f" $lat]
    $w.grid.mouseLatDeg config -text "[format {%4d} ${latDeg}] [format {%02d} ${latMin}]' [format {%05.2f} ${latSec}]\""

    set xCoord [expr round ([xToLon $x] * pow (2, $scalePower))]
    set yCoord [expr - round ([yToLat $y] * pow (2, $scalePower))]
    set vec [$c find closest $xCoord $yCoord]
    if {$vec == ""} {
	clearObjInfo
	return
    }

    # hightlight closest vector
    #      global fgColor
    #      $c itemconfig all -fill $fgColor
    #      $c itemconfig $vec -fill red
    #      set lastRedVec $vec

    set file [canvasItemToFilename $vec]

    if {$file == ""} {
	clearObjInfo
	return
    }

    set fileType [getLoadedFileVectorType $file]

    set file [split $file "/"]
    set file [lindex $file end]

    set vecId [canvasItemToVectorId $vec]

    if {$vecId < 0} {
	setObjInfo "$file $fileType (TMP_${vec})"
    } else {
	setObjInfo "$file $fileType #${vecId}"
    }
}

proc clearMouseDoc {} {
    global w

    $w.grid.mouseLonDec config -text ""
    $w.grid.mouseLonDeg config -text ""
    $w.grid.mouseLatDec config -text ""
    $w.grid.mouseLatDeg config -text ""
}

proc setObjInfo {info} {
    global creatingMainWindow ; if {$creatingMainWindow} {return}

    global w

    $w.objInfo config -text $info
}

proc clearObjInfo {} {
    setObjInfo ""
}

proc echoArgs {args} {
    global checkingEventLoop
    puts "echoing $args, checkingEventLoop now $checkingEventLoop"
}

set vectorEditorStarted false
proc startVectorEditor {} {
    # used by vector editor
    global aoiN aoiS aoiE aoiW
    global playbox_aoiMinLat playbox_aoiMinLon playbox_aoiMaxLat playbox_aoiMaxLon

    set aoiN $playbox_aoiMaxLat
    set aoiS $playbox_aoiMinLat
    set aoiE $playbox_aoiMaxLon
    set aoiW $playbox_aoiMinLon

    global vectorEditorStarted

    if {$vectorEditorStarted} {
	global w
	wm deiconify $w
	return
    }

    set vectorEditorStarted true

    global scalePower winsized winxgap maxZoom

    # loadedFiles is an array mapping shapeFilename to {vectorType vectorCount fieldDescriptionList canvasItemList}
    clearLoadedFiles

    # vectorData is an array mapping canvasItem to {shapeFilename vectorId}
    clearVectorData

    set scalePower 0
    set maxZoom 20
    set winsized 0
    set winxgap -1

    createVectorEditorWindow
    unloadAll

    zoomTo 1

    centerViewPercent 0.5 0.5

    setAppCursor left_ptr
}
