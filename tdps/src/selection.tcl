set selectMode vector

proc unselectById {} {
    global c

    set selected [$c find withtag selected]

    global editingVertices
    if {$selected == {} || $editingVertices} {
	tk_messageBox -message "No vectors are selected."
	return
    }

    set w .selectByFile
    catch {destroy $w}
    toplevel $w
    set title "Unselect By Vector ID"
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set grid $w.frame
    frame $grid
    pack $grid -expand true -fill both

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.ok
    button $b -text "OK" -command "unselectByIdInternal $w"
    pack $b -side left -fill both -expand true
    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -fill both -expand true

    set row 0

    foreach canvasId [lsort -integer $selected] {
	set vecId [canvasItemToVectorId $canvasId]

	if {$vecId < 0} {
	    set vecId "TMP_${canvasId}"
	}

	set b $grid.button${row}
	radiobutton $b -text $vecId -variable canvasIdToUnselect -value $canvasId
	grid $b -row $row -column 0
	incr row

	if {$row > 10} {
	    set l $grid.fileLabel${row}
	    label $l -text "(Remaining [expr [llength $selected] - 10] vectors ignored)"
	    grid $l -row $row -column 0
	    break
	}	    
    }
}

proc unselectByIdInternal {w} {
    global canvasIdToUnselect

    selectCanvasItems $canvasIdToUnselect "shift"

    destroy $w
}

proc selectByFile {} {
    set fileNames [getLoadedFilenames]

    if {$fileNames == {}} {
	tk_messageBox -message "There are no loaded files"
	return
    }

    set w .selectByFile
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Select By File"
    wm iconname $w "Select By File"

    set grid $w.frame
    frame $grid
    pack $grid -expand true -fill both

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.ok
    button $b -text "OK" -command [list selectCheckedFiles $w $fileNames]
    pack $b -side left -fill both -expand true
    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -fill both -expand true

    set row 0

    set l $grid.fileLabel${row}
    label $l -text "File"
    grid $l -row $row -column 0

    set l $grid.typeLabel${row}
    label $l -text "Type"
    grid $l -row $row -column 1

    set l $grid.saveLabel${row}
    label $l -text "Select"
    grid $l -row $row -column 2
    
    incr row
    
    foreach fileName $fileNames {
	set l $grid.file${row}
	label $l -text $fileName
	grid $l -row $row -column 0

	set fileType [getLoadedFileVectorType $fileName]
	set l $grid.type${row}
	label $l -text $fileType
	grid $l -row $row -column 1

	set cb $grid.cb${row}
	checkbutton $cb -variable _Set_File_Color_${fileName}
	grid $cb -row $row -column 2

	incr row
    }
}

proc selectCheckedFiles {w fileNames} {
    foreach fileName $fileNames {
	global _Set_File_Color_${fileName}

	if {[set _Set_File_Color_${fileName}]} {
	    set canvasItems [getLoadedFileCanvasItems $fileName]

	    selectCanvasItems $canvasItems "none"
	}
    }

    destroy $w
}

proc unselectAll {} {
    global editingVertices

    if {$editingVertices} {
	unselectAllVertices
    } else {
	unselectAllVectors
    }    
}

proc unselectAllVectors {} {
    global c

    $c itemconfigure selected -width 1
    $c dtag selected
}

proc unselectAllVertices {} {
    global c

    $c itemconfigure selected -fill [getNormalVertexColor]
    $c dtag selected
}

proc initRubberBand {x y key} {
    global w c scalePower selectionBoxColor bgColor

    set selectionBoxColor [xorColor $w $bgColor]

    global downCoordX downCoordY
    set downCoordX [expr round ([xToLon $x] * pow (2, $scalePower))]
    set downCoordY [expr - round ([yToLat $y] * pow (2, $scalePower))]

    global cursorMode
    set cursorMode rubberBand

    global selectionBox fgColor
    set selectionBox [$c create rectangle $downCoordX $downCoordY $downCoordX $downCoordY -outline $selectionBoxColor]
}

proc acceptSelection {x y key} {
    global c selectionBox scalePower
    global selectionRadius

    global cursorMode
    set cursorMode left_ptr

    set x [expr round ([xToLon $x] * pow (2, $scalePower))]
    set y [expr - round ([yToLat $y] * pow (2, $scalePower))]

    $c delete $selectionBox

    global downCoordX downCoordY

    set top [min $y $downCoordY]
    set bottom [max $y $downCoordY]
    set left [min $x $downCoordX]
    set right [max $x $downCoordX]

    # find all items contained in the selection box
    #   if selection box is too small, enlarge it
    if {[expr $bottom - $top < 2 * $selectionRadius]} {
	set yCenter [expr ($top + $bottom) / 2.0]
	set top [expr int (floor ($yCenter - $selectionRadius))]
	set bottom [expr int (ceil ($yCenter + $selectionRadius))]
    }

    if {[expr $right - $left < 2 * $selectionRadius]} {
	set xCenter [expr ($left + $right) / 2.0]
	set left [expr int (floor ($xCenter - $selectionRadius))]
	set right [expr int (ceil ($xCenter + $selectionRadius))]
    }

    #   find all items touching the box
    set selected [$c find overlapping $left $top $right $bottom]

    set keep $selected
    # filter out vertex dots, if any
    # why ?? (because)
    global editingVertices
    set keep {}
    set dotIds [getVertexIds]
    foreach item $selected {
	if {([lsearch $dotIds $item] < 0) == ! $editingVertices} {
	    lappend keep $item
	}
    }

    # filter out grid items
    set keep2 $keep
    set keep {}
    global c
    foreach item $keep2 {
	set tags [$c gettags $item]
	if {[lsearch $tags "grid"] < 0} {
	    lappend keep $item
	}
    }

    if {$key == "none"} {
	set alreadySelected [$c find withtag selected]

	if {[llength $alreadySelected] > 0} {
	    # No shift key was pressed, so we unselect items already selected
	    unselectAll
	} else {
	    # If none were already selected, and none are being selected now, and we are editingVertices, then clear the vertices
	    if {[llength $keep] == 0 && $editingVertices} {
		clearVertices
	    }
	}
    }
	
    # select the chosen items
    if {[llength $keep] > 0} {
	selectCanvasItems $keep $key
    }
}

# key is needed so that items can be toggled, rather than simply selected, if the shift-key is pressed
proc selectCanvasItems {citems key} {
    global c mapImage

    set citems [lremove $citems $mapImage]

    set items $citems
    foreach item $citems {
	foreach part [canvasItemToCanvasParts $item] {
	    if {[lsearch -exact $citems $part] < 0} {
		lappend items $part
	    }
	}
    }

    set selectedItems [$c find withtag selected]

    global editingVertices
    foreach item $items {
	if {$key == "none" || [lsearch -exact $selectedItems $item] < 0} {
	    # if no key, or item not already selected, select it
	    $c addtag selected withtag $item
	    if {$editingVertices} {
		$c itemconfigure $item -fill [getSelectedVertexColor]
	    } else {
		$c itemconfigure $item -width 3
	    }
	} else {
	    # item previously selected and shift key pressed, so unselect it
	    $c dtag $item selected
	    if {$editingVertices} {
		$c itemconfigure $item -fill [getNormalVertexColor]
	    } else {
		$c itemconfigure $item -width 1
	    }
	}
    }

    if {$key == "none"} {
	set selectCount [llength $items]

	if {$selectCount == 1 } {
	    # displayVectorProperties $item selectListItem
	    # editCanvasItem $item
	}
    }
}

proc selectAll {} {
    global c

    selectCanvasItems [$c find all] "none"
}

proc rubberBandSelection {x y} {
    global downCoordX downCoordY
    global c selectionBox selectionBoxColor scalePower

    set x [expr round ([xToLon $x] * pow (2, $scalePower))]
    set y [expr - round ([yToLat $y] * pow (2, $scalePower))]

    $c itemconfigure $selectionBox -outline $selectionBoxColor
    $c coords $selectionBox $downCoordX $downCoordY $x $y
}

proc selectByCbsRecordId {cbsFile} {
    global tdpsDatabaseDir
    global tdpsDatabaseName

    if {! [file exists ${tdpsDatabaseDir}/cbsFileIndexing]} {
	tk_messageBox -message "${tdpsDatabaseDir}/cbsFileIndexing does not exist;\nfirst prepare CBS terrain output files"
	return
    }
	
    set cbsFile ${tdpsDatabaseName}_${cbsFile}

    set w .enterCbsRecordId
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Enter CBS Record ID"
    wm iconname $w "Enter CBS Record ID"

    label $w.label -text "Enter CBS Record ID for ${cbsFile}:"
    pack $w.label -side top -anchor w

    entry $w.entry
    pack $w.entry -side top -fill both
    focus $w.entry

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true -side top
    set b $f.ok
    set command "selectByCbsRecordId_2 $cbsFile \[$w.entry get\] $w"
    button $b -text "OK" -command $command
    bind $w.entry <Return> $command
    pack $b -side left -fill both -expand true
    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -fill both -expand true
}    

proc selectByCbsRecordId_2 {cbsFile cbsRecordId w} {
    global tdpsDatabaseDir

    set file [open ${tdpsDatabaseDir}/cbsFileIndexing]
    set data [read $file]
    close $file

    # data is cbsFile.txt {sourceFile offset sourceFile offset ...} cbsFile.txt ...
    array set dataSource $data
    
    set source $dataSource([fileFromPath ${cbsFile}])

    for {set index [expr [llength $source] - 1]} {$index > 0} {incr index -2} {
	if {$cbsRecordId >= [lindex $source $index]} {
	    set sourceFile ${tdpsDatabaseDir}/[lindex $source [expr $index - 1]].shp
	    set baseIndex [lindex $source $index]

	    if {[lsearch [getLoadedFilenames] ${sourceFile}] < 0} {
		if {[promptUser "Load source file now?" "Vector found in source file not loaded.\nLoad source file now?" {Yes No}] == "Yes"} {
		    setAppCursor watch
		    loadFile $sourceFile
		    restoreCursor

		    if {[selectByCbsRecordId_3 $sourceFile [expr $cbsRecordId - $baseIndex]]} {
			destroy $w
		    }
		}
	    } else {
		if {[selectByCbsRecordId_3 $sourceFile [expr $cbsRecordId - $baseIndex]]} {
		    destroy $w
		}
	    }

	    return
	}
    }

    if {$index <= 0} {
	tk_messageBox -message "Record $cbsRecordId not found in ${cbsFile}"
    }
}

proc selectByCbsRecordId_3 {sourceFile vectorId} {
    set parts [vectorIdToCanvasParts $vectorId $sourceFile]

    if {[llength $parts] == 0} {
	tk_messageBox -message "Record $vectorId not found in ${sourceFile}"
	return 0
    }
	
    clearVertices
    unselectAll
    selectCanvasItems $parts "none"
    zoomToFit selected

    return 1
}