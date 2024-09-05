registerDatabaseVar colorPaletteValues {White red4 DarkGreen NavyBlue gray75 Red Green Blue gray50 Yellow Cyan Magenta Black Brown DarkSeaGreen DarkViolet} "Color palette"
proc createColorPalette2 {} {
    set topBorderColor gray50
    set bottomBorderColor gray75

    global colorPaletteValues
    set index 0
    foreach color $colorPaletteValues {
	image create photo image_colorPalette_${index} -height 16 -width 16
	image_colorPalette_${index} put $topBorderColor -to 0 0 16 1
	image_colorPalette_${index} put $topBorderColor -to 0 1 1 16
	image_colorPalette_${index} put $bottomBorderColor -to 0 15 16 16
	image_colorPalette_${index} put $bottomBorderColor -to 15 1 16 16
	image_colorPalette_${index} put $color -to 1 1 15 15

	image create photo image_colorPalette_${index}_s -height 16 -width 16
	image_colorPalette_${index}_s put Black -to 0 0 16 2
	image_colorPalette_${index}_s put Black -to 0 2 2 16
	image_colorPalette_${index}_s put Black -to 2 14 16 16
	image_colorPalette_${index}_s put Black -to 14 2 16 14
	image_colorPalette_${index}_s put $color -to 2 2 14 14

	incr index
    }
}

proc makeColorOptionMenu {w varName} {
    set m [tk_optionMenu $w.$varName $varName 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]

    global $varName
    global colorPaletteValues
    set color [lindex $colorPaletteValues [expr [set $varName] - 10]]

    $w.$varName config -bg $color -fg $color -activebackground $color -activeforeground $color

    bind $m <<MenuSelect>> "selectColorMenu2 $w $varName"

    for {set i 0} {$i <= [$m index last]} {incr i} {
	global colorPaletteValues
	set color [lindex $colorPaletteValues $i]
	$m entryconfigure $i -image image_colorPalette_${i} -selectimage image_colorPalette_${i}_s -hidemargin 1
    }

    foreach i {0 4 8 12} {
	$m entryconfigure $i -columnbreak 1
    }

    return $w.$varName
}

proc makeColorMenu {w name cmd} {
    set m $w.name
    menu $m -tearoff 0

    foreach i {0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15} {
	$m add command -image image_colorPalette_${i} -command "$cmd $i" -hidemargin 1
    }

    foreach i {0 4 8 12} {
	$m entryconfigure $i -columnbreak 1
    }

    return $m
}

proc selectColorMenu2 {w varName} {
    global $varName
    set index [expr [set $varName] - 10]
    global colorPaletteValues
    set color [lindex $colorPaletteValues $index]

    $w.$varName config -bg $color
    $w.$varName config -fg $color
    $w.$varName config -activebackground $color
    $w.$varName config -activeforeground $color

    writeDatabase
}

createColorPalette2

# collec all files in path's directory
# select path for opening
# include color selection option
# finally call loadFile on each selected file

proc loadFiles {path} {

    # this should happen with debug mode
#      set splitPath [split $path /]
#      if {[llength $splitPath] == 1} {
#  	set dir "."
#      } else {
#  	set splitDir [lrange $splitPath 0 [expr [llength $splitPath] - 2]]
#  	set dir [join $splitDir /]
#      }

    set dir $path

    set allFiles [glob ${dir}/*.shp]

    set w .filesToLoad
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Files To Load"
    wm iconname $w "Files To Load"

    set f $w.cFrame
    frame $f
    pack $f -expand true -fill both

    set c $f.canvas
    canvas $c -yscrollcommand "$f.vscroll set" -height 15c -width 30c
    grid $c -row 0 -column 0 -sticky news

    set sb $f.vscroll
    scrollbar $sb -command "$c yview"
    grid $sb -row 0 -column 1 -sticky news

    grid rowconfig $f 0 -weight 1
    grid columnconfig $f 0 -weight 1

    set f $w.buttons
    frame $f
    pack $f -fill both

    set b $f.ok
    button $b -text "OK" -command [list loadCheckedFiles $allFiles $w]
    pack $b -side left -expand true -fill both

    set b $f.cancel 
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -expand true -fill both

    set grid $c.grid
    frame $grid
    set gridWin [$c create window 0 0 -window  $grid -anchor nw]

    set row 0

    global currentColoredFiles
    set currentColoredFiles {}
    foreach file [lsort $allFiles] {
	set cb $grid.check${row}
	checkbutton $cb -variable doLoad_${file}
	grid $cb -row ${row} -column 0

	set cleanedPath [cleanPath $file]
	lappend currentColoredFiles $cleanedPath
	grid [makeColorOptionMenu $grid $cleanedPath] -row ${row} -column 1

	global fileColorMap
	global $cleanedPath
	set index [lsearch $fileColorMap $cleanedPath]
	if {$index < 0} {
	    set $cleanedPath 10
	    lappend fileColorMap $cleanedPath 10
	} else {
	    set $cleanedPath [lindex $fileColorMap [expr $index + 1]]
	}

	set l $grid.label${row}
	label $l -text $file
	grid $l -row ${row} -column 2 -sticky w

	incr row
    }

    $c config -scrollregion "0c 0c 20c [expr 1.2 * ${row}]c"
    $c xview moveto 0
}

proc fileToColor {file} {
    set cleanedPath [cleanPath $file]
    global $cleanedPath
    set $cleanedPath
    global colorPaletteValues
    lindex $colorPaletteValues [expr [set $cleanedPath] - 10]
}

registerDatabaseVar fileColorMap {} "File colors mapping"

proc cleanPath {path} {
    set split [split $path /]
    set join [join $split ""]
    set split [split $join .]
    return "_[join $split ""]"
}

proc loadCheckedFiles {files w} {
    foreach file $files {
	global doLoad_${file}

	if {[set doLoad_${file}]} {
	    global [cleanPath $file]
	    loadFile $file [set [cleanPath $file]]
	}
    }

    destroy $w
}

proc unscale {coords} {
    global scalePower

    set list {}

    foreach coord $coords {
	lappend list [expr $coord / pow(2, $scalePower)]
    }

    return $list
}

proc loadFile {path {color ""}} {
    if {[lsearch [getLoadedFilenames] $path] >= 0} {
	tk_messageBox -message "Note: $path is already loaded" -title "File already loaded"
	return
    }

    global c scalePower aoiN aoiS aoiE aoiW

    if {$color == ""} {
	global fgColor
	set color $fgColor
    } else {
	global colorPaletteValues
	set color [lindex $colorPaletteValues [expr $color - 10]]
    }

    # load a shape file
    if {[catch {getShapeFile $path $scalePower $aoiN $aoiS $aoiE $aoiW} shapeSet]} {
  	tk_messageBox -message "Error loading shapefile:\n$shapeSet" -title "Load Error"
  	return false
    }

    set filename [lindex $shapeSet 0]
    set vectorType [lindex $shapeSet 1]
    set vectorCount [lindex $shapeSet 2]
    set fieldDescriptions [lindex $shapeSet 3]
    set vecs [lindex $shapeSet 4]

    if {$vecs == {}} {
	tk_messageBox -message "Note: no vectors were loaded from $path."
    }

    set items {}
    set vecFirstItems {}
    set vecIds {}
    foreach vec $vecs {
	set vectorId [lindex $vec 0] ; # unique for this shape file
	lappend vecIds $vectorId
	set coords [lindex $vec 1] ; # scaled by 2**scalePower
	if {[llength $coords] < 4} {
	    puts "short coords: $coords"
	}
	set partIndices [lindex $vec 2] ; # zero-based offset of the start vertex of the nth part of the vector
	set partCount [llength $partIndices]

	global fillPolygons
	if {$partCount == 1} {
	    if {$vectorType == "POLYGON"} {
		if {[llength $coords] < 8} {
		    puts "short polygon vector discarded"
		    continue
		}
	    } else {
		if {[llength $coords] < 4} {
		    puts "short line vector discarded"
		    continue
		}
	    }

	    if {$vectorType == "POLYGON" && $fillPolygons} {
		set itemId [eval [concat $c create polygon $coords -fill $color -outline $color -tags vec]]
	    } else {
		set itemId [eval [concat $c create line $coords -fill $color -tags vec]]
	    }
	    lappend items $itemId
	    lappend vecFirstItems $itemId
	    set parts $itemId
	    setVectorData $itemId $filename $vectorId $parts $partIndices
	} else {
	    set parts {}
	    set partCoordCount 0
	    set vectorItems {}
	    for {set i 0} {$i < [expr $partCount - 1]} {incr i} {
		set firstIndex [expr 2 * [lindex $partIndices $i]]
		set lastIndex [expr 2 * ([lindex $partIndices [expr $i + 1]] - 1) + 1]
		set part [lrange $coords $firstIndex $lastIndex]

		if {$vectorType == "POLYGON"} {
		    if {[llength $part] < 8} {
			puts "short polygon vector part discarded"
			continue
		    }
		} else {
		    if {[llength $part] < 4} {
			puts "short line vector part discarded"
			continue
		    }
		}

		incr partCoordCount [llength $part]
		if {$vectorType == "POLYGON" && $fillPolygons} {
		    set itemId [eval [concat $c create polygon $part -fill $color -outline $color -tags vec]]
		} else {
		    set itemId [eval [concat $c create line $part -fill $color -tags vec]]
		}
		lappend vectorItems $itemId
		lappend parts $itemId
	    }
	    set part [lrange $coords [expr 2 * [lindex $partIndices end]] end]
	    incr partCoordCount [llength $part]
	    if {$vectorType == "POLYGON" && $fillPolygons} {
		set itemId [eval [concat $c create polygon $part -fill $color -outline $color -tags vec]]
	    } else {
		set itemId [eval [concat $c create line $part -fill $color -tags vec]]
	    }
	    lappend vectorItems $itemId
	    lappend parts $itemId

	    foreach itemId $vectorItems {
		setVectorData $itemId $filename $vectorId $parts $partIndices
	    }

	    lappend items $vectorItems
	    lappend vecFirstItems [lindex $vectorItems 0]
	}
    }

    # loadedFiles is an array mapping shapeFilename to {vectorType vectorCount fieldDescriptionList canvasItemList vectorIds firstitems changedFlag}
    setLoadedFile $filename $vectorType $vectorCount $fieldDescriptions $items $vecIds $vecFirstItems 0
    initModifiedVectorListForLoadedFile $filename
    initAddedVectorsForLoadedFile $filename
}

set idMaps {}
proc unloadAll {} {
    mapOff

    global c

    $c delete all

    clearLoadedFiles
    clearVectorData
    clearVertices

    global editedFile
    set editedFile ""
    unloadModifiedVectorList
    unloadAddedVectors

    stopEditingVertices

    clearUndoList

    global idMaps
    foreach idMap $idMaps {
	global $idMap
	array unset $idMap
    }
    set idMaps {}
}

proc startEditingVertices {} {
    global editingVertices
    set editingVertices true

    global vectorMenu
    global deleteVertexButton
    global addVertexButton
    global editVerticesButton
    $deleteVertexButton config -state normal
    $addVertexButton config -state normal
    $editVerticesButton config -state disabled
    $vectorMenu entryconfigure 1 -state disabled
    $vectorMenu entryconfigure 2 -state normal
    $vectorMenu entryconfigure 3 -state normal
}

proc stopEditingVertices {} {
    global editingVertices
    set editingVertices false

    global vectorMenu
    global deleteVertexButton
    global addVertexButton
    global editVerticesButton
    $deleteVertexButton config -state disabled
    $addVertexButton config -state disabled
    $editVerticesButton config -state normal
    $vectorMenu entryconfigure 1 -state normal
    $vectorMenu entryconfigure 2 -state disabled
    $vectorMenu entryconfigure 3 -state disabled

    global editedVectorCanvasIds
    set editedVectorCanvasIds {}
}

proc unloadAFile {} {
    if {[promptUser "Unload One File?" "Unloading a single large file may take a long time.\nUnloading all files is quick. Continue to unload one?" {Yes No}] == "No"} {
	return
    }
    
    doToFile "Unload A File" "Unload" "unloadFile \$file"
}

proc doToFile {title label command} {
    set names [getLoadedFilenames]

    if {$names == {}} {
	tk_messageBox -message "There are no loaded files"
	return
    }

    set w .unloadAFile
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title
    
    listbox $w.lb
    pack $w.lb -expand true -fill both
    eval "$w.lb insert 0 $names"
    $w.lb selection set 0

    set f $w.buttons
    frame $f
    pack $f -fill both

    set b $f.unload
    button $b -text $label -command [list evalCommandWithFileParm $w $command " \[$w.lb get \[$w.lb curselection\] \[$w.lb curselection\]\]"]
    pack $b -expand true -fill both -side left

    set b $f.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -expand true -fill both -side left

    update idletasks

    setUnloadAFileWidth
}

proc evalCommandWithFileParm {w command file} {
    global cmd
    set cmd [subst -nocommands $command]

    uplevel #0 {eval $cmd}

    destroy $w
}

proc setUnloadAFileWidth {} {
    set w .unloadAFile

    set maxWidth 0
    set maxIndex 0
    for {set index 0} {$index < [$w.lb size]} {incr index} {
	set bbox [$w.lb bbox $index]
	set width [lindex $bbox 2]
	if {$width > $maxWidth} {
	    set maxWidth $width
	    set maxIndex $index
	}
    }

    set item [$w.lb get $maxIndex]
    $w.lb config -width [string length $item]
}

proc unloadFile {path} {
    if {[getLoadedFileChangedFlag $path]} {
	if {[promptUser "Unload without saving?" "Unload $path without saving?" {Yes No}] == "No"} {
	    return
	}
    }

    global c

    clearVertices
    unselectAll

    set canvasItems [getLoadedFileCanvasItems $path]

    # remove file from loadedFiles
    unsetLoadedFile $path

    global editedFile
    if {$editedFile == $path} {
	set editedFile ""
    }

    foreach item $canvasItems {
	# remove canvasItems from vectorData
	unsetVectorData $item

	# remove canvas
	$c delete $item
    }

    catch {destroy .unloadAFile}

    unloadModifiedVectorListForLoadedFile $path
    unloadAddedVectorsForLoadedFile $path

    global editedFile
    if {$editedFile == $path} {
	set editedFile ""
    }

    removeFromUndoList $path

    global idMaps
    if {[set index [lsearch $idMaps ${path}_idMap]] >= 0} {
	set idMaps [lreplace $idMaps $index $index]
	global ${path}_idMap
	array unset ${path}_idMap
    }
}

proc loadSomeFile {loadAction} {
    global tdpsDatabaseDir

    if {$loadAction == "loadFiles"} {
	setAppCursor watch
	loadFiles $tdpsDatabaseDir
	restoreCursor
	return
    }	

    global w c

    set types {{"Shape files" {.shp}} {"All files" *}}

    while {1} {
	if {$tdpsDatabaseDir == ""} {
	    set file [tk_getOpenFile -filetypes $types -parent $w]
	} else {
	    set file [tk_getOpenFile -filetypes $types -parent $w -initialdir $tdpsDatabaseDir]
	}

	if {[string compare $file ""]} {
	    if {[lsearch [getLoadedFilenames] $file] < 0} {
		setAppCursor watch
		$loadAction $file
		restoreCursor

		return
	    } else {
		tk_messageBox -message "$file is already loaded"
	    }
	} else {
	    return
	}
    }
}

# called by menu button
proc loadSeveralFiles {} {
    if {[getLoadedFilenames] == {}} {
	loadSomeFile loadFiles
    } else {
	loadSomeFile loadFiles
    }
}

# called by menu button
proc loadAFile {} {
    if {[getLoadedFilenames] == {}} {
	loadSomeFile loadFile
    } else {
	loadSomeFile loadFile
    }
}

proc saveOneOrMoreChangedFiles {} {
    set changedFiles {}
    foreach file [getLoadedFilenames] {
	if {[getLoadedFileChangedFlag $file]} {
	    lappend changedFiles $file
	}
    }

    if {[llength $changedFiles] == 0} {
	return
    } elseif {[llength $changedFiles] == 1} {
	checkFieldMapping 0 $changedFiles [getLoadedFileVectorType $changedFiles] true true
    } else {
	saveSomeFiles $changedFiles
    }    
}

proc saveSomeFiles {fileNames} {
    set w .dataSources
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    set title "Which Files To Save?"
    wm title $w $title
    wm iconname $w $title

    set grid $w.frame
    frame $grid
    pack $grid -expand true -fill both

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.ok
    button $b -text "OK" -command [list saveSelectedFiles $fileNames]
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
    label $l -text "Save"
    grid $l -row $row -column 2

    incr row

    global _saveVectorType_
    set _saveVectorType_ ""
    foreach fileName $fileNames {
	set l $grid.file${row}
	label $l -text $fileName
	grid $l -row $row -column 0

	set fileType [getLoadedFileVectorType $fileName]
	set l $grid.type${row}
	label $l -text $fileType
	grid $l -row $row -column 1

	set cb $grid.cb${row}
	checkbutton $cb -variable _Save_${fileName}
	grid $cb -row $row -column 2
	global _Save_${fileName}
	set _Save_${fileName} 0

	incr row
    }
}

proc saveOneFile {} {
    set fileNames [getLoadedFilenames]

    if {[llength $fileNames] != 1} {
	selectDataToSave
	return
    }

    checkFieldMapping 0 $fileNames [getLoadedFileVectorType $fileNames] true true
}

proc selectDataToSave {} {
    set fileNames [getLoadedFilenames]

    if {$fileNames == {}} {
	tk_messageBox -message "There are no loaded files"
	return
    }

    if {[llength $fileNames] == 1} {
	global argv
	if {[lsearch $argv "expert"] < 0} {
	    checkFieldMapping 0 $fileNames [getLoadedFileVectorType $fileNames] true
	} else {
	    createFieldMapping $fileNames [getLoadedFileVectorType $fileNames]
	}
	
	return
    }

    set w .dataSources
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Data Sources"
    wm iconname $w "Data Sources"

    set grid $w.frame
    frame $grid
    pack $grid -expand true -fill both

    set f $w.buttons
    frame $f
    pack $f -fill both -expand true
    set b $f.ok
    button $b -text "OK" -command [list ignoreUnselectedFiles $fileNames]
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
    label $l -text "Save"
    grid $l -row $row -column 2

    incr row

    global _saveVectorType_
    set _saveVectorType_ ""
    foreach fileName $fileNames {
	set l $grid.file${row}
	label $l -text $fileName
	grid $l -row $row -column 0

	set fileType [getLoadedFileVectorType $fileName]
	set l $grid.type${row}
	label $l -text $fileType
	grid $l -row $row -column 1

	set cb $grid.cb${row}
	checkbutton $cb -variable _Save_${fileName} -command [list checkSavedDataSourceTypes $fileNames $fileName $fileType]
	grid $cb -row $row -column 2
	global _Save_${fileName}
	set _Save_${fileName} 0

	incr row
    }
}

proc checkSavedDataSourceTypes {fileNames selectedFileName selectedFileType} {
    global _saveVectorType_

    set _saveVectorType_ $selectedFileType

    foreach fileName $fileNames {
	if {$fileName != $selectedFileName} {
	    set fileType [getLoadedFileVectorType $fileName]
	    if {$fileType != $selectedFileType} {
		global _Save_${fileName}

		if {[set _Save_${fileName}]} {
		    tk_messageBox -message "${fileName} will be deselected, because its vector type, \"$fileType\", does not match \"${selectedFileType}\", the vector type of ${selectedFileName}."
		}
		set _Save_${fileName} 0
	    }
	}
    }
}

proc saveSelectedFiles {fileNames} {
    destroy .dataSources

    set selectedFileNames {}

    foreach fileName $fileNames {
	global _Save_${fileName}

	if  {[set _Save_${fileName}]} {
	    lappend selectedFileNames $fileName
	}
    }

    foreach file $selectedFileNames {
	checkFieldMapping 0 $file [getLoadedFileVectorType $file] true true
    }
}

proc ignoreUnselectedFiles {fileNames} {
    global _saveVectorType_

    destroy .dataSources

    set selectedFileNames {}

    foreach fileName $fileNames {
	global _Save_${fileName}

	if  {[set _Save_${fileName}]} {
	    lappend selectedFileNames $fileName
	}
    }

    global argv
    if {[lsearch $argv "expert"] < 0} {
	checkFieldMapping 0 $selectedFileNames $_saveVectorType_ true false
    } else {
	createFieldMapping $selectedFileNames $_saveVectorType_
    }
}

proc createFieldMapping {fileNames vectorType} {
    set w .dbfFields
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Vector Fields"
    wm iconname $w "Vector Fields"

    set f $w.cFrame
    frame $f
    pack $f -expand true -fill both

    set c $f.canvas
    canvas $c -yscrollcommand "$f.vscroll set" -height 5c -width 10c
    grid $c -row 0 -column 0 -sticky news

    set sb $f.vscroll
    scrollbar $sb -command "$c yview"
    grid $sb -row 0 -column 1 -sticky news

    grid rowconfig $f 0 -weight 1
    grid columnconfig $f 0 -weight 1

    set f $w.buttons
    frame $f
    pack $f -fill both

    set b $f.ok
    button $b -text "OK" -command [list checkFieldMapping $w $fileNames $vectorType]
    pack $b -side left -expand true -fill both

    set b $f.cancel 
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left -expand true -fill both

    set grid $c.grid
    frame $grid
    $c create window 0 0 -window  $grid -anchor nw

    set row 0

    global bgColor fgColor
    foreach file $fileNames {
	set l $grid.label${row}
	label $l -text $file -bg $bgColor -fg $fgColor
	grid $l -row ${row} -column 0 -columnspan 5 -sticky w
	incr row

	set l $grid.spacer${row}
	label $l -text "    "
	grid $l -row ${row} -column 0 -sticky w

	set l $grid.current${row}
	label $l -text "Current"
	grid $l -row ${row} -column 1 -sticky w

	set l $grid.typeLabel${row}
	label $l -text "Type"
	grid $l -row ${row} -column 2

	set l $grid.exportLabel${row}
	label $l -text "Export"
	grid $l -row ${row} -column 3

	set l $grid.new${row}
	label $l -text "New"
	grid $l -row ${row} -column 4 -sticky w
	incr row

	foreach description [getLoadedFileFieldDescriptions $file] {
	    set field [lindex $description 0]
	    set type  [lindex $description 1]

	    set l $grid.field${row}
	    label $l -text $field
	    grid $l -row ${row} -column 1 -sticky w

	    set l $grid.type${row}
	    label $l -text $type
	    grid $l -row ${row} -column 2 -sticky w

	    set cb $grid.check${row}
	    checkbutton $cb -variable _${file}_${field}
	    global _${file}_${field}
	    set _${file}_${field} 1
	    grid $cb -row ${row} -column 3

	    set e $grid.entry${row}
	    entry $e -width 13
	    grid $e -row ${row} -column 4 -sticky w
	    $e insert 0 $field
	    bind $e <Key> "update idletasks; truncateEntry $e"
	    global fileFieldEntries
	    array set fileFieldEntries [list ${file}_${field} $e]

	    incr row
	}

	set label $grid.filterLabel${row}
	label $label -text "Use Filter"
	grid $label -row $row -column 1 -sticky e

	set option $grid.filter${row}
	eval tk_optionMenu $option ${file}__filter none [getFilterNames]
	grid $option -row $row -column 2 -columnspan 2 -sticky w
	global ${file}__filter
	set ${file}__filter none

	set viewButton $grid.vb${row}
	button $viewButton -text "View Filter" -command "viewFilter ${file}__filter"
	grid $viewButton -row $row -column 4 -sticky w

	incr row

	set label $grid.spacer${row}
	label $label -text " "
	grid $label -row $row -column 0

	incr row
    }

    $c config -scrollregion "0c 0c 20c [expr int (0.75 * ${row})]c"
    $c xview moveto 0
}

proc getFilterNames {} {
    global filters

    set names [array names filters]

    set names [lremove $names "none"]

    lremove $names "#"
}

proc getFilter {name} {
    global filters

    set filters($name)
}

proc applyFilter {name fieldsAndValues} {
    global filters

    testFilter $fieldsAndValues $filters($name)
}

proc viewFilter {optionVar} {
    global $optionVar

    set name [set $optionVar]
    set filter [getFilter $name]

    tk_messageBox -message "$name filter:\n$filter" -title "$name filter"
}

# to export the fields, we need a mapping from (file, field) to (newField, type)
# and a list of (newField, type, width, decimals)
proc checkFieldMapping {dialog fileNames vectorType {acceptAll false} {justSaveOne false}} {
    global c

    array unset schema

    foreach file $fileNames {
	if {$acceptAll} {
	    global ${file}__filter
	    set ${file}__filter none
	}

	foreach description [getLoadedFileFieldDescriptions $file] {
	    set field [lindex $description 0]
	    set type  [lindex $description 1]
	    set width  [lindex $description 2]
	    set decimals [lindex $description 3]

	    global _${file}_${field}
	    if {$acceptAll || [set _${file}_${field}]} {
		global fileFieldEntries
		set e [lindex [array get fileFieldEntries ${file}_${field}] 1]

		if {$acceptAll} {
		    set newField $field
		} else {
		    set newField [$e get]
		}

		set pair [array get schema $newField]

		if {$pair != {}} {
		    set pair [lindex $pair 1]
		    set curType [lindex $pair 0]
		    set curWidth [lindex $pair 1]
		    set curDecimals [lindex $pair 2]
		    set curFile [lindex $pair 3]
		    set curField [lindex $pair 4]

		    if {$curType != $type} {
			tk_messageBox -message [list Field $curField from $curFile is mapped to output field \
				$newField with type $curType, but field $field from $file is mapped \
				to output field $newField with type $type, so field $newField has been \
				mapped to two different types. Change one of the output field names to remove the type conflict.]
			return
		    }

		    if {$curWidth > $width} {
			set width $curWidth
		    }
		    if {$curDecimals > $decimals} {
			set decimals $curDecimals
		    }
		}
		array set schema [list $newField [list $type $width $decimals $file $field]]
		lappend mapping ${file}_${field} $newField
	    }
	}
    }

    catch {destroy $dialog}

    set types {{"Shape files" {.shp}} {"All files" *}}
    if {$justSaveOne} {
	set file $fileNames
    } else {
	global w
	set file [tk_getSaveFile -filetypes $types -parent $w \
		-initialfile Untitled -defaultextension .shp]
    }

    if {[string compare $file ""]} {
	setAppCursor watch
	saveAs $file $fileNames [array get schema] $mapping $vectorType
	restoreCursor
    } else {
	tk_messageBox -message "Save cancelled"
    }
}

# A filter is an expression including variables named with dbf field
# names. A use of a filter will provide a list of field names and
# values. The values will be bound to the names; then the filter
# expression will be evaluated. If the expression returns true without
# error, then the vector will pass through the filter. Otherwise, the
# vector will be blocked by the filer.

# fieldsAndValues is a list {name value name value ... }
# filter is a boolean expression based on field names expected to be in fieldsAndValues

# Example use:
#  % testFilter {foo 42 bar 17} {$foo > $bar}
#  true
#  % testFilter {foo 42 bar 17} {$foo < $bar}
#  false
#  % testFilter {foo 15 bar 17} {$foo + 2 == $bar}
#  true
#  % testFilter {foo 15 bar 17} {$foo == $barf} ; # note the undefined var barf
#  false

proc testFilter {fieldsAndValues filter} {
    foreach {name value} $fieldsAndValues {
	set $name $value
    }

    if {[catch {eval [list expr $filter]} result]} {
	tk_messageBox -message "Filter error:\n$result\nThis could mean that a filter is being\napplied to an inappropriate data set." -title "Filter Error"
	return false
    }

    if {$result == 0 || $result == "false"} {
	return false
    } else {
	return true
    }
}

# The filter file holds a list of {name filter name filter ...}
# Example filter file contents (excluding "#"):
#  fubar {$foo == $bar}
#  easy {true}
#  tough {false}

proc loadFilters {path} {
    set file [open $path r]
    set data [read $file]
    close $file

    global filters
    array set filters $data
    array set filters {none true}
}

proc clearFilters {} {
    global filters

    array unset filters
}

# When we save, copy unaltered vectors from the original file to the
# new file. However, some of the vectors will have new ids if any of
# them are deleted. When a save is made to a loaded file, the loaded
# vector ids may become obsolete. This is a problem if another save is
# made, because obsolete vector ids would be used for the copying of
# unaltered vectors. So a map is made for the loaded file to record
# the new file ids for the loaded vectors. The mapping is used during
# subsequent saves. Unloading the file must include discarding the map.

proc saveAs {path fileNames schema mapping vectorType} {
    set mapName ${path}_idMap

    global $mapName

    set savedPath $path
    # use a temp file if the save file is among the source files
    set moveTempFileTo ""
    if {[lsearch $fileNames $path] >= 0} {
	set doMapIds 1
	global idMaps
	if {[lsearch $idMaps $mapName] < 0} {
	    lappend idMaps $mapName
	}
	set path [split $path .]
	if {[string tolower [lindex $path end]] == "shp"} {
	    set path [lrange $path 0 [expr [llength $path] - 2]]
	    set path [join $path .]
	}
	set moveTempFileTo $path
	set num 0
	while {[file exists ${path}_tmp${num}]} {
	    incr num
	}
	set path ${path}_tmp${num}
    } else {
	set doMapIds 0
    }

    # create new SHP and DBF files for output
    set savedDBFFile [DBFCreate $path]
    set savedSHPFile [SHPCreate $path $vectorType]

    foreach {field description} $schema {
	set type [lindex $description 0]
	set width [lindex $description 1]
	set decimals [lindex $description 2]

	DBFAddField $savedDBFFile $field $type $width $decimals
    }

    set newShapeIndex 0
    foreach loadedFilename $fileNames {
	# determine which fields to save
	set transferredFields {}
	set oldFieldIndex 0
	foreach description [getLoadedFileFieldDescriptions $loadedFilename] {
	    set field [lindex $description 0]
	    set type  [lindex $description 1]
	    set index [lsearch $mapping ${loadedFilename}_${field}]
	    if {$index >= 0} {
		set newField [lindex $mapping [expr $index + 1]]
		set newFieldIndex [expr [lsearch $schema $newField] / 2]
		lappend transferredFields $oldFieldIndex $newFieldIndex $type
	    }
	    incr oldFieldIndex
	}

	# open input file
	set loadedDBFFile [DBFOpen $loadedFilename]
	set loadedSHPFile [SHPOpen $loadedFilename]
	
	foreach oldShapeIndex [getLoadedFileVectorIds $loadedFilename] {
	    # global $mapName
	    set was $oldShapeIndex

	    set pair [array get $mapName $oldShapeIndex]
	    set mapTo [lindex $pair 1]

	    if {$mapTo == ""} {
		set sourceShapeIndex $oldShapeIndex
	    } else {
		set sourceShapeIndex $mapTo
	    }


	    if {$doMapIds && $oldShapeIndex != $newShapeIndex} {
		array set $mapName [list $oldShapeIndex $newShapeIndex]
	    }

	    global ${loadedFilename}__filter
	    set filterName [set ${loadedFilename}__filter]

	    # check filter
	    if {$filterName == "none" || [applyFilter $filterName [getVectorProperties $loadedFilename $sourceShapeIndex]]} {
		# copy dbf data
		foreach {oldFieldIndex newFieldIndex type} $transferredFields {
		    switch $type {
			FTString {
			    DBFCopyString $loadedDBFFile $sourceShapeIndex $oldFieldIndex $savedDBFFile $newShapeIndex $newFieldIndex
			}
			FTInteger {
			    DBFCopyInteger $loadedDBFFile $sourceShapeIndex $oldFieldIndex $savedDBFFile $newShapeIndex $newFieldIndex
			}
			FTDouble {
			    DBFCopyDouble $loadedDBFFile $sourceShapeIndex $oldFieldIndex $savedDBFFile $newShapeIndex $newFieldIndex
			}
		    }
		}

		# copy shp data
		global modifiedLoadedVectorsSHP
		if {[vectorModifiedSHP $loadedFilename $oldShapeIndex]} {
		    # it's been modified, so save current values
		    global c scalePower
		    set coords [$c coords [vectorIdToCanvasParts $oldShapeIndex $loadedFilename]]

		    SHPWriteSimpleObject $savedSHPFile -1 $coords $scalePower
		} else {
		    # it hasn't been modified, so just copy it
		    SHPCopyVector $loadedSHPFile $sourceShapeIndex $savedSHPFile
		}

		incr newShapeIndex
	    }
	}

	foreach canvasId [getAddedVectorCanvasIds $loadedFilename] {
	    # initialize dbf data
	    foreach {oldFieldIndex newFieldIndex type} $transferredFields {
		switch $type {
		    FTString {
			DBFWriteString $savedDBFFile $newShapeIndex $newFieldIndex ""
		    }
		    FTInteger {
			DBFWriteInteger $savedDBFFile $newShapeIndex $newFieldIndex 0
		    }
		    FTDouble {
			DBFWriteDouble $savedDBFFile $newShapeIndex $newFieldIndex 0.0
		    }
		}
	    }

	    global c scalePower
	    set coords [$c coords $canvasId]

	    SHPWriteSimpleObject $savedSHPFile -1 $coords $scalePower

	    incr newShapeIndex
	}

	# close input file
	DBFClose $loadedDBFFile
	SHPClose $loadedSHPFile
    }


    DBFClose $savedDBFFile
    SHPClose $savedSHPFile

    if {$moveTempFileTo != ""} {
	exec mv ${path}.shp ${moveTempFileTo}.shp
	exec mv ${path}.shx ${moveTempFileTo}.shx
	exec mv ${path}.dbf ${moveTempFileTo}.dbf
    }

    clearLoadedFileChangedFlag $savedPath
}

proc truncateEntry {e} {
    if {[string length [$e get]] > 11} {
	$e delete 11 end
	bell
    }
}
