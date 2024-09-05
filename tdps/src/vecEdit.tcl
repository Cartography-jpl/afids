##### Vector Editing Overview ####
#
# There are two mutually exclusive selection modes: vector and vertex.
# The editor initializes to vector selection mode.  Mouse behavior,
# context menus and edit menu behavior depend on the selection mode.

##### Vector selection mode #####

#+ M-L far from any canvas items, and Select->None deselects all
#+ selected canvas items (which should be only lines and polygons)

#+ Mouse movement near a closest canvas item that is selected changes
#+ the cursor to the four arrow move icon

#+ M-L near a closest canvas item that is not selected deselects all
#+ others, if any, and selects that closest item

#+ M-L near a closest canvas item that is already selected initiates a
#+ drag of all selected items

#+ Selection of any part of a multi-part polygon causes selection of
#+ the whole polygon. Multi-part polygons cannot be moved.

#+ S-M-L adds the closest canvas item to the current collection of
#+ selected items if it is not selected, and removes it from the
#+ collection if it is already selected. It does not effect non close
#+ items. It toggles the selection status of the moused items.

# M-R->Delete, 
#+ Edit->Delete, 
# and <Del> delete all selected lines and
# polygon canvas items. If part of a multi-part polygon is selected,
# then the entire polygon is deleted, after confirmation.

# M-R->Split, Edit->Split will replace all selected multi-part
# polygons with two or more single part polygons.

# M-R->Edit Vertices, Edit->Vertices deselects all vectors and creates
# vertex canvas items for the previously selected vectors (though not
# for multi-part polygons). Then selection mode switches to vertex
# selection mode.

##### Vertex selection mode ####
#
# M-L far from any canvas items, and Select->None deselects all
# selected canvas items (which should be only vertices)

# Mouse movement near a closest canvas item that is selected changes
# the cursor to the four arrow move icon

# M-L near a closest vertex canvas item that is not selected deselects
# all others, if any, and selects that closest item

# M-L near a closest vertex canvas item that is already selected
# initiates a drag of all selected items (which should be only
# vertices)

# M-L near a closest canvas item that is a line or polygon removes all
# vertex canvas items and switches to vector selection mode.

# S-M-L adds the closest canvas item to the current collection of
# selected items if it is not selected, and removes it from the
# collection if it is already selected

# M-R->Delete, Edit->Delete, and <Del> delete all selected
# vertices. If this leaves a line vector with fewer than two vertices,
# or a polygon vector with fewer than three vertices, then the vector
# will be completely deleted.

# M-R->Add Vertices, Edit->Add Vertices, <Alt-P> insert vertices
# between all adjacent selected pairs of vertices

#####  Implementation ####
#

# delete selected vertices
# if this deletion leaves a line with <2 points, delete the line; if a polygon with <4 points, delete the polygon

proc deleteVertices {} {
    global editingVertices

    if {! $editingVertices} {
	tk_messageBox -message "Must be editing vertices to insert vertices"
	return
    }

    global c
    set selectedItems [$c find withtag selected]

    if {$selectedItems == {}} {
	tk_messageBox -message "No vertices are selected"
	return
    }

    global editingVertices
    # collect a list of vector canvas ids with selected vertices
    set vectorsBeingEdited {}
    foreach item $selectedItems {
	set vectorCanvasId [getVectorFromVertex $item]
	
	if {[lsearch $vectorsBeingEdited $vectorCanvasId] < 0} {
	    lappend vectorsBeingEdited $vectorCanvasId
	}
    }

    set undoActions {}
    global vectorVertexItems
    # copy each vector canvas item unless its selected
    foreach vectorCanvasId $vectorsBeingEdited {
	set vertexItems $vectorVertexItems($vectorCanvasId)
	set coords [$c coords $vectorCanvasId]

	set newCoords {}
	set newVertexItems {}
	
	# for each vertex item in the edited vector
	for {set index 0} {$index < [llength $vertexItems]} {incr index} {
	    # get the vertex item
	    set vertexItem [lindex $vertexItems $index]
	    
	    # if this vertex is not selected, keep it
	    if {[lsearch $selectedItems $vertexItem] < 0} {
		
		# get the coords of this vertex
		set x [lindex $coords [expr 2 * $index]]
		set y [lindex $coords [expr 2 * $index + 1]]

		lappend newVertexItems $vertexItem
		lappend newCoords $x $y
	    }
	}

	# determine vector type
	set vectorType [getLoadedFileVectorType [canvasItemToFilename $vectorCanvasId]]

	if {$vectorType == "POLYGON"} {
	    # check to make sure polygons are still closed
	    set ccnt [llength $newCoords]
	    # if the first and last vertices are not equal, duplicate the first vertex
	    if {[lindex $newCoords 0] != [lindex $newCoords [expr $ccnt -2]] || [lindex $newCoords 1] != [lindex $newCoords [expr $ccnt -1]]} {
		# copy the first vertex to the end
		set xnew [lindex $newCoords 0]
		set ynew [lindex $newCoords 1]

		# append the new vertex coords
		lappend newCoords $xnew $ynew

		# create the new vertex canvas item
		set id [$c create oval [expr $xnew - 3] [expr $ynew - 3] [expr $xnew + 3] [expr $ynew + 3] -fill [getNormalVertexColor] -outline [getNormalVertexColor] -tag vertex]
		mapVertexToVector $id $vectorCanvasId
		lappend newVertexItems $id
	    }
	}

	# need to check for too few points here
	if {($vectorType == "POLYGON" && [llength $newVertexItems] < 4) || [llength $newVertexItems] < 2} {
	    lappend undoActions [captureUndoAction $vectorCanvasId deleteVector]
	    
	    foreach item $newVertexItems {
		$c delete $item
	    }
	    $c delete $vectorCanvasId
	    if {[canvasItemToVectorId $vectorCanvasId] < 0} {
		removeAddedVector $vectorCanvasId
	    } else {
		deleteVectorIgnoringCanvasItems $vectorCanvasId
	    }
	} else {
	    lappend undoActions [captureUndoAction $vectorCanvasId deleteVertex]
	    
	    # update vector
	    array set vectorVertexItems [list $vectorCanvasId $newVertexItems]
	    eval [concat $c coords $vectorCanvasId $newCoords]
	    saveModifiedVectorCanvasItem $vectorCanvasId
	}
    }

    if {$undoActions != {}} {
	addUndoActions $undoActions deleteVertex
    }

    # delete vertices
    foreach vertexCanvasItem $selectedItems {
	$c delete $vertexCanvasItem
	unmapVertexToVector $vertexCanvasItem
    }
}

# insert vertices between all adjacent selected vertices
proc insertVertices {} {
    global editingVertices

    if {! $editingVertices} {
	tk_messageBox -message "Must be editing vertices to insert vertices"
	return
    }

    global c
    set selectedItems [$c find withtag selected]

    if {$selectedItems == {}} {
	tk_messageBox -message "No vertices are selected"
	return
    }

    # collect a list of vector canvas ids with selected vertices
    set vectorsBeingEdited {}
    foreach item $selectedItems {
	set vectorCanvasId [getVectorFromVertex $item]
	
	if {[lsearch $vectorsBeingEdited $vectorCanvasId] < 0} {
	    lappend vectorsBeingEdited $vectorCanvasId
	}
    }

    global vectorVertexItems
    set foundAnAdjacentPair false
    # check each vector canvas item for adjacent selected vertices
    foreach vectorCanvasId $vectorsBeingEdited {
	set vertexItems $vectorVertexItems($vectorCanvasId)
	set coords [$c coords $vectorCanvasId]

	set newCoords {}
	set newVertexItems {}
	set lastWasSelected false
	
	# for each vertex item in the edited vector
	for {set index 0} {$index < [llength $vertexItems]} {incr index} {
	    # get the coords of this vertex
	    set x1 [lindex $coords [expr 2 * $index]]
	    set y1 [lindex $coords [expr 2 * $index + 1]]

	    # if this index is selected
	    if {[lsearch $selectedItems [lindex $vertexItems $index]] >= 0} {

		# if the last one was selected too, then insert a point between this one and the last one
		if {$lastWasSelected} {
		    set foundAnAdjacentPair true

		    # get coords of last vertex
		    set x0 [lindex $coords [expr 2 * ($index - 1)]]
		    set y0 [lindex $coords [expr 2 * ($index - 1) + 1]]

		    # compute new point
		    set xnew [expr (0.0 + $x0 + $x1) / 2]
		    set ynew [expr (0.0 + $y0 + $y1) / 2]

		    # append the new vertex coords
		    lappend newCoords $xnew $ynew

		    # create the new vertex canvas item
		    set id [$c create oval [expr $xnew - 3] [expr $ynew - 3] [expr $xnew + 3] [expr $ynew + 3] -fill [getSelectedVertexColor] -outline [getNormalVertexColor] -tag {vertex selected}]
		    mapVertexToVector $id $vectorCanvasId
		    lappend newVertexItems $id
		} else {
		    set lastWasSelected true
		}
	    } else {
		set lastWasSelected false
	    }

	    # append the current vertex
	    lappend newVertexItems [lindex $vertexItems $index]
	    lappend newCoords $x1 $y1
	}

	# update vector
	array set vectorVertexItems [list $vectorCanvasId $newVertexItems]
	eval [concat $c coords $vectorCanvasId $newCoords]
	saveModifiedVectorCanvasItem $vectorCanvasId
    }

    if {! $foundAnAdjacentPair} {
	tk_messageBox -message "Select at least two adjacent vertices"
    }
}

# Any change to a line or polygon vector, whether moving,
# adding/removing points, removing holes, should be noted as modified,
# so that it will not be remotely saved on the next save command. Note
# that proc deleteLoadedCanvasItem removes the canvasIds and vectorIds
# associated with the shapeFile in the loadedFiles array.

proc saveModifiedVectorCanvasItem {item} {
    # if this is an added item, mark the file as changed, but don't
    # track changed vector id (there may be one from a save, but it's
    # not in LoadedFileVectorIds, so it will just get saved from
    # AddedVectorCanvasIds, again).
    set filename [canvasIdToAddedVectorFilename $item]
    if {$filename != ""} {
	setLoadedFileChangedFlag $filename
	return
    }
    
    if {! [partOfMultiPartVector $item]} {
	recordModifiedVectorSHP [canvasItemToFilename $item] [canvasItemToVectorId $item]
    }
}

proc saveModifiedSelection {} {
    global editingVertices

    if {$editingVertices} {
	global editedVectorCanvasIds
	# it's possible that only some of the edited vectors actually were changed...
	set selected $editedVectorCanvasIds
    } else {
	global c
	set selected [$c find withtag selected]
    }

    foreach item $selected {
	saveModifiedVectorCanvasItem $item
    }
}

proc splitVector {} {
    global editingVertices
    if {! $editingVertices} {
	tk_messageBox -title "Not editing vertices" -message "At least one vertex of exactly one object must be selected to break the object"
	return
    }

    global c
    set selectedItems [$c find withtag selected]
    if {[llength $selectedItems] == 0} {
	tk_messageBox -title "No vertices selected" -message "At least one vertex of exactly one object must be selected to break the object"
	return
    }

    # collect a list of vector canvas ids with selected vertices
    set vectorsBeingEdited {}
    foreach item $selectedItems {
	set vectorCanvasId [getVectorFromVertex $item]
	
	if {[lsearch $vectorsBeingEdited $vectorCanvasId] < 0} {
	    lappend vectorsBeingEdited $vectorCanvasId
	}
    }

    if {[llength $vectorsBeingEdited] != 1} {
	tk_messageBox -title "Breaking more than one object" -message "At least one vertex of exactly one object must be selected to break the object"
	return
    }
    
    set editedVector $vectorsBeingEdited

    set filename [canvasItemToFilename $vectorsBeingEdited]
    set objectVectorType [getLoadedFileVectorType $filename]

    # new lines go to editedFile, which better be a line file
    global editedFile

    if {$editedFile == ""} {
	selectEditedFile splitVector
	return
    }

    set vectorType [getLoadedFileVectorType $editedFile]

    if {$vectorType != "ARC"} {
	tk_messageBox -title "Cannot add lines to polygon file" -message "The line vectors derived from breaking the object cannot be added to $editedFile. A line vector file must be opened/selected for receiving new line vectors."
	return
    }

    # editedVector is the canvas of the line/polygon to be split
    # selectedItems holds the canvas ids of the selected vertices
    # editedFile is the ARC file to receive the polygon segments
    global vectorVertexItems
    set vectorVertices $vectorVertexItems($editedVector)
    set vectorLength [llength $vectorVertices]
    set selectedItemCount [llength $selectedItems]

    set first [lindex $vectorVertices 0]
    set last [lindex $vectorVertices [expr $vectorLength - 1]]

    # if it's a polygon and neither the first, nor last vertex is selected, join the ends
    set joinHeadTail 0
    if {$objectVectorType == "POLYGON"} {
	if {[lsearch $selectedItems $first] < 0 && [lsearch $selectedItems $last] < 0} {
	    set joinHeadTail 1
	}
    }

    set segments {}

    if {$selectedItemCount == 1} {
	# special case where only one vertex is selected
	set itemIndex [lsearch $vectorVertices $selectedItems]
	if {$joinHeadTail} {
	    lappend segments [concat [lrange $vectorVertices $itemIndex end] [lrange $vectorVertices 1 $itemIndex]]
	} else {
	    if {$itemIndex == 0 || $itemIndex == [expr $vectorLength - 1]} {
		lappend segments $vectorVertices
	    } else {
		lappend segments [lrange $vectorVertices 0 $itemIndex] [lrange $vectorVertices $itemIndex end]
	    }
	}
    } elseif {$selectedItemCount == 2 && [lsearch $selectedItems $first] >= 0 && [lsearch $selectedItems $last] >= 0} {
	#special case where only first and last vertices are selected
	lappend segments $vectorVertices
    } else {
	# general case
	set vertexIndex 1

	# get first segment
	while {[lsearch $selectedItems [lindex $vectorVertices $vertexIndex]] < 0} {
	    incr vertexIndex
	}

	if {$joinHeadTail} {
	    set head [lrange $vectorVertices 1 $vertexIndex]
	} else {
	    lappend segments [lrange $vectorVertices 0 $vertexIndex]
	}

	# get the next segment(s)
	while {$vertexIndex < [expr $vectorLength - 1]} {
	    set startIndex $vertexIndex
	    incr vertexIndex
	    while {[lsearch $selectedItems [lindex $vectorVertices $vertexIndex]] < 0 && $vertexIndex < [expr $vectorLength - 1]} {
		incr vertexIndex
	    }
	    set endIndex $vertexIndex

	    set segment [lrange $vectorVertices $startIndex $endIndex]
	    if {$joinHeadTail && $vertexIndex == [expr $vectorLength - 1]} {
		lappend segments [concat $segment $head]
	    } else {
		lappend segments $segment
	    }
	}
    }

    global c

    set vectorCoords [$c coords $editedVector]

    set coordList {}
    foreach segment $segments {
	set coords {}
	foreach vertex $segment {
	    set index [lsearch $vectorVertices $vertex]
	    lappend coords [lindex $vectorCoords [expr 2 * $index]] [lindex $vectorCoords [expr 2 * $index + 1]]
	}
	lappend coordList $coords
    }

    foreach lineCoords $coordList {
	addVector [eval [concat $c create line $lineCoords -fill [fileToColor $editedFile] -tags vec]] $editedFile
    }

    unselectAll
    clearVertices

    selectCanvasItems $editedVector none
}

proc copySelectedVectors {} {
    global c

    set selected [$c find withtag selected]

    if {$selected == {}} {
	tk_messageBox -message "No vectors are selected."
	return
    }

    clearClipboard

    global c

    set selected [$c find withtag selected]

    if {$selected == {}} {
	tk_messageBox -message "No vectors are selected."
	return
    }

    foreach canvasId $selected {
	set canvasIds [canvasItemToCanvasParts $canvasId]
	set canvasIdCount [llength $canvasIds]

	# don't copy multipart vectors
	if {$canvasIdCount == 1} {
	    appendToClipboard $canvasId
	}
    }
}

set clipboard {}
proc clearClipboard {} {
    global clipboard

    set clipboard {}
}

proc appendToClipboard {canvasId} {
    # clipboard holds list of coordinate lists
    global c clipboard
    lappend clipboard [$c coords $canvasId]
}

proc cutSelectedVectors {} {
    clearClipboard

    cutSelectedVectorsInternal 0
}

proc deleteSelectedVectors {} {
    cutSelectedVectorsInternal 1
}

proc cutSelectedVectorsInternal {justDelete} {
    global c

    set selected [$c find withtag selected]

    if {$selected == {}} {
	tk_messageBox -message "No vectors are selected."
	return
    }

    set undoList {}
    set deletedIds {}
    set canvasIdsToDelete {}
    foreach canvasId $selected {
	if {[lsearch $deletedIds $canvasId] >= 0} {
	    continue
	}

	set canvasIds [canvasItemToCanvasParts $canvasId]
	set filename [canvasItemToFilename $canvasId]
	set canvasItems [getLoadedFileCanvasItems $filename]

	set canvasIdCount [llength $canvasIds]
	if {$canvasIdCount > 1} {
	    if {$justDelete} {
		set msg "A selected item is one part of a $canvasIdCount part vector with holes.\nIt will not be available for undo.\nContinue permanently deleting entire vector?"
	    } else {
		set msg "A selected item is one part of a $canvasIdCount part vector with holes.\nIt will not be available for pasting or undo.\nContinue permanently deleting entire vector?"
	    }
	    if {[promptUser "Continue Delete Vector?" $msg {Yes No}] == "No"} {
		continue
	    }
	}

	setLoadedFileChangedFlag $filename

	set deletedIds [concat $deletedIds $canvasIds]

	if {$canvasIdCount == 1} {
	    if {! $justDelete} {
		appendToClipboard $canvasId
	    }
	    lappend undoList $canvasId
	}

	lappend canvasIdsToDelete $canvasId
    }

    if {$undoList != {}} {
	set actions {}
	foreach canvasId $undoList {
	    lappend actions [captureUndoAction $canvasId deleteVector]
	}
	addUndoActions $actions deleteVector
    }

    foreach canvasId $canvasIdsToDelete {
	deleteCanvasItem $canvasId
    }
}

proc captureUndoAction {canvasId operation} {
    switch $operation {
	deleteVector {
	    global c scalePower
	    set coords {}
	    foreach coord [$c coords $canvasId] {
		lappend coords [expr $coord / pow (2, $scalePower)]
	    }

	    set color [lindex [$c itemconfigure $canvasId -fill] 4]
	    set file [canvasItemToFilename $canvasId]

	    # to undo a delete, call addVector, like this
	    # addVector [$c create line $x1 $y1 $x2 $y2 -fill $color -tags vec] $editedFile
	    set action [list scaleAndRestoreVector $coords $color vec $file $canvasId]
	}

	deleteVertex - moveVector {
	    global c scalePower
	    set coords {}
	    foreach coord [$c coords $canvasId] {
		lappend coords [expr $coord / pow (2, $scalePower)]
	    }

	    set color [lindex [$c itemconfigure $canvasId -fill] 4]
	    set file [canvasItemToFilename $canvasId]

	    # file is here only to facilitate searching undoList when a file is closed
	    set action [list restoreVectorShape $canvasId $coords $file $operation]
	}
	default {
	    set action ""
	}
    }
    return $action
}

proc restoreVectorShape {vectorCanvasId coords file operation} {

    # If a vector is moved, deleted, undeleted, and finally unmoved,
    # the undo move action will reference a former canvas id replaced when
    # the vector was undeleted. So the undo move canvas id needs to be
    # checked for earlier values.

    if {$operation == "moveVector"} {
	global originalToRestoredVectorCanvasId

	set names [array names originalToRestoredVectorCanvasId]
	while {[lsearch $names $vectorCanvasId] >= 0} {
	    set vectorCanvasId $originalToRestoredVectorCanvasId($vectorCanvasId)
	}
    }

    global c scalePower

    set scaledCoords {}
    foreach coord $coords {
	lappend scaledCoords [expr $coord * pow (2, $scalePower)]
    }

    # restore vector canvas item
    eval [concat $c coords $vectorCanvasId $scaledCoords]

    # check to see if it is being edited and replace its dots
    global editedVectorCanvasIds
    if {[lsearch $editedVectorCanvasIds $vectorCanvasId] >= 0} {
	# remove existing dots
	global vectorVertexItems
	set existingVertexIds $vectorVertexItems($vectorCanvasId)
	foreach vertexId $existingVertexIds {
	    unmapVertexToVector $vertexId
	    global c
	    $c delete $vertexId
	}

	# restore dots
	editCanvasVectorItem $vectorCanvasId
    }
}

set undoList {}
proc addUndoActions {actions operation} {
    global undoList

    switch $operation {
	moveVertex {
	    set label "Move Vertices (Ctl-U)"
	    lappend undoList [list $label $actions]
	    updateEditMenuUndo $label
	}
	moveVector {
	    set label "Move Vectors (Ctl-U)"
	    lappend undoList [list $label $actions]
	    updateEditMenuUndo $label
	}
	deleteVector {
	    set label "Cut/Delete Vectors (Ctl-U)"
	    lappend undoList [list $label $actions]
	    updateEditMenuUndo $label
	}
	deleteVertex {
	    set label "Remove Vertices (Ctl-U)"
	    lappend undoList [list $label $actions]
	    updateEditMenuUndo $label
	}
    }
}

proc clearUndoList {} {
    global undoList
    set undoList {}
    updateEditMenuUndo ""
}

proc removeFromUndoList {path} {
    global undoList

    set newUndoList {}
    foreach undoItem $undoList {
	set undoLabel [lindex $undoItem 0]
	set actionSet [lindex $undoItem 1]
	set newActionSet {}
	foreach action $actionSet {
	    if {[lsearch $action $path] < 0} {
		lappend newActionSet $action
	    }
	}
	if {$newActionSet != {}} {
	    lappend newUndoList [list $undoLabel $newActionSet]
	}
    }

    set undoList $newUndoList

    updateEditMenuUndo [lindex [lindex $undoList 0] 0]
}

array set originalToRestoredVectorCanvasId {}

proc scaleAndRestoreVector {coords fillColor tag file originalCanvasId} {
    global c scalePower

    set newCoords {}
    foreach coord $coords {
	lappend newCoords [expr $coord * pow (2, $scalePower)]
    }

    set vectorType [getLoadedFileVectorType $file]

    global fillPolygons
    if {$vectorType == "POLYGON" && $fillPolygons} {
	set newCanvasId [eval [concat $c create polygon $newCoords -fill $fillColor -outline $fillColor -tags $tag]]
    } else {
	set newCanvasId [eval [concat $c create line $newCoords -fill $fillColor -tags $tag]]
    }
    global originalToRestoredVectorCanvasId
    array set originalToRestoredVectorCanvasId [list $originalCanvasId $newCanvasId]
    addVector $newCanvasId $file
}

proc updateEditMenuUndo {label} {
    global editMenuWidget
    set m $editMenuWidget
    $m delete 6
    $m add command -label "Undo $label" -command undo -underline 0
}

proc undo {} {
    global undoList

    if {$undoList == {}} {
	tk_messageBox -message "Nothing to undo"
	return
    }

    set length [llength $undoList]
    set last [expr $length - 1]

    set actionSet [lindex $undoList $last]
    set undoList [lrange $undoList 0 [expr $last - 1]]

    foreach action [lindex $actionSet 1] {
	eval $action
    }

    set length [llength $undoList]
    set last [expr $length - 1]

    set actionSet [lindex $undoList $last]
    updateEditMenuUndo [lindex $actionSet 0]
}

proc pasteVectorPasteBuffer {} {
    global editedFile
    if {$editedFile == ""} {
	selectEditedFile "pasteVectorPasteBuffer"
	return
    }

    global c clipboard

    set vectorType [getLoadedFileVectorType $editedFile]
    set newItems {}
    foreach coordList $clipboard {
	# check coords to match file editedFile type (POLYGON or ARC)
	if {$vectorType == "POLYGON"} {
	    if {[llength $coordList] < 6} {
		tk_messageBox -message "Unable to close two-vertex line into a polygon;\nfirst add another vertex"
		continue
	    }
	    set firstX [lindex $coordList 0]
	    set firstY [lindex $coordList 1]
	    set lastX [lindex $coordList [expr [llength $coordList] - 2]]
	    set lastY [lindex $coordList [expr [llength $coordList] - 1]]

	    # close the polygon
	    if {$firstX != $lastX || $firstY != $lastY} {
		lappend coordList $firstX $firstY
	    }
	}

	global fillPolygons
	if {$vectorType == "POLYGON" && $fillPolygons} {
	    lappend newItems [addVector [eval concat [$c create polygon $coordList -fill [fileToColor $editedFile] -outline [fileToColor $editedFile] -tags vec]] $editedFile]
	} else {
	    lappend newItems [addVector [eval concat [$c create line $coordList -fill [fileToColor $editedFile] -tags vec]] $editedFile]
	}
    }

    unselectAll

    selectCanvasItems $newItems "none"
}

set editingVertices false
proc editVertices {} {
    # notify bindings using "bind", rather than "canvas bind"
    global editingVertices

    if {$editingVertices} {
	tk_messageBox -message "Already editing vertices"
	return
    }

    global c

    set selected {}
    foreach item [$c find withtag selected] {
	# filter out any multi part vectors
	if {[llength [canvasItemToCanvasParts $item]] == 1} {
	    lappend selected $item
	}
    }

    if {$selected == {}} {
	# tk_messageBox -message "No (single-part) vectors are selected."
	return
    }

    startEditingVertices

    unselectAllVectors

    # create and bind canvas items with "canvas bind"
    global editedVectorCanvasIds
    set editedVectorCanvasIds $selected

    foreach item $selected {
	editCanvasVectorItem $item
    }
}

# create vertex handles for item
proc editCanvasVectorItem {item} {
    global c

    set vectorType [getLoadedFileVectorType [canvasItemToFilename $item]]

    set vertexItems {}
    set coords [$c coords $item]
    set vertexCount [expr [llength $coords] / 2]
    set vertex 0
    set firstId ""
    foreach {x y} $coords {
	set id [$c create oval [expr $x - 3] [expr $y - 3] [expr $x + 3] [expr $y + 3] -fill [getNormalVertexColor] -outline [getNormalVertexColor] -tag vertex]
	if {$firstId == ""} {
	    set firstId $id
	}
	# the canvas will tell us all the items with tag vertex, but we need to store the line/poly item in order
	lappend vertexItems $id
	mapVertexToVector $id $item

	incr vertex
    }
    set lastId $id

    selectOneVertex $firstId

    if {$vectorType == "POLYGON"} {
	selectOneVertex $lastId
    }	

    global vectorVertexItems
    array set vectorVertexItems [list $item $vertexItems]
}

proc selectOneVertex {id} {
    global c

    $c addtag selected withtag $id
    $c itemconfigure $id -fill [getSelectedVertexColor]
}

proc gotoNextVertex {{dontJumpToFront 0}} {
    global editingVertices c

    if {! $editingVertices} {
	return
    }

    set selected [lindex [$c find withtag selected] 0]

    # if nothing is selected, pick one of the edited vector canvas items' first vertex
    set dontGoto 0
    if {$selected == ""} {
	global editedVectorCanvasIds
	set vectorCanvasId [lindex $editedVectorCanvasIds 0]

	global vectorVertexItems
	set selected [lindex $vectorVertexItems($vectorCanvasId) 0]
	set dontGoto 1
    }

    set vectorCanvasItem [vertexIdToVectorCanvasItem $selected]
    set vectorType [getLoadedFileVectorType [canvasItemToFilename $vectorCanvasItem]]

    unselectAllVertices
    
    # get the list of all vertices for this vertex's vector canvas item
    set vertexList [vertexIdToVertexList $selected]
    set vertexCount [llength $vertexList]

    set index [lsearch $vertexList $selected]

    if {$vectorType == "POLYGON"} {
	if {$dontGoto} {
	    # select the first and last
	    selectOneVertex [lindex $vertexList 0]
	    selectOneVertex [lindex $vertexList [expr $vertexCount - 1]]
	} elseif {$index == 0 || $index == [expr $vertexCount - 1]} {
	    # if it's the first or last, select the second
	    selectOneVertex [lindex $vertexList 1]
	} elseif {$index == [expr $vertexCount - 2]} {
	    if {! $dontJumpToFront} {
		# if it's the second to last, select the first and last
		selectOneVertex [lindex $vertexList 0]
		selectOneVertex [lindex $vertexList [expr $vertexCount - 1]]
	    }
	} else {
	    # else select the next
	    selectOneVertex [lindex $vertexList [expr $index + 1]]
	}
    } else {
	# it's a line vector
	if {$dontGoto} {
	    # select the first
	    selectOneVertex [lindex $vertexList 0]
	} elseif {$index == [expr $vertexCount - 1]} {
	    if {! $dontJumpToFront} {
		# if it's the last, select the first
		selectOneVertex [lindex $vertexList 0]
	    }
	} else {
	    # else select the next
	    selectOneVertex [lindex $vertexList [expr $index + 1]]
	}
    }
}

proc gotoPreviousVertex {} {
    global editingVertices c

    if {! $editingVertices} {
	return
    }

    set selected [lindex [$c find withtag selected] 0]

    # if nothing is selected, pick one of the edited vector canvas items' first vertex
    set dontGoto 0
    if {$selected == ""} {
	global editedVectorCanvasIds
	set vectorCanvasId [lindex $editedVectorCanvasIds 0]

	global vectorVertexItems
	set selected [lindex $vectorVertexItems($vectorCanvasId) 0]
	set dontGoto 1
    }

    set vectorCanvasItem [vertexIdToVectorCanvasItem $selected]
    set vectorType [getLoadedFileVectorType [canvasItemToFilename $vectorCanvasItem]]

    unselectAllVertices
    
    # get the list of all vertices for this vertex's vector canvas item
    set vertexList [vertexIdToVertexList $selected]
    set vertexCount [llength $vertexList]

    set index [lsearch $vertexList $selected]

    if {$vectorType == "POLYGON"} {
	if {$dontGoto} {
	    # select the first and last
	    selectOneVertex [lindex $vertexList 0]
	    selectOneVertex [lindex $vertexList [expr $vertexCount - 1]]
	} elseif {$index == 0 || $index == [expr $vertexCount - 1]} {
	    # if it's the first or last, select the second to last
	    selectOneVertex [lindex $vertexList [expr $vertexCount - 2]]
	} elseif {$index == 1} {
	    # if it's the second, select the first and last
	    selectOneVertex [lindex $vertexList 0]
	    selectOneVertex [lindex $vertexList [expr $vertexCount - 1]]
	} else {
	    # else select the previous
	    selectOneVertex [lindex $vertexList [expr $index - 1]]
	}
    } else {
	# it's a line vector
	if {$dontGoto} {
	    # select the first
	    selectOneVertex [lindex $vertexList 0]
	} elseif {$index == 0} {
	    # if it's the first, select the last
	    selectOneVertex [lindex $vertexList [expr $vertexCount - 1]]
	} else {
	    # else select the previous
	    selectOneVertex [lindex $vertexList [expr $index - 1]]
	}
    }
}

proc clearVertices {} {
    global c
    global editingVertices
    global vectorVertexItems

    $c delete withtag vertex
    array unset vectorVertexItems
    stopEditingVertices
    clearVertexDotIds
}

set editedFile ""

proc addLineAt {x y} {
    unselectAll
    clearVertices

    global editedFile
    if {$editedFile == ""} {
	selectEditedFile "addLineAt $x $y"
	return
    }

    set vectorType [getLoadedFileVectorType $editedFile]

    if {$vectorType == "POLYGON"} {
	tk_messageBox -message "Only polygon vectors can be added to $editedFile"
	if {[llength [getLoadedFilenames]] > 1} {
	    selectEditedFile "addLineAt $x $y"
	}
	return
    }

    global c scalePower
    set x1 [expr $x - 40]
    set x2 [expr $x + 40]
    set y1 [expr $y + 40]
    set y2 [expr $y - 40]

    set x1 [expr round ([xToLon $x1] * pow (2, $scalePower))]
    set y1 [expr - round ([yToLat $y1] * pow (2, $scalePower))]
    set x2 [expr round ([xToLon $x2] * pow (2, $scalePower))]
    set y2 [expr - round ([yToLat $y2] * pow (2, $scalePower))]

    set item [addVector [$c create line $x1 $y1 $x2 $y2 -fill [fileToColor $editedFile] -tags vec] $editedFile]
    selectCanvasItems $item "none"
    editVertices

    global vectorVertexItems
    foreach vertex $vectorVertexItems($item) {
	selectOneVertex $vertex
    }

#      # select the vertices
#      foreach vertex [$c find withtag vertex] {
#  	$c addtag selected withtag $vertex
#  	$c itemconfigure $vertex -fill [getSelectedVertexColor]
#      }

    setupSelectionCursor
}

proc importLineVectors {} {
    unselectAll
    clearVertices

    global editedFile
    if {$editedFile == ""} {
	selectEditedFile "importLineVectors"
	return
    }

    set vectorType [getLoadedFileVectorType $editedFile]

    if {$vectorType == "POLYGON"} {
	tk_messageBox -message "Only polygon vectors can be added to $editedFile"
	if {[llength [getLoadedFilenames]] > 1} {
	    selectEditedFile "importLineVectors"
	}
	return
    }

    set path [getFileForOpen txt]

    if [string compare $path ""] {
	set file [open $path "r"]

	set data [read $file]

	close $file

	global c scalePower

	set coords {}
	foreach coord $data {
	    if {$coord == "END" || $coord == "end"} {
		if {[llength $coords] > 3} {
		    importLineVectors1 $coords
		    set coords {}
		}
	    } else {
		lappend coords $coord
	    }
	}

	if {[llength $coords] > 3} {
	    importLineVectors1 $coords
	}
    }
}

proc importPolygonVectors {} {
    unselectAll
    clearVertices

    global editedFile
    if {$editedFile == ""} {
	selectEditedFile "importPolygonVectors"
	return
    }

    set vectorType [getLoadedFileVectorType $editedFile]

    if {$vectorType != "POLYGON"} {
	tk_messageBox -message "Only line vectors can be added to $editedFile"
	if {[llength [getLoadedFilenames]] > 1} {
	    selectEditedFile "importPolygonVectors"
	}
	return
    }

    set path [getFileForOpen txt]

    if [string compare $path ""] {
	set file [open $path "r"]

	set data [read $file]

	close $file

	global c scalePower

	set coords {}
	foreach coord $data {
	    if {$coord == "END" || $coord == "end"} {
		if {[llength $coords] > 5} {
		    importPolygonVectors1 $coords
		    set coords {}
		}
	    } else {
		lappend coords $coord
	    }
	}

	if {[llength $coords] > 5} {
	    importPolyVectors1 $coords
	}
    }
}

proc importLineVectors1 {coords} {
    global c editedFile scalePower

    set points {}

    foreach {x y} $coords {
	set x [expr ($x * pow (2, $scalePower))]
	set y [expr - ($y * pow (2, $scalePower))]
	lappend points $x $y
    }

    set item [addVector [$c create line $points -fill [fileToColor $editedFile] -tags vec] $editedFile]
    selectCanvasItems $item "none"
}

proc importPolygonVectors1 {coords} {
    global c editedFile scalePower

    set points {}

    foreach {x y} $coords {
	set x [expr round ($x * pow (2, $scalePower))]
	set y [expr - round ($y * pow (2, $scalePower))]
	lappend points $x $y
    }

    set firstX [lindex $points 0]
    set firstY [lindex $points 1]
    set lastX [lindex $points [expr [llength $points] - 2]]
    set lastY [lindex $points [expr [llength $points] - 1]]


    if {$firstX != $lastX || $firstY != $lastY} {
	lappend points $firstX $firstY
    }

    global fillPolygons
    if {$fillPolygons} {
	set item [addVector [$c create polygon $points -fill [fileToColor $editedFile] -outline [fileToColor $editedFile] -tags vec] $editedFile]
    } else {
	set item [addVector [$c create line $points -fill [fileToColor $editedFile] -tags vec] $editedFile]
    }

    selectCanvasItems $item "none"
}

proc addPolygonAt {x y} {
    unselectAll
    clearVertices

    global editedFile
    if {$editedFile == ""} {
	selectEditedFile "addPolygonAt $x $y"
	return
    }

    if {[getLoadedFileVectorType $editedFile] == "ARC"} {
	tk_messageBox -message "Only line vectors can be added to $editedFile"
	if {[llength [getLoadedFilenames]] > 1} {
	    selectEditedFile "addPolygonAt $x $y"
	}
	return
    }

    global c scalePower
    set x1 [expr $x + 0]
    set x2 [expr $x + 40]
    set x3 [expr $x - 40]
    set y1 [expr $y - 40]
    set y2 [expr $y + 20]
    set y3 [expr $y + 40]

    set x1 [expr round ([xToLon $x1] * pow (2, $scalePower))]
    set y1 [expr - round ([yToLat $y1] * pow (2, $scalePower))]
    set x2 [expr round ([xToLon $x2] * pow (2, $scalePower))]
    set y2 [expr - round ([yToLat $y2] * pow (2, $scalePower))]
    set x3 [expr round ([xToLon $x3] * pow (2, $scalePower))]
    set y3 [expr - round ([yToLat $y3] * pow (2, $scalePower))]

    global fillPolygons
    if {$fillPolygons} {
	set item [addVector [$c create polygon $x1 $y1 $x2 $y2 $x3 $y3 $x1 $y1 -fill [fileToColor $editedFile] -outline [fileToColor $editedFile] -tags vec] $editedFile]
    } else {
	set item [addVector [$c create line $x1 $y1 $x2 $y2 $x3 $y3 $x1 $y1 -fill [fileToColor $editedFile] -tags vec] $editedFile]
    }
    selectCanvasItems $item "none"
    editVertices

    global vectorVertexItems
    foreach vertex $vectorVertexItems($item) {
	selectOneVertex $vertex
    }

#      # select the vertices
#      foreach vertex [$c find withtag vertex] {
#  	$c addtag selected withtag $vertex
#  	$c itemconfigure $vertex -fill [getSelectedVertexColor]
#      }

    setupSelectionCursor
}

proc selectEditedFile {cmd {notifyOnOne 0}} {
    set names [getLoadedFilenames]

    if {$names == {}} {
	tk_messageBox -message "There are no loaded files"
	return
    } elseif {[llength $names] == 1} {
	global editedFile
	set editedFile [lindex $names 0]
	eval $cmd

	if {$notifyOnOne} {
	    tk_messageBox -message "The only open file, $editedFile, will\nreceive pastes and other new vectors."
	}
    } else {
	if {$cmd == ""} {
	    doToFile "Select File For New Vector" "Accept" "set editedFile \$file"
	} else {
	    doToFile "Select File For New Vector" "Accept" "set editedFile \$file ; $cmd"
	}
    }
}
