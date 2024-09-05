proc displayVectorProperties {canvasItem {selectListItem ""}} {
    global c scalePower

    set filename [canvasItemToFilename $canvasItem]
    set vecId [canvasItemToVectorId $canvasItem]

    if {$vecId < 0} {
	set props {}
	set vecId "TMP_$canvasItem"
    } else {
	global ${filename}_idMap
	if {[catch { set sourceShapeIndex [set ${filename}_idMap($vecId)] } ] } {
	    set sourceShapeIndex $vecId
	}

	set props [getVectorProperties $filename $sourceShapeIndex]
    }

    set w .vecData_${vecId}
    catch {toplevel $w}
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Vector $vecId"
    wm iconname $w "Vector $vecId"

    set f $w.f
    catch {destroy $f}
    frame $f
    pack $f -fill both -expand true -anchor w
    
    set grid $f.grid
    frame $grid
    pack $grid -fill both -expand true
    grid columnconfigure $grid 1 -weight 1

    set row 0

    set l $grid.filenameLabel
    label $l -text "Filename"
    grid $l -row $row -column 0 -sticky w -padx 2
    set l $grid.filename
    label $l -text $filename
    grid $l -row $row -column 1 -sticky w -padx 2
    incr row

    set l $grid.vecIdLabel
    label $l -text "Vector ID"
    grid $l -row $row -column 0 -sticky w -padx 2
    set l $grid.vecId
    label $l -text $vecId
    grid $l -row $row -column 1 -sticky w -padx 2
    incr row

    global argv
    if {[lsearch $argv "expert"] >= 0} {
	foreach {name value} $props {
	    set l $grid.propRow${row}Name
	    label $l -text $name
	    grid $l -row $row -column 0 -sticky w -padx 2
	    set l $grid.propRow${row}Value
	    label $l -text $value
	    grid $l -row $row -column 1 -sticky w -padx 2
	    incr row
	}
    }

    # part list
    set parts $grid.parts
    frame $parts
    grid $parts -row $row -column 0 -sticky news
    grid rowconfigure $grid $row -weight 1
    grid configure $parts -columnspan 2 
    incr row

    set partList $parts.partList
    listbox $partList -yscroll "$parts.scroll set" -height 1 -selectmode multiple
    scrollbar $parts.scroll -command "$parts.partList yview"
    pack $partList -expand true -fill both -side left
    pack $parts.scroll -side right -fill y

    # vertex list
    set vertices $grid.vertices
    frame $vertices
    grid $vertices -row $row -column 0 -sticky news
    grid configure $vertices -columnspan 2
    grid rowconfigure $grid $row -weight 4
    incr row

    set vertexList $vertices.vertexList
    listbox $vertexList -yscroll "$vertices.scroll set" -height 4 -selectmode multiple
    scrollbar $vertices.scroll -command "$vertices.vertexList yview"
    pack $vertexList -expand true -fill both -side left
    pack $vertices.scroll -side right -fill y

    set parts [canvasItemToCanvasParts $canvasItem]
    set partIndices [canvasItemToPartIndices $canvasItem]
    set vertices [canvasItemToVertices $canvasItem]

    bind $partList <ButtonPress-1> [list clickListItem $partList %y $parts]

    set partIndicesIndex 0
    set coordIndex 0
    foreach {x y} $vertices {
	set y [expr - $y]
	if {$coordIndex == [lindex $partIndices $partIndicesIndex]} {
	    $partList insert end "Part $partIndicesIndex ([expr [llength [$c coords [lindex $parts $partIndicesIndex]]] / 2] Vertices)"
	    $vertexList insert end "*** Part $partIndicesIndex Vertices ***"
	    incr partIndicesIndex
	}

	$vertexList insert end "$y $x"
	incr coordIndex
    }

    set ok $grid.ok
    button $ok -text "OK" -command "destroy $w"
    grid $ok -row $row -column 0
    grid configure $ok -columnspan 2
    incr row

    set selected [$c find withtag selected]

    # highlight selected parts in list widget
    if {$selectListItem != ""} {
	set index 0
	foreach part $parts {
	    if {[lsearch $selected $part] >= 0} {
		$partList selection set $index $index
	    }
	    incr index
	}
    }
}

#note that clicking can unselect, as well as select
proc clickListItem {partList y parts} {
    update idletasks
    # get listbox index of clicked item
    set index [$partList nearest $y]
    # get list of selected listbox items
    set listSelection [$partList curselection]
    # parts is a list of canvasItemIds for the parts in the list
    set clickedCanvasItem [lindex $parts $index]

    set listItemIsSelected [expr [lsearch $listSelection $index] >= 0]
    global c
    set selected [$c find withtag selected]
    if {[lsearch $selected $clickedCanvasItem] < 0 && ! $listItemIsSelected} {
	$c addtag selected withtag $clickedCanvasItem
	$c itemconfigure $clickedCanvasItem -width 3
    } elseif {[lsearch $selected $clickedCanvasItem] >= 0 && $listItemIsSelected} {
	$c itemconfigure $clickedCanvasItem -width 1
	$c dtag $clickedCanvasItem selected
    }
}

proc vectorProperties {} {
    global c
    global editingVertices
    
    set selected [$c find withtag selected]

    if {$selected == {}} {
	if {$editingVertices} {
	    tk_messageBox -message "No vertices are selected."
	} else {
	    tk_messageBox -message "No vectors are selected."
	}
	return
    }

    set selectCount [llength $selected]

    if {$editingVertices} {
	displayVertexCoords $selected
    } else {
	if {$selectCount == 1 } {
	    displayVectorProperties $selected selectListItem
	} elseif {[promptUser "Display Many?" "Display properties for all $selectCount vector parts?" {Yes No}] == "Yes"} {
	    foreach item $selected {
		displayVectorProperties $item
	    }
	}
    }   
}

proc displayVertexCoords {canvasIds} {
    set w .vertexCoords
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    set title "Vertex Coords"
    wm title $w $title
    wm iconname $w $title

    set row 0

    set l $w.label$row
    label $l -text "Vertex ID"
    grid $l -row $row -column 0
    set l $w.label2$row
    label $l -text "Lat"
    grid $l -row $row -column 1
    set l $w.label3$row
    label $l -text "Lon"
    grid $l -row $row -column 2

    foreach id $canvasIds {
	global c
	set coords [$c coords $id]
	set x1 [lindex $coords 0]
	set y1 [lindex $coords 1]
	set x2 [lindex $coords 2]
	set y2 [lindex $coords 3]
	global scalePower
	set x [expr ($x1 + $x2) / 2.0 / pow (2, $scalePower)]
	set y [expr - ($y1 + $y2) / 2.0 / pow (2, $scalePower)]

	# [expr $coord / pow (2, $scalePower)]

	incr row
	set l $w.label$row
	label $l -text $id
	grid $l -row $row -column 0
	set l $w.label2$row
	label $l -text $y
	grid $l -row $row -column 1
	set l $w.label3$row
	label $l -text $x
	grid $l -row $row -column 2
    }

    incr row
    button $w.b -text "Close" -command "destroy $w"
    grid $w.b -row $row -column 0
}