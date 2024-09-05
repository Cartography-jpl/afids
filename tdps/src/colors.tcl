proc setSelectedVectorColor {i} {
    global c editingVertices

    if {$editingVertices} {
	tk_messageBox -message "Vertex color can be changed in the Vector->Color->Options dialog"
	return
    }

    set selected [$c find withtag selected]

    if {$selected == {}} {
	tk_messageBox -message "No vectors are selected."
	return
    }

#    set color [tk_chooseColor]
    global colorPaletteValues
    set color [lindex $colorPaletteValues $i]

    if {$color == ""} {
	return
    }

    global fillPolygons
    foreach item $selected {
	set file [canvasItemToFilename $item]
	set vectorType [getLoadedFileVectorType $file]
	if {$vectorType == "POLYGON" && $fillPolygons} {
	    $c itemconfigure $item -fill $color -outline $color
	} else {
	    $c itemconfigure $item -fill $color
	}
    }
}

proc setVectorColorOptions {} {
    set w .vectorColorOptions
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    set title "Vector Color Options"
    wm title $w $title
    wm iconname $w $title
    
    set l $w.palette
    label $l -text "Palette" -anchor w
    grid $l -row 0 -column 0 -sticky w

    set l $w.vertexColor
    label $l -text "Vertex Color" -anchor w
    grid $l -row 0 -column 1 -sticky w

    set pf $w.paletteFrame
    frame $pf
    grid $pf -row 1 -column 0

    set bf $w.colorButtonFrame

    global colorPaletteValues
    for {set row 0} {$row < 4} {incr row} {
	for {set col 0} {$col < 4} {incr col} {
	    set index [expr $row + $col * 4]
	    set b $pf.b${index}
	    button $b -image image_colorPalette_${index} -command "choosePaletteColor [expr $row + $col * 4] $bf"
	    grid $b -row $row -column $col
	}
    }

    frame $bf
    grid $bf -row 1 -column 1 -sticky n

    set l $bf.l1
    label $l -text "Normal"
    grid $l -row 0 -column 0 -sticky w
    set menu [makeColorOptionMenu $bf normalVertexColor]
    grid $menu -row 0 -column 1 -sticky news

    set l $bf.l2
    label $l -text "Selected"
    grid $l -row 1 -column 0 -sticky w
    set menu [makeColorOptionMenu $bf selectedVertexColor]
    grid $menu -row 1 -column 1 -sticky news

    set cb $bf.checkButton
    checkbutton $cb -text "Fill Polygons" -variable fillPolygons -command "writeDatabase"
    grid $cb -row 2 -column 0 -sticky w
    grid configure $cb -columnspan 2

    set cb $w.closeButton
    button $cb -text "Close" -command "destroy $w"
    grid $cb -row 4 -column 0
    grid configure $cb -columnspan 2
}

registerDatabaseVar fillPolygons 0 "Fill polygon vectors"
registerDatabaseVar normalVertexColor 19 "Normal vertex color table index"
registerDatabaseVar selectedVertexColor 15 "Selected vertex color table index"

proc getNormalVertexColor {} {
    global colorPaletteValues
    global normalVertexColor

    lindex $colorPaletteValues [expr $normalVertexColor - 10]
}

proc getSelectedVertexColor {} {
    global colorPaletteValues
    global selectedVertexColor

    lindex $colorPaletteValues [expr $selectedVertexColor - 10]
}

proc choosePaletteColor {index bf} {
    set color [tk_chooseColor]

    if {$color == ""} {
	return
    }

    global colorPaletteValues
    set colorPaletteValues [lreplace $colorPaletteValues $index $index $color]

    image_colorPalette_${index} put $color -to 1 1 15 15
    image_colorPalette_${index}_s put $color -to 2 2 14 14
    
    selectColorMenu2 $bf normalVertexColor
    selectColorMenu2 $bf selectedVertexColor

    writeDatabase
}
