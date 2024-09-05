# this is no longer called by the vector editor
# it no longer uses the correct interface to DBFCopyBreakingHoles
proc reclassifyPolygonHoles {} {
    set title "Reclassify Polygon Holes"
    set w .reclassifyPolygonHoles
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set f $w.ioSelection
    frame $f
    pack $f -expand true -fill both
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 0

    set row 0

    set l $f.label${row}
    label $l -text "Input Shapefile:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable reclassifyHolesInput
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.browse
    button $mb -text "Open ..." -command "set reclassifyHolesInput \[getFileForOpen shp\]"
    grid $mb -row $row -column 2 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Output Shapefile:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable reclassifyHolesOutput
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    set b $f.create
    button $b -text "Save As ..." -command "set reclassifyHolesOutput \[getFileForSave shp\]"
    grid $b -row $row -column 2 -sticky news
    
    incr row

    set l $f.label${row}
    label $l -text "Field To Change:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable reclassifyFieldName
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    set b $f.inputSummary
    button $b -text "View Attribute Sample ..." -command "viewAttributeSummary \$reclassifyHolesInput"
    grid $b -row $row -column 2
    
    incr row

    set l $f.label${row}
    label $l -text "New Value:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable reclassifyFieldValue
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    incr row

    set subf $f.f
    frame $subf
    grid $subf -row $row -column 0 -sticky news
    grid configure $subf -columnspan 2

    set b $subf.inputSummary
    button $b -text "Reclassify" -command "DBFCopyBreakingHoles \$reclassifyHolesInput \$reclassifyHolesOutput \$reclassifyFieldName \$reclassifyFieldValue ; destroy $w"
    pack $b -side left

    set b $subf.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left
}

proc cropVectors {} {
    set title "Crop Vectors"
    set w .reclassifyPolygonHoles
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set f $w.ioSelection
    frame $f
    pack $f -expand true -fill both
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 0

    set row 0

    set l $f.label${row}
    label $l -text "Input Shapefile:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable trimInput
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.browse
    button $mb -text "Open ..." -command "set trimInput \[getFileForOpen shp\]"
    grid $mb -row $row -column 2 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Output Shapefile:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable trimOutput
    global tdpsDatabase
    set tdpsDatabase ""
    grid $e -row $row -column 1 -sticky news

    set b $f.create
    button $b -text "Save As ..." -command "set trimOutput \[getFileForSave shp\]"
    grid $b -row $row -column 2 -sticky news
    
    incr row

    set subf $f.f
    frame $subf
    grid $subf -row $row -column 0 -sticky news
    grid configure $subf -columnspan 2

    set b $subf.inputSummary
    button $b -text "Trim To AOI" -command "trimShapes \$trimInput \$trimOutput \$aoiN \$aoiE \$aoiW \$aoiS ; destroy $w"
    pack $b -side left

    set b $subf.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left
}