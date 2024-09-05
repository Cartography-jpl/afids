# This file contains the mover editor dialogs
# usage: moverEditor::newMover applicationId
# usage: moverEditor::editMovers applicationId
# usage: moverEditor::deleteMover applicationId

package provide TMASRV 1.0
package require dbtcl

namespace eval moverEditor {
    namespace export newMover
    namespace export editMovers
    namespace export deleteMover
}

proc moverEditor::newMover {appId} {
    global selectedMoverName

    variable applicationId
    variable movers
    variable selectedMoverId
    variable editingMover

    set editingMover 0

    set applicationId $appId

    set movers [Data_Select tma_moverName {MOVER_ID MOVER_NAME} [list [list APPLICATION_ID $applicationId]]]

    set selectedMoverId [lindex [lindex $movers 0] 0]
    set selectedMoverName [lindex [lindex $movers 0] 1]

    set top .moverEditor
    if {[info commands $top] != ""} {
	destroy $top
    }

    toplevel $top

    set w $top

    label $w.label1 -text "Select a mover to use for default values:"
    pack $w.label1 -side top

#    label $w.movers -textvariable selectedMoverName -relief sunken -borderwidth 1
#    bind $w.movers <ButtonPress> [list moverEditor::moverMenu %X %Y $w.f.c.f]
    global currentMover
    set currentMover(code) [lindex [getMoverCodes] 0]
    set currentMover(description) [lindex [getMoverDescriptions] 0]
    label $w.movers -textvariable currentMover(description) -relief sunken -borderwidth 1
    bind $w.movers <ButtonPress> {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]; moverEditor::selectMover .moverEditor.f.c.f $currentMover(code)}

    pack $w.movers -side top -fill x -expand false

    label $w.label2 -text "Enter a name for the new mover:"
    pack $w.label2 -side top

    entry $w.entry -textvariable newMoverName
    pack $w.entry -side top

    frame $top.buttons
    pack $top.buttons -fill x -expand false -side top
    set buttonsFrame $top.buttons
    button $buttonsFrame.saveButton -text Save -command [list moverEditor::saveNewMover $top]
    pack $buttonsFrame.saveButton -fill x -expand true -side left
    button $buttonsFrame.cancelButton -text Cancel -command [list destroy $top]
    pack $buttonsFrame.cancelButton -fill x -expand true -side left
}

proc moverEditor::deleteMover {appId} {
    global selectedMoverName

    variable applicationId
    variable movers
    variable selectedMoverId
    variable editingMover

    set editingMover 0

    set applicationId $appId

    set movers [Data_Select tma_moverName {MOVER_ID MOVER_NAME} [list [list APPLICATION_ID $applicationId]]]

    set selectedMoverId [lindex [lindex $movers 0] 0]
    set selectedMoverName [lindex [lindex $movers 0] 1]

    set top .moverEditor
    if {[info commands $top] != ""} {
	destroy $top
    }

    toplevel $top

    set w $top

    label $w.label1 -text "Select a mover to delete:"
    pack $w.label1 -side top

#    label $w.movers -textvariable selectedMoverName -relief sunken -borderwidth 1
#    bind $w.movers <ButtonPress> [list moverEditor::moverMenu %X %Y $w.f.c.f]
    global currentMover
    set currentMover(code) [lindex [getMoverCodes] 0]
    set currentMover(description) [lindex [getMoverDescriptions] 0]
    label $w.movers -textvariable currentMover(description) -relief sunken -borderwidth 1
    bind $w.movers <ButtonPress> {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]; moverEditor::selectMover .moverEditor.f.c.f $currentMover(code)}

    pack $w.movers -side top -fill x -expand false

    frame $top.buttons
    pack $top.buttons -fill x -expand false -side top
    set buttonsFrame $top.buttons
    button $buttonsFrame.deleteButton -text Delete -command [list moverEditor::deleteMoverInternal $top]
    pack $buttonsFrame.deleteButton -fill x -expand true -side left
    button $buttonsFrame.cancelButton -text Cancel -command [list destroy $top]
    pack $buttonsFrame.cancelButton -fill x -expand true -side left
}

proc moverExists {id} {
    llength [Data_Select tma_moverName {MOVER_ID} [list [list MOVER_ID $id]]]
}

proc findNewId {table idCol} {
    set ids [Data_Select $table [list $idCol] {}]
    for {set id 2} {[lsearch $ids $id] >= 0} {incr id} {
    }
    return $id
}

proc applicationId {} {
    global env
    return $env(TMA_APPLICATION_ID)
}

proc copyMoverChangingName {moverId newMoverName} {
    set id [findNewId tma_moverName MOVER_ID]

    set recordSet [Data_Select tma_moverName {USER_ID} [list [list MOVER_ID $moverId] [list APPLICATION_ID [applicationId]]]]
    set record [lindex $recordSet 0]
    set values [list \
	    [list APPLICATION_ID [applicationId]] \
	    [list MOVER_ID $id] \
	    [list MOVER_NAME $newMoverName] \
	    [list USER_ID [lindex $record 0]]]
    Data_Insert tma_moverName $values

    set recordSet [Data_Select tma_mover {TERRAIN_ID VALUE} [list [list MOVER_ID $moverId] [list APPLICATION_ID [applicationId]]]]
    foreach record $recordSet {
	set values [list \
		[list APPLICATION_ID [applicationId]] \
		[list MOVER_ID $id] \
		[list TERRAIN_ID [lindex $record 0]] \
		[list VALUE [lindex $record 1]]]
	Data_Insert tma_mover $values
    }
}

proc moverEditor::saveNewMover {top} {
    global newMoverName

    variable applicationId
    variable selectedMoverId

	if {$selectedMoverId == "{}"} {
	    tk_messageBox -message "There is no mover to copy. Please revert to the factory load."
	    destroy $top
	} elseif {$newMoverName == ""} {
	    tk_messageBox -message "A mover cannot be copied without a new name."
	} else {
	    if {[moverExists $selectedMoverId]} {
		copyMoverChangingName $selectedMoverId $newMoverName
		readMoverMetaData
		tk_messageBox -message "Created new mover $newMoverName."
		destroy $top
	    } else {
		tk_messageBox -message "The copied mover seems to have been deleted. No copying was performed."
	    }
	}
}

proc moverEditor::deleteMoverInternal {top} {
    variable applicationId
    variable selectedMoverId

    # If dnets were saved (they're not yet) then dnet files of this mover (in tma_dnet) would need to be deleted. Then the following db delete:
    # Data_Delete tma_dnet [list [list MOVER_ID $selectedMoverId]]

    Data_Delete tma_moverName [list [list MOVER_ID $selectedMoverId] [list APPLICATION_ID [applicationId]]]
    Data_Delete tma_mover [list [list MOVER_ID $selectedMoverId] [list APPLICATION_ID [applicationId]]]

    readMoverMetaData

    tk_messageBox -message "Mover deleted."

    destroy $top
}

# The proc creates a dialog, loading the mover data for the specified
# application. Pressing "Save" will save only changes (if any) to the
# database, then destroy the dialog. Pressing "Cancel" will destroy
# the dialog. Calling when the dialog exists will destroy the dialog
# and recreate it. AFIDS_DATA must be defined in the environment,
# since the dbtcl package is required.
proc moverEditor::editMovers {appId} {
    global selectedMoverName

    variable applicationId
    variable movers
    variable selectedMoverId
    variable terrainIds
    variable editingMover

    set editingMover 1

    set applicationId $appId

    set terrainIds [Data_Select tma_terrain {TERRAIN_ID TERRAIN_NAME UNITS} [list [list APPLICATION_ID $applicationId]]]
    set movers [Data_Select tma_moverName {MOVER_ID MOVER_NAME} [list [list APPLICATION_ID $applicationId]]]

    set selectedMoverId [lindex [lindex $movers 0] 0]
    set selectedMoverName [lindex [lindex $movers 0] 1]

    set top .moverEditor
    if {[info commands $top] != ""} {
	destroy $top
    }

    toplevel $top

    set w $top

#    label $w.movers -textvariable selectedMoverName -relief sunken -borderwidth 1
#    bind $w.movers <ButtonPress> [list moverEditor::moverMenu %X %Y $w.f.c.f]
    global currentMover
    set currentMover(code) [lindex [getMoverCodes] 0]
    set currentMover(description) [lindex [getMoverDescriptions] 0]
    label $w.movers -textvariable currentMover(description) -relief sunken -borderwidth 1
    bind $w.movers <ButtonPress> {Gui_SelectMover .d "Select Mover" [getMoverDescriptions]; moverEditor::selectMover .moverEditor.f.c.f $currentMover(code)}
    pack $w.movers -side top -fill x -expand false

    frame $w.f
    pack $w.f -side top -fill both -expand true
    set w $w.f

    frame $w.grid
    set c $w.c
    scrollbar $w.vscroll -command "$c yview"
    canvas $c -yscrollcommand "$w.vscroll set" -width 200 -height 200
    pack $w.grid -expand yes -fill both -padx 1 -pady 1
    grid rowconfig    $w.grid 0 -weight 1 -minsize 0
    grid columnconfig $w.grid 0 -weight 1 -minsize 0
    
    grid $c -padx 1 -in $w.grid -pady 1 -row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
    grid $w.vscroll -in $w.grid -padx 1 -pady 1 -row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
    
    frame $c.f
    pack $c.f -expand yes -fill both
    $c create window 0 0 -anchor nw -window $c.f

    set f $c.f

    set row 0

    set index 0
    foreach terrainId $terrainIds {
	set units [lindex $terrainId 2]
	if {$units != ""} {
	    set units "($units)"
	}
	label $f.label$index -text "[lindex $terrainId 1] $units"
	grid $f.label$index -row $row -column 0 -sticky w -padx 5
	entry $f.value$index -width 6
	grid $f.value$index -row $row -column 1 -sticky w -padx 5
	incr row
	incr index
    }

    loadMoverTerrainValues $f [lindex [lindex $movers 0] 0]

    frame $top.buttons
    pack $top.buttons -fill x -expand false -side top
    set buttonsFrame $top.buttons
    button $buttonsFrame.saveButton -text Save -command [list moverEditor::saveMoverTerrainValues $f $top]
    pack $buttonsFrame.saveButton -fill x -expand true -side left
    button $buttonsFrame.cancelButton -text Cancel -command [list destroy $top]
    pack $buttonsFrame.cancelButton -fill x -expand true -side left

    update
    adjustScrollbar $c

    return
}

proc moverEditor::adjustScrollbar {c} {
    set bbox [grid bbox $c.f]
    eval "$c configure -scrollregion \{$bbox\}"
    $c configure -width [lindex $bbox 2]
}

proc moverEditor::moverMenu {x y f} {
    variable movers

    set menu .popup
    if {[info commands $menu] != ""} {
	destroy .popup
    }
    menu .popup -tearoff false

    foreach mover $movers {
	set id [lindex $mover 0]
	set name [lindex $mover 1]
	.popup add command -label $name -command [list moverEditor::selectMover $f $id]
    }

    tk_popup $menu $x $y
}

proc moverEditor::selectMover {f moverId} {
    global selectedMoverName

    variable movers 
    variable selectedMoverId
    variable editingMover

    set moverIndex [lsearch -glob $movers [list $moverId *]]

    set selectedMoverId $moverId
    set selectedMoverName [lindex [lindex $movers $moverIndex] 1]

    if {$editingMover} {
	loadMoverTerrainValues $f $moverId
    }
}

proc moverEditor::loadMoverTerrainValues {f moverId} {
    variable applicationId
    variable terrainIds
    variable moverTerrainValues

    set index 0
    set moverTerrainValues [Data_Select tma_mover {TERRAIN_ID VALUE} [list [list APPLICATION_ID $applicationId] [list MOVER_ID $moverId]]]
    foreach terrainId $terrainIds {
	set valueIndex [lsearch -glob $moverTerrainValues [list [lindex $terrainId 0] *]]
	$f.value$index delete 0 end
	$f.value$index insert 0 [lindex [lindex $moverTerrainValues $valueIndex] 1]

	incr index
    }
}

proc moverEditor::saveMoverTerrainValues {f top} {
    variable applicationId
    variable movers 
    variable selectedMoverId
    variable moverTerrainValues

    set index 0
    foreach terrainValue $moverTerrainValues {
	set id [lindex $terrainValue 0]
	set oldValue [lindex $terrainValue 1]

	set valueIndex [lsearch -glob $moverTerrainValues [list $id *]]
	set currentValue [$f.value$index get]

	if [string compare $oldValue $currentValue] {
	    Data_Update tma_mover [list [list VALUE $currentValue]] \
		    [list [list APPLICATION_ID $applicationId] [list MOVER_ID $selectedMoverId] [list TERRAIN_ID $id]]
	}

	incr index
    }

    destroy $top
}