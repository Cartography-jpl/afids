source "$env(AFIDS_TCL)/html_library.tcl"
source "$env(AFIDS_TCL)/api.tcl"
source "$env(AFIDS_TCL)/classes.tcl"
source "$env(AFIDS_TCL)/ngclasses.tcl"

ListUtils::import

#if {$env(OS_NAME) == "Linux"} {
    source "$env(AFIDS_TCL)/cibmos.tcl"
#}

set defaults {
    defaultCibSourceDirs /cdrom/cdrom0
    defaultDtedSourceDirs {./dtedraw /cdrom/cdrom0}
    defaultSrtmSourceDirs {}
    defaultAfidswdbSourceDirs {}
    defaultAfidswcib1dbSourceDirs {}
    defaultAfidswcib5dbSourceDirs {}
}

proc loadDefaults {} {
    global defaults
    foreach {var val} $defaults {
	global $var
	set $var $val
    }

    if {[file exists ~/.afids]} {
	source ~/.afids
    }
}

proc saveDefaults {} {
    if {[catch {set file [open ~/.afids w]}]} {
	return
    }

    global defaults
    foreach {var val} $defaults {
	global $var
	if {[llength [set $var]] > 1} {
	    puts $file "set $var [list [set $var]]"
	} else {
	    puts $file "set $var [set $var]"
	}
    }

    close $file
}

loadDefaults

image create photo helpButton -file $env(AFIDS_DATA)/gui/helpicon.gif

catch {font delete italic}
font create italic -family lucida -slant italic

proc createAOIframe {w Type {command ""}} {
    set TYPE [string toupper $Type]

    global ${TYPE}_AOI_MIN_LAT ${TYPE}_AOI_MAX_LAT ${TYPE}_AOI_MIN_LON ${TYPE}_AOI_MAX_LON

    set f $w.aoiFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 0
    grid columnconfigure $f 4 -weight 0
    grid columnconfigure $f 5 -weight 0
    grid columnconfigure $f 6 -weight 1

    set row 0

    set l $f.label${row}_n
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "Max Lat"
    } else {
	label $l -text "Top Line"
    }
    grid $l -row $row -column 2

    if {$Type != "Cib" && $Type != "Dted" && $Type != "Srtm" && $Type != "Afidswdb" && $Type != "Afidswcibdb"} {
	set b $f.viewButton
	if {$Type == "Utility"} {
	    if {$command == ""} {
		button $b -text "View Input Image" -command "viewUtilityInput"
	    } else {
		button $b -text "View Input Image" -command $command
	    }
	} else {
	    global RTYPE
	    button $b -text "View Input Image" -command "startCoregistration $Type y $RTYPE"
	}
	grid $b -row $row -column 5 -sticky news

	set l $f.label${row}_2
	label $l -text "to determine" -font italic
	grid $l -row $row -column 6 -sticky w
    }

    incr row

    set e $f.aoiN
    entry $e -textvariable ${TYPE}_AOI_MAX_LAT -width 9
    set ${TYPE}_AOI_MAX_LAT ""
    grid $e -row $row -column 2 -sticky news

    set l $f.label${row}_2
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "Enter the geographic limits" -font italic
    } else {
	label $l -text "the line/sample limits from" -font italic
    }
    grid $l -row $row -column 5 -sticky w
    grid configure $l -columnspan 2

    incr row

    if {$Type == "Hyperion" || $Type == "Ali"} {
	set state disabled
    } else {
	set state normal
    }

    set l $f.label${row}_w
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "Min Lon"
    } else {
	label $l -text "Left Sample" -state $state
    }
    grid $l -row $row -column 0 -sticky e

    set e $f.aoiW
    entry $e -textvariable ${TYPE}_AOI_MIN_LON -width 9 -state $state
    set ${TYPE}_AOI_MIN_LON ""
    grid $e -row $row -column 1 -sticky news

    set l $f.aoiLabel
    label $l -text "AOI"
    grid $l -row $row -column 2

    set e $f.aoiE
    entry $e -textvariable ${TYPE}_AOI_MAX_LON -width 9 -state $state
    set ${TYPE}_AOI_MAX_LON ""
    grid $e -row $row -column 3 -sticky news

    set l $f.label${row}_e
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "Max Lon"
    } else {
	label $l -text "Right Sample" -state $state
    }
    grid $l -row $row -column 4 -sticky w

    set l $f.label${row}_2
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "of the area of interest. Use (-)" -font italic
    } else {
	label $l -text "the input image. Leave fields" -font italic
    }
    grid $l -row $row -column 5 -sticky w
    grid configure $l -columnspan 2

    incr row

    set e $f.aoiS
    entry $e -textvariable ${TYPE}_AOI_MIN_LAT -width 9
    set ${TYPE}_AOI_MIN_LAT ""
    grid $e -row $row -column 2 -sticky news

    set l $f.label${row}
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "degrees for W Lon and S Lat." -font italic
    } else {
	label $l -text "blank for entire image." -font italic
    }
    grid $l -row $row -column 5 -sticky w
    grid configure $l -columnspan 2

    incr row

    set l $f.label${row}_s
    if {$Type == "Cib" || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	label $l -text "Min Lat"
    } else {
	label $l -text "Bottom Line"
    }
    grid $l -row $row -column 2

    if {$Type == "Srtm" || $Type == "Afidswdb"} {
	set l $f.label${row}
	label $l -text "Enter whole degree values." -font italic
	grid $l -row $row -column 5 -sticky w
	grid configure $l -columnspan 2
    }

    return $f
}

#  proc createAOIframe2 {w topLabel topVar rightLabel rightVar leftLabel leftVar bottomLabel bottomVar} {
#      global $topVar $leftVar $bottomVar $rightVar

#      set f $w.aoiFrame

#      frame $f -bd 1 -relief solid
#      grid columnconfigure $f 0 -weight 0
#      grid columnconfigure $f 1 -weight 0
#      grid columnconfigure $f 2 -weight 0
#      grid columnconfigure $f 3 -weight 0
#      grid columnconfigure $f 4 -weight 0
#      grid columnconfigure $f 5 -weight 1

#      set row 0

#      set l $f.topLabel
#      label $l -text $topLabel
#      grid $l -row $row -column 2

#      incr row

#      set e $f.aoiN
#      entry $e -textvariable $topVar
#      set $topVar ""
#      grid $e -row $row -column 2 -sticky news

#      incr row

#      set l $f.leftLabel
#      label $l -text $leftLabel
#      grid $l -row $row -column 0 -sticky e

#      set e $f.aoiW
#      entry $e -textvariable $leftVar -width 9
#      set $leftVar ""
#      grid $e -row $row -column 1 -sticky news

#      set l $f.aoiLabel
#      label $l -text "AOI"
#      grid $l -row $row -column 2

#      set e $f.aoiE
#      entry $e -textvariable $rightVar -width 9
#      set $rightVar ""
#      grid $e -row $row -column 3 -sticky news

#      set l $f.rightLabel
#      label $l -text $rightLabel
#      grid $l -row $row -column 4 -sticky w

#      incr row

#      set e $f.aoiS
#      entry $e -textvariable $bottomVar -width 9
#      set $bottomVar ""
#      grid $e -row $row -column 2 -sticky news

#      incr row

#      set l $f.bottomLabel
#      label $l -text $bottomLabel
#      grid $l -row $row -column 2

#      return $f
#  }

proc createTargetSelectionFrame {w} {
    set f $w.tsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 0
    grid columnconfigure $f 4 -weight 0
    grid columnconfigure $f 5 -weight 0
    grid columnconfigure $f 6 -weight 1

    set row 0

    set rb $f.rb${row}
    set enoc $f.enoc
    set epoi $f.epoi
    radiobutton $rb -text "Number of Chips:" -variable targetSelectionMode -value numberOfChips -anchor w -command "chooseNumberOfChips $enoc $epoi"
    grid $rb -row $row -column 0 -sticky w

    entry $enoc -textvariable numberOfChips -width 9
    global numberOfChips
    set numberOfChips "1"
    grid $enoc -row $row -column 1 -sticky news

    global targetSelectionMode
    set targetSelectionMode percentOfImage

    incr row

    set rb $f.rb${row}
    radiobutton $rb -text "Percent of Image:" -variable targetSelectionMode -value percentOfImage -anchor w -command "choosePercentOfImage $enoc $epoi"
    grid $rb -row $row -column 0 -sticky w

    set l $f.lab${row}
    label $l -text "integer value, e.g. 3 for 3%" -font italic
    grid $l -row $row -column 2 -sticky w
    
    entry $epoi -textvariable percentOfImage -width 9 -state disabled
    global percentOfImage
    set percentOfImage "3"
    grid $epoi -row $row -column 1 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Random Seed:"
    grid $l -row $row -column 0 -sticky e

    set cb $f.cb${row}
    set e $f.e${row}
    checkbutton $cb -text "Use Default" -variable useDefaultRandomSeed -anchor w -command "chooseDefaultRandomSeed $e"
    grid $cb -row $row -column 1 -sticky w

    global useDefaultRandomSeed
    set useDefaultRandomSeed 1

    entry $e -textvariable randomSeed -width 15 -state disabled
    global randomSeed
    set randomSeed ""
    grid $e -row $row -column 2 -sticky news
    grid configure $e -columnspan 2

    global targetSelectionMode
    set targetSelectionMode numberOfChips

    return $f
}

proc createTargetSelectionFrame2 {w} {
    set f $w.tsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1

    set row 0

    set subf $f.subf
    frame $subf
    grid $subf -column 0 -row $row -sticky news
    grid configure $subf -columnspan 2

    set tfe $f.tfe
    set xe $subf.xe
    set ye $subf.ye
    set browseButton $f.browseButton
    set targetLabel $f.targetLabel

    global targetXYSelectionMode
    set targetXYSelectionMode xy

    set rb $subf.rb${row}_0
    radiobutton $rb -text "X-Sample" -variable targetXYSelectionMode -value xy -anchor w -command "chooseXYLL $tfe $xe $ye $browseButton $targetLabel"
    pack $rb -side left

    entry $xe -textvariable xSample -width 9
    global xSample
    set xSample "1"
    pack $xe -side left

    set l $subf.l${row}
    label $l -text "Y-Line:"
    pack $l -side left
    
    entry $ye -textvariable yLine -width 9
    global yLine
    set yLine "1"
    pack $ye -side left

    incr row

    set rb $f.rb${row}_2
    radiobutton $rb -text "File of Targets" -variable targetXYSelectionMode -value tf -anchor w -command "chooseTargetFile $tfe $xe $ye $browseButton $targetLabel"
    grid $rb -row $row -column 0 -sticky w

    entry $tfe -textvariable targetFile -state disabled
    global targetFile
    set targetFile ""
    grid $tfe -row $row -column 1 -sticky news

    button $browseButton -text "Browse ..." -command "getFileForOpen text targetFile text" -state disabled
    grid $browseButton -row $row -column 2 -sticky news

    incr row

    label $targetLabel -text "Text File Format with 2 Columns (xy); Numbers Only" -font italic -state disabled
    grid $targetLabel -row $row -column 1 -sticky news
    grid configure $targetLabel -columnspan 2

    return $f
}

proc chooseXYLL {entry xe ye bb tl} {
    $entry config -state disabled
    $bb config -state disabled
    $tl config -state disabled
    $xe config -state normal
    $ye config -state normal
}

proc chooseTargetFile {entry xe ye bb tl} {
    $entry config -state normal
    $bb config -state normal
    $tl config -state normal
    $xe config -state disabled
    $ye config -state disabled
}

proc chooseDefaultRandomSeed {defaultEntry} {
    global useDefaultRandomSeed
    if {$useDefaultRandomSeed} {
	$defaultEntry config -state disabled
    } else {
	$defaultEntry config -state normal
	focus $defaultEntry
    }
}

proc choosePercentOfImage {numberOfChipsEntry percentOfImageEntry} {
    $numberOfChipsEntry config -state disabled
    $percentOfImageEntry config -state normal
    focus $percentOfImageEntry
}

proc chooseNumberOfChips {numberOfChipsEntry percentOfImageEntry} {
    $numberOfChipsEntry config -state normal
    focus $numberOfChipsEntry
    $percentOfImageEntry config -state disabled
}

proc createSourceDestEntries {w Type defaultSourceDirs defaultDiskImgDir} {
    set type [string tolower $Type]
    set TYPE [string toupper $Type]

    set f $w.sourceDestFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0

    set row 0

    if {$Type == "Landsat"} {
	for {set i 1} {$i < 5} {incr i} {
	    if {$i > 1} {
		incr row
	    }

	    set subf $f.subf${row}
	    frame $subf -bd 1 -relief solid
	    grid $subf -row $row -column 0 -sticky news
	    grid configure $subf -columnspan 3
	    grid columnconfigure $subf 0 -weight 0
	    grid columnconfigure $subf 1 -weight 1
	    grid columnconfigure $subf 2 -weight 0

	    set l $subf.label${row}_$i
	    label $l -text "$Type Image $i:"
	    grid $l -row $row -column 0 -sticky e

	    set e $subf.entry${row}_$i
	    entry $e -textvariable ${Type}_sourceFile_$i -width 40

	    lappend ${Type}_fileEntries [list ${Type}_sourceFile_$i [$l cget -text]]

	    global ${Type}_sourceFile_$i
	    set ${Type}_sourceFile_$i ""
	    grid $e -row $row -column 1 -sticky news

	    set mb $subf.default${row}_$i
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile_$i tif"
	    grid $mb -row $row -column 2 -sticky news

	    incr row

	    set l $subf.label${row}_$i
	    label $l -text "$Type Metadata $i:"
	    grid $l -row $row -column 0 -sticky e

	    set e $subf.entry${row}_$i
	    entry $e -textvariable ${Type}_metadataFile_$i -width 40

	    lappend ${Type}_fileEntries [list ${Type}_metadataFile_$i [$l cget -text]]

	    global ${Type}_metadataFile_$i
	    set ${Type}_metadataFile_$i ""
	    grid $e -row $row -column 1 -sticky news

	    set mb $subf.default${row}_$i
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_metadataFile_$i met"
	    grid $mb -row $row -column 2 -sticky news
	}
    } else {

	if {$Type != "Cib" && $Type != "Afidswcibdb"} {

	    set l $f.label${row}
	    label $l -text "$TYPE Source Dir:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable ${TYPE}_DISKIMG_DIR
	    global ${TYPE}_DISKIMG_DIR
	    set ${TYPE}_DISKIMG_DIR $defaultDiskImgDir
	    grid $e -row $row -column 1 -sticky news

	    set mb $f.default${row}
	    menubutton $mb -text "<- Default" -direction left -menu $mb.m -relief raised
	    menu $mb.m -tearoff 0 
	    foreach choice $defaultSourceDirs {
		$mb.m add command -label $choice -command "set ${TYPE}_DISKIMG_DIR $choice"
	    }
	    grid $mb -row $row -column 2 -sticky news
	}

	if {$Type == "Dted"} {
	    incr row

	    set l $f.label_${row}
	    label $l -text "DTED Level:"
	    grid $l -row $row -column 0 -sticky e

	    set lf $f.levelFrame
	    frame $lf
	    grid $lf -row $row -column 1 -sticky w
	    grid configure $lf -columnspan 2

	    set rb $lf.level1
	    radiobutton $rb -text "Level 1 (*.dt1 files)" -variable dtedLevel -value 1 -anchor w
	    pack $rb -side left

	    set rb $lf.level2
	    radiobutton $rb -text "Level 2 (*.dt2 files)" -variable dtedLevel -value 2 -anchor w
	    pack $rb -side left

	    incr row
	    set l $f.spacer
	    label $l -text "  "
	    grid $l -row $row -column 0 -sticky news

	    global dtedLevel
	    set dtedLevel 1
	}	

	if {$Type == "Afidswcibdb"} {
	    if {[lindex $defaultSourceDirs 0] == {}} {
		set defaultSourceDirs $defaultDiskImgDir
	    }

	    incr row

	    set l $f.label${row}
	    label $l -text "CIB1 Source Dir:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable CIB1_DISKIMG_DIR
	    global CIB1_DISKIMG_DIR
	    set CIB1_DISKIMG_DIR [lindex [lindex $defaultSourceDirs 0] 0]
	    grid $e -row $row -column 1 -sticky news

	    set mb $f.default${row}
	    menubutton $mb -text "<- Default" -direction left -menu $mb.m -relief raised
	    menu $mb.m -tearoff 0 

	    foreach choice [lindex $defaultSourceDirs 0] {
		$mb.m add command -label $choice -command "set CIB1_DISKIMG_DIR $choice"
	    }
	    grid $mb -row $row -column 2 -sticky news

	    incr row

	    set l $f.label${row}
	    label $l -text "CIB5 Source Dir:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable CIB5_DISKIMG_DIR
	    global CIB5_DISKIMG_DIR
	    set CIB5_DISKIMG_DIR [lindex [lindex $defaultSourceDirs 1] 0]
	    grid $e -row $row -column 1 -sticky news

	    set mb $f.default${row}
	    menubutton $mb -text "<- Default" -direction left -menu $mb.m -relief raised
	    menu $mb.m -tearoff 0 
	    foreach choice [lindex $defaultSourceDirs 1] {
		$mb.m add command -label $choice -command "set CIB5_DISKIMG_DIR $choice"
	    }
	    grid $mb -row $row -column 2 -sticky news
	}

	if {$Type == "Cib"} {
	    global env
	    if {true || $env(OS_NAME) == "Linux"} {
		set dirCount 4
	    } else {
		set dirCount 1
	    }
	    for {set i 0} {$i < $dirCount} {incr i} {
		incr row

		set l $f.label${row}
		label $l -text "$TYPE Source Dir:"
		grid $l -row $row -column 0 -sticky e

		set e $f.entry${row}
		if {true || $env(OS_NAME) == "Linux"} {
		    entry $e -textvariable ${TYPE}_DISKIMG_DIR_$i
		    global ${TYPE}_DISKIMG_DIR_$i
		    set ${TYPE}_DISKIMG_DIR_$i ""
		} else {
		    entry $e -textvariable ${TYPE}_DISKIMG_DIR
		    global ${TYPE}_DISKIMG_DIR
		    set ${TYPE}_DISKIMG_DIR ""
		}
		grid $e -row $row -column 1 -sticky news

		if {true || $env(OS_NAME) == "Linux"} {
		    set mb $f.default${row}_$i
		    button $mb -text "Browse ..." -command "getFileForOpen $Type ${TYPE}_DISKIMG_DIR_$i cib"
		    grid $mb -row $row -column 2 -sticky news

		} else {

		    set mb $f.default${row}
		    menubutton $mb -text "<- Default" -direction left -menu $mb.m -relief raised
		    menu $mb.m -tearoff 0 
		    foreach choice $defaultSourceDirs {
			$mb.m add command -label $choice -command "set ${TYPE}_DISKIMG_DIR $choice"
		    }
		    grid $mb -row $row -column 2 -sticky news
		}
	    }

	    if {false && $env(OS_NAME) != "Linux"} {
		incr row

		set l $f.label_${row}
		label $l -text "CIB Resolution:"
		grid $l -row $row -column 0 -sticky e

		set lf $f.levelFrame
		frame $lf
		grid $lf -row $row -column 1 -sticky w
		grid configure $lf -columnspan 2

		set rb $lf.level1
		radiobutton $rb -text "5 Meter" -variable cibRes -value "5M" -anchor w
		pack $rb -side left

		set rb $lf.level2
		radiobutton $rb -text "1 Meter" -variable cibRes -value "1M" -anchor w
		pack $rb -side left

		incr row
		set l $f.spacer
		label $l -text "  "
		grid $l -row $row -column 0 -sticky news

		global cibRes
		set cibRes "5M"
	    }
	}	

    }

    incr row

    global guiFlavor
    if {$guiFlavor == "afids"} {
	set label "Output Mosaic:"
    } else {
	set label "Output Prefix:"
    }

    set l $f.label${row}
    label $l -text $label
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${TYPE}_OUT_IMAGE_NAME
    grid $e -row $row -column 1 -sticky news
    if {$Type != "Srtm" && $Type != "Afidswdb" && $Type != "Afidswcibdb"} {
	global ${TYPE}_OUT_IMAGE_NAME
	global SESSION_ID
	global guiFlavor
	if {$guiFlavor == "afids"} {
	    set ${TYPE}_OUT_IMAGE_NAME "${SESSION_ID}_${type}.img"
	} else {
	    set ${TYPE}_OUT_IMAGE_NAME ""
	}
    }

    global guiFlavor
    if {$guiFlavor == "afids"} {
	if {$Type == "Landsat" || $Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	    set mb $f.browse${row}
	    button $mb -text "Browse ..." -command "getFileForSave $Type ${TYPE}_OUT_IMAGE_NAME _${type}.img"
	    grid $mb -row $row -column 2 -sticky news
	} else {
	    set mb $f.default${row}
	    menubutton $mb -text "<- *_${type}.img" -direction left -menu $mb.m -relief raised
	    menu $mb.m -tearoff 0 
	    foreach choice [glob -nocomplain basemos/*_${type}.img] {
		set file [lindex [split $choice /] 1]
		$mb.m add command -label $file -command "set ${TYPE}_OUT_IMAGE_NAME $file"
	    }
	    grid $mb -row $row -column 2 -sticky news
	}
    }

    return $f
}

proc createDirectories {} {
    global SESSION_ID
    global guiFlavor

    if {$guiFlavor == "afids"} {
	set dirs [list scratch dtedraw dtedmos baseraw basemos raw${SESSION_ID} final${SESSION_ID}]
    } else {
	set dirs $SESSION_ID
    }

    foreach dir $dirs {
	catch {exec mkdir $dir}
    }

    global buttonsToEnable

    foreach b $buttonsToEnable {
	$b config -state normal
    }
}

proc createDirInitFrame {w} {
    set f $w.dirInitFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "Session ID:"
    grid $l -row $row -column 0 -sticky w

    set e $f.entry${row}
    entry $e -textvariable SESSION_ID
    global SESSION_ID
    set SESSION_ID ""
    grid $e -row $row -column 1 -sticky news
    bind $e <Return> createDirectories

#      set l $f.label${row}_2
#      label $l -text "Enter one to ten characters," -font italic
#      grid $l -row $row -column 2 -sticky w

    set b $f.start
    button $b -text "Continue" -command createDirectories
    grid $b -row $row -column 2

#      set l $f.label${row}_2
#      label $l -text "then click to initialize directories." -font italic
#      grid $l -row $row -column 2 -sticky w

    return $f
}

proc createMosaicProcessingFrame {w Type {noxvd ""}} {
    set type [string tolower $Type]

    set f $w.processingFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 1

    set row 0

    if {$noxvd == "" && $Type != "Srtm" && $Type != "Afidswdb" && $Type != "Afidswcibdb"} {
	set cb $f.viewOutImage
	checkbutton $cb -text "View image when complete" -variable view${type}whenComplete
	grid $cb -row $row -column 0 -sticky w

	incr row
    }
    global view${type}whenComplete
    set view${type}whenComplete 0

    set bf $f.buttons
    frame $bf
    grid $bf -row $row -column 0 -sticky news

    set b $bf.start
    button $b -text "Start" -command "startMosaic $Type view${type}whenComplete"
    pack $b -side left

    set b $bf.stop
    button $b -text "Stop" -command "killTaetm $type"
    pack $b -side left

    incr row

    set infoF $f.infoLabels
    frame $infoF
    grid $infoF -row $row -column 0 -sticky news

    set l $infoF.statusLabel
    label $l -text "Ready" -anchor w -width 20 -relief sunken
    pack $l -fill both -side left -expand true
    global ${type}statusLabel
    set ${type}statusLabel $l

    set l $infoF.noticeLabel
    label $l -text "" -anchor w -width 20 -relief sunken
    pack $l -fill both -side left -expand true
    global ${type}noticeLabel
    set ${type}noticeLabel $l

    return $f
}

proc help {{helpDoc help/help.html} {sublink "#"}} {
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

proc createMosaicButtonsFrame {w} {
    global guiFlavor

    set f $w.mosaicButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 1

    set subf $f.subf
    frame $subf
    grid $subf -row 0 -column 0 -sticky w

    global mosaicType

    if {$guiFlavor == "afids"} {
	set rb $subf.cib
	radiobutton $rb -text "Controlled Image Base" -variable mosaicType -value cib -anchor w
	pack $rb -side top -anchor w -fill both
	set mosaicType cib

	set rb $subf.afidswcibdb
	radiobutton $rb -text "AFIDS/CIB World DB" -variable mosaicType -value afidswcibdb -anchor w
	pack $rb -side top -anchor w -fill both
    } else {
	set mosaicType dted
    }
    
    set rb $subf.dted
    radiobutton $rb -text "DTED Formatted DEM" -variable mosaicType -value dted -anchor w
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.srtm
    radiobutton $rb -text "AFIDS/SRTM World DEM DB" -variable mosaicType -value srtm -anchor w
    pack $rb -side top -anchor w -fill both
    
    if {$guiFlavor == "afids"} {
	set rb $subf.lsat
	radiobutton $rb -text "Landsat" -variable mosaicType -value landsat -anchor w
	pack $rb -side top -anchor w -fill both
	
	set rb $subf.afidswdb
	radiobutton $rb -text "AFIDS/Landsat World DB" -variable mosaicType -value afidswdb -anchor w
	pack $rb -side top -anchor w -fill both
    }
    
    set l $f.spacer
    label $l -text "  "
    grid $l -row 0 -column 1 -sticky news

    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command selectMosaickingDialog -state disabled
    grid $b -row 0 -column 2 -sticky w
    lappend buttonsToEnable $b

    return $f
}

proc createAfidsMosaicButtonsFrame {w} {
    set f $w.mosaicButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 1

    set subf $f.subf
    frame $subf
    grid $subf -row 0 -column 0 -sticky w

    global mosaicType

    set l $f.extlab
    label $l -text "External"
    grid $l -row 0 -column 1 -sticky news

    set l $f.intlab
    label $l -text "Internal World Database"
    grid $l -row 0 -column 2 -sticky news

    set l $f.dtedsrtmlab
    label $l -text "DTED/SRTM"
    grid $l -row 1 -column 0 -sticky news

    set l $f.landsatlab
    label $l -text "LANDSAT"
    grid $l -row 2 -column 0 -sticky news

    set l $f.ciblab
    label $l -text "CIB"
    grid $l -row 3 -column 0 -sticky news

    # DTED Formatted DEM
    set rb $f.dted
    radiobutton $rb -text "" -variable mosaicType -value dted -anchor w
    grid $rb -row 1 -column 1
    set mosaicType dted
    
    # AFIDS/SRTM World DEM DB
    set rb $f.srtm
    radiobutton $rb -text "" -variable mosaicType -value srtm -anchor w
    grid $rb -row 1 -column 2
    
    # Landsat
    set rb $f.lsat
    radiobutton $rb -text "" -variable mosaicType -value landsat -anchor w
    grid $rb -row 2 -column 1
    
    # AFIDS/Landsat World DB
    set rb $f.afidswdb
    radiobutton $rb -text "" -variable mosaicType -value afidswdb -anchor w
    grid $rb -row 2 -column 2
    
    # Controlled Image Base
    set rb $f.cib
    radiobutton $rb -text "" -variable mosaicType -value cib -anchor w
    grid $rb -row 3 -column 1

    # AFIDS/CIB World DB
    set rb $f.afidswcibdb
    radiobutton $rb -text "" -variable mosaicType -value afidswcibdb -anchor w
    grid $rb -row 3 -column 2
    
    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command selectMosaickingDialog -state disabled
    grid $b -row 4 -column 0
    grid configure $b -columnspan 3
    lappend buttonsToEnable $b

    set l $f.notelab
    label $l -text "* Or let AFIDS automatically create mosaics (no"
    grid $l -row 5 -column 0 -sticky w
    grid configure $l -columnspan 3

    set l $f.notelab2
    label $l -text "action required)"
    grid $l -row 6 -column 0 -sticky w
    grid configure $l -columnspan 3

    return $f
}

proc createNlcMosaicButtonsFrame {w} {
    set f $w.mosaicButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 1

    set subf $f.subf
    frame $subf
    grid $subf -row 0 -column 0 -sticky w

    set rb $subf.dted
    radiobutton $rb -text "DTED Formatted DEM" -variable mosaicType -value dted -anchor w
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.srtm
    radiobutton $rb -text "AFIDS/SRTM World DEM DB" -variable mosaicType -value srtm -anchor w
    pack $rb -side top -anchor w -fill both
    
    global mosaicType
    set mosaicType dted

    set l $f.spacer
    label $l -text "  "
    grid $l -row 0 -column 1 -sticky news

    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command selectMosaickingDialog -state disabled
    grid $b -row 0 -column 2 -sticky w
    lappend buttonsToEnable $b

    return $f
}

proc createNlcLogIngestButtonsFrame {w} {
    set f $w.logIngestButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 1

    set subf $f.subf
    frame $subf
    grid $subf -row 0 -column 0 -sticky w

    set rb $subf.ntm
    radiobutton $rb -text "NTM" -variable nlcLogType -value nlcLogNtm -anchor w
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.quickbird
    radiobutton $rb -text "Quickbird" -variable nlcLogType -value nlcLogQuickbird -anchor w -state disabled
    pack $rb -side top -anchor w -fill both

    set rb $subf.spot
    radiobutton $rb -text "SPOT" -variable nlcLogType -value nlcLogSpot -anchor w -state disabled
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.ikonos
    radiobutton $rb -text "Ikonos" -variable nlcLogType -value nlcLogIkonos -anchor w -state disabled
    pack $rb -side top -anchor w -fill both
    
    global nlcLogType
    set nlcLogType nlcLogNtm

    set l $f.spacer
    label $l -text "  "
    grid $l -row 0 -column 1 -sticky news

    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command "set utilityType \$nlcLogType ; setupUtilities" -state disabled
    grid $b -row 0 -column 2 -sticky w
    lappend buttonsToEnable $b

    return $f
}

proc createNlcChipImagesButtonsFrame {w} {
    set f $w.chipImagesButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 1

    set subf $f.subf
    frame $subf
    grid $subf -row 0 -column 0 -sticky w

    set rb $subf.entireImage
    radiobutton $rb -text "Entire Image or Large Subarea" -variable nlcChippingType -value chipEntireImage -anchor w
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.percentage
    radiobutton $rb -text "Percentage of Image; Random Locations" -variable nlcChippingType -value chipPercentage -anchor w
    pack $rb -side top -anchor w -fill both
    
    set rb $subf.selected
    radiobutton $rb -text "Selected XY Locations" -variable nlcChippingType -value chipSelected -anchor w
    pack $rb -side top -anchor w -fill both
    
    global nlcChippingType
    set nlcChippingType chipEntireImage

    set l $f.spacer
    label $l -text "  "
    grid $l -row 0 -column 1 -sticky news

    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command "setup\$nlcChippingType" -state disabled
    grid $b -row 0 -column 2 -sticky w
    lappend buttonsToEnable $b

    return $f
}

proc setupchipPercentage {} {
    set title "Chip Random"
    set w .chipRandomDialog
    catch {destroy $w}
    toplevel $w
    wm title $w $title
    wm iconname $w $title
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm geometry $w +200+200

    set l $w.labelTitle
    label $l -text "TARGET SELECTION"
    pack $l -side top -anchor w

    set targFrame [createTargetSelectionFrame $w]
    pack $targFrame -side top -fill both

    pack [createChipperParametersFrame $w] -side top -anchor w -fill both

    set l $w.utilityExecutionSpacer
    label $l -text " "
    pack $l -side top -anchor w

    set l $w.executionFrameTitle
    label $l -text "CHIPPER EXECUTION"
    pack $l -side top -anchor w

    global utilityType
    set utilityType chipPercentage
    set processingFrame [createMosaicProcessingFrame $w Utility noxvd]
    pack $processingFrame -side top -fill x

    set b $w.close
    global chipperStandalone
    if {$chipperStandalone} {
	button $b -text "Exit" -command "exit"
    } else {
	button $b -text "Close" -command "destroy $w"
    }
    pack $b -side top
}

proc setupchipSelected {} {
    set title "Chip Targets"
    set w .chipTargetsDialog
    catch {destroy $w}
    toplevel $w
    wm title $w $title
    wm iconname $w $title
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm geometry $w +200+200

    set l $w.labelTitle
    label $l -text "TARGET SELECTION"
    pack $l -side top -anchor w

    set targFrame [createTargetSelectionFrame2 $w]
    pack $targFrame -side top -fill both

    pack [createChipperParametersFrame $w] -side top -anchor w -fill both

    set l $w.utilityExecutionSpacer
    label $l -text " "
    pack $l -side top -anchor w

    set l $w.executionFrameTitle
    label $l -text "CHIPPER EXECUTION"
    pack $l -side top -anchor w

    global utilityType
    set utilityType chipSelected
    set processingFrame [createMosaicProcessingFrame $w Utility noxvd]
    pack $processingFrame -side top -fill x

    set b $w.close
    global chipperStandalone
    if {$chipperStandalone} {
	button $b -text "Exit" -command "exit"
    } else {
	button $b -text "Close" -command "destroy $w"
    }
    pack $b -side top
}

proc setupchipEntireImage {} {
    set title "Chip Entire Image"
    set w .chipEntireImageDialog
    catch {destroy $w}
    toplevel $w
    wm title $w $title
    wm iconname $w $title
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm geometry $w +200+200

    set l $w.labelTitle
    label $l -text "SUB IMAGE AREA"
    pack $l -side top -anchor w

    global VDEV_DIR SESSION_ID nlcLogNtmOutputPrefix
    set cmd "exec ${VDEV_DIR}/xvd ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img &"

    set aoiFrame [createAOIframe $w Utility $cmd]
    pack $aoiFrame -side top -anchor w

    pack [createChipperParametersFrame $w] -side top -anchor w -fill both

    set l $w.utilityExecutionSpacer
    label $l -text " "
    pack $l -side top -anchor w

    set l $w.executionFrameTitle
    label $l -text "CHIPPER EXECUTION"
    pack $l -side top -anchor w

    global utilityType
    set utilityType chipEntireImage
    set processingFrame [createMosaicProcessingFrame $w Utility noxvd]
    pack $processingFrame -side top -fill x

    set b $w.close
    global chipperStandalone
    if {$chipperStandalone} {
	button $b -text "Exit" -command "exit"
    } else {
	button $b -text "Close" -command "destroy $w"
    }
    pack $b -side top
}

proc createChipperParametersFrame {w} {
    set l $w.chipperParmSpacer
    label $l -text " "
    pack $l -side top -anchor w

    set l $w.chipperParmTitle
    label $l -text "CHIPPER PARAMETERS"
    pack $l -side top -anchor w

    set f $w.chipperParmFrame
    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 0
    grid columnconfigure $f 3 -weight 0
    grid columnconfigure $f 4 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "Output Lines:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable chipperOutputLines -width 20
    global chipperOutputLines
    set chipperOutputLines "1024"
    grid $e -row $row -column 1 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Output Samples:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable chipperOutputSamples -width 20
    global chipperOutputSamples
    set chipperOutputSamples "1024"
    grid $e -row $row -column 1 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Overlap:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable chipperOverlap -width 20
    global chipperOverlap
    set chipperOverlap "128"
    grid $e -row $row -column 1 -sticky news

    return $f
}

set buttonsToEnable {}

proc selectMosaickingDialog {} {
    global mosaicType

    switch $mosaicType {
	cib {
	    global defaultCibSourceDirs
	    createMosaicDialog Cib $defaultCibSourceDirs [lindex $defaultCibSourceDirs 0]
	}
	dted {
	    global defaultDtedSourceDirs
	    createMosaicDialog Dted $defaultDtedSourceDirs [lindex $defaultDtedSourceDirs 0]
	}
	landsat {
	    createMosaicDialog Landsat {./ ./lsatraw} ./lsatraw
	}
	srtm {
	    global defaultSrtmSourceDirs
	    set defaultDir [lindex $defaultSrtmSourceDirs 0]
	    global env
	    if {$env(ELEV_ROOT) != ""} {
		set defaultDir $env(ELEV_ROOT)
	    }
	    createMosaicDialog Srtm $defaultSrtmSourceDirs $defaultDir
	}
	afidswdb {
	    global defaultAfidswdbSourceDirs
	    set defaultDir [lindex $defaultAfidswdbSourceDirs 0]
	    global env
	    if {$env(LANDSAT_ROOT) != ""} {
		set defaultDir $env(LANDSAT_ROOT)
	    }
	    createMosaicDialog Afidswdb $defaultAfidswdbSourceDirs $defaultDir
	}
	afidswcibdb {
	    global defaultAfidswcib1dbSourceDirs defaultAfidswcib5dbSourceDirs

	    set defaultCib1Dir [lindex $defaultAfidswcib1dbSourceDirs 0]
	    set defaultCib5Dir [lindex $defaultAfidswcib5dbSourceDirs 0]

	    global env
	    if {$env(CIB1_ROOT) != ""} {
		set defaultCib1Dir $env(CIB1_ROOT)
	    }
	    if {$env(CIB5_ROOT) != ""} {
		set defaultCib5Dir $env(CIB5_ROOT)
	    }
	    createMosaicDialog Afidswcibdb [list $defaultAfidswcib1dbSourceDirs $defaultAfidswcib5dbSourceDirs] [list $defaultCib1Dir $defaultCib5Dir]
	}
    }
}

proc createCoregButtonsFrame {w} {
    set f $w.coregButtonsFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0
#    grid columnconfigure $f 3 -weight 1

    set row 0

    set l $f.label_${row}_0
    label $l -text "Sensor Type:"
    grid $l -row $row -column 0 -sticky w

    set l $f.spacer
    label $l -text "  "
    grid $l -row $row -column 1 -sticky news

    set l $f.label_${row}_1
    label $l -text "Coregistration Type:"
    grid $l -row $row -column 2 -sticky w

    incr row

    set subf $f.frame_${row}_0
    frame $subf -bd 1 -relief solid
    grid $subf -row ${row} -column 0 -sticky nw
    grid configure $subf -rowspan 2

    foreach sensor {ALI Aster Hyperion Ikonos Landsat Modis NTM Quickbird SPOT} {
	set lower [string tolower $sensor]
	set rb $subf.${lower}
	if {$sensor == "NTM" || $sensor == "SPOT"} {
	    radiobutton $rb -text $sensor -variable CoregSensor -value $sensor -anchor w -command "\$addBandRadioButton config -state disabled ; if {\$RTYPE == \"additionalband\"} {set RTYPE master}"
	} elseif {$sensor == "Quickbird"} {
	    radiobutton $rb -text "Quickbird/WV2" -variable CoregSensor -value $sensor -anchor w -command "\$addBandRadioButton config -state normal"
	} else {
	    radiobutton $rb -text $sensor -variable CoregSensor -value $sensor -anchor w -command "\$addBandRadioButton config -state normal"
	}
	pack $rb -side top -anchor w -fill both
    }

    global CoregSensor
    set CoregSensor ALI

    set subf $f.frame_${row}_1
    frame $subf -bd 1 -relief solid
    grid $subf -row ${row} -column 2 -sticky nw

    set subrow 0
    foreach {step type link} {"STEP 3:" Master "step3" "STEP 4:" Secondary "step4" "STEP 5:" "Additional Band" "step5" "STEP 3M:" "Multi-File" "step3m"} {
	set lower [join [string tolower $type] ""]
	set lbf $subf.${lower}_frame
	frame $lbf
	pack $lbf -side top -anchor w -expand true -fill both
	grid columnconfigure $lbf 2 -weight 1

	set l $lbf.label_${lower}
	label $l -text $step -anchor w
	grid $l -row $subrow -column 0 -sticky w

	set rb $lbf.${lower}
	radiobutton $rb -text $type -variable RTYPE -value $lower -anchor w
	grid $rb -row $subrow -column 1 -sticky w

	set hb $lbf.helpButton
	global env
	button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help.html #${link}"
	grid $hb -row $subrow -column 2 -sticky e

	incr subrow
    }	

    global addBandRadioButton
    set addBandRadioButton $subf.additionalband_frame.additionalband

    global RTYPE
    set RTYPE master

    incr row

    global buttonsToEnable

    set b $f.setupButton
    button $b -text "Setup ..." -command selectCoregistrationDialog -state disabled
    grid $b -row ${row} -column 2
    lappend buttonsToEnable $b

    return $f
}

proc selectCoregistrationDialog {} {
    global CoregSensor
    global RTYPE

    switch $RTYPE {
	master - secondary {
	    createMasterSecondaryCoregDialog $CoregSensor
	}
	additionalband {
	    createAddBandDialog $CoregSensor
	}
	multi-file {
	    createMultiFileDialog $CoregSensor
	}
    }
}

proc createMultiFileDialog {Type} {
    set type [string tolower $Type]

    set w .${type}MultiCoregDialog
    catch {destroy $w}
    toplevel $w

    wm title $w "Multi File Coreg"

    switch $Type {
	Quickbird {
	    frame $w.inframe
	    pack $w.inframe -side top -expand no -fill x

#	    label $w.inframe.l -text "Inp Dir"
#	    pack $w.inframe.l -side left -fill x

#	    entry $w.inframe.e -textvariable multifiledirvar
#	    pack $w.inframe.e -side left -expand yes -fill x
#	    global multifiledirvar
#	    set multifiledirvar "/raid21/wlb/wv2"

	    frame $w.inframe.econtainer
	    pack $w.inframe.econtainer -side left -fill x -expand true -anchor w

	    global inpDirEntry
	    set inpDirEntry [Entry \#auto $w.inframe.econtainer \
				 -label "Input Dir:" \
				 -browse "SelectDir" \
				 -initialDir "/raid21/wlb/wv2"]
#	    $inpDirEntry setValue "/raid21/wlb/wv2"

	    $inpDirEntry grid -row 0 -column 0 -sticky news
	    grid columnconfigure $w.inframe.econtainer 0 -weight 0
	    grid columnconfigure $w.inframe.econtainer 1 -weight 1

#	    frame $w.filtframe
#	    pack $w.filtframe -side top -expand no -fill x

#	    label $w.filtframe.l -text "Input Filter:"
#	    pack $w.filtframe.l -side left
	    label $w.inframe.econtainer.l -text "Input Filter:"
	    grid $w.inframe.econtainer.l -row 1 -column 0 -sticky news

#	    entry $w.filtframe.e -textvariable multifiltervar
#	    pack $w.filtframe.e -side left -expand yes -fill x
	    entry $w.inframe.econtainer.e -textvariable multifiltervar
	    grid $w.inframe.econtainer.e -row 1 -column 1 -sticky news
	    global multifiltervar
	    set multifiltervar "*.NTF"

#	    button $w.filtframe.b -text "Filter" -command "multifilter $w"
#	    pack $w.filtframe.b -side left
	    button $w.inframe.econtainer.b -text "Filter" -command "multifilter $w"
	    grid $w.inframe.econtainer.b -row 1 -column 2 -sticky news

	    frame $w.frames
	    pack $w.frames -side top -expand yes -fill both

	    frame $w.frames.leftframe
	    pack $w.frames.leftframe -side left -expand yes -fill both

	    button $w.frames.leftframe.b -text "Add ->" -command "multiFileAdd .${type}MultiCoregDialog"
	    pack $w.frames.leftframe.b -side top -fill x

	    frame $w.frames.leftframe.leftlist
	    pack $w.frames.leftframe.leftlist -side top -expand yes -fill both

	    scrollbar $w.frames.leftframe.leftlist.scroll -command "$w.frames.leftframe.leftlist.list yview"
	    listbox $w.frames.leftframe.leftlist.list -yscroll "$w.frames.leftframe.leftlist.scroll set" -setgrid 1 -height 12 -selectmode extended
	    pack $w.frames.leftframe.leftlist.scroll -side right -fill y
	    pack $w.frames.leftframe.leftlist.list -side left -expand 1 -fill both

	    button $w.frames.leftframe.preview -text "Preview" -command "previewcandidate $w"
	    pack $w.frames.leftframe.preview -side top -fill x

	    frame $w.frames.rightframe
	    pack $w.frames.rightframe -side left -expand yes -fill both

	    button $w.frames.rightframe.remove -text "<- Remove" -command "multiFileRemove .${type}MultiCoregDialog"
	    pack $w.frames.rightframe.remove -side top -fill x

	    frame $w.frames.rightframe.rightlist
	    pack $w.frames.rightframe.rightlist -side top -expand yes -fill both

	    scrollbar $w.frames.rightframe.rightlist.scroll -command "$w.frames.rightframe.rightlist.list yview"
	    listbox $w.frames.rightframe.rightlist.list -yscroll "$w.frames.rightframe.rightlist.scroll set" -setgrid 1 -height 12 -selectmode extended
	    pack $w.frames.rightframe.rightlist.scroll -side right -fill y
	    pack $w.frames.rightframe.rightlist.list -side right -expand 1 -fill both

	    button $w.frames.rightframe.preview -text "Preview" -command "previewcandidate $w right"
	    pack $w.frames.rightframe.preview -side top -fill x

	    button $w.frames.rightframe.master -text "Use as Master" -command "useasmaster $w"
	    pack $w.frames.rightframe.master -side top -fill x

	    frame $w.masterframe
	    pack $w.masterframe -side top -fill x

	    label $w.masterframe.l -text "Master:"
	    pack $w.masterframe.l -side left -fill x

	    label $w.masterframe.l2 -text "" -textvariable multimastername
	    pack $w.masterframe.l2 -side left -fill x
	    global multimastername
	    set multimastername ""

	    frame $w.outframe
	    pack $w.outframe -side top -fill both

	    frame $w.outframe.econtainer
	    pack $w.outframe.econtainer -side left -fill x -expand true -anchor w

	    global outpathEntry
	    set outpathEntry [Entry \#auto $w.outframe.econtainer \
				  -label "Script Out Path:" -initialDir [pwd] \
				  -browse "Save -filetypes [FileUtils::fileTypes pdf]"]

	    $outpathEntry grid -row 0 -column 0 -sticky news
	    grid columnconfigure $w.outframe.econtainer 0 -weight 0
	    grid columnconfigure $w.outframe.econtainer 1 -weight 1

	    button $w.outframe.b -text "Generate" -command "generatemultifile $w"
	    pack $w.outframe.b -side left

	    button $w.outframe.bc -text "Close" -command "destroy $w"
	    pack $w.outframe.bc -side left

	}
	default {
	    catch {destroy $w}
	    puts "$Type not supported for multi file coregistration"
	    return
	}
    }
}

proc fileType {path} {
    # For NITF, could be one of
    # IKONOS_nn
    # QBnn
    # WVnn
    set mission [exec gdalinfo $path | grep "NITF_STDIDC_MISSION"]
    set mission [split $mission "="]
    set mission [lindex $mission 1]
    return $mission
}

proc generatemultifile {w} {
    global multimastername
    global outpathEntry
    global multiFileDirs
    global CoregSensor
    
    if {$multimastername == ""} {
	tk_messageBox -message "Select a master image"
	return
    }

    set masterPath "$multiFileDirs($multimastername)/$multimastername"

    if {[string range [fileType $masterPath] 0 1] != "WV"} {
	tk_messageBox -message "Only World View 2 supported for multi coregistration"
	return
    }

    set fullSensorName "World View 2"

    if {[string first "P1BS" $multimastername] <= 0} {
	tk_messageBox -message "Select a PAN master WV2 image"
	return
    }

    set list [$w.frames.rightframe.rightlist.list get 0 [$w.frames.rightframe.rightlist.list size]]

    set outPath [$outpathEntry getValue]
    set outRoot [split $outPath "/"]
    set outRoot [lrange $outRoot 0 [expr [llength $outRoot] - 2]]
    set outRoot [join $outRoot "/"]

    global env
    set file [open "$env(AFIDS_DATA)/api/wvMultiMasterSecondaryTemplate.pdf" r]
    set masterTemplate [read $file]
    close $file
    set file [open "$env(AFIDS_DATA)/api/wvMultiAddBandTemplate.pdf" r]
    set addbandTemplate [read $file]
    close $file
    
    set file [open $outPath "w"]
    printProcPreamble $file

    puts $file "\n! Master $fullSensorName coregistration"
    set rawimg $masterPath
    set outimg "${outRoot}/[swapSuffix $multimastername NTF img]"
    set refimg ""
    set mpix 0.5
    set nah 950
    set nav 950
    set rtype master
    puts $file [subst $masterTemplate]

    puts $file "\n! Make a 2 meter version of the PAN master for bands 1-8 of the master and any secondaries"
    puts $file "gtsize ${outRoot}/[swapSuffix $multimastername NTF img] +\n    ${outRoot}/2meter_master.img azoom=.25"

    set nah 0
    set nav 0

    foreach src $list {
	if {$src != $multimastername && [string first "P1BS" $src] >= 0} {
	    puts $file "\n! Secondary $fullSensorName coregistration"
	    set rawimg $multiFileDirs($src)/$src
	    set outimg "${outRoot}/[swapSuffix $src NTF img]"
	    set refimg "refimg=${outRoot}/[swapSuffix $multimastername NTF img]"
	    set mpix 0.5
	    set rtype secondary
	    puts $file [subst $masterTemplate]
	} elseif {$src != $multimastername && [string first "M1BS" $src] >= 0} {
	    puts $file "\n! Secondary $fullSensorName coregistration"
	    set rawimg $multiFileDirs($src)/$src
	    set outimg "${outRoot}/[swapSuffix2 $src NTF _b1 img]"
	    set refimg "refimg=${outRoot}/2meter_master.img"
	    set mpix 2.0
	    set rtype secondary
	    puts $file [subst $masterTemplate]

	    for {set band 2} {$band < 9} {incr band} {
		puts $file "\n! Additional band $fullSensorName coregistration"
		set outimg "${outRoot}/[swapSuffix2 $src NTF _b${band} img]"
		set fnamein "${outRoot}/[swapSuffix2 $src NTF _b1 img]"
		puts $file [subst $addbandTemplate]
	    }
	}
    }

    puts $file "\nend-proc"
    close $file
}

proc swapSuffix {path old new} {
    set split [split $path "."]
    set length [llength $split]
    set last [lindex $split [expr $length - 1]]
    if {[lsearch $split $old] >= 0} {
	set split [lrange $split 0 [expr $length - 2]]
    }
    lappend split $new
    join $split "."
}

proc swapSuffix2 {path old suff new} {
    set split [split $path "."]
    set length [llength $split]
    set last [lindex $split [expr $length - 1]]
    if {[lsearch $split $old] >= 0} {
	set split [lrange $split 0 [expr $length - 2]]
    }
    set join "[join $split "."]${suff}.${new}"

    return $join
}

proc printProcPreamble {stream} {
    puts $stream "procedure"
    puts $stream "body"
    puts $stream "setlib-add library=(\$R2LIB)"
    puts $stream "local afidsroot type=(string,128)"
    puts $stream "translog AFIDS_ROOT afidsroot"
    puts $stream "setlib-delete library=(\$R2LIB)"
    puts $stream "setlib-add library=(&afidsroot/vdev,\$R2LIB)"
}

proc useasmaster {w} {
    global multimastername

    set sel [$w.frames.rightframe.rightlist.list curselection]

    if {$sel == {}} {
	tk_messageBox -message "Select a file to use as a master"
	return
    }
    if {[llength $sel] > 1} {
	tk_messageBox -message "Using first file as master"
	set sel [lindex $sel 0]
    }

    set multimastername [$w.frames.rightframe.rightlist.list get $sel]
}

proc previewcandidate {w {side left}} {
    if {$side == "left"} {
	set sel [$w.frames.leftframe.leftlist.list curselection]
    } else {
	set sel [$w.frames.rightframe.rightlist.list curselection]
    }
    if {$sel == {}} {
	tk_messageBox -message "Select a file to preview"
	return
    }
    if {[llength $sel] > 1} {
	tk_messageBox -message "Using first file in selection"
	set sel [lindex $sel 0]
    }

    if {$side == "left"} {
	global inpDirEntry
	exec ctv2 [$inpDirEntry getValue]/[$w.frames.leftframe.leftlist.list get $sel] &
    } else {
	set file [$w.frames.rightframe.rightlist.list get $sel]
	global multiFileDirs
	exec ctv2 $multiFileDirs($file)/$file &
    }
}

proc multifilter {w} {
    global inpDirEntry
    global multifiltervar

    $w.frames.leftframe.leftlist.list delete 0 [$w.frames.leftframe.leftlist.list size]

    foreach file [glob -path [$inpDirEntry getValue]/ -tails -nocomplain $multifiltervar] {
	$w.frames.leftframe.leftlist.list insert end $file
    }
}

proc multiFileRemove {w} {
    global multiFileDirs
    global multimastername

    set names {}
    foreach i [${w}.frames.rightframe.rightlist.list curselection] {
	set name [${w}.frames.rightframe.rightlist.list get $i]
	lappend names $name
	array unset multiFileDirs $name

	if {$name == $multimastername} {
	    set multimastername ""
	}
    }

    foreach name $names {
	for { set i 0} {$i < [${w}.frames.rightframe.rightlist.list size]} {incr i} {
	    if {$name == [${w}.frames.rightframe.rightlist.list get $i]} {
		${w}.frames.rightframe.rightlist.list delete $i
	    }
	}
    }
}

proc multiFileAdd {w} {
    global multiFileDirs
    global inpDirEntry

    set rightItems [${w}.frames.rightframe.rightlist.list get 0 [${w}.frames.rightframe.rightlist.list size]]

    foreach i [${w}.frames.leftframe.leftlist.list curselection] {
	set item [${w}.frames.leftframe.leftlist.list get $i]

	if {[lsearch $rightItems $item] < 0} {
	    lappend rightItems $item
	    set rightItems [lsort $rightItems]

	    array set multiFileDirs [list $item [$inpDirEntry getValue]]
	}
    }

    ${w}.frames.rightframe.rightlist.list delete 0 [${w}.frames.rightframe.rightlist.list size]

    eval "${w}.frames.rightframe.rightlist.list insert 0 $rightItems"
}

proc createMasterSecondaryCoregDialog {Type} {
    set type [string tolower $Type]

    set w .${type}CIBCoregDialog
    catch {destroy $w}
    toplevel $w

    global RTYPE
    switch $RTYPE {
	master {set mOrS "Master Image"}
	secondary {set mOrS "Secondary Image"}
    }

    switch $Type {
	Hyperion - ALI {
	    set baseType "/Landsat"
	}
	Ikonos - Quickbird - NTM - SPOT {
	    set baseType "/CIB"
	}
	default {
	    set baseType ""
	}
    }

    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "${Type}${baseType} $mOrS Coregistration"
    wm iconname $w "${Type}${baseType} $mOrS Coregistration"

    set f $w.frame
    frame $f
    pack $f -expand true -fill both
    grid columnconfigure $f 0 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "SOURCE/OUTPUT SELECTION"
    grid $l -row $row -column 0 -sticky w

    incr row

    set sourceFrame [createCoregSourceFrame $f $Type]
    grid $sourceFrame -row $row -column 0 -sticky news

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "AREA OF INTEREST SELECTION"
    grid $l -row $row -column 0 -sticky w

    incr row

    set aoiFrame [createAOIframe $f $Type]
    grid $aoiFrame -row $row -column 0 -sticky news

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "COREGISTRATION PARAMETERS"
    grid $l -row $row -column 0 -sticky w

    incr row

    set coregParmFrame [createCoregParmFrame $f $Type $RTYPE]
    grid $coregParmFrame -row $row -column 0 -sticky news

    if {$RTYPE == "secondary" && ($Type == "NTM" || $Type == "Quickbird" || $Type == "SPOT")} {
	incr row

	set l $f.label${row}
	label $l -text " "
	grid $l -row $row -column 0 -sticky w

	incr row

	set l $f.label${row}
	if {$Type == "Quickbird"} {
	    label $l -text "BATTLE FIELD ENTITY SELECTION (ONLY FOR NITF FORMAT)"
	} else {
	    label $l -text "BATTLE FIELD ENTITY SELECTION"
	}
	grid $l -row $row -column 0 -sticky w

	incr row

	set beParmFrame [createBeParmFrame $f]
	grid $beParmFrame -row $row -column 0 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "COREGISTRATION PROCESSING"
    grid $l -row $row -column 0 -sticky w

    incr row

    set processingFrame [createCoregistrationProcessingFrame $f $Type]
    grid $processingFrame -row $row -column 0 -sticky news

    incr row

    set b $f.close
    button $b -text "Close" -command "destroy $w"
    grid $b -row $row -column 0 -pady 10 -sticky e
}

proc createAddBandDialog {Type} {
    set type [string tolower $Type]

    set w .${type}AddBandCoregDialog
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "$Type Additional Band Coregistration"
    wm iconname $w "$Type Additional Band Coregistration"

    set f $w.frame
    frame $f
    pack $f -expand true -fill both
    grid columnconfigure $f 0 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "SOURCE/OUTPUT SELECTION"
    grid $l -row $row -column 0 -sticky w

    incr row

    set sourceFrame [createCoreg2SourceFrame $f $Type]
    grid $sourceFrame -row $row -column 0 -sticky news

    if {$Type == "Hyperion" || $Type == "ALI" || $Type == "Aster" || $Type == "Modis" || $Type == "Quickbird" || $Type == "SPOT"} {
	incr row

	set l $f.label${row}
	label $l -text " "
	grid $l -row $row -column 0 -sticky w

	incr row

	set l $f.label${row}
	label $l -text "COREGISTRATION PARAMETERS"
	grid $l -row $row -column 0 -sticky w

	incr row

	set coregParmFrame [createAddBandCoregParmFrame $f $Type]
	grid $coregParmFrame -row $row -column 0 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "COREGISTRATION PROCESSING"
    grid $l -row $row -column 0 -sticky w

    incr row

    set processingFrame [createCoregistrationProcessingFrame $f ${Type}2]
    grid $processingFrame -row $row -column 0 -sticky news

    incr row

    set b $f.close
    button $b -text "Close" -command "destroy $w"
    grid $b -row $row -column 0 -pady 10 -sticky e
}

proc createAddBandCoregParmFrame {w Type} {
    set f $w.addBandCoregParmFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 1

    set row 0

    puts "Type $Type"

    switch $Type {
	"Hyperion" - "ALI" - "Quickbird" - "SPOT" {
	    set l $f.label${row}
	    label $l -text "First Band:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}_2
	    entry $e -textvariable ${Type}_band -width 5
	    global ${Type}_band
	    if {$Type == "ALI"} {
		set ${Type}_band 1
	    } elseif {$Type == "Quickbird"} {
		set ${Type}_band 1
	    } elseif {$Type == "SPOT"} {
		set ${Type}_band 2
	    } else {
		set ${Type}_band 15
	    }
	    grid $e -row $row -column 1 -sticky w

	    if {$Type == "ALI"} {
		set l $f.label${row}_right
		label $l -text "(Band 1 is panchromatic)" -anchor w
		grid $l -row $row -column 2 -sticky w
	    }

	    incr row

	    set l $f.label${row}_2
	    label $l -text "Number Of Bands:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}_3
	    entry $e -textvariable ${Type}_nband -width 5
	    global ${Type}_nband
	    if {$Type == "ALI"} {
		set ${Type}_nband 10
	    } elseif {$Type == "Quickbird"} {
		set ${Type}_nband 4
	    } elseif {$Type == "SPOT"} {
		set ${Type}_nband 3
	    } else {
		set ${Type}_nband 2
	    }
	    grid $e -row $row -column 1 -sticky w
	}
	"Modis" {
	    set l $f.label${row}
	    label $l -text "Band:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}_2
	    entry $e -textvariable ${Type}_band -width 5
	    global ${Type}_band
	    set ${Type}_band 15
	    grid $e -row $row -column 1 -sticky w

	    incr row

	    set dnf $f.dnf
	    frame $dnf
	    grid $dnf -row $row -column 0 -sticky w
	    grid configure $dnf -columnspan 5

	    set l $dnf.label${row}
	    label $l -text "Acquisition Time:"
	    pack $l -side left

	    set rb $dnf.master
	    radiobutton $rb -text "Daytime" -variable ${Type}2_time -relief flat -value day
	    pack $rb -side left

	    set rb $dnf.secondary
	    radiobutton $rb -text "Nighttime" -variable ${Type}2_time -relief flat -value night
	    pack $rb -side left

	    global ${Type}2_time
	    set ${Type}2_time day
	}
	"Aster" {
	    set l $f.label${row}
	    label $l -text "Sensor:"
	    grid $l -row $row -column 0 -sticky e

	    set sm $f.sensorMenu
	    tk_optionMenu $sm Aster_sensor VNIR SWIR TIR SE-05 SKT-08 SRTIR-09
	    grid $sm -row $row -column 1 -sticky w

	    incr row

	    set l $f.label${row}
	    label $l -text "Band:"
	    grid $l -row $row -column 0 -sticky e

	    set bm $f.bandMenu
	    # the band menu is built in updateAsterBandMenu, which is called when the menu value changes

	    bind $sm <Configure> "updateAsterBandMenu $bm $row"

	    global Aster_sensor
	    set Aster_sensor VNIR
	}
    }

    return $f
}

proc updateAsterBandMenu {bm row} {
    catch {destroy $bm}
    
    global Aster_sensor Aster_band

    switch $Aster_sensor {
	VNIR {
	    # 2 removed, since it's used for the primary coreg
	    # 3B is not an option, since lookback is hard-wired to 0 in astwarpad.pdf
	    tk_optionMenu $bm Aster_band 1 3N
	    set Aster_band 1
	}
	SWIR {
	    tk_optionMenu $bm Aster_band 4 5 6 7 8 9
	    set Aster_band 4
	}
	TIR - SE-05 - SRTIR-09 {
	    tk_optionMenu $bm Aster_band 10 11 12 13 14
	    set Aster_band 10
	}
	SKT-08 {
	    label $bm -text "none"
	}
    }

    grid $bm -row $row -column 1 -sticky w
}

proc createCoregistrationProcessingFrame {w Type} {
    set type [string tolower $Type]

    set f $w.processingFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 1

    set row 0

    set bf $f.buttons
    frame $bf
    grid $bf -row $row -column 0 -sticky news

    global RTYPE
    set b $bf.start
    button $b -text "Start" -command "startCoregistration $Type n $RTYPE"
    pack $b -side left

    set b $bf.stop
    button $b -text "Stop" -command "killTaetm $type"
    pack $b -side left

    incr row

    set infoF $f.infoLabels
    frame $infoF
    grid $infoF -row $row -column 0 -sticky news

    set l $infoF.statusLabel
    label $l -text "Ready" -anchor w -width 20 -relief sunken
    pack $l -fill both -side left -expand true
    global ${type}statusLabel
    set ${type}statusLabel $l

    set l $infoF.noticeLabel
    label $l -text "" -anchor w -width 20 -relief sunken
    pack $l -fill both -side left -expand true
    global ${type}noticeLabel
    set ${type}noticeLabel $l

    # review
    
    incr row

    set f3 $f.reviewFrame
    frame $f3
    grid $f3 -row $row -column 0 -sticky news
    grid columnconfigure $f3 0 -weight 0
    grid columnconfigure $f3 0 -weight 1

    set row 0

    set f3b $f3.reviewFrameButtons
    frame $f3b
    grid $f3b -row $row -column 0 -sticky w

    set l $f3b.label${row}
    label $l -text "Registration Review:"
    pack $l -side left -anchor w -fill both

    set rb $f3b.output
    radiobutton $rb -text "Output" -variable ${Type}_revtype -value output -anchor w
    pack $rb -side left -anchor w -fill both
    
    global RTYPE

    if {! ($Type == "Modis" || $RTYPE == "additionalband")} {
	set rb $f3b.base
	radiobutton $rb -text "Base Overlay" -variable ${Type}_revtype -value base -anchor w
	pack $rb -side left -anchor w -fill both
    }
    
    if {$RTYPE == "secondary" || $RTYPE == "additionalband"} {
	set rb $f3b.secondary
	radiobutton $rb -text "Master Overlay" -variable ${Type}_revtype -value master -anchor w
	pack $rb -side left -anchor w -fill both
    }
    
    if {$RTYPE != "additionalband"} {
	set rb $f3b.accuracy
	radiobutton $rb -text "Accuracy" -variable ${Type}_revtype -value accuracy -anchor w
	pack $rb -side left -anchor w -fill both
	
	set rb $f3b.accuracyOverlay
	radiobutton $rb -text "Accuracy Overlay" -variable ${Type}_revtype -value accuracyOverlay -anchor w
	pack $rb -side left -anchor w -fill both
    }
    
    global ${Type}_revtype
    set ${Type}_revtype output

    set b $f3b.startReview
    button $b -text "Review" -command "reviewCoregistration $Type"
    pack $b -side left -anchor w -fill both

    # this button path is captured here so that it can be set normal if/when the coreg completes
    global reviewButton${Type}
    set reviewButton${Type} $f3b.startReview

    return $f
}

proc reviewCoregistration {Type} {
    set TYPE [string toupper $Type]
    set type [string tolower $Type]
    
    global RTYPE
    global ${Type}_outimg
    set file1 [set [set Type]_outimg]

    if {$Type == "Hyperion2" && $RTYPE == "additionalband"} {
	global Hyperion_band
	set file1 "${file1}_${Hyperion_band}"
    }

    if {$Type == "ALI2" && $RTYPE == "additionalband"} {
	global ALI_band
	set file1 "${file1}_${ALI_band}"
    }

    global ${Type}_revtype
    switch [set ${Type}_revtype] {
	output {
	    set procCmd "revprod file1=${file1}"
	}
	base {
	    global ${Type}_coregBaseInputImage
	    set file2 [set [set Type]_coregBaseInputImage]

	    set procCmd "revbase file1=${file1} file2=${file2}"
	}
	master {
	    global RTYPE
	    if {$RTYPE == "secondary"} {
		global ${Type}_coregMasterFile
		set file2 [set [set Type]_coregMasterFile]

		set procCmd "revmast file1=${file1} file2=${file2}"
	    } elseif {$RTYPE == "additionalband"} {
		global ${Type}_previousCoregImage
		set file2 [set [set Type]_previousCoregImage]

		set procCmd "revmast file1=${file1} file2=${file2}"
	    }
	}
	accuracy {
	    set procCmd "revacc file1=${file1}"
	}
	accuracyOverlay {
	    set procCmd "revacc2 file1=${file1}"
	}
    }

    set file [open ${type}review.pdf w]

    global VDEV_DIR
    puts $file "procedure\n\nbody\n\nsetlib-add library=(${VDEV_DIR},\$R2LIB)\n"
    puts $file $procCmd
    puts $file "\nend-proc"

    close $file

    puts "procedure\n\nbody\n\nsetlib-add library=(${VDEV_DIR},\$R2LIB)\n"
    puts $procCmd
    puts "\nend-proc"
    
    catch {exec vicarb ${type}review.pdf}

}

proc createBeParmFrame {w} {
    set f $w.beParmFrame
    frame $f -bd 1 -relief solid

    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0

    set row 0

    set beListEntry [Entry \#auto $f \
	    -width 40 \
	    -label "BE List:" \
	    -browse "Open -filetypes [FileUtils::fileTypes text all]"]
    $beListEntry grid -row $row -column 0 -sticky ew

    global beList
    set beList $beListEntry

    incr row

    set l $f.beOutTypeLabel
    label $l -text "Output Chip Type:"
    grid $l -row $row -column 0 -sticky e

    set bf $f.buttonFrame_${row}
    frame $bf
    grid $bf -row $row -column 1
    grid configure $bf -columnspan 2 -sticky w

    set rb $bf.beOutTypeVicar
    radiobutton $rb -text "VICAR" -variable beOutType -value vic -anchor w
    pack $rb -side left

    set rb $bf.beOutTypeTiff
    radiobutton $rb -text "TIFF" -variable beOutType -value tif -anchor w
    pack $rb -side left

    global beOutType
    set beOutType vic

    set l $bf.outPrefixLabel
    label $l -text "   Output Chip Prefix:"
    pack $l -side left

    set e $bf.outPrefixEntry
    entry $e -textvariable outputChipPrefix -width 20
    pack $e -side left
    global outputChipPrefix
    set outputChipPrefix ""

    incr row

    set l $f.chipPixelSizeLabel
    label $l -text "Chip Pixel Size (m):"
    grid $l -row $row -column 0 -sticky e

    set bf $f.buttonFrame_${row}
    frame $bf
    grid $bf -row $row -column 1
    grid configure $bf -columnspan 3 -sticky w

    set e $bf.chipPixelSizeEntry
    entry $e -textvariable chipPixelSize -width 20
    pack $e -side left
    global chipPixelSize
    set chipPixelSize "1.0"

    set l $bf.chipWindowSizeLabel
    label $l -text "    Chip Window Size (pixels):"
    pack $l -side left

    set e $bf.chipWindowSizeEntry
    entry $e -textvariable chipWindowSize -width 20
    pack $e -side left
    global chipWindowSize
    set chipWindowSize "4096"

    incr row

    set beRefRpcEntry [Entry \#auto $f \
	    -width 40 \
	    -label "Master Reference RPC:" \
	    -browse "Open -filetypes [FileUtils::fileTypes tiff all]"]
    $beRefRpcEntry grid -row $row -column 0 -sticky ew
    global beRefRpc
    set beRefRpc $beRefRpcEntry

    return $f
}

proc createCoregParmFrame {w Type RTYPE} {
    set f $w.coregParmFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 0
    grid columnconfigure $f 2 -weight 1

    set row -1

    # Types are (ALI Aster Hyperion Ikonos Landsat Modis NTM Quickbird SPOT)
    # RTYPEs are (master secondary additionalband), but this proc is called only for master and secondary

    if {$RTYPE == "master"} {
	incr row

	set gff $f.gridFinenessFrame
	frame $gff
	grid $gff -row $row -column 0 -sticky w
	grid configure $gff -columnspan 3

	set l $gff.labelx1
	label $l -text "Grid Fineness (range 100 to 4000): "
	pack $l -side left -anchor w

	set l $gff.labelx2
	label $l -text "Horizontal"
	pack $l -side left -anchor w

	set e $gff.entryx3
	entry $e -textvariable ${Type}_NAH -width 5
	global ${Type}_NAH
	pack $e -side left -anchor w

	set l $gff.labelx4
	label $l -text "Vertical"
	pack $l -side left -anchor w

	set e $gff.entryx5
	entry $e -textvariable ${Type}_NAV -width 5
	global ${Type}_NAV
	pack $e -side left -anchor w

	switch $Type {
	    "Hyperion" - "ALI" {
		set ${Type}_NAH 480
		set ${Type}_NAV 800
	    }
	    "Aster" - "Landsat" {
		set ${Type}_NAH 500
		set ${Type}_NAV 500
	    }
	    default {
		set ${Type}_NAH 950
		set ${Type}_NAV 950
	    }
	}

	if {$Type == "Ikonos" || $Type == "Quickbird" || $Type == "NTM" || $Type == "SPOT"} {
	    incr row

	    set f2 $f.f2
	    frame $f2
	    grid $f2 -row $row -column 0 -sticky news
	    grid configure $f2 -columnspan 3

	    set l $f2.label_1
	    label $l -text "Output Pixel Size: "
	    pack $l -side left

	    set e $f2.entry${row}_4
	    entry $e -textvariable ${Type}_MPIX -width 5
	    global ${Type}_MPIX
	    switch $Type {
		Quickbird {set ${Type}_MPIX 0.61}
		Ikonos {set ${Type}_MPIX 1.0}
		SPOT {set ${Type}_MPIX 10.0}
		NTM {set ${Type}_MPIX 1.0}
	    }
	    pack $e -side left

	    if {$Type == "NTM"} {
		set l $f2.label_2
		label $l -text "(meters)" -font italic
		pack $l -side left
	    } elseif {$Type == "Quickbird"} {
		set l $f2.label_2
		label $l -text "(meters, range 0.1 to 100, use 0.5 for World View 2)" -font italic
		pack $l -side left
	    } else {
		set l $f2.label_2
		label $l -text "(meters, range 0.1 to 100)" -font italic
		pack $l -side left
	    }
	}

	incr row

	set f3 $f.f3
	frame $f3
	grid $f3 -row $row -column 0 -sticky news
	grid configure $f3 -columnspan 3

	set l $f3.label${row}
	label $l -text "Output Map Projection:"
	pack $l -side left -anchor w -fill both

	set rb $f3.pc
	radiobutton $rb -text "Platte Carree" -variable ${Type}_maptype -value pc -anchor w
	if {$Type == "Aster" || $Type == "Landsat"} {
	    $rb config -command "$f3.refEntry config -state disabled ; $f3.refBrowse config -state disabled"
	}
	pack $rb -side left -anchor w -fill both
	
	set rb $f3.utm
	radiobutton $rb -text "UTM" -variable ${Type}_maptype -value utm -anchor w
	if {$Type == "Aster" || $Type == "Landsat"} {
	    $rb config -command "$f3.refEntry config -state disabled ; $f3.refBrowse config -state disabled"
	}
	pack $rb -side left -anchor w -fill both
	
	if {$Type == "Aster" || $Type == "Landsat" || $Type == "Hyperion" || $Type == "ALI" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Ikonos" || $Type == "Modis"} {
	    set rb $f3.reference
	    radiobutton $rb -text "Reference" -variable ${Type}_maptype -value ref -anchor w -command "$f3.refEntry config -state normal ; $f3.refBrowse config -state normal"
	    pack $rb -side left -anchor w -fill both
	    
	    set e $f3.refEntry
	    entry $e -textvariable ${Type}_referenceProjection -width 40 -state disabled

	    lappend ${Type}_fileEntries [list ${Type}_referenceProjection "Projection Reference Image"]

	    global ${Type}_referenceProjection
	    set ${Type}_referenceProjection ""
	    pack $e -side left -anchor w -fill both

	    set mb $f3.refBrowse
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_referenceProjection vicarGeoTIFF" -state disabled
	    pack $mb -side left -anchor w -fill both
	}

	if {$Type == "Aster" || $Type == "Landsat" || $Type == "Hyperion" || $Type == "ALI" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Ikonos" || $Type == "Modis"} {
	    incr row

	    set f4 $f.f4
	    frame $f4
	    grid $f4 -row $row -column 0 -sticky news
	    grid configure $f4 -columnspan 3

	    set l $f4.label${row}
	    label $l -text "Resample Method:"
	    pack $l -side left -anchor w -fill both

	    set rb $f4.bilin
	    radiobutton $rb -text "Bilinear" -variable ${Type}_resampleMethod -value bilin -anchor w
	    pack $rb -side left -anchor w -fill both

	    set rb $f4.noin
	    radiobutton $rb -text "Nearest Neighbor" -variable ${Type}_resampleMethod -value noin -anchor w
	    pack $rb -side left -anchor w -fill both

	    global ${Type}_resampleMethod
	    set ${Type}_resampleMethod bilin

	    incr row

	    set f5 $f.f5
	    frame $f5
	    grid $f5 -row $row -column 0 -sticky news
	    grid configure $f5 -columnspan 3

	    set l $f5.label${row}
	    label $l -text "Raster Type:"
	    pack $l -side left -anchor w -fill both

	    set rb $f5.area
	    radiobutton $rb -text "Area" -variable ${Type}_rasterType -value area -anchor w
	    pack $rb -side left -anchor w -fill both

	    set rb $f5.point
	    radiobutton $rb -text "Point" -variable ${Type}_rasterType -value point -anchor w
	    pack $rb -side left -anchor w -fill both

	    global ${Type}_rasterType
	    set ${Type}_rasterType area
	}
    } ; #     if {($Type != "Landsat" && $Type != "Hyperion" && $Type != "ALI" && $Type != "Modis" && $Type != "Ikonos" && $Type != "Quickbird" && $Type != "SPOT" && $Type != "NTM") || $RTYPE != "secondary"}

    global ${Type}_maptype
    set ${Type}_maptype pc

    if {$Type == "Hyperion" || $Type == "ALI"} {
	incr row

	set f3 $f.f3_$row
	frame $f3
	grid $f3 -row $row -column 0 -sticky news
	grid configure $f3 -columnspan 3

	set l $f3.label${row}
	label $l -text "Initial FFT Search Size (pixels):"
	pack $l -side left -anchor w -fill both

	switch $Type {
	    "Hyperion" {
		set firstVal "256"
		set secondVal "512"
	    }
	    "ALI" {
		set firstVal "196"
		set secondVal "256"
	    }
	}

	set rb $f3.pc
	radiobutton $rb -text $firstVal -variable ${Type}_fftSize -value $firstVal -anchor w -command "$f3.pickTiepoints config -state disabled"
	pack $rb -side left -anchor w -fill both
	
	set rb $f3.utm
	radiobutton $rb -text $secondVal -variable ${Type}_fftSize -value $secondVal -anchor w -command "$f3.pickTiepoints config -state disabled"
	pack $rb -side left -anchor w -fill both
	
	set rb $f3.manual
	radiobutton $rb -text "Manual Tiepoints" -variable ${Type}_fftSize -value 128 -anchor w -command "$f3.pickTiepoints config -state normal"
	pack $rb -side left -anchor w -fill both
	
	global ${Type}_tiepoints
	set ${Type}_tiepoints {-999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0}

	set b $f3.pickTiepoints
	button $b -text "Choose ..." -command "chooseTiepoints $Type Landsat $RTYPE" -state disabled
	pack $b -side left -anchor w -fill both

	global ${Type}_fftSize
	set ${Type}_fftSize $secondVal

    } ; # if {$Type == "Hyperion" || $Type == "ALI"}
    
    if {$Type == "Aster"} {
	if {$RTYPE == "master"} {
	    incr row

	    set f3 $f.f3_$row
	    frame $f3
	    grid $f3 -row $row -column 0 -sticky news
	    grid configure $f3 -columnspan 3

	    set l $f3.label${row}
	    label $l -text "Manual Tiepoints"
	    pack $l -side left -anchor w -fill both

	    global ${Type}_tiepoints
	    set ${Type}_tiepoints {-999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0}

	    set b $f3.pickTiepoints
	    button $b -text "Choose ..." -command "chooseTiepoints $Type CIB/Landsat $RTYPE"
	    pack $b -side left -anchor w -fill both
	}

	incr row

	set f3 $f.f3_$row
	frame $f3
	grid $f3 -row $row -column 0 -sticky news
	grid configure $f3 -columnspan 3

	set cb $f3.doAllAdBands
	checkbutton $cb -text "Run All Additional Bands" -variable runAllAdditionalBands
	pack $cb -side left -anchor w -fill both
	global runAllAdditionalBands
	set runAllAdditionalBands 0
    }

    if {$Type == "Ikonos" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM"} {
	if {$Type == "Hyperion" || $Type == "ALI"} {
	    set baseType "Landsat"
	} elseif {$Type == "Aster" || $Type == "Landsat"} {
	    set baseType "CIB/Landsat"
	} else {
	    set baseType "CIB"
	}

	global optionalControlPointWidgets
	set optionalControlPointWidgets {}
	global manualTiepointsWidgets
	set manualTiepointsWidgets {}

	# get optional control point for NTM
	if {$Type == "NTM"} {
	    incr row

	    set subf $f.controlPointFrame
	    frame $subf
	    grid $subf -row $row -column 0 -sticky w

	    set l $subf.label${row}
	    label $l -text "Optional Control Point:  Line"
	    pack $l -side left

	    set e $subf.lineEntry
	    entry $e -textvariable controlPointLine -width 10
	    pack $e -side left

	    lappend optionalControlPointWidgets $e

	    set l $subf.sampLabel
	    label $l -text " Sample"
	    pack $l -side left

	    set e $subf.sampEntry
	    entry $e -textvariable controlPointSamp -width 10
	    pack $e -side left

	    lappend optionalControlPointWidgets $e

	    set l $subf.latLabel
	    label $l -text " Lat"
	    pack $l -side left

	    set e $subf.latEntry
	    entry $e -textvariable controlPointLat -width 15
	    pack $e -side left

	    lappend optionalControlPointWidgets $e

	    set l $subf.lonLabel
	    label $l -text " Lon"
	    pack $l -side left

	    set e $subf.lonEntry
	    entry $e -textvariable controlPointLon -width 15
	    pack $e -side left

	    lappend optionalControlPointWidgets $e

	    if {$RTYPE == "master"} {
		set l $subf.noticeLabel
		label $l -font italic -text "Required if $baseType omitted."
		pack $l -side left
	    }
	}

	incr row

	set f3 $f.f3_$row
	frame $f3
	grid $f3 -row $row -column 0 -sticky news
	grid configure $f3 -columnspan 3

	set rb $f3.manual
	checkbutton $rb -text "Manual Tiepoints" -variable useTiePoints -state disabled -anchor w -command "if {\$useTiePoints} {$f3.pickTiepoints config -state normal} else {$f3.pickTiepoints config -state disabled}"
	pack $rb -side left -anchor w -fill both
	
	lappend manualTiepointsWidgets $rb

	global ${Type}_tiepoints
	set ${Type}_tiepoints {-999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0 -999.0}

	set b $f3.pickTiepoints
	button $b -text "Choose ..." -command "chooseTiepoints $Type CIB $RTYPE" -state disabled
	pack $b -side left -anchor w -fill both

	lappend manualTiepointsWidgets $b

	set l $f3.noticeLabel
	label $l -font italic -text "Tying ${Type} image to $baseType base."
	pack $l -side left

    } ; # if {$Type == "Ikonos" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM"}

    if {$Type == "SPOT" && $RTYPE == "secondary"} {
	incr row

	set l $f.label${row}
	label $l -text "First Band:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}_2
	entry $e -textvariable ${Type}_band -width 5
	global ${Type}_band
	set ${Type}_band 1
	grid $e -row $row -column 1 -sticky w

	incr row

	set l $f.label${row}_2
	label $l -text "Number Of Bands:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}_3
	entry $e -textvariable ${Type}_nband -width 5
	global ${Type}_nband
	set ${Type}_nband 4
	grid $e -row $row -column 1 -sticky w
    }

    if {$Type == "ALI"} {
	incr row

	set f4 $f.f4_$row
	frame $f4
	grid $f4 -row $row -column 0 -sticky news
	grid configure $f4 -columnspan 3

	set l $f4.label${row}
	label $l -text "Sensor:"
	pack $l -side left -anchor w -fill both

	set rb $f4.ms
	radiobutton $rb -text "Multi-spectral" -variable ${Type}_sensor -value "ms" -anchor w
	pack $rb -side left -anchor w -fill both
	
	set rb $f4.pn
	radiobutton $rb -text "Panchromatic" -variable ${Type}_sensor -value "pn" -anchor w
	pack $rb -side left -anchor w -fill both

	global ${Type}_sensor
	set ${Type}_sensor "pn"
    }

    if {$Type == "Modis"} {
	incr row

	set dnf $f.dnf
	frame $dnf
	grid $dnf -row $row -column 0 -sticky w
	grid configure $dnf -columnspan 5

	set l $dnf.label${row}
	label $l -text "Acquisition Time:"
	pack $l -side left

	set rb $dnf.master
	radiobutton $rb -text "Daytime" -variable ${Type}_time -relief flat -value day
	pack $rb -side left

	set rb $dnf.secondary
	radiobutton $rb -text "Nighttime" -variable ${Type}_time -relief flat -value night
	pack $rb -side left

	global ${Type}_time
	set ${Type}_time day
    }

    if {$Type == "Landsat" && $RTYPE == "secondary"} {
	incr row

	set cb $f.cb
	checkbutton $cb -text "Use base for master registration" -variable useBaseForMasterRegistration
	grid $cb -row $row -column 0

	global useBaseForMasterRegistration
	set useBaseForMasterRegistration 1
    }

    return $f
}

proc chooseTiepoints {Type Base RTYPE} {
    global ${Type}_sourceFile
    set file [set ${Type}_sourceFile]
    if {! ([file isfile $file] && [file readable $file] && [file size $file] > 0)} {
  	tk_messageBox -message "Input image file \"${file}\" is not readable"
  	return
    }

    global ${Type}_coregBaseInputImage
    set file [set ${Type}_coregBaseInputImage]
    if {$file != "" || $Type != "NTM"} {
	if {! ([file isfile $file] && [file readable $file] && [file size $file] > 0)} {
	    tk_messageBox -message "Base image file \"${file}\" is not readable"
	    return
	}
    }

    createTiepointDialog $Type $Base $RTYPE
    update

    if {$file != ""} {
	# this will view the base image
	global VDEV_DIR
	catch {exec ${VDEV_DIR}/xvd $file &}
    }

    # this will log and view the input image
    startCoregistration $Type y $RTYPE    
}

proc createTiepointDialog {Type Base RTYPE} {
    set w .tiepoints_${Type}
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "Enter ${Type}/${Base} Tiepoints"
    wm iconname $w "Enter ${Type}/${Base} Tiepoints"

    set g $w.frame
    frame $g
    pack $g -fill both -expand true

    set row 0

    set l $g.l0
    text $l -wrap word -height 3

    $l insert 0.1 "When the $Type and $Base images appear in XVD windows, choose the tiepoints, and enter the line and sample number from XVD in the fields below. Then press \"Accept\"."

    grid $l -row $row -column 0
    grid configure $l -columnspan 5

    incr row

    set l $g.l_${row}
    label $l -text " "
    grid $l -row $row -column 0

    incr row

    set l $g.l1
    label $l -text "$Type Image"
    grid $l -row $row -column 1
    grid configure $l -columnspan 2

    set l $g.l2
    label $l -text "$Base Image"
    grid $l -row $row -column 3
    grid configure $l -columnspan 2

    incr row

    set l $g.l3
    label $l -text "Line"
    grid $l -row $row -column 1

    set l $g.l4
    label $l -text "Sample"
    grid $l -row $row -column 2

    set l $g.l5
    label $l -text "Line"
    grid $l -row $row -column 3

    set l $g.l6
    label $l -text "Sample"
    grid $l -row $row -column 4

    incr row

    set l $g.l9
    label $l -text "First"
    grid $l -row $row -column 0

    set e $g.e_src_upper_line
    entry $e
    grid $e -row $row -column 1

    set e $g.e_src_upper_samp
    entry $e
    grid $e -row $row -column 2

    set e $g.e_base_upper_line
    entry $e
    grid $e -row $row -column 3

    set e $g.e_base_upper_samp
    entry $e
    grid $e -row $row -column 4

    incr row

    set l $g.l10
    label $l -text "Second"
    grid $l -row $row -column 0

    set e $g.e_src_lower_line
    entry $e
    grid $e -row $row -column 1

    set e $g.e_src_lower_samp
    entry $e
    grid $e -row $row -column 2

    set e $g.e_base_lower_line
    entry $e
    grid $e -row $row -column 3

    set e $g.e_base_lower_samp
    entry $e
    grid $e -row $row -column 4

    if {$Type == "Quickbird" || $Type == "SPOT" || $Type == "Ikonos" || $Type == "NTM" || $Type == "Aster"} {
	incr row

	set l $g.l103
	label $l -text "Third"
	grid $l -row $row -column 0

	set e $g.e_src_third_line
	entry $e
	grid $e -row $row -column 1

	set e $g.e_src_third_samp
	entry $e
	grid $e -row $row -column 2

	set e $g.e_base_third_line
	entry $e
	grid $e -row $row -column 3

	set e $g.e_base_third_samp
	entry $e
	grid $e -row $row -column 4
    }

    incr row

    set l $g.l_${row}
    label $l -text " "
    grid $l -row $row -column 0

    incr row

    set b $g.b
    button $b -text "Accept" -command "grabTiepoints $Type $g ; destroy $w"
    grid $b -row $row -column 0
    grid configure $b -columnspan 5
}

proc grabTiepoints {Type g} {
    global ${Type}_tiepoints
    global $g.e_src_upper_line
    global $g.e_src_upper_samp
    global $g.e_base_upper_line
    global $g.e_base_upper_samp
    global $g.e_src_lower_line
    global $g.e_src_lower_samp
    global $g.e_base_lower_line
    global $g.e_base_lower_samp
    global $g.e_src_third_line
    global $g.e_src_third_samp
    global $g.e_base_third_line
    global $g.e_base_third_samp

    if {$Type == "Quickbird" || $Type == "SPOT" || $Type == "Ikonos" || $Type == "NTM" || $Type == "Aster"} {
	set ${Type}_tiepoints [list [$g.e_src_upper_line get] [$g.e_src_upper_samp get] [$g.e_base_upper_line get] [$g.e_base_upper_samp get] \
				   [$g.e_src_lower_line get] [$g.e_src_lower_samp get] [$g.e_base_lower_line get] [$g.e_base_lower_samp get] \
				   [$g.e_src_third_line get] [$g.e_src_third_samp get] [$g.e_base_third_line get] [$g.e_base_third_samp get]]
    } else {
	set ${Type}_tiepoints [list [$g.e_src_upper_line get] [$g.e_src_upper_samp get] [$g.e_base_upper_line get] [$g.e_base_upper_samp get] \
				   [$g.e_src_lower_line get] [$g.e_src_lower_samp get] [$g.e_base_lower_line get] [$g.e_base_lower_samp get]]
    }
}

proc createCoregSourceFrame {w Type} {
    global ${Type}_fileEntries
    set ${Type}_fileEntries {}

    set f $w.coregSourceFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0

    set row 0

    set l $f.label${row}
    label $l -text "$Type Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type}_sourceFile -width 40

    lappend ${Type}_fileEntries [list ${Type}_sourceFile [$l cget -text]]

    global ${Type}_sourceFile
    set ${Type}_sourceFile ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.default${row}
    switch $Type {
	"Hyperion" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile l1r"
	}
	"ALI" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile m1r"
	}
	"NTM" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile ntf"
	}
	"Aster" - "Modis" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile hdf"
	}
	"Landsat" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile i3tif"
	}
	"Quickbird" - "Ikonos" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile tifntf"
	}
	"SPOT" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile dimap"
	}
	default {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_sourceFile tif"
	}
    }
    grid $mb -row $row -column 2 -sticky news

    # get metadata for these sensors
    if {$Type == "Quickbird" || $Type == "Ikonos" || $Type == "Hyperion" || $Type == "ALI" || $Type == "Modis" || $Type == "Landsat"} {
	incr row

	set l $f.label${row}
	switch $Type {
	    "Hyperion" - "ALI" {
		label $l -text "${Type} Metadata:"
	    }
	    "Modis" {
		label $l -text "Modis Geoloc:"
	    }
	    default {
		label $l -text "$Type Metadata:"
	    }
	}
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_metadataFile -width 40

	lappend ${Type}_fileEntries [list ${Type}_metadataFile [$l cget -text]]

	global ${Type}_metadataFile
	set ${Type}_metadataFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	switch $Type {
	    "Quickbird" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile imd"
	    }
	    "Ikonos" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile txt"
	    }
	    "Landsat" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile h1met"
	    }
	    "Hyperion" - "ALI" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile hdfmet"
	    }
	    "Modis" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile hdf"
	    }
	}
	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$Type == "Modis"} {
	incr row

	set l $f.label${row}
	label $l -text "$Type Image 2:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_sourceFile_2 -width 40

	global ${Type}_sourceFile_2
	set ${Type}_sourceFile_2 ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen Modis Modis_sourceFile_2 hdf"
	grid $mb -row $row -column 2 -sticky news

	# get metadata for these sensors
	incr row

	set l $f.label${row}
	label $l -text "Modis Geoloc 2:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_metadataFile_2 -width 40

	global ${Type}_metadataFile_2
	set ${Type}_metadataFile_2 ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type ${Type}_metadataFile_2 hdf"

	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$Type == "Ikonos"} {
	incr row

	set l $f.label${row}
	label $l -text "$Type Header:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_headerFile -width 40

	lappend ${Type}_fileEntries [list ${Type}_headerFile [$l cget -text]]

	global ${Type}_headerFile
	set ${Type}_headerFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type ${Type}_headerFile hdr"
	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$Type != "Modis"} {
	incr row

	set l $f.label${row}
	if {$Type == "Hyperion" || $Type == "ALI"} {
	    label $l -text "Landsat Image:"
	} elseif {$Type == "Aster" || $Type == "Landsat"} {
	    label $l -text "CIB/Landsat Image:"
	} else {
	    label $l -text "CIB Image:"
	}
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_coregBaseInputImage -width 40 -validate key -validatecommand "validateBaseImage $Type"

	lappend ${Type}_fileEntries [list ${Type}_coregBaseInputImage [$l cget -text]]

	global ${Type}_coregBaseInputImage
	set ${Type}_coregBaseInputImage ""

	global CIB_OUT_IMAGE_NAME
	catch {set ${Type}_coregBaseInputImage basemos/$CIB_OUT_IMAGE_NAME}
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_coregBaseInputImage lsat_cib"
	grid $mb -row $row -column 2 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text "Elevation Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type}_coregElevInputImage -width 40

    lappend ${Type}_fileEntries [list ${Type}_coregElevInputImage [$l cget -text]]

    global ${Type}_coregElevInputImage
    set ${Type}_coregElevInputImage ""
    global DTED_OUT_IMAGE_NAME
    catch {set ${Type}_coregElevInputImage dtedmos/$DTED_OUT_IMAGE_NAME}
    grid $e -row $row -column 1 -sticky news

    set mb $f.default${row}
    if {$Type == "Modis"} {
	button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_coregElevInputImage etopo"
    } else {
	button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_coregElevInputImage elev"
    }
    grid $mb -row $row -column 2 -sticky news

    global RTYPE
    if {$RTYPE == "secondary"} {
	incr row

	set l $f.label${row}
	label $l -text "$Type Master Image:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_coregMasterFile -width 40

	lappend ${Type}_fileEntries [list ${Type}_coregMasterFile [$l cget -text]]

	global ${Type}_coregMasterFile
	set ${Type}_coregMasterFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_coregMasterFile img"
	grid $mb -row $row -column 2 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text "Output Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type}_outimg -width 40
    grid $e -row $row -column 1 -sticky news

    global ${Type}_outimg
    set ${Type}_outimg ""

    set mb $f.default${row}
    button $mb -text "Browse ..." -command "getFileForSave $Type ${Type}_outimg img"
    grid $mb -row $row -column 2 -sticky news

    if {$Type == "Ikonos" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM"} {
	incr row

	set l $f.label${row}
	label $l -text "Optional Out RPC:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_outrpctiff -width 40
	grid $e -row $row -column 1 -sticky news

	global ${Type}_outrpctiff
	set ${Type}_outrpctiff ""

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForSave $Type ${Type}_outrpctiff geoTIFF"
	grid $mb -row $row -column 2 -sticky news

	incr row

	# Resample Method
	set f4 $f.f4
	frame $f4
	grid $f4 -row $row -column 1 -sticky news

	set l $f4.label
	label $l -text "RPC Format:"
	pack $l -side left -anchor w -fill both

	set rb $f4.nitf
	radiobutton $rb -text "NITF" -variable optionalOutRpcformat -value ntf -anchor w
	pack $rb -side left -anchor w -fill both

	set rb $f4.tif
	radiobutton $rb -text "TIFF" -variable optionalOutRpcformat -value tif -anchor w
	pack $rb -side left -anchor w -fill both

	set rb $f4.vic
	radiobutton $rb -text "Vicar" -variable optionalOutRpcformat -value vic -anchor w
	pack $rb -side left -anchor w -fill both

	global optionalOutRpcformat
	set optionalOutRpcformat ntf
    }

    if {$Type != "NTM"} {
	incr row

	set sg $f.subGrid
	frame $sg
	grid $sg -row $row -column 0 -sticky news
	grid configure $sg -columnspan 2
	grid configure $sg -rowspan 2
	grid columnconfigure $sg 0 -weight 0
	grid columnconfigure $sg 1 -weight 1
	grid rowconfigure $sg 0 -weight 1
	grid rowconfigure $sg 1 -weight 1

	set l $sg.labelx0
	label $l -text "Optional Site Reference Image:"
	grid $l -row 0 -column 0 -sticky nes

	set e $sg.entryx0
	entry $e -textvariable ${Type}_siteref -width 40
	grid $e -row 0 -column 1 -sticky news

	global ${Type}_siteref
	set ${Type}_siteref ""

	set mb $f.browsex0
	button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type}_siteref img"
	grid $mb -row $row -column 2 -sticky news

	incr row

	set l $sg.labelx1
	label $l -text "Optional Site Output Image:"
	grid $l -row 1 -column 0 -sticky nes

	set e $sg.entryx1
	entry $e -textvariable ${Type}_siteout -width 40
	grid $e -row 1 -column 1 -sticky news

	global ${Type}_siteout
	set ${Type}_siteout ""

	set mb $f.browsex1
	button $mb -text "Browse ..." -command "getFileForSave $Type ${Type}_siteout img"
	grid $mb -row $row -column 2 -sticky news
    }

    return $f
}

proc checkBaseImage {Type} {
    if {$Type == "NTM" || $Type == "Ikonos" || $Type == "Quickbird" || $Type == "SPOT"} {
	global ${Type}_coregBaseInputImage

	global optionalControlPointWidgets
	global manualTiepointsWidgets
	global useTiePoints

	if {[set ${Type}_coregBaseInputImage] == ""} {
	    #enable control point
	    foreach w $optionalControlPointWidgets {
		$w configure -state normal
	    }

	    #disable manual tiepoints
	    foreach w $manualTiepointsWidgets {
		$w configure -state disabled
	    }
	} else {
	    #disable control point
	    foreach w $optionalControlPointWidgets {
		$w configure -state disabled
	    }

	    #enable manual tiepoints
	    [lindex $manualTiepointsWidgets 0] configure -state normal

	    if {$useTiePoints} {
		[lindex $manualTiepointsWidgets 1] configure -state normal
	    }		
	}
    }

}

proc validateBaseImage {Type} {
    after 1 "checkBaseImage $Type"
    return true
}

proc createCoreg2SourceFrame {w Type} {
    set Type2 ${Type}2

    global ${Type2}_fileEntries
    set ${Type2}_fileEntries {}

    set f $w.coregSourceFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0

    set row 0

    set l $f.label${row}
    label $l -text "$Type Additional Band Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type2}_sourceFile -width 40

    lappend ${Type2}_fileEntries [list ${Type2}_sourceFile [$l cget -text]]

    global ${Type2}_sourceFile
    set ${Type2}_sourceFile ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.default${row}

    switch $Type {
	"Hyperion" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile l1r"
	}
	"ALI" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile m1r"
	}
	"Aster" - "Modis" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile hdf"
	}
	"Landsat" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile i3tif"
	}
	"Quickbird" - "Ikonos" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile tifntf"
	}
	"SPOT" {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile dimap"
	}
	default {
	    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_sourceFile tif"
	}
    }

    grid $mb -row $row -column 2 -sticky news

    if {$Type == "Quickbird"} {
	incr row

	set l $f.label${row}
	label $l -text "$Type Metadata:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type2}_metadataFile -width 40

	lappend ${Type2}_fileEntries [list ${Type2}_metadataFile [$l cget -text]]

	global ${Type2}_metadataFile
	set ${Type2}_metadataFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type2 ${Type2}_metadataFile imd"
	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$Type == "Modis"} {
	incr row

	set l $f.label${row}
	label $l -text "Modis Additional Band Geoloc:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}2_metadataFile -width 40

	global ${Type}2_metadataFile
	set ${Type}2_metadataFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type ${Type}2_metadataFile hdf"

	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news

	incr row

	set l $f.label${row}
	label $l -text "$Type Additional Band Image 2:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}2_sourceFile_2 -width 40

	global ${Type}2_sourceFile_2
	set ${Type}2_sourceFile_2 ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen Modis Modis2_sourceFile_2 hdf"
	grid $mb -row $row -column 2 -sticky news

	incr row

	set l $f.label${row}
	label $l -text "Modis Additional Band Geoloc 2:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}2_metadataFile_2 -width 40

	global ${Type}2_metadataFile_2
	set ${Type}2_metadataFile_2 ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type ${Type}2_metadataFile_2 hdf"

	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text "$Type Master/Secondary/Site Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type2}_previousCoregImage -width 40

    lappend ${Type2}_fileEntries [list ${Type2}_previousCoregImage [$l cget -text]]

    global ${Type2}_previousCoregImage
    set ${Type2}_previousCoregImage ""
    grid $e -row $row -column 1 -sticky news

    global SESSION_ID

    set mb $f.default${row}
    button $mb -text "Browse ..." -command "getFileForOpen $Type ${Type2}_previousCoregImage img"
    grid $mb -row $row -column 2 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Output Additional Band Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type2}_outimg -width 40
    grid $e -row $row -column 1 -sticky news

    global ${Type2}_outimg
    set ${Type2}_outimg ""

    set mb $f.default${row}
    button $mb -text "Browse ..." -command "getFileForSave $Type ${Type2}_outimg img"
    grid $mb -row $row -column 2 -sticky news

#      set mb $f.default${row}
#      button $mb -text "Browse ..." -command "getFileForOpen ${Type2}_previousCoregAreaCaseName cib"
#      grid $mb -row $row -column 2 -sticky news

    return $f
}

proc createAreaCaseFrame {w Type} {
    set f $w.areaCaseFrame

    frame $f -bd 1 -relief solid
    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    grid columnconfigure $f 2 -weight 0

    set row 0

    set l $f.label${row}
    label $l -text "Output Case Name:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ${Type}_areaCaseName
    global ${Type}_areaCaseName
    set ${Type}_areaCaseName "test1"
    grid $e -row $row -column 1 -sticky news
    set areaCaseNameEntry $e

    set l $f.label${row}_2
    label $l -text "Prefix for output products." -font italic
    grid $l -row $row -column 2 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "Output Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    label $e -textvariable ${Type}_outputImageFile -width 40 -anchor w
    global ${Type}_outputImageFile
    set ${Type}_outputImageFile ""
    grid $e -row $row -column 1 -sticky w

    bind $areaCaseNameEntry <FocusOut> "set ${Type}_outputImageFile \[set ${Type}_areaCaseName]_map.img"
    bind $areaCaseNameEntry <Leave> "set ${Type}_outputImageFile \[set ${Type}_areaCaseName]_map.img"

    return $f
}

proc getFileForOpen {Type globalVar ext} {
    set initialDir ""

    switch $ext {
	cib {
	    set types {
		{"CIB Table of Contents"   {.toc}}
		{"CIB Table of Contents"   {.TOC}}
		{"All files"		*}
	    }
	}
	i3tif {
	    set types {
		{"Landsat Raw Images"   {.i3}}
		{"Landsat Raw Images"   {.I3}}
		{"TIFF Images"		{.tif}}
		{"TIFF Images"		{.TIF}}
		{"All files"		*}
	    }
	}
	geoTIFF {
	    set types {
		{"GeoTIFF Images"	{.tif}}
		{"GeoTIFF Images"	{.TIF}}
		{"All files"		*}
	    }
	}
	vicarGeoTIFF {
	    set types {
		{"VICAR Images"     	{.img}}
		{"GeoTIFF Images"	{.tif}}
		{"GeoTIFF Images"	{.TIF}}
		{"All files"	       	*}
	    }
	}
	geoTiffVicar {
	    set types {
		{"GeoTIFF Images"	{.tif}}
		{"GeoTIFF Images"	{.TIF}}
		{"VICAR Images"     	{.img}}
		{"All files"	       	*}
	    }
	}
	h1met {
	    set types {
		{"Landsat Meta Data" 	{.h1}}
		{"Landsat Meta Data" 	{.H1}}
		{"Landsat Meta Data" 	{.met}}
		{"Landsat Meta Data" 	{.MET}}
		{"All files"		*}
	    }
	}
	met {
	    set types {
		{"Landsat Meta Data" 	{.met}}
		{"Landsat Meta Data" 	{.MET}}
		{"All files"		*}
	    }
	}
	lsat_cib {
	    set initialDir "basemos"
	    switch $Type {
		"Hyperion" - "ALI" {
		    set types {
			{"Landsat Mosaic Images"	{*_landsat.img}}
			{"VICAR Images"		     	{.img}}
			{"All files"	        	*}
		    }
		}
		"Aster" - "Landsat" {
		    set types {
			{"CIB Mosaic Images"		{*_cib.img}}
			{"Landsat Mosaic Images"	{*_landsat.img}}
			{"VICAR Images"		     	{.img}}
			{"All files"	        	*}
		    }
		}
		default {
		    set types {
			{"CIB Mosaic Images"		{*_cib.img}}
			{"VICAR Images"		     	{.img}}
			{"All files"	        	*}
		    }
		}
	    }
	}
	elev {
	    set initialDir "dtedmos"
	    set types {
		{"DTED Mosaic Images"	{*.img}}
		{"DEM Images"		*}
		{"SRTM Images"		*}
		{"VICAR Images"		     {.img}}
		{"All files"		*}
	    }
	}
	etopo {
	    set types {
		{"Earth Topographic Images" *}
		{"All files"		    *}
	    }
	}
	tif {
	    set types {
		{"TIFF Images"		{.tif}}
		{"TIFF Images"		{.TIF}}
		{"All files"		*}
	    }
	}
	img {
	    set types {
		{"VICAR Image"		{.img}}
		{"VICAR Image"		{.IMG}}
		{"All files"		*}
	    }
	}
	imd {
	    set types {
		{"Quickbird Meta Data"	{.imd}}
		{"Quickbird Meta Data"	{.IMD}}
		{"All files"		*}
	    }
	}
	dimap {
	    set types {
		{"SPOT Data"	{.dim}}
		{"SPOT Data"	{.DIM}}
		{"All files"		*}
	    }
	}
	txt {
	    set types {
		{"Ikonos Meta Data"	{.txt}}
		{"Ikonos Meta Data"	{.TXT}}
		{"All files"		*}
	    }
	}
	hdr {
	    set types {
		{"Ikonos Headers"	{.hdr}}
		{"Ikonos Headers"	{.HDR}}
		{"All files"		*}
	    }
	}
	l1r {
	    set types {
		{"Hyperion Image" {.l1r}}
		{"Hyperion Image" {.L1R}}
		{"Hyperion Data"  {.hdf}}
		{"Hyperion Data"  {.HDF}}
		{"All files"		*}
	    }
	}
	l1r {
	    set types {
		{"Hyperion Image" {.l1r}}
		{"Hyperion Image" {.L1R}}
		{"Hyperion Data"  {.hdf}}
		{"Hyperion Data"  {.HDF}}
		{"All files"		*}
	    }
	}
	ntf {
	    set types {
		{"NITF Image" {.ntf}}
		{"NITF Image" {.NTF}}
		{"NITF Image" {.nitf}}
		{"NITF Image" {.NITF}}
		{"NITF Image" {.ntm}}
		{"NITF Image" {.NTM}}
		{"All files"		*}
	    }
	}
	tifntf {
	    set types {
		{"NITF Image" 	{.ntf}}
		{"NITF Image" 	{.NTF}}
		{"NITF Image" 	{.nitf}}
		{"NITF Image" 	{.NITF}}
		{"NITF Image" 	{.ntm}}
		{"NITF Image" 	{.NTM}}
		{"TIFF Images"	{.tif}}
		{"TIFF Images"	{.TIF}}
		{"All files"	*}
	    }
	}
	hdfmet {
	    switch $Type {
		"Hyperion" {
		    set types {
			{"Hyperion Meta Data" {.hdf}}
			{"Hyperion Meta Data" {.HDF}}
			{"Hyperion Meta Data" {.met}}
			{"Hyperion Meta Data" {.MET}}
			{"All files"		*}
		    }
		}
		"ALI" {
		    set types {
			{"ALI Meta Data" {.hdf}}
			{"ALI Meta Data" {.HDF}}
			{"ALI Meta Data" {.met}}
			{"ALI Meta Data" {.MET}}
			{"All files"	 *}
		    }
		}
	    }
	}
	hdf {
	    switch $Type {
		"Aster" - "Landsat" {
		    set types {
			{"Aster Data" {.hdf}}
			{"Aster Data" {.HDF}}
			{"All files"  *}
		    }
		}
		"Hyperion" - "ALI" {
		    set types {
			{"Hyperion Data" {.hdf}}
			{"Hyperion Data" {.HDF}}
			{"All files"		*}
		    }
		}
		"Modis" {
		    set types {
			{"Modis Data" {.hdf}}
			{"Modis Data" {.HDF}}
			{"All files"		*}
		    }
		}
	    }
	}
	default {
	    set types {
		{"All files"		*}
	    }
	}
    }

    if {$initialDir == ""} {
	set file [tk_getOpenFile -filetypes $types -parent .]
    } else {
	set file [tk_getOpenFile -filetypes $types -parent . -initialdir $initialDir]
    }

    if [string compare $file ""] {
	global $globalVar

	set $globalVar $file
    }
}

proc getFileForSave {Type globalVar ext} {
    set initialDir ""

    switch $ext {
	vicarGeoTIFF {
	    set types {
		{"VICAR Images"     	{.img}}
		{"GeoTIFF Images"	{.tif}}
		{"GeoTIFF Images"	{.TIF}}
		{"All files"	       	*}
	    }
	}
	geoTIFF {
	    set types {
		{"GeoTIFF Images"	{.tif}}
		{"GeoTIFF Images"	{.TIF}}
		{"All files"	       	*}
	    }
	}
	img - _srtm.img - _afidswdb.img - _afidswcib1.img - _afidswcib5.img {
	    set types {
		{"VICAR Image"		{.img}}
		{"VICAR Image"		{.IMG}}
		{"All files"		*}
	    }
	}
	_landsat.img			{
	    set types {
		{"Landsat Mosaic"	{*_landsat.img}}
		{"All files"		*}
	    }
	}
	default {
	    set types {
		{"All files"		*}
	    }
	}
    }

    if {$initialDir == ""} {
	set file [tk_getSaveFile -filetypes $types -parent . -defaultextension " "]
    } else {
	set file [tk_getSaveFile -filetypes $types -parent . -initialdir $initialDir -defaultextension " "]
    }

    set file [string trim $file]

    if [string compare $file ""] {
	global $globalVar

	set $globalVar $file
    }
}

proc createMainWindow {} {
    wm title . "Multi-Sensor Coregistration"
    wm protocol . WM_DELETE_WINDOW "exit"

    set f .frame
    catch {destroy $f}
    frame $f
    pack $f -fill both -expand true
    grid columnconfigure $f 0 -weight 1

    set row 0

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "WHAT YOU NEED TO PROCEED ..."
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    global env
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help.html #toproceed"
    pack $hb -anchor e -side left -expand true

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 1: DIRECTORY INITIALIZATION"
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help.html #step1"
    pack $hb -anchor e -side left -expand true

    incr row

    set dirInitFrame [createDirInitFrame $f]
    grid $dirInitFrame -row $row -column 0 -sticky news
    grid configure $dirInitFrame

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 2: IMAGE MOSAIC GENERATION (optional *)"
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help.html #step2"
    pack $hb -anchor e -side left -expand true

    incr row

    global guiFlavor
    if {$guiFlavor == "afids"} {
	set mosaicButtonsFrame [createAfidsMosaicButtonsFrame $f]
    } else {
	set mosaicButtonsFrame [createMosaicButtonsFrame $f]
    }
    grid $mosaicButtonsFrame -row $row -column 0 -sticky news

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "IMAGE COREGISTRATION PROCESSING"
    grid $l -row $row -column 0 -sticky w

    incr row

    set coregButtonsFrame [createCoregButtonsFrame $f]
    grid $coregButtonsFrame -row $row -column 0 -sticky news

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf -bd 1 -relief solid
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 6: UTILITIES"
    pack $l -anchor w -side left

    global buttonsToEnable

    set b $subf.utilitiesButton
    button $b -text "Utilities ..." -command "utilities" -state disabled
    pack $b -anchor w -side left
    lappend buttonsToEnable $b

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help.html #step6"
    pack $hb -anchor e -side left -expand true

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set bf $f.buttonFrame
    frame $bf
    grid $bf -row $row -column 0 -sticky news
    grid columnconfigure $bf 0 -weight 1
    #    grid columnconfigure $bf 1 -weight 1

    set b $bf.exitButton
    button $b -text "Exit" -command "exit"
    grid $b -row 0 -column 0
}

proc createMosaicDialog {Type defaultSourceDirs defaultDiskImgDir} {
    set type [string tolower $Type]
    set TYPE [string toupper $Type]

    set w .${type}MosaicDialog
    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w "${TYPE} Mosaic"
    wm iconname $w "${TYPE} Mosaic"

    set f $w.frame
    frame $f
    pack $f -expand true -fill both
    grid columnconfigure $f 0 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "SOURCE/DESTINATION SELECTION"
    grid $l -row $row -column 0 -sticky w

    incr row

    set sourceDestFrame [createSourceDestEntries $f $Type $defaultSourceDirs $defaultDiskImgDir]
    grid $sourceDestFrame -row $row -column 0 -sticky news

    if {$Type == "Afidswcibdb"} {
	incr row

	set l $f.label${row}
	label $l -text " "
	grid $l -row $row -column 0 -sticky w

	incr row

	set l $f.label${row}
	label $l -text "MODE SELECTION"
	grid $l -row $row -column 0 -sticky w

	incr row

	set bf $f.modeButtons
	frame $bf -bd 1 -relief solid
	grid $bf -row $row -column 0 -sticky news
	grid $bf -columnspan 3
	
	set b $bf.rbBoth
	radiobutton $b -text "CIB1 With CIB5 Fill" -variable cibMosaicMode -value cib1And5 -anchor w
	pack $b -side left
	global cibMosaicMode
	set cibMosaicMode cib1And5

	set b $bf.rbCib1
	radiobutton $b -text "CIB1 Only" -variable cibMosaicMode -value cib1Only -anchor w
	pack $b -side left

	set b $bf.rbCib5
	radiobutton $b -text "CIB5 Only" -variable cibMosaicMode -value cib5Only -anchor w
	pack $b -side left
    }

    if {$Type != "Landsat"} {
	incr row

	set l $f.label${row}
	label $l -text " "
	grid $l -row $row -column 0 -sticky w

	incr row

	set l $f.label${row}
	label $l -text "AREA OF INTEREST SELECTION"
	grid $l -row $row -column 0 -sticky w

	incr row

	set aoiFrame [createAOIframe $f $Type]
	grid $aoiFrame -row $row -column 0 -sticky news
    }

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.label${row}
    label $l -text "MOSAIC PROCESSING"
    grid $l -row $row -column 0 -sticky w

    incr row

    set processingFrame [createMosaicProcessingFrame $f $Type]
    grid $processingFrame -row $row -column 0 -sticky news

    incr row

    set b $f.close
    button $b -text "Close" -command "destroy $w"
    grid $b -row $row -column 0 -pady 10 -sticky e
}

proc status {type msg {color "#d9d9d9"}} {
    global ${type}statusLabel
    catch {[set [set type]statusLabel] configure -text $msg -bg $color}

    update
}

proc notice {type msg} {
    global ${type}noticeLabel
    catch {[set [set type]noticeLabel] configure -text $msg}
    update
}

proc startMosaic {Type viewOutImageVar} {
    set TYPE [string toupper $Type]

    global env
    if {($Type == "Cib" && (false && $env(OS_NAME) != "Linux")) || $Type == "Dted" || $Type == "Srtm" || $Type == "Afidswdb"} {
	global default${Type}SourceDirs
	global ${TYPE}_DISKIMG_DIR
	
	set index [lsearch [set default${Type}SourceDirs] [set ${TYPE}_DISKIMG_DIR]]
	if {$index >= 0} {
	    set default${Type}SourceDirs [lreplace [set default${Type}SourceDirs] $index $index]
	}

	set default${Type}SourceDirs [concat [set ${TYPE}_DISKIMG_DIR] [set default${Type}SourceDirs]]

	set default${Type}SourceDirs [lrange [set default${Type}SourceDirs] 0 4]

	saveDefaults
    }

    if {$Type == "Srtm" || $Type == "Afidswdb" || $Type == "Afidswcibdb"} {
	set TYPE [string toupper $Type]
	global ${TYPE}_AOI_MIN_LAT ${TYPE}_AOI_MAX_LAT ${TYPE}_AOI_MIN_LON ${TYPE}_AOI_MAX_LON LOCAL_DIR

	set MIN_LAT [set [set TYPE]_AOI_MIN_LAT]
	set MAX_LAT [set [set TYPE]_AOI_MAX_LAT]
	set MIN_LON [set [set TYPE]_AOI_MIN_LON]
	set MAX_LON [set [set TYPE]_AOI_MAX_LON]

	if {$Type != "Afidswcibdb"} {
	    if {[string first "." $MIN_LAT] >= 0 ||
		[string first "." $MAX_LAT] >= 0 ||
		[string first "." $MIN_LON] >= 0 ||
		[string first "." $MAX_LON] >= 0} {
		tk_messageBox -message "Only whole integer values are allowed for AOI limits."
		return
	    }
	}
    }

    if {$Type == "Landsat"} {
	global Landsat_sourceFile_1 Landsat_metadataFile_1
	global Landsat_sourceFile_2 Landsat_metadataFile_2
	global Landsat_sourceFile_3 Landsat_metadataFile_3
	global Landsat_sourceFile_4 Landsat_metadataFile_4

	set quit 0
	set someFilled 0
	for {set i 1} {$i < 5} {incr i} {
	    if {[set Landsat_sourceFile_${i}] != "" || [set Landsat_metadataFile_${i}] != ""} {
		set someFilled 1
		if {! [FileUtils::isReadableNonEmptyFile [set Landsat_sourceFile_${i}]]} {
		    tk_messageBox -message "Error reading Landsat Image $i: [set Landsat_sourceFile_${i}]"
		    set quit 1
		}
		if {! [FileUtils::isReadableNonEmptyFile [set Landsat_metadataFile_${i}]]} {
		    tk_messageBox -message "Error reading Landsat Metadata $i: [set Landsat_metadataFile_${i}]"
		    set quit 1
		}
	    }
	}

	if {! $someFilled} {
	    tk_messageBox -message "No Landsat source images specified"
	    set quit 1
	}

	if {$quit} {
#	    return
	}
    }

    set type [string tolower $Type]

    if {$Type == "Utility"} {
	global utilityType
	switch $utilityType {
	    chipEntireImage -
	    chipPercentage -
	    chipSelected {
		set elevationMosaic [findElevationMosaic]
		if {$elevationMosaic == ""} {
		    return
		}
		global ${utilityType}_elevInputImage
		set ${utilityType}_elevInputImage $elevationMosaic
	    }
	    nitfGeometry {
		global utilityInputImage
		set imageCount [ossim::getEntryListSize $utilityInputImage]
		set geomText ""
		for {set i 0} {$i < $imageCount} {incr i} {
		    set geomText "${geomText}Geometry for image $i:"
		    set geom [ossim::getEntryGeometry $utilityInputImage $i]
		    set geom [split $geom "\n"]
		    foreach line $geom {
			if { [string first "_lat" $line] >= 0 || [string first "_lon" $line] >= 0 || [string first "ref_point_" $line] >= 0 } {
			    set geomText "${geomText}\n$line"
			}
			if { [string first "number_lines" $line] >= 0 || [string first "number_samples" $line] >= 0 } {
			    set geomText "${geomText}\n$line"
			}
		    }

		    set path $utilityInputImage
		    set data [ossim::getImageHeaderFields $path 0]
		    set data [split $data "\n"]
		    foreach line $data {
			if { [string first "IDATIM" $line] >= 0 } {
			    set geomText "${geomText}\n${line}"
			    break
			}
		    }

		    if {! [catch {set tag [ossim::getTagValue $path 0 "RPC00A"]} ] } {
			set geomText "${geomText}\nNITF_CETAG=RPC00A"
		    }

		    if {! [catch {set tag [ossim::getTagValue $path 0 "RPC00B"]} ] } {
			set geomText "${geomText}\nNITF_CETAG=RPC00B"
		    }

		    if {! [catch {set tag [ossim::getTagValue $path 0 "USE00A"]} ] } {
			set data [splitUse00a ${tag}]
			set data [split $data "\n"]
			foreach line $data {
			    if { [string first "ANGLE_TO_NORTH" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "MEAN_GSD" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "DYNAMIC_RANGE" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "OBL_ANG" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "ROLL_ANG" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "SUN_EL" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			    if { [string first "SUN_AZ" $line] >= 0 } {
				set geomText "${geomText}\n${line}"
			    }
			}

		    }

		    set tmpNam "tmpnam[clock format [clock seconds] -format %j%k%M%S]"
		    catch {
			exec vextract $path $tmpNam -t
		    }
		    set use26aName ${tmpNam}use26a_0_0.txt
		    if {[file exists $use26aName]} {
			set file [open $use26aName r]
			set data [read $file]
			close $file
			set data [split $data "\n"]

			foreach line $data {
			    if { [string first "FIELD1 " $line] >= 0 } {
				set geomText "${geomText}\n${line} (ANGLE_TO_NORTH)"
			    }
			    if { [string first "FIELD2 " $line] >= 0 } {
				set geomText "${geomText}\n${line} (MEAN_GSD)"
			    }
			    if { [string first "FIELD4 " $line] >= 0 } {
				set geomText "${geomText}\n${line} (DYNAMIC_RANGE)"
			    }
			    if { [string first "FIELD8 " $line] >= 0 } {
				set geomText "${geomText}\n${line} (OBL_ANG)"
			    }
			    if { [string first "FIELD9 " $line] >= 0 } {
				set geomText "${geomText}\n${line} (ROLL_ANG)"
			    }
			}
		    }

		    foreach name [glob -nocomplain ${tmpNam}*] {
			exec rm $name
		    }

#  		    if {! [catch {set tag [ossim::getTagValue $path 0 "USE26A"]} ] } {
#  			set data [splitUse26a ${tag}]
#  			set data [split $data "\n"]
#  			foreach line $data {
#  			    if { [string first "FIELD1:" $line] >= 0 } {
#  				set geomText "${geomText}\n${line} (ANGLE_TO_NORTH)"
#  			    }
#  			    if { [string first "FIELD2:" $line] >= 0 } {
#  				set geomText "${geomText}\n${line} (MEAN_GSD)"
#  			    }
#  			    if { [string first "FIELD4:" $line] >= 0 } {
#  				set geomText "${geomText}\n${line} (DYNAMIC_RANGE)"
#  			    }
#  			    if { [string first "FIELD8:" $line] >= 0 } {
#  				set geomText "${geomText}\n${line} (OBL_ANG)"
#  			    }
#  			    if { [string first "FIELD9:" $line] >= 0 } {
#  				set geomText "${geomText}\n${line} (ROLL_ANG)"
#  			    }
#  			}

#  		    }

		    set geomText "${geomText}\n"
		}

		set w .geom
		catch {destroy $w}
		toplevel $w
		wm title $w "$utilityInputImage Geometry"

		frame $w.buttons
		pack $w.buttons -side bottom -fill x -pady 2m
		button $w.buttons.dismiss -text "Close" -command "destroy $w"
		pack $w.buttons.dismiss -side left -expand 1

		text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set" -setgrid 1 \
			-height 30 -undo 1 -autosep 1
		scrollbar $w.scroll -command "$w.text yview"
		pack $w.scroll -side right -fill y
		pack $w.text -expand yes -fill both
		$w.text insert 0.0 $geomText
		$w.text mark set insert 0.0

		return
	    }
	}

	setupUtilityRun
    } else {
	setupMosaicRun $Type
    }

    notice $type "Ensuring taetm is not running"

    if {[llength [taetmRunning]] > 0} {
	if {[promptUser "Terminate All VICARS?" "There is a VICAR job running that must be\nterminated before starting a new one.\nContinue with termination?" {Yes No}] == "No"} {
	    return
	}
    }

    killTaetm $type

    global terminated
    set terminated 0
    notice $type "Starting job"
    exec vicarb ${type}run.pdf | tee teeFile &

    notice $type ""
    while {[llength [taetmRunning]]} {
	status $type "Running" green
	status $type "Running." green 
	status $type "Running.." green 
	status $type "Running..." green
	after 1000
    }

    if {$terminated} {
	notice $type "Job terminated by user"
    } else {
	notice $type "Job ended"
	global $viewOutImageVar
	if {$Type != "Utility" && $Type != "Srtm" && $Type != "Afidswdb" && $Type != "Afidswcibdb" && [set $viewOutImageVar]} {
	    switch $type {
		cib {
		    global CIB_OUT_IMAGE_NAME
		    set file basemos/$CIB_OUT_IMAGE_NAME
		}
		dted {
		    global DTED_OUT_IMAGE_NAME
		    set file dtedmos/$DTED_OUT_IMAGE_NAME
		}
		landsat {
		    global LANDSAT_OUT_IMAGE_NAME
		    set file $LANDSAT_OUT_IMAGE_NAME
		}
		default {
		    set file ""
		}
	    }
	    if {[file isfile $file] && [file readable $file] && [file size $file] > 0} {
		puts "xvd ${file}"
		global VDEV_DIR
		catch {exec ${VDEV_DIR}/xvd $file &} result
		puts "xvd returned \"${result}\""
	    } else {
		puts "XVD could not be called on \"${file}\" because it is not a readable file with non-zero length"
		puts "The current directory is [pwd]"
	    }
	}
    }

    status $type "Ready"

    if {! [catch {exec grep -sv write teeFile | egrep "(ABEND|ERROR|WARNING|MSG|COMPLETED)"} results]} {
	if {$results != ""} {
	    set results [split $results "\n"]
	    set results [last $results]
	    tk_messageBox -message $results -title "Job Status Log"
	}
    }

    exec rm -f teeFile
}

proc splitUse26a {use26a} {
    set split ""
    set split "${split}\nFIELD1: [string range $use26a 0 2]"
    set split "${split}\nFIELD2: [string range $use26a 3 7]"
    set split "${split}\nFIELD3: [string range $use26a 8 8]"
    set split "${split}\nFIELD4: [string range $use26a 9 13]"
    set split "${split}\nFIELD5: [string range $use26a 14 16]"
    set split "${split}\nFIELD6: [string range $use26a 17 17]"
    set split "${split}\nFIELD7: [string range $use26a 18 20]"
    set split "${split}\nFIELD8: [string range $use26a 21 25]"
    set split "${split}\nFIELD9: [string range $use26a 26 31]"
    set split "${split}\nFIELD10: [string range $use26a 32 43]"
    set split "${split}\nFIELD11: [string range $use26a 44 58]"
    set split "${split}\nFIELD12: [string range $use26a 59 62]"
    set split "${split}\nFIELD13: [string range $use26a 63 63]"
    set split "${split}\nFIELD14: [string range $use26a 64 66]"
    set split "${split}\nFIELD15: [string range $use26a 67 67]"
    set split "${split}\nFIELD16: [string range $use26a 68 68]"
    set split "${split}\nFIELD17: [string range $use26a 69 70]"
    set split "${split}\nFIELD18: [string range $use26a 71 75]"
    set split "${split}\nFIELD19: [string range $use26a 76 78]"
    set split "${split}\nFIELD20: [string range $use26a 79 84]"
    set split "${split}\nFIELD21: [string range $use26a 85 90]"
    set split "${split}\nFIELD22: [string range $use26a 91 96]"

    return $split
}

proc splitUse00a {use00a} {
    set split ""
    #  ANGLE_TO_NORTH  X 00003
    set split "${split}\nANGLE_TO_NORTH: [string range $use00a 0 2]"
    #  MEAN_GSD        X 00005
    set split "${split}\nMEAN_GSD: [string range $use00a 3 7]"
    #  FIELD3(RESERV)  X 00001
    set split "${split}\nFIELD3: [string range $use00a 8 8]"
    #  DYNAMIC_RANGE   X 00005
    set split "${split}\nDYNAMIC_RANGE: [string range $use00a 9 13]"
    #  FIELD5(RESERV)  X 00003
    set split "${split}\nFIELD5: [string range $use00a 14 16]"
    #  FIELD6(RESERV)  X 00001
    set split "${split}\nFIELD6: [string range $use00a 17 17]"
    #  FIELD7(RESERV)  X 00003
    set split "${split}\nFIELD7: [string range $use00a 18 20]"
    #  OBL_ANG         X 00005
    set split "${split}\nOBL_ANG: [string range $use00a 21 25]"
    #  ROLL_ANG        X 00006
    set split "${split}\nROLL_ANG: [string range $use00a 26 31]"
    #  FIELD10(RESERV) X 00012
    set split "${split}\nFIELD10: [string range $use00a 32 43]"
    #  FIELD11(RESERV) X 00015
    set split "${split}\nFIELD11: [string range $use00a 44 58]"
    #  FIELD12(RESERV) X 00004
    set split "${split}\nFIELD12: [string range $use00a 59 62]"
    #  FIELD13(RESERV) X 00001
    set split "${split}\nFIELD13: [string range $use00a 63 63]"
    #  FIELD14(RESERV) X 00003
    set split "${split}\nFIELD14: [string range $use00a 64 66]"
    #  FIELD15(RESERV) X 00001
    set split "${split}\nFIELD15: [string range $use00a 67 67]"
    #  FIELD16(RESERV) X 00001
    set split "${split}\nFIELD16: [string range $use00a 68 68]"
    #  N_REF           X 00002
    set split "${split}\nN_REF: [string range $use00a 69 70]"
    #  REV_NUM         X 00005
    set split "${split}\nREV_NUM: [string range $use00a 71 75]"
    #  N_SEG           X 00003
    set split "${split}\nN_SEG: [string range $use00a 76 78]"
    #  MAX_LP_SEG      X 00006
    set split "${split}\nMAX_LP_SEG: [string range $use00a 79 84]"
    #  FIELD20(RESERV) X 00006
    set split "${split}\nFIELD20: [string range $use00a 85 90]"
    #  FIELD21(RESERV) X 00006
    set split "${split}\nFIELD21: [string range $use00a 91 96]"
    #  SUN_EL          X 00005
    set split "${split}\nSUN_EL: [string range $use00a 97 101]"
    #  SUN_AZ          X 00005
    set split "${split}\nSUN_AZ: [string range $use00a 102 106]"

    return $split
}

proc findElevationMosaic {} {
    global chipperElevDataSource
    if {$chipperElevDataSource == "old"} {
	global nlcElevationDataMosaic

	return $nlcElevationDataMosaic
    }

    global SESSION_ID
    global nlcLogNtmOutputPrefix
    
    set text ${SESSION_ID}/${nlcLogNtmOutputPrefix}.txt
    set file [open $text "r"]
    set data [read $file]
    close $file

    set data [split $data "\n"]

    set minLat 90.0
    set maxLat -90.0
    set minLon 180.0
    set maxLon -180.0
    
    foreach nameValue $data {
	set split [split $nameValue "="]
	switch [lindex $split 0] {
	    NITF_CornerLat1 -
	    NITF_CornerLat2 -
	    NITF_CornerLat3 -
	    NITF_CornerLat4 {
		set lat [lindex $split 1]
		if {$lat > $maxLat} {
		    set maxLat $lat
		}
		if {$lat < $minLat} {
		    set minLat $lat
		}
	    }

	    NITF_CornerLon1 -
	    NITF_CornerLon2 -
	    NITF_CornerLon3 -
	    NITF_CornerLon4 {
		set lon [lindex $split 1]
		if {$lon > $maxLon} {
		    set maxLon $lon
		}
		if {$lon < $minLon} {
		    set minLon $lon
		}
	    }
	}
    }

    set fudge 0.2

    set minLat [expr int(floor($minLat - $fudge))]
    if {$minLat < -90} {
	set minLat -90
    }
    set maxLat [expr int(ceil($maxLat + $fudge))]
    if {$maxLat > 90} {
	set maxLat 90
    }
    set minLon [expr int(floor($minLon - $fudge))]
    if {$minLon < -180} {
	set minLon -180
    }
    set maxLon [expr int(ceil($maxLon + $fudge))]
    if {$maxLon > 180} {
	set maxLon 180
    }

    set mosaic [previouslyMosaickedElevationData $maxLat $maxLon $minLon $minLat]
    if {$mosaic != ""} {
	return $mosaic
    } else {
	global nlcElevationDataDirectory

	return [mosaicElevation $nlcElevationDataDirectory $maxLat $maxLon $minLon $minLat]
    }

    return ""
}

proc mosaicElevation {elevDir maxLat maxLon minLon minLat} {
    # find doesn't seem to follow the cdrom0 link, so follow it here, if there is one
    catch {
	set linkTarget [file link $elevDir]
	set split [split $linkTarget "/"]
	set linkTarget [last $split]

	set split [split $elevDir "/"]
	set split [removeLast $split]
	lappend split $linkTarget
	set elevDir [join $split "/"]
    }

    global dtedLevel
    if {[exec find $elevDir -name "*.dt2" -print] != ""} {
	set dtedLevel 2
    } elseif {[exec find $elevDir -name "*.dt1" -print] != ""} {
	set dtedLevel 1
    } else {
	set dtedLevel ""
    }

    global SESSION_ID

    if {[glob -nocomplain ${SESSION_ID}/srtmdteddb] != ""} {
	set file [open ${SESSION_ID}/srtmdteddb "r"]
	set srtmdteddb [read $file]
	close $file
    } else {
	set srtmdteddb {{elev1 0 0 0 0} {elev2 0 0 0 0} {elev3 0 0 0 0}}
    }

    set elevName [first [last $srtmdteddb]]
    set newMosaic [list [list $elevName $maxLat $maxLon $minLon $minLat]]
    set srtmdteddb [ListUtils::appendList $newMosaic [removeLast $srtmdteddb]]

    set cellsH [expr $maxLon - $minLon]
    set cellsV [expr $maxLat - $minLat]

    if {$dtedLevel == ""} {
	set srtmtraceLinesPerCell 5
	set srtmtraceEpilogueLines 4
	set mosSrtmtraceLines [expr $srtmtraceEpilogueLines + $srtmtraceLinesPerCell * $cellsH * $cellsV]
    }
    
    global nlcLogNtmOutputPrefix

    set outDir $SESSION_ID
    set outName $elevName

    set pdfFilename [FileUtils::makeTemp "nlc" ".pdf"]
    set file [open $pdfFilename "w"]

    puts $file "procedure"
    puts $file "body"
    puts $file "setlib-add library=(\$R2LIB)"
    puts $file "local afidsroot type=(string,128)"
    puts $file "translog AFIDS_ROOT afidsroot"
    puts $file "setlib-delete library=(\$R2LIB)"
    puts $file "setlib-add library=(&afidsroot/vdev,\$R2LIB)"

    if {$dtedLevel == ""} {
	puts $file "mos_l2_dem +"
	puts $file "  slat=$minLat +"
	puts $file "  slon=$minLon +"
	puts $file "  elat=$maxLat +"
	puts $file "  elon=$maxLon +"
	puts $file "  DIRin=\"$elevDir\" +"
	puts $file "  out=\"$outName\" +"
	puts $file "  DIRout=\"$outDir\""
    } else {
	file mkdir dtedraw
	puts $file "dtedquad2 +"
	puts $file "  qlon=($minLon,$maxLon) +"
	puts $file "  qlat=($minLat,$maxLat) +"
	puts $file "  out=\"${outDir}/${outName}\" +"
	puts $file "  diskimg=\"$elevDir\" +"
	puts $file "  key=\"$outDir\" +"
	puts $file "  level=\"$dtedLevel\""
    }

    puts $file "end-proc"

    close $file
    
    set logFilename [FileUtils::makeTemp "nlc" ".log"]

    if {$dtedLevel == ""} {
	# clean up any stray srtmtrace files
	set srtmtraceFiles [glob -nocomplain "srtmMosTrace*"]
	if {$srtmtraceFiles != ""} {
	    eval [concat file delete -force $srtmtraceFiles]
	}
    } else {
	# clean up any stray temp dted mosaic files
	set dtedtraceFiles [glob -nocomplain "dtedraw/*.log"]
	if {$dtedtraceFiles != ""} {
	    eval [concat file delete -force $dtedtraceFiles]
	}
    }

    # start mosaic
    set pid [exec vicarb $pdfFilename > $logFilename &]

    set meterValue 0.0
    set meterLabel "0%"

    if {$dtedLevel == ""} {
	set status "Opening SRTM"
    } else {
	set status "Opening DTED Level $dtedLevel"
    }
    set dialog [ProgressDialog \#auto -title "NLC: Mosaicking Elevation Data" \
	    -description "Mosaicking Elevation Data" -status $status -meter $meterValue -meterLabel $meterLabel -terminate "userTerminatingProcess" -userData $pid]

    set user [exec whoami]
    while {! [catch {exec ps -u$user -opid | grep $pid}]} {
	update

	if {$dtedLevel == ""} {
	    set lineCount [llength [glob -nocomplain "srtmMosTrace*"]]
	    set meterValue [expr $lineCount / double($mosSrtmtraceLines)]
	    set meterLabel "[expr int(100 * $meterValue)]%"
	    set srtmtraceLinesPerCell 5

	    if {$lineCount <= $mosSrtmtraceLines - $srtmtraceEpilogueLines} {
		# processing cells
		set col [expr $lineCount / $srtmtraceLinesPerCell / $cellsV]
		set row [expr ($lineCount - ($col * $srtmtraceLinesPerCell * $cellsV)) / $srtmtraceLinesPerCell]
		set status "Processing Cell at Lat [expr $maxLat - $row - 1] Lon [expr $minLon + $col]"
	    } else {
		# wrapping up
		set status "Creating Mosaic"
	    }
	} else {
	    set lineCount [llength [glob -nocomplain "dtedraw/*.log"]]
	    set meterValue [expr 0.9 * ($lineCount / double($cellsH))]
	    set meterLabel "[expr int(100 * $meterValue)]%"

	    if {$lineCount <= $cellsH} {
		set status "Processing Cells at Lon [expr $minLon + $lineCount - 1]"
	    } else {
		set status "Creating Mosaic"
	    }
	}

	if {[catch {$dialog setMeter $meterValue $meterLabel ; $dialog setStatus $status}]} {
	    tk_messageBox -title "Processing Terminated By User" -message "Processing Terminated By User"
	    return ""
	}
	after 1000
    }    

    itcl::delete object $dialog

    if {! [catch {exec grep -sv write $logFilename | egrep "COMPLETED"} results]} {
	# update srtmdteddb
	set file [open ${SESSION_ID}/srtmdteddb "w"]
	puts $file $srtmdteddb
	close $file

	file delete -force $pdfFilename
	file delete -force $logFilename

	if {$dtedLevel == ""} {
	    # clean up any stray srtmtrace files
	    set srtmtraceFiles [glob -nocomplain "srtmMosTrace*"]
	    if {$srtmtraceFiles != ""} {
		eval [concat file delete -force $srtmtraceFiles]
	    }
	}

	return ${SESSION_ID}/${elevName}_dem.img
    } else {
	tk_messageBox -title "Job Status Log" -message "Elevation Mosaicking Did Not Complete\nSee Script $pdfFilename and Log $logFilename"
	return ""
    }
}

proc previouslyMosaickedElevationData {n e w s} {
    global SESSION_ID
    if {[glob -nocomplain ${SESSION_ID}/srtmdteddb] != ""} {
	set file [open ${SESSION_ID}/srtmdteddb "r"]
	set srtmdteddb [read $file]
	close $file

	foreach mosaic $srtmdteddb {
	    set name [lindex $mosaic 0]
	    set nM [lindex $mosaic 1]
	    set eM [lindex $mosaic 2]
	    set wM [lindex $mosaic 3]
	    set sM [lindex $mosaic 4]

	    if {$n <= $nM && $e <= $eM && $s >= $sM && $w >= $wM && [file exists ${SESSION_ID}/${name}_dem.img]} {
		return ${SESSION_ID}/${name}_dem.img
	    }
	}
    }

    return ""
}

#  proc isReadableNonEmptyFile {file} {
#      if {[llength [glob -nocomplain $file]] > 0} {
#  	return 1
#      } else {
#  	return 0
#      }

#      # the rest of this won't work for 64 bit files in version <= 8.3

#      if {[file isfile $file]} {
#  	if {[file readable $file]} {
#  	    if {[file size $file] > 0} {
#  		return 1
#  	    }
#  	}
#      }

#      return 0
#  }

proc startCoregistration {Type xvdOnly RTYPE} {
    global ${Type}_fileEntries

    set quit 0

    # verify input file access
    foreach entryvarLabelPair [set ${Type}_fileEntries] {
	set entryvar [lindex $entryvarLabelPair 0]
	set label [lindex $entryvarLabelPair 1]
	global $entryvar

	puts "checking [set $entryvar]"

	if {[set $entryvar] != "" && [set $entryvar] != {""} && ! [FileUtils::isReadableNonEmptyFile [set $entryvar]]} {
	    if {$Type == "NTM" && $label == "CIB Image:" && [set $entryvar] == ""} {
		if {$RTYPE == "master"} {
		    global controlPointLine controlPointSamp controlPointLat controlPointLon
		    if {$controlPointLine == "" || $controlPointSamp == "" || $controlPointLat == "" || $controlPointLon == ""} {
			tk_messageBox -message "Either The CIB base or Optional Control Point Must Be Specified"
			return
		    }
		}
	    } else {
		tk_messageBox -message "Error reading $label [set $entryvar]"
		set quit 1
	    }
	}
    }

    if {$quit} {
#	return
    }

    setupCoregistrationRun $Type $xvdOnly $RTYPE
    set type [string tolower $Type]

    notice $type "Ensuring taetm is not running"

    if {[llength [taetmRunning]] > 0} {
	if {[promptUser "Terminate All VICARS?" "There is a VICAR job running that must be\nterminated before starting a new one.\nContinue with termination?" {Yes No}] == "No"} {
	    return
	}
    }

    killTaetm $type

    global terminated
    set terminated 0
    notice $type "Starting job"

    if {[string first "landsat" $type] < 0} {
	puts "Running ${type}run.pdf"
	exec vicarb ${type}run.pdf | tee teeFile &
    } else {
	puts "Running ${type}coregrun.pdf"
	exec vicarb ${type}coregrun.pdf | tee teeFile &
    }

    notice $type ""
    while {[llength [taetmRunning]]} {
	status $type "Running" green
	status $type "Running." green 
	status $type "Running.." green
	status $type "Running..." green
	after 1000
    }

    set itCompleted true

    if {! [catch {exec grep -sv write teeFile | egrep "(ABEND|ERROR|WARNING|MSG|COMPLETED)"} results]} {
	if {$results != ""} {
	    set itCompleted false
	    tk_messageBox -message $results -title "Job Status Log"
	}
    }

    exec rm -f teeFile

    if {$terminated} {
	set itCompleted false
	notice $type "Job terminated by user"
	set terminated 0
    } else {
	notice $type "Job ended"
    }

    status $type "Ready"

    # the intent here was to enable the review button if/when the job completed, but it's not disabled now
#      if {$itCompleted} {
#  	global reviewButton${Type}

#  	[set reviewButton[set Type]] configure -state normal
#      }
}

proc killTaetm {type} {
    set pids [taetmRunning]

    if {[llength $pids] == 1} {
	notice $type "Terminating taetm process"
    } elseif {[llength $pids] > 1} {
	notice $type "Terminating [llength $pids] taetm processes"
    }

    global terminated
    set terminated 1
    foreach pid $pids {
	exec kill -9 $pid
    }
}

proc taetmRunning {} {
    after 1000
    if {[catch {exec ps -u[exec whoami] | grep taetm} result]} {
	return {}
    }

    set pids {}
    foreach line [split $result \n] {
	lappend pids [lindex $line 0]
    }

    return $pids
}

set promptUserCounter 0
proc promptUser {title prompt choices} {
    set w .promptUserWin

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w; set promptUserVar {}"
    wm title $w $title
    wm iconname $w $title
    
    label $w.label -text $prompt
    pack $w.label -fill both -expand true -side top

    frame $w.buttons
    pack $w.buttons -fill both -side top

    set bNum 0
    foreach choice $choices {
	set label [lindex $choice 0]
	set retval [lindex $choice 1]
	if {$retval == ""} {
	    set retval $label
	}
	button $w.buttons.b_${bNum} -text $label -command "set promptUserVar $retval; destroy $w"
	pack $w.buttons.b_${bNum} -fill both -side left -expand true
	incr bNum
    }

    # promptUseCounter is used to deal with reentrant calls to
    # promptUser. If it changes while tkwaiting, another call to
    # promptUser must have been made, so return nothing, rather than
    # the user value, allowing the later call to preempt this one.
    global promptUserCounter
    incr promptUserCounter
    set localCounter $promptUserCounter

    # promptUserVar is set by each command button created above
    global promptUserVar
    tkwait variable promptUserVar

    if {$promptUserCounter == $localCounter} {
	return $promptUserVar
    } else {
	return ""
    }
}

proc utilities {} {
    global guiFlavor

    set title "Select Utility Operation"
    set w .utilitiesWin

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set f $w.optionFrame
    frame $f
    pack $f -side top -anchor w
    
    set row 0

    set l $f.label_0
    label $l -text "PROCESS:"
    grid $l -row $row -column 0 -sticky w

    global utilityType
    set utilityType cutSubimageVicarOut

    if {$guiFlavor == "nlc"} {
	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "Create Entity / Non-Entity Display" -variable utilityType -value createEntityNonEntityDisplay -anchor w -state disabled
	grid $rb -row $row -column 0 -sticky w
    }

    incr row

    set rb $f.rb_0_${row}
    radiobutton $rb -text "Cut VICAR Sub-Image (VICAR Output)" -variable utilityType -value cutSubimageVicarOut -anchor w
    grid $rb -row $row -column 0 -sticky w

    incr row

    set rb $f.rb_0_${row}
    radiobutton $rb -text "Cut VICAR Sub-Image (GeoTIFF Output)" -variable utilityType -value cutSubimageGeoTIFFOut -anchor w
    grid $rb -row $row -column 0 -sticky w

    if {$guiFlavor == "afids"} {
	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "Remap VICAR To Existing GeoTIFF Reference" -variable utilityType -value cutToGeoTIFFReference -anchor w
	grid $rb -row $row -column 0 -sticky w

	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "Correlation Correction GeoTIFF to GeoTIFF" -variable utilityType -value correlationCorrection -anchor w
	grid $rb -row $row -column 0 -sticky w

	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "Convert NTM to GeoTIFF" -variable utilityType -value ntmToGeoTiff -anchor w
	grid $rb -row $row -column 0 -sticky w

	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "Convert RPC To GeoTIFF Map" -variable utilityType -value rpcToGeoTIFFMap -anchor w
	grid $rb -row $row -column 0 -sticky w

	incr row

	set rb $f.rb_0_${row}
	radiobutton $rb -text "RID Connected Component Analysis" -variable utilityType -value ridca -anchor w
	grid $rb -row $row -column 0 -sticky w

#  	incr row

#  	set rb $f.rb_0_${row}
#  	radiobutton $rb -text "NTM Logger/Chipper" -variable utilityType -value chipper -anchor w
#  	grid $rb -row $row -column 0 -sticky w
    }

    set l $f.label_space_0
    label $l -text " "
    grid $l -row 0 -column 1 -sticky w

    set row 0

    set l $f.label_1
    label $l -text "DISPLAY:"
    grid $l -row $row -column 2 -sticky w

    incr row

    set rb $f.rb_1_${row}
    radiobutton $rb -text "Display image" -variable utilityType -value displayImage -anchor w
    grid $rb -row $row -column 2 -sticky w

    if {$guiFlavor == "afids"} {
	incr row

	set rb $f.rb_1_${row}
	radiobutton $rb -text "Display image vs. base or master" -variable utilityType -value accuracyVsBaseOrMaster -anchor w
	grid $rb -row $row -column 2 -sticky w

	incr row

	set rb $f.rb_1_${row}
	radiobutton $rb -text "Display image vs. water mask" -variable utilityType -value imageVsWaterMask -anchor w
	grid $rb -row $row -column 2 -sticky w

	incr row

	set rb $f.rb_1_${row}
	radiobutton $rb -text "Display RMS 90% vectors" -variable utilityType -value rms90Vectors -anchor w
	grid $rb -row $row -column 2 -sticky w

	incr row

	set rb $f.rb_1_${row}
	radiobutton $rb -text "Display image with RMS 90% vectors" -variable utilityType -value imageWithRms90Vectors -anchor w
	grid $rb -row $row -column 2 -sticky w
    }

    set l $f.label_space_1
    label $l -text " "
    grid $l -row 0 -column 3 -sticky w

    set row 0

    set l $f.label_2
    label $l -text "INFO:"
    grid $l -row $row -column 4 -sticky w

    if {$guiFlavor == "afids"} {
	incr row

	set rb $f.rb_2_${row}
	radiobutton $rb -text "RMS 90% (pixels)" -variable utilityType -value rmsPixels -anchor w
	grid $rb -row $row -column 4 -sticky w

	incr row

	set rb $f.rb_2_${row}
	radiobutton $rb -text "CEP 90% (pixels)" -variable utilityType -value cepPixels -anchor w
	grid $rb -row $row -column 4 -sticky w

	incr row

	set rb $f.rb_2_${row}
	radiobutton $rb -text "Satellite pointing error" -variable utilityType -value satPointError -anchor w
	grid $rb -row $row -column 4 -sticky w
    }

    incr row

    set rb $f.rb_2_${row}
    radiobutton $rb -text "GeoTIFF Label" -variable utilityType -value geotiffLabel -anchor w
    grid $rb -row $row -column 4 -sticky w

    incr row

    set rb $f.rb_2_${row}
    radiobutton $rb -text "VICAR Mapping" -variable utilityType -value vicarMapping -anchor w
    grid $rb -row $row -column 4 -sticky w

    incr row

    set rb $f.rb_2_${row}
    radiobutton $rb -text "NITF Geometry" -variable utilityType -value nitfGeometry -anchor w
    grid $rb -row $row -column 4 -sticky w

    set bf $w.bottomButtons
    frame $bf
    pack $bf -side top -anchor w -fill both
    grid columnconfigure $bf 0 -weight 1
    grid columnconfigure $bf 1 -weight 1

    set b $bf.utilitiesButton
    button $b -text "Setup ..." -command "setupUtilities"
    grid $b -row 0 -column 0 -sticky e

    set b $bf.exitButton
    button $b -text "Close" -command "destroy $w"
    grid $b -row 0 -column 1 -sticky w
}

proc setupUtilities {} {
    global utilityType
    switch $utilityType {
	ridca {
	    ridca
	    return
	}
	rpcToGeoTIFFMap {
	    createRpcToGeoTIFFMapDialog
	    return
	}

	# These three come from the chipper gui
	chipEntireImage {
	    set title "Chip Entire Image"
	}
	chipPercentage {
	    set title "Chip Random"
	}
	chipSelected {
	    set title "Chip Targets"
	}

	nlcLogNtm {
	    set title "NTM Logger"
	}
	nlcLogQuickbird {
	    set title "Quickbird Logger"
	}
	nlcLogSpot {
	    set title "SPOT Logger"
	}
	nlcLogIkonos {
	    set title "Ikonos Logger"
	}
	chipper {
	    set title "NTM Logger/Chipper"
	}
	cutSubimageVicarOut {
	    set title "Cut Sub-Image (Vicar Output)"
	}
	cutSubimageGeoTIFFOut {
	    set title "Cut Sub-Image (GeoTIFF Output)"
	}
	cutToGeoTIFFReference {
	    set title "Cut To Match GeoTIFF"
	}
	correlationCorrection {
	    set title "Correlation Correction"
	}
	rmsPixels {
	    set title "RMS 90% (pixels)"
	}
	nitfGeometry {
	    set title "NITF Geometry"
	}
	geotiffLabel {
	    set title "GeoTIFF Label"
	}
	vicarMapping {
	    set title "VICAR Mapping"
	}
	accuracyVsBaseOrMaster {
	    set title "Image vs. Base or Master"
	}
	imageVsWaterMask {
	    set title "Image vs. Water Mask"
	}
	displayImage {
	    set title "Display Image"
	}
	rms90Vectors {
	    set title "RMS 90% Vectors"
	}
	imageWithRms90Vectors {
	    set title "Image with RMS 90% Vectors"
	}
	cepPixels {
	    set title "CEP 90% Pixels"
	}
	satPointError {
	    set title "Satellite Ephemeris Model Pointing Error"
	}
	ntmToGeoTiff {
	    set title "NTM To GeoTIFF"
	}
	default {
	    set title ""
	}
    }

    set w .utilitiesSetupWin

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    set l $w.label_1
    label $l -text "SOURCE/DESTINATION SELECTION"
    pack $l -side top -anchor w

    set f $w.fileFrame
    frame $f -bd 1 -relief solid
    pack $f -side top -fill x

    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "Input Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable utilityInputImage -width 40
    global utilityInputImage

    global nlcLogNtmOutputPrefix
    if {$nlcLogNtmOutputPrefix != "" &&
	($utilityType == "chipEntireImage" ||
	 $utilityType == "chipPercentage" ||
	 $utilityType == "chipSelected")} {
	global SESSION_ID
	set utilityInputImage "${SESSION_ID}/${nlcLogNtmOutputPrefix}.img"
    } else {
	set utilityInputImage ""
    }
    grid $e -row $row -column 1 -sticky news

    set mb $f.default${row}
    if {$utilityType == "correlationCorrection"} {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage geoTIFF"
    } elseif {$utilityType == "ntmToGeoTiff" ||
	      $utilityType == "chipper" ||
	      $utilityType == "chipEntireImage" ||
	      $utilityType == "chipPercentage" ||
	      $utilityType == "chipSelected" ||
	      $utilityType == "nlcLogNtm"} {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage ntf"
    } elseif {$utilityType == "displayImage"} {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage geoTiffVicar"
    } elseif {$utilityType == "nlcLogIkonos" ||
	      $utilityType == "nlcLogQuickbird" ||
	      $utilityType == "nlcLogSpot" } {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage tifntf"
    } elseif {$utilityType == "nitfGeometry"} {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage ntf"
    } else {
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityInputImage img"
    }
    grid $mb -row $row -column 2 -sticky news

    if {$utilityType == "chipPercentage" || $utilityType == "chipSelected"} {
	incr row

	set subf $f.subf${row}
	frame $subf
	grid $subf -row $row -column 1 -sticky news

	set b $subf.viewButton
	button $b -text "View" -command "viewUtilityInput"
	pack $b -side left -anchor w

	set l $subf.label${row}_2
	if {$utilityType == "chipPercentage"} {
	    label $l -text "to determine image quality" -font italic
	} else {
	    label $l -text "to locate targets" -font italic
	}
	pack $l -side left -anchor w
    }

    if {$utilityType == "cutToGeoTIFFReference" || $utilityType == "correlationCorrection"} {
	incr row

	set l $f.label${row}
	label $l -text "Reference Image:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable utilityReferenceImage -width 40
	global utilityReferenceImage
	set utilityReferenceImage ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen utility utilityReferenceImage geoTIFF"
	grid $mb -row $row -column 2 -sticky news
    }

    if {$utilityType == "chipper" || $utilityType == "chipEntireImage" || $utilityType == "chipPercentage" || $utilityType == "chipSelected"} {
	incr row

	set l $f.label${row}
	label $l -text "Elevation Image:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${utilityType}_elevInputImage -width 40
	
	global ${utilityType}_elevInputImage
	set ${utilityType}_elevInputImage ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	button $mb -text "Browse ..." -command "getFileForOpen $utilityType ${utilityType}_elevInputImage elev"
	grid $mb -row $row -column 2 -sticky news
    }

    if {$utilityType == "nlcLogQuickbird" || $utilityType == "nlcLogSpot" || $utilityType == "nlcLogIkonos"} {
	incr row

	set l $f.label${row}
	switch $utilityType {
	    "nlcLogQuickbird" {
		set Type "Quickbird"
	    }
	    "nlcLogSpot" {
		set Type "SPOT"
	    }
	    "nlcLogIkonos" {
		set Type "Ikonos"
	    }
	}
	label $l -text "$Type Metadata:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_metadataFile -width 40

	lappend ${Type}_fileEntries [list ${Type}_metadataFile [$l cget -text]]

	global ${Type}_metadataFile
	set ${Type}_metadataFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	switch $Type {
	    "Quickbird" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile imd"
	    }
	    "Ikonos" {
		set cmd "getFileForOpen $Type ${Type}_metadataFile txt"
	    }
	}
	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$utilityType == "nlcLogIkonos"} {
	set Type "Ikonos"

	incr row

	set l $f.label${row}
	label $l -text "$Type Header:"
	grid $l -row $row -column 0 -sticky e

	set e $f.entry${row}
	entry $e -textvariable ${Type}_headerFile -width 40

	lappend ${Type}_fileEntries [list ${Type}_headerFile [$l cget -text]]

	global ${Type}_headerFile
	set ${Type}_headerFile ""
	grid $e -row $row -column 1 -sticky news

	set mb $f.default${row}
	set cmd "getFileForOpen $Type ${Type}_headerFile hdr"
	button $mb -text "Browse ..." -command $cmd
	grid $mb -row $row -column 2 -sticky news
    }

    if {$utilityType == "cutSubimageVicarOut" ||
	$utilityType == "chipper" ||
	$utilityType == "chipEntireImage" || $utilityType == "chipPercentage" || $utilityType == "chipSelected" ||
	$utilityType == "cutSubimageGeoTIFFOut" ||
	$utilityType == "cutToGeoTIFFReference" ||
	$utilityType == "correlationCorrection" ||
	$utilityType == "ntmToGeoTiff" ||
	$utilityType == "nlcLogNtm" ||
	$utilityType == "nlcLogIkonos" ||
	$utilityType == "nlcLogQuickbird" ||
	$utilityType == "nlcLogSpot"} {
	incr row

	set l $f.label${row}
	if {$utilityType == "chipper" ||
	    $utilityType == "chipEntireImage" || $utilityType == "chipPercentage" || $utilityType == "chipSelected" ||
	    $utilityType == "nlcLogNtm" ||
	    $utilityType == "nlcLogQuickbird" ||
	    $utilityType == "nlcLogSpot" ||
	    $utilityType == "nlcLogIkonos"} {
	    label $l -text "Output Prefix:"
	} else {
	    label $l -text "Output Image:"
	}
	grid $l -row $row -column 0 -sticky e

	if {$utilityType == "nlcLogNtm"} {
	    set e $f.entry${row}
	    entry $e -textvariable nlcLogNtmOutputPrefix -width 40
	    global nlcLogNtmOutputPrefix
	    set nlcLogNtmOutputPrefix ""
	    grid $e -row $row -column 1 -sticky news
	} else {
	    set e $f.entry${row}
	    entry $e -textvariable utilityOutputImage -width 40
	    global utilityOutputImage

	    if {$nlcLogNtmOutputPrefix != "" &&
		($utilityType == "chipEntireImage" ||
		 $utilityType == "chipPercentage" ||
		 $utilityType == "chipSelected")} {
		global nlcLogNtmOutputPrefix
		set utilityOutputImage $nlcLogNtmOutputPrefix
	    } else {
		set utilityOutputImage ""
	    }

	    grid $e -row $row -column 1 -sticky news
	}

	if {$utilityType != "chipper" &&
	    $utilityType != "nlcLogNtm" &&
	    $utilityType != "nlcLogQuickbird" &&
	    $utilityType != "nlcLogSpot" &&
	    $utilityType != "nlcLogIkonos" &&
	    $utilityType != "chipEntireImage" &&
	    $utilityType != "chipPercentage" &&
	    $utilityType != "chipSelected"} {

	    set mb $f.default${row}

	    switch $utilityType {
		cutSubimageVicarOut {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage img"
		}
		chipper - chipEntireImage - chipPercentage {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage prefix"
		}
		cutSubimageGeoTIFFOut {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage geoTIFF"
		}
		cutToGeoTIFFReference {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage geoTIFF"
		}
		correlationCorrection {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage geoTIFF"
		}
		ntmToGeoTiff {
		    button $mb -text "Browse ..." -command "getFileForSave utility utilityOutputImage geoTIFF"
		}
	    }

	    grid $mb -row $row -column 2 -sticky news
	}

	if {$utilityType == "chipPercentage"} {
		set l $w.labelSpace
		label $l -text " "
		pack $l -side top -anchor w

		incr row

		set l $w.labelTitle
		label $l -text "TARGET SELECTION"
		pack $l -side top -anchor w

		set targFrame [createTargetSelectionFrame $w]
		pack $targFrame -side top -fill both
	}

	if {$utilityType == "chipSelected"} {
		set l $w.labelSpace
		label $l -text " "
		pack $l -side top -anchor w

		incr row

		set l $w.labelTitle
		label $l -text "TARGET SELECTION"
		pack $l -side top -anchor w

		set targFrame [createTargetSelectionFrame2 $w]
		pack $targFrame -side top -fill both
	}

	switch $utilityType {
	    "cutSubimageVicarOut" - "cutSubimageGeoTIFFOut" - "chipper" - "chipEntireImage" {
		set l $w.labelSpace
		label $l -text " "
		pack $l -side top -anchor w

		incr row

		set l $w.labelTitle
		label $l -text "SUB IMAGE AREA"
		pack $l -side top -anchor w

		set aoiFrame [createAOIframe $w Utility]
		pack $aoiFrame -side top -anchor w
	    }

	    "correlationCorrection" {
		incr row

		set l $f.label${row}
		label $l -text "Cut To Cover: "
		grid $l -row $row -column 0 -sticky e

		set rf $f.radioFrame1
		frame $rf
		grid $rf -row $row -column 1 -sticky w

		set rb $rf.rb1
		radiobutton $rb -text "Reference" -variable typref -value coverref -command "$rf.l config -state disabled ; $rf.entry config -state disabled"
		pack $rb -side left -anchor w
		global typref
		set typref coverref

		set rb $rf.rb2
		radiobutton $rb -text "Input" -variable typref -value coverinp -command "$rf.l config -state normal ; $rf.entry config -state normal"
		pack $rb -side left -anchor w

		set l $rf.l
		label $l -text "Gore Width:" -state disabled
		pack $l -side left -anchor w

		set e $rf.entry
		entry $e -textvariable Utility_GoreWidth -width 5 -state disabled
		pack $e -side left -anchor w
		global Utility_GoreWidth
		set Utility_GoreWidth 2

		incr row

		set l $f.label${row}
		label $l -text "Interpolation: "
		grid $l -row $row -column 0 -sticky e

		set rf $f.radioFrame2
		frame $rf
		grid $rf -row $row -column 1 -sticky w

		set rb $rf.rb2
		radiobutton $rb -text "Bilinear" -variable interp -value bilin
		pack $rb -side left -anchor w
		global interp
		set interp bilin

		set rb $rf.rb1
		radiobutton $rb -text "Nearest Neighbor" -variable interp -value noin
		pack $rb -side left -anchor w

		incr row

		set l $f.label${row}
		label $l -text "Poly Fit: "
		grid $l -row $row -column 0 -sticky e

		set rf $f.radioFrame3
		frame $rf
		grid $rf -row $row -column 1 -sticky w

		set rb $rf.rb1
		radiobutton $rb -text "Piecewise Linear" -variable polyfit -value pwl
		pack $rb -side left -anchor w
		global polyfit
		set polyfit pwl

		set rb $rf.rb2
		radiobutton $rb -text "Linear" -variable polyfit -value linear
		pack $rb -side left -anchor w

		set rb $rf.rb3
		radiobutton $rb -text "Keystone" -variable polyfit -value keystone
		pack $rb -side left -anchor w

		set rb $rf.rb4
		radiobutton $rb -text "Quadratic" -variable polyfit -value quad
		pack $rb -side left -anchor w

		set rb $rf.rb5
		radiobutton $rb -text "Cubic" -variable polyfit -value cubic
		pack $rb -side left -anchor w

		incr row

		set ff $f.frame${row}
		frame $ff
		grid $ff -row $row -column 0 -sticky w
		grid configure $ff -columnspan 2

		set l $ff.label
		label $l -text "Initial FFT Window Size (pixels, range 32-1024):"
		pack $l -side left -anchor w
		
		set e $ff.entry
		entry $e -textvariable Utility_fftSize -width 6
		pack $e -side left -anchor w
		global Utility_fftSize
		set Utility_fftSize "64"

		incr row

		set ff $f.frame${row}
		frame $ff
		grid $ff -row $row -column 0 -sticky w
		grid configure $ff -columnspan 2

		set l $ff.label
		label $l -text "FFT Footprint Zoom (range 0.0 to 5000.0):"
		pack $l -side left -anchor w
		
		set e $ff.entry
		entry $e -textvariable Utility_magnify -width 6
		pack $e -side left -anchor w
		global Utility_magnify
		set Utility_magnify "1.0"
		
		incr row

		set ff $f.frame${row}
		frame $ff
		grid $ff -row $row -column 0 -sticky w
		grid configure $ff -columnspan 2

		set l $ff.label${row}
		label $l -text "Correlation Tolerance (pixels, range 0.01 to 20.0):"
		pack $l -side left -anchor w
		
		incr row

		set l $f.label${row}
		label $l -text "Grid Fineness (range 100 to 4000)"
		grid $l -row $row -column 0 -sticky w
		grid configure $l -columnspan 3

		incr row

		set l $f.label${row}
		label $l -text "Horizontal:"
		grid $l -row $row -column 0 -sticky e

		set e $f.entry${row}_2
		entry $e -textvariable Utility_NAH -width 5
		grid $e -row $row -column 1 -sticky w

		incr row

		set l $f.label${row}_2
		label $l -text "Vertical:"
		grid $l -row $row -column 0 -sticky e

		set e $f.entry${row}_3
		entry $e -textvariable Utility_NAV -width 5
		grid $e -row $row -column 1 -sticky w

		global Utility_NAH
		global Utility_NAV
		set Utility_NAH 100
		set Utility_NAV 100

		incr row

		set e $ff.entry${row}
		entry $e -textvariable Utility_tolerance -width 5
		pack $e -side left -anchor w
		global Utility_tolerance
		set Utility_tolerance "3.0"
		
	    }
	}
    }

    if {$utilityType == "chipper" ||
	$utilityType == "nlcLogNtm" ||
	$utilityType == "nlcLogQuickbird" ||
	$utilityType == "nlcLogSpot" ||
	$utilityType == "nlcLogIkonos" ||
	$utilityType == "chipEntireImage" ||
	$utilityType == "chipPercentage" ||
	$utilityType == "chipSelected"} {
	set l $w.labelSpace_chip
	label $l -text " "
	pack $l -side top -anchor w

	set l $w.labelChipTitle
	if {$utilityType == "chipper" || $utilityType == "chipEntireImage" || $utilityType == "chipPercentage" || $utilityType == "chipSelected"} {
	    label $l -text "CHIPPER PARAMETERS"
	} else {
	    label $l -text "LOGGING PARAMETERS"
	}
	pack $l -side top -anchor w

	set f $w.chipperParmFrame
	frame $f -bd 1 -relief solid
	pack $f -side top -anchor w -fill both
	grid columnconfigure $f 0 -weight 0
	grid columnconfigure $f 1 -weight 0
	grid columnconfigure $f 2 -weight 0
	grid columnconfigure $f 3 -weight 0
	grid columnconfigure $f 4 -weight 1

	set row 0

	if {$utilityType == "chipper" || $utilityType == "chipEntireImage" || $utilityType == "chipPercentage" || $utilityType == "chipSelected"} {
	    set l $f.label${row}
	    label $l -text "Output Lines:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable chipperOutputLines -width 20
	    global chipperOutputLines
	    set chipperOutputLines "1024"
	    grid $e -row $row -column 1 -sticky news

	    incr row

	    set l $f.label${row}
	    label $l -text "Output Samples:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable chipperOutputSamples -width 20
	    global chipperOutputSamples
	    set chipperOutputSamples "1024"
	    grid $e -row $row -column 1 -sticky news

	    incr row

	    set l $f.label${row}
	    label $l -text "Overlap:"
	    grid $l -row $row -column 0 -sticky e

	    set e $f.entry${row}
	    entry $e -textvariable chipperOverlap -width 20
	    global chipperOverlap
	    set chipperOverlap "128"
	    grid $e -row $row -column 1 -sticky news
	}

	if {$utilityType == "chipper" || $utilityType == "nlcLogNtm" } {
	    set row 0

	    if {$utilityType == "chipper"} {
		set l $f.label${row}_1
		label $l -text "Target Line:"
		grid $l -row $row -column 2 -sticky e

		set e $f.entry${row}_1
		entry $e -textvariable chipperTargetLine -width 20
		global chipperTargetLine
		set chipperTargetLine "0"
		grid $e -row $row -column 3 -sticky w

		incr row

		set l $f.label${row}_1
		label $l -text "Target Sample:"
		grid $l -row $row -column 2 -sticky e

		set e $f.entry${row}_1
		entry $e -textvariable chipperTargetSample -width 20
		global chipperTargetSample
		set chipperTargetSample "0"
		grid $e -row $row -column 3 -sticky w

		incr row
	    }

	    set l $f.label${row}_1
	    label $l -text "TIFF:"
	    grid $l -row $row -column 2 -sticky e

	    set f2 $f.tiffButtonFrame
	    frame $f2
	    grid $f2 -row $row -column 3 -sticky w

	    set rb $f2.bBoth
	    radiobutton $rb -text "Both" -variable runNtmTiff -value b
	    pack $rb -side left -anchor w
	    global runNtmTiff
	    set runNtmTiff b
	    set rb $f2.bSmall
	    radiobutton $rb -text "Small" -variable runNtmTiff -value s
	    pack $rb -side left -anchor w
	    set rb $f2.bFullSize
	    radiobutton $rb -text "Full-Size" -variable runNtmTiff -value f
	    pack $rb -side left -anchor w
	    set rb $f2.bNone
	    radiobutton $rb -text "None" -variable runNtmTiff -value n
	    pack $rb -side left -anchor w
	}

# 	global UTILITY_AOI_MIN_LAT
# 	global UTILITY_AOI_MAX_LAT
# 	global UTILITY_AOI_MIN_LON
# 	global UTILITY_AOI_MAX_LON
# 	set UTILITY_AOI_MIN_LAT 0
# 	set UTILITY_AOI_MAX_LAT 1
# 	set UTILITY_AOI_MIN_LON 1
# 	set UTILITY_AOI_MAX_LON 0
    }

    set l $w.labelSpace_2
    label $l -text " "
    pack $l -side top -anchor w

    set l $w.label_42
    label $l -text "UTILITY EXECUTION"
    pack $l -side top -anchor w

    if {$utilityType == "cutSubimageVicarOut" || \
	$utilityType == "cutSubimageGeoTIFFOut" || \
	$utilityType == "cutToGeoTIFFReference" || \
	$utilityType == "correlationCorrection" || \
	$utilityType == "ntmToGeoTiff" || \
	    $utilityType == "nlcLogNtm" || $utilityType == "nlcLogQuickbird" || $utilityType == "nlcLogSpot" || $utilityType == "nlcLogIkonos"} {
	set processingFrame [createMosaicProcessingFrame $w Utility]
    } else {
	set processingFrame [createMosaicProcessingFrame $w Utility noxvd]
    }
    pack $processingFrame -side top -fill x

    set b $w.close
    global chipperStandalone
    if {$chipperStandalone} {
	button $b -text "Exit" -command "exit"
    } else {
	button $b -text "Close" -command "destroy $w"
    }
    pack $b -side top
}

set nlcLogNtmOutputPrefix ""
set chipperStandalone 0

proc ridca {} {
    set title "Registered Image Difference Connected Component Analysis"
    set w .ridca

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    wm title $w $title
    wm iconname $w $title

    # Date (input) Frame
    set f $w.dateFrame
    frame $f
    pack $f -side top -fill both -expand true -anchor nw
    grid columnconfigure $f 1 -weight 1

    set row 0

    set l $f.label${row}
    label $l -text "INPUT IMAGES"
    grid $l -row $row -column 0
    grid configure $l -columnspan 3

    incr row

    set l $f.label${row}
    label $l -text "First Date:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ridcaFirstDate -width 40
    global ridcaFirstDate
    set ridcaFirstDate ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.browse${row}
    button $mb -text "Browse ..." -command "getFileForOpen utility ridcaFirstDate img"
    grid $mb -row $row -column 2 -sticky news

    incr row

    set l $f.label${row}
    label $l -text "Second Date:"
    grid $l -row $row -column 0 -sticky e

    set e $f.entry${row}
    entry $e -textvariable ridcaSecondDate -width 40
    global ridcaSecondDate
    set ridcaSecondDate ""
    grid $e -row $row -column 1 -sticky news

    set mb $f.browse${row}
    button $mb -text "Browse ..." -command "getFileForOpen utility ridcaSecondDate img"
    grid $mb -row $row -column 2 -sticky news

    # First parm frame (three columns wide)
    set f $w.parmFrame1
    frame $f
    pack $f -side top -fill both -expand true

    # First frame, column 0
    set f $w.parmFrame1.col0
    frame $f -relief solid -bd 1
    pack $f -side left -fill both -expand true -anchor n
    grid rowconfigure $f 4 -weight 1

    set row 0

    set l $f.titleLabel
    label $l -text "THRESHOLDS"
    grid $l -row $row -column 0
    grid configure $l -columnspan 2

    incr row

    set l $f.lightLabel
    label $l -text "Plus:"
    grid $l -row $row -column 0 -sticky e

    set e $f.lightThreshold
    entry $e -textvariable lightThreshold -width 10
    global lightThreshold
    set lightThreshold ""
    grid $e -row $row -column 1 -sticky news
    bind $e <FocusOut> "set darkThreshold -\$lightThreshold"

    incr row

    set l $f.darkLabel
    label $l -text "Minus:"
    grid $l -row $row -column 0 -sticky e

    set e $f.darkThreshold
    entry $e -textvariable darkThreshold -width 10
    global darkThreshold
    set darkThreshold ""
    grid $e -row $row -column 1 -sticky news

    incr row

    set l $f.shadowLabel
    label $l -text "Shadow:"
    grid $l -row $row -column 0 -sticky e

    set e $f.shadowThreshold
    entry $e -textvariable shadowThreshold -width 10
    global shadowThreshold
    set shadowThreshold "0"
    grid $e -row $row -column 1 -sticky news

    # First frame, column 1
    set f $w.parmFrame1.col1
    frame $f -relief solid -bd 1
    pack $f -side left -fill both -expand true -anchor n
    grid rowconfigure $f 5 -weight 1

    set row 0

    set l $f.titleLabel
    label $l -text "FILTERS"
    grid $l -row $row -column 0
    grid configure $l -columnspan 3

    incr row

    set l $f.min
    label $l -text "Min"
    grid $l -row $row -column 1

    set l $f.max
    label $l -text "Max"
    grid $l -row $row -column 2

    incr row

    set l $f.areaLabel
    label $l -text "Area:"
    grid $l -row $row -column 0 -sticky e

    set e $f.areaMin
    entry $e -textvariable areaMin -width 10
    global areaMin
    set areaMin ""
    grid $e -row $row -column 1 -sticky news

    set e $f.areaMax
    entry $e -textvariable areaMax -width 10
    global areaMax
    set areaMax ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.diameterLabel
    label $l -text "Diameter:"
    grid $l -row $row -column 0 -sticky e

    set e $f.diameterMin
    entry $e -textvariable diameterMin -width 10
    global diameterMin
    set diameterMin ""
    grid $e -row $row -column 1 -sticky news

    set e $f.diameterMax
    entry $e -textvariable diameterMax -width 10
    global diameterMax
    set diameterMax ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.ratioLabel
    label $l -text "A/D Ratio:"
    grid $l -row $row -column 0 -sticky e

    set e $f.ratioMin
    entry $e -textvariable ratioMin -width 10
    global ratioMin
    set ratioMin ""
    grid $e -row $row -column 1 -sticky news

    set e $f.ratioMax
    entry $e -textvariable ratioMax -width 10
    global ratioMax
    set ratioMax ""
    grid $e -row $row -column 2 -sticky news

    # First frame, column 2
    set f $w.parmFrame1.col2
    frame $f -relief solid -bd 1
    pack $f -side left -fill both -expand true -anchor n
    grid rowconfigure $f 5 -weight 1

    set row 0

    set l $f.titleLabel
    label $l -text "SPECTRAL FILTERS"
    grid $l -row $row -column 0
    grid configure $l -columnspan 3

    incr row

    set l $f.fg
    label $l -text "Foreground"
    grid $l -row $row -column 0

    set l $f.min
    label $l -text "Min"
    grid $l -row $row -column 1

    set l $f.max
    label $l -text "Max"
    grid $l -row $row -column 2

    incr row

    set l $f.band${row}Label
    label $l -text "First Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.band${row}Min
    entry $e -textvariable minImg1 -width 10
    global minImg1
    set minImg1 ""
    grid $e -row $row -column 1 -sticky news

    set e $f.band${row}Max
    entry $e -textvariable maxImg1 -width 10
    global maxImg1
    set maxImg1 ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.band${row}Label
    label $l -text "Second Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.band${row}Min
    entry $e -textvariable minImg2 -width 10
    global minImg2
    set minImg2 ""
    grid $e -row $row -column 1 -sticky news

    set e $f.band${row}Max
    entry $e -textvariable maxImg2 -width 10
    global maxImg2
    set maxImg2 ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.spacer${row}
    label $l -text " "
    grid $l -row $row -column 0

    incr row

    set l $f.fg${row}
    label $l -text "Background"
    grid $l -row $row -column 0

    set l $f.min${row}
    label $l -text "Min"
    grid $l -row $row -column 1

    set l $f.max${row}
    label $l -text "Max"
    grid $l -row $row -column 2

    incr row

    set l $f.band${row}Label
    label $l -text "First Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.band${row}Min
    entry $e -textvariable bkminImg1 -width 10
    global bkminImg1
    set bkminImg1 ""
    grid $e -row $row -column 1 -sticky news

    set e $f.band${row}Max
    entry $e -textvariable bkmaxImg1 -width 10
    global bkmaxImg1
    set bkmaxImg1 ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.band${row}Label
    label $l -text "Second Image:"
    grid $l -row $row -column 0 -sticky e

    set e $f.band${row}Min
    entry $e -textvariable bkminImg2 -width 10
    global bkminImg2
    set bkminImg2 ""
    grid $e -row $row -column 1 -sticky news

    set e $f.band${row}Max
    entry $e -textvariable bkmaxImg2 -width 10
    global bkmaxImg2
    set bkmaxImg2 ""
    grid $e -row $row -column 2 -sticky news

    incr row

    set l $f.spacer${row}
    label $l -text " "
    grid $l -row $row -column 0

    incr row

    set cb $f.mirrorCb
    checkbutton $cb -text "Mirror" -variable ridcaSpectralFilterMirror
    grid $cb -row $row -column 0
    global ridcaSpectralFilterMirror
    set ridcaSpectralFilterMirror 1

    # Second parm frame (two columns wide)
    set f $w.parmFrame2
    frame $f
    pack $f -side top -fill both -expand true

    # Second frame, column 0
    set f $w.parmFrame2.col0
    frame $f -relief solid -bd 1
    pack $f -side left -fill both -expand true -anchor n
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 10 -weight 1

    set row 0

    set l $f.titleLabel
    label $l -text "FILTER FILES"
    grid $l -row $row -column 0

    incr row

    set l $f.space${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.saveLabel
    label $l -text "Save Filter"
    grid $l -row $row -column 0 -sticky w

    incr row

    set e $f.saveFileEntry
    entry $e -textvariable ridcaSaveFileEntry
    grid $e -row $row -column 0 -sticky ew
    bind $e <Return> "saveFilterFiles"

    incr row

    set bf $f.buttonFrame${row}
    frame $bf
    grid $bf -row $row -column 0 -sticky w

    set b $bf.browseButton
    button $b -text "Browse" -command "set ridcaSaveFileEntry \[tk_getSaveFile -parent . -filetypes [FileUtils::fileTypes filter all]\]"
    pack $b -side left -expand 1 -fill both

    set b $bf.saveButton
    button $b -text "Save" -command "saveFilterFiles"
    pack $b -side left -expand 1 -fill both

    incr row

    set l $f.space${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set l $f.reloadLabel
    label $l -text "Reload Filter"
    grid $l -row $row -column 0 -sticky w

    incr row

    set e $f.openFileEntry
    entry $e -textvariable ridcaOpenFileEntry
    grid $e -row $row -column 0 -sticky ew
    bind $e <Return> "reloadFilterFiles"

    incr row

    set bf $f.buttonFrame${row}
    frame $bf
    grid $bf -row $row -column 0 -sticky w

    set b $bf.browseButton
    button $b -text "Browse" -command "set ridcaOpenFileEntry \[tk_getOpenFile -parent . -filetypes [FileUtils::fileTypes filter all]\]"
    pack $b -side left -expand 1 -fill both

    set b $bf.reloadButton
    button $b -text "Reload" -command "reloadFilterFiles"
    pack $b -side left -expand 1 -fill both

    # Second frame, column 1
    set f $w.parmFrame2.col1
    frame $f -relief solid -bd 1
    pack $f -side left -fill both -expand true -anchor n
    grid rowconfigure $f 4 -weight 1

    set row 0

    set l $f.titleLabel
    label $l -text "TOOLS"
    grid $l -row $row -column 0

    incr row

    set b $f.row${row}
    radiobutton $b -text "Display Input Images" -variable ridcaTool -value 1
    grid $b -row $row -column 0 -sticky w

    incr row

    set b $f.row${row}
    radiobutton $b -text "Input Histograms" -variable ridcaTool -value 2
    grid $b -row $row -column 0 -sticky w

    incr row

    set b $f.row${row}
    radiobutton $b -text "Equalized Histograms" -variable ridcaTool -value 3
    grid $b -row $row -column 0 -sticky w

    incr row

    set b $f.row${row}
    radiobutton $b -text "Plus/Minus Components" -variable ridcaTool -value 4
    grid $b -row $row -column 0 -sticky w

    incr row

    set b $f.row${row}
    radiobutton $b -text "Component Numbers" -variable ridcaTool -value 5
    grid $b -row $row -column 0 -sticky w

    incr row

    set subf $f.row${row}_frame
    frame $subf
    grid $subf -row $row -column 0 -sticky w

    set b $subf.row${row}button
    radiobutton $b -text "Property List for" -variable ridcaTool -value 6
    pack $b -side left

    set e $subf.row${row}entry
    entry $e -width 8 -textvariable ridcaComponentId
    pack $e -side left

    global ridcaComponentId
    set ridcaComponentId ""

    incr row

    set b $f.row${row}
    radiobutton $b -text "Filtered Property List" -variable ridcaTool -value 7
    grid $b -row $row -column 0 -sticky w

    incr row

    set b $f.row${row}
    radiobutton $b -text "None" -variable ridcaTool -value 0
    grid $b -row $row -column 0 -sticky w

    global ridcaTool
    set ridcaTool 0

    # Start, Refilter, Cancel Buttons
    set bf $w.buttonFrame
    frame $bf
    pack $bf -side top

    set b $bf.start
    button $b -text "Start" -command "runRidca 0"
    pack $b -side left

    set b $bf.refilter
    button $b -text "Refilter" -command "runRidca 1"
    pack $b -side left

    set b $bf.cancel
    button $b -text "Cancel" -command "destroy $w"
    pack $b -side left
}

proc saveFilterFiles {} {
    global ridcaSaveFileEntry

    set f [open $ridcaSaveFileEntry w]

    set vars "ridcaFirstDate ridcaSecondDate lightThreshold darkThreshold shadowThreshold areaMin areaMax ratioMin ratioMax diameterMin diameterMax minImg1 maxImg1 minImg2 maxImg2 bkminImg1 bkmaxImg1 bkminImg2 bkmaxImg2 ridcaTool ridcaComponentId ridcaSpectralFilterMirror"

    foreach var $vars {
	global $var
	puts $f "global $var ; set $var \"[set $var]\""
    }

    close $f
}

proc reloadFilterFiles {} {
    global ridcaOpenFileEntry

    set f [open $ridcaOpenFileEntry r]
    set data [read $f]
    close $f

    eval $data
}

proc runRidca {skip} {
    global ridcaSkip
    set ridcaSkip $skip

    setupUtilityRun

    global terminated
    set terminated 0
    puts "Starting RIDCA job"
    exec vicarb utilityrun.pdf | tee teeFile &

    while {[llength [taetmRunning]]} {
	puts "Running"
	after 1000
    }

    if {$terminated} {
	tk_messageBox -message "Job terminated by user"
    } else {
	tk_messageBox -message "Job ended"
    }
}

proc viewUtilityInput {} {
    global utilityInputImage

    global VDEV_DIR
    exec ${VDEV_DIR}/xvd $utilityInputImage &
}

proc createOldNlcWindow {} {
    wm withdraw .
    global utilityType
    set utilityType chipper
    global chipperStandalone
    set chipperStandalone 1
    setupUtilities
}

global guiFlavor
set guiFlavor afids

proc createNlcDialog {} {
    global guiFlavor
    set guiFlavor "nlc"

    set title "NITF Logger/Chipper"
    wm title . $title
    wm iconname . $title
    wm protocol . WM_DELETE_WINDOW "exit"
    wm geometry . +200+200

    set f .frame
    catch {destroy $f}
    frame $f
    pack $f -fill both -expand true
    grid columnconfigure $f 0 -weight 1

    set row 0

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "NLC INFORMATION ..."
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    global env
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help_nlc.htm #toproceed"
    pack $hb -anchor e -side left -expand true

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 1: INPUT/OUTPUT SELECTION"
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help_nlc.htm #step1"
    pack $hb -anchor e -side left -expand true

    incr row

    set subf $f.dirInitFrame
    frame $subf -bd 1 -relief solid
    grid $subf -row $row -column 0 -sticky ew
    grid columnconfigure $subf 0 -weight 0
    grid columnconfigure $subf 1 -weight 1
    grid columnconfigure $subf 2 -weight 0
    grid columnconfigure $subf 3 -weight 0

    set subRow 0

    global nlcValueHistory

    set inputImageEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Input Image:" \
	    -history $nlcValueHistory inputImageHistory \
	    -updateHistoryOnReturn \
	    -browse "Open -filetypes [FileUtils::fileTypes nitf all]"]
    $inputImageEntry grid -row $subRow -column 0

    incr subRow

    set l $subf.elevSourceChoiceLabel
    label $l -text "Elev Source:"
    grid $l -row $subRow -column 0 -sticky e

    set bf $subf.buttonFrame
    frame $bf
    grid $bf -row $subRow -column 1 -sticky w

    set rb $bf.newMosaic
    set rbNew $rb
    radiobutton $rb -text "Create New Mosaic" -variable chipperElevDataSource -value new
    pack $rb -side left -anchor w
    set rb $bf.oldMosaic
    set rbOld $rb
    radiobutton $rb -text "Use Existing Mosaic" -variable chipperElevDataSource -value old
    pack $rb -side left -anchor w

    global chipperElevDataSource
    set chipperElevDataSource new

    incr subRow
 
    set elevDataDirectoryEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Elev Data Directory:" \
	    -history $nlcValueHistory elevDataDirHistory \
	    -updateHistoryOnReturn \
	    -browse "SelectDir"]
    $elevDataDirectoryEntry grid -row $subRow -column 0

    incr subRow
 
    set elevDataMosaicEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Elev Data Mosaic:" \
	    -history $nlcValueHistory elevDataMosaicHistory \
	    -updateHistoryOnReturn \
	    -browse "Open -filetypes [FileUtils::fileTypes vicar all]" \
	    -state disabled]
    $elevDataMosaicEntry grid -row $subRow -column 0

    $rbNew config -command "$elevDataDirectoryEntry enable ; $elevDataMosaicEntry disable"
    $rbOld config -command "$elevDataDirectoryEntry disable ; $elevDataMosaicEntry enable"

    incr subRow

    set workingDirectoryEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Working Directory:" \
	    -history $nlcValueHistory workingDirHistory \
	    -updateHistoryOnReturn \
	    -browse "SelectDir"]
    $workingDirectoryEntry grid -row $subRow -column 0

    incr subRow
 
    set outputPrefixEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Output Prefix:" \
	    -history $nlcValueHistory outputPrefixHistory \
	    -updateHistoryOnReturn]
    $outputPrefixEntry grid -row $subRow -column 0

    incr subRow

    set l $subf.label${subRow}
    label $l -text "TIFF Output of Input:"
    grid $l -row $subRow -column 0 -sticky e

    set tbf $subf.tiffButtonFrame
    frame $tbf
    grid $tbf -row $subRow -column 1 -sticky w
    grid configure $tbf -columnspan 3

    global runNtmTiff
    set runNtmTiff n

    set rb $tbf.bSmall
    radiobutton $rb -text "Small" -variable runNtmTiff -value s
    pack $rb -side left -anchor w
    set rb $tbf.bFullSize
    radiobutton $rb -text "Full-Size" -variable runNtmTiff -value f
    pack $rb -side left -anchor w
    set rb $tbf.bBoth
    radiobutton $rb -text "Both" -variable runNtmTiff -value b
    pack $rb -side left -anchor w
    set rb $tbf.bNone
    radiobutton $rb -text "None" -variable runNtmTiff -value n
    pack $rb -side left -anchor w

    incr subRow

    set l $subf.label${subRow}
    label $l -text "Preview Input Image:"
    grid $l -row $subRow -column 0 -sticky e

    set cb $subf.viewOutImage
    checkbutton $cb -text "" -variable previewNlcInputImage
    grid $cb -row $subRow -column 1 -sticky w

    incr subRow

    set b $subf.continueButton
    button $b -text "Accept" -command "acceptStepOne $inputImageEntry $elevDataDirectoryEntry $elevDataMosaicEntry $workingDirectoryEntry $outputPrefixEntry"
    grid $b -row $subRow -column 0
    grid configure $b -columnspan 4

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 2: OPTIONAL GEOREFERENCE REFINEMENT TIEPOINT"
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help_nlc.htm #step2"
    pack $hb -anchor e -side left -expand true

    incr row

    set tf $f.f_${row}
    frame $tf -bd 1 -relief solid
    grid $tf -row $row -column 0 -sticky ew
    grid columnconfigure $tf 9 -weight 1

    set l $tf.latLabel
    label $l -text "Lat:"
    grid $l -row 0 -column 0 -sticky w

    set eLat $tf.eLat
    entry $eLat -width 8 -textvariable tpLat
    grid $eLat -row 0 -column 1 -sticky w
    
    set l $tf.lonLabel
    label $l -text "Lon:"
    grid $l -row 0 -column 2 -sticky w

    set eLon $tf.eLon
    entry $eLon -width 8 -textvariable tpLon
    grid $eLon -row 0 -column 3 -sticky w

    set l $tf.lineLabel
    label $l -text "Line:"
    grid $l -row 0 -column 4 -sticky w

    set eLine $tf.eLine
    entry $eLine -width 8 -textvariable tpLine
    grid $eLine -row 0 -column 5 -sticky w

    set l $tf.sampLabel
    label $l -text "Samp:"
    grid $l -row 0 -column 6 -sticky w

    set eSamp $tf.eSamp
    entry $eSamp -width 8 -textvariable tpSamp
    grid $eSamp -row 0 -column 7 -sticky w

    set b $tf.fixRpc
    button $b -text "Accept" -command "fixRpc $outputPrefixEntry \$tpLat \$tpLon \$tpLine \$tpSamp" -state disabled
    grid $b -row 0 -column 8 -sticky w

    global buttonsToEnable
    lappend buttonsToEnable $b

    set l $tf.exampleLabel
    label $l -text "Use signed decimal degrees, e.g. Lat: 34.19881667 Lon: -118.17456667"
    grid $l -row 1 -column 0 -sticky w
    grid configure $l -columnspan 9

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 3: CREATE CHIP IMAGES"
    pack $l -anchor w -side left

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help_nlc.htm #step3"
    pack $hb -anchor e -side left -expand true

    incr row

    set chipImagesButtonsFrame [createNlcChipImagesButtonsFrame $f]
    grid $chipImagesButtonsFrame -row $row -column 0 -sticky news

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set subf $f.f_${row}
    frame $subf -bd 1 -relief solid
    grid $subf -row $row -column 0 -sticky ew

    set l $subf.label${row}
    label $l -text "STEP 4: UTILITIES"
    pack $l -anchor w -side left

    set b $subf.utilitiesButton
    button $b -text "Utilities ..." -command "utilities" -state disabled
    pack $b -anchor w -side left
    lappend buttonsToEnable $b

    set hb $subf.label${row}_help
    button $hb -image helpButton -command "help $env(AFIDS_ROOT)/doc/help_nlc.htm #step4"
    pack $hb -anchor e -side left -expand true

    incr row

    set l $f.label${row}
    label $l -text " "
    grid $l -row $row -column 0 -sticky w

    incr row

    set bf $f.buttonFrame
    frame $bf
    grid $bf -row $row -column 0 -sticky news
    grid columnconfigure $bf 0 -weight 1
    #    grid columnconfigure $bf 1 -weight 1

    set b $bf.exitButton
    button $b -text "Exit" -command "exit"
    grid $b -row 0 -column 0
}

proc fixRpc {outputPrefixEntry tpLat tpLon tpLine tpSamp} {
    set abort 0

    if {$tpLat != "" || $tpLon != "" || $tpLine != "" || $tpSamp != ""} {
	set tiePointSet 1
	if {$tpLat == "" || $tpLon == "" || $tpLine == "" || $tpSamp == ""} {
	    set abort 1
	    tk_messageBox -title "Incomplete Tiepoint" -message "All (Or None) Of The Tiepoint Fields Must Be Specified"
	}
    } else {
	set tiePointSet 0
    }

    if {$abort} {
	return
    }

    if {[otherTaetmsRunning]} {
	return
    }

    global SESSION_ID
    global utilityType
    global chipperFixrpcInp chipperFixrpcElev
    global chipperTiepointLine chipperTiepointSamp chipperTiepointLon chipperTiepointLat

    set utilityType fixRpc
    set chipperFixrpcInp $SESSION_ID/[$outputPrefixEntry getValue].img
    set chipperFixrpcElev [findElevationMosaic]
    set chipperTiepointLine $tpLine
    set chipperTiepointSamp $tpSamp
    set chipperTiepointLon $tpLon
    set chipperTiepointLat $tpLat

    setupUtilityRun

    set tempFilename [FileUtils::makeTemp "nlc"]

    set pid [exec vicarb utilityrun.pdf > $tempFilename &]

    while {! [catch {exec ps -u[exec whoami] -opid | grep $pid}]} {
	update

	after 1000
    }    

    file delete -force $tempFilename
    file delete -force utilityrun.pdf
}

proc otherTaetmsRunning {} {
    set user [exec whoami]

    set pids {}

    if {[catch {exec ps -u$user | grep taetm} pids]} {
	return 0
    }

    if {[promptUser "Terminate All VICARS?" "User $user has one or more VICAR jobs running that must be\nterminated before starting a new one.\nContinue with termination?" {Yes No}] == "No"} {
	return 1
    }

    set pids [split $pids "\n"]

    foreach pid $pids {
	catch {exec kill -9 [lindex $pid 0]}
    }

    return 0
}

proc acceptStepOne {inputImageEntry elevDataDirectoryEntry elevDataMosaicEntry workingDirectoryEntry outputPrefixEntry} {
    set abort 0

    if {[$inputImageEntry getValue] == ""} {
	set abort 1
	tk_messageBox -title "Incomplete Entry" -message "Input Image Must Be Specified"
    }

    if {[$elevDataDirectoryEntry getValue] == "" && [$elevDataMosaicEntry getValue] == ""} {
	set abort 1
	tk_messageBox -title "Incomplete Entry" -message "Either Elevation Data Directory Or Mosaic Must Be Specified"
    }

    if {[$workingDirectoryEntry getValue] == ""} {
	set abort 1
	tk_messageBox -title "Incomplete Entry" -message "Working Directory Must Be Specified"
    }

    if {[$outputPrefixEntry getValue] == ""} {
	set abort 1
	tk_messageBox -title "Incomplete Entry" -message "Output Prefix Must Be Specified"
    }

    if {$abort} {
	return
    }

    $inputImageEntry updateHistory
    $elevDataDirectoryEntry updateHistory
    $elevDataMosaicEntry updateHistory
    $workingDirectoryEntry updateHistory
    $outputPrefixEntry updateHistory

    file mkdir [$workingDirectoryEntry getValue]

    global buttonsToEnable

    foreach b $buttonsToEnable {
	$b config -state normal
    }

    # used by setupUtilityRun when creating pdf
    global runNtmTiff
    global utilityType
    global SESSION_ID
    global nlcLogNtmOutputPrefix
    global utilityInputImage
    global viewutilitywhenComplete
    global nlcElevationDataDirectory
    global nlcElevationDataMosaic

    # gui option button
    global previewNlcInputImage

    # pass to setupUtilityRun; should pass as args; should tempfile the pdf below
    set utilityType nlcLogNtm
    set utilityInputImage [$inputImageEntry getValue]
    set nlcElevationDataDirectory [$elevDataDirectoryEntry getValue]
    set nlcElevationDataMosaic [$elevDataMosaicEntry getValue]
    set SESSION_ID [$workingDirectoryEntry getValue]
    set nlcLogNtmOutputPrefix [$outputPrefixEntry getValue]
    set viewutilitywhenComplete $previewNlcInputImage

    set previouslyLogged 0
    if {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img] != "" && \
	    [glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.txt] != ""} {
	switch $runNtmTiff {
	    s {
		if {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}_small.tif] != ""} {
		    set previouslyLogged 1
		}
	    }
	    f {
		if {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.tif] != ""} {
		    set previouslyLogged 1
		}
	    }
	    b {
		if {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}_small.tif] != "" && \
		    [glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.tif] != ""} {
		    set previouslyLogged 1
		}
	    }
	    n {
		set previouslyLogged 1
	    }
	}
    }

    if {$previouslyLogged} {
	if {$viewutilitywhenComplete} {
	    global VDEV_DIR
	    exec ${VDEV_DIR}/xvd ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img &
	}

	return
    }

    if {[otherTaetmsRunning]} {
	return
    }

    # Creates utilityrun.pdf calling run_ntm.pdf (calls utilf_tll.pdf), eventually producing
    # (via vextract) ${nlcLogNtmOutputPrefix}.img and
    # ${nlcLogNtmOutputPrefix}.txt.
    setupUtilityRun

    set tempFilename [FileUtils::makeTemp "nlc"]

    file delete -force ${SESSION_ID}/${nlcLogNtmOutputPrefix}_small.tif
    file delete -force ${SESSION_ID}/${nlcLogNtmOutputPrefix}.tif
    file delete -force ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img
    file delete -force ${SESSION_ID}/${nlcLogNtmOutputPrefix}image0.raw

    set pid [exec vicarb utilityrun.pdf > $tempFilename &]

    set meterValue 0.0
    set meterLabel "0%"
    set status "Opening Source NITF"
    set dialog [ProgressDialog \#auto -title "NLC: Extracting Imagery" \
	    -description "Extracting Imagery From\n[$inputImageEntry getValue]" \
	    -status $status -meter $meterValue -meterLabel $meterLabel -terminate "userTerminatingProcess" -userData $pid]

    while {! [catch {exec ps -u[exec whoami] -opid | grep $pid}]} {
	update

	if {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.tif] != ""} {
	    set status "Creating TIFF Image"
	    set meterValue [expr double([file size ${SESSION_ID}/${nlcLogNtmOutputPrefix}.tif]) / [file size [$inputImageEntry getValue]]]
	    set meterLabel "[expr int(100 * $meterValue)]%"
	} elseif {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img] != ""} {
	    set status "Creating VICAR Image"
	    set meterValue [expr 0.5 + double([file size ${SESSION_ID}/${nlcLogNtmOutputPrefix}.img]) / 2 / [file size [$inputImageEntry getValue]]]
	    set meterLabel "[expr int(100 * $meterValue)]%"
	} elseif {[glob -nocomplain ${SESSION_ID}/${nlcLogNtmOutputPrefix}image0.raw] != ""} {
	    set status "Creating VICAR Image"
	    set meterValue [expr double([file size ${SESSION_ID}/${nlcLogNtmOutputPrefix}image0.raw]) / 2 / [file size [$inputImageEntry getValue]]]
	    set meterLabel "[expr int(100 * $meterValue)]%"
	}

	if {[catch {$dialog setMeter $meterValue $meterLabel ; $dialog setStatus $status}]} {
	    tk_messageBox -title "Processing Terminated By User" -message "Processing Terminated By User"
	    return
	}
	after 1000
    }    

    itcl::delete object $dialog

    if {! [catch {exec grep -sv write $tempFilename | egrep "(ABEND|ERROR|WARNING|MSG|COMPLETED)"} results]} {
	if {$results != ""} {
	    tk_messageBox -title "Job Status Log" -message $results
	}
    } else {
	tk_messageBox -title "Job Status Log" -message "$utilityInputImage Processing Did Not Complete"
    }

    file delete -force $tempFilename
    file delete -force utilityrun.pdf
}

proc userTerminatingProcess {dialog} {
    set pid [$dialog cget -userData]

    exec kill -9 $pid

    itcl::delete object $dialog
}

#tixBalloon .tixBalloon -initwait 250

set nlcValueHistory [ValueHistory \#auto -persist ~/.nlcHistory]
$nlcValueHistory register workingDirHistory 5
$nlcValueHistory register inputImageHistory 5
$nlcValueHistory register outputPrefixHistory 5
$nlcValueHistory register elevDataDirHistory 5
$nlcValueHistory register elevDataMosaicHistory 5

proc createRpcToGeoTIFFMapDialog {} {
    set f .rpcToGeoTiffMap
    catch {destroy $f}
    toplevel $f
    set title "Convert RPC To GeoTIFF Map"
    wm title $f $title
    wm iconname $f $title
    wm protocol $f WM_DELETE_WINDOW "destroy $f"
    wm geometry $f +200+200

    set l $f.sourceOutputFrameLabel
    label $l -text "SOURCE/OUTPUT SELECTION"
    pack $l -side top -anchor w

    set subf $f.sourceOutputFrame
    frame $subf -bd 1 -relief solid
    pack $subf -side top -anchor w -fill x
    grid columnconfigure $subf 0 -weight 0
    grid columnconfigure $subf 1 -weight 1
    grid columnconfigure $subf 2 -weight 0
    grid columnconfigure $subf 3 -weight 0

    set row 0

    global afidsValueHistory

    global rpcToGeoTIFFMapInputImageEntry
    set rpcToGeoTIFFMapInputImageEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Input Image:" \
	    -history $afidsValueHistory rpcToGeoTIFFMapInputImageHistory \
	    -updateHistoryOnReturn \
	    -browse "Open -filetypes [FileUtils::fileTypes vicar geoTiff all]"]
    $rpcToGeoTIFFMapInputImageEntry grid -row $row -column 0

    incr row

    global rpcToGeoTIFFMapElevationImageEntry
    set rpcToGeoTIFFMapElevationImageEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Elevation Image:" \
	    -history $afidsValueHistory rpcToGeoTIFFMapElevationImageHistory \
	    -updateHistoryOnReturn \
	    -browse "Open -filetypes [FileUtils::fileTypes vicar all]"]
    $rpcToGeoTIFFMapElevationImageEntry grid -row $row -column 0

    incr row

    global rpcToGeoTIFFMapOutputImageEntry
    set rpcToGeoTIFFMapOutputImageEntry [Entry \#auto $subf \
	    -width 40 \
	    -label "Output Image:" \
	    -history $afidsValueHistory rpcToGeoTIFFMapOutputImageHistory \
	    -updateHistoryOnReturn \
	    -browse "Save -filetypes [FileUtils::fileTypes vicar all]"]
    $rpcToGeoTIFFMapOutputImageEntry grid -row $row -column 0

    set l $f.labelSpace_0
    label $l -text " "
    pack $l -side top -anchor w
    
    set l $f.parametersFrameLabel
    label $l -text "PROCESSING PARAMETERS"
    pack $l -side top -anchor w

    set subf $f.parametersFrame
    frame $subf -bd 1 -relief solid
    pack $subf -side top -anchor w -fill x -expand true

    # Grid Fineness
    set gff $subf.gridFinenessFrame
    frame $gff
    pack $gff -side top -anchor w -fill x

    set l $gff.labelx1
    label $l -text "Grid Fineness (range 100 to 4000): "
    pack $l -side left -anchor w

    set l $gff.labelx2
    label $l -text "Horizontal"
    pack $l -side left -anchor w

    set e $gff.entryx3
    entry $e -textvariable Utility_NAH -width 5
    global Utility_NAH
    pack $e -side left -anchor w

    set l $gff.labelx4
    label $l -text "Vertical"
    pack $l -side left -anchor w

    set e $gff.entryx5
    entry $e -textvariable Utility_NAV -width 5
    global Utility_NAV
    pack $e -side left -anchor w

    set Utility_NAH 1000
    set Utility_NAV 1000

    # Output Pixel Size
    set f2 $subf.f2
    frame $f2
    pack $f2 -side top -anchor w -fill x

    set l $f2.label_1
    label $l -text "Output Pixel Size: "
    pack $l -side left

    set e $f2.entry_ops
    entry $e -textvariable Utility_MPIX -width 5
    global Utility_MPIX
    set Utility_MPIX 1.0
    pack $e -side left

    set l $f2.label_2
    label $l -text "(meters)" -font italic
    pack $l -side left

    # Output Map Projection
    set f3 $subf.f3
    frame $f3
    pack $f3 -side top -anchor w -fill x -expand true

    set l $f3.label
    label $l -text "Output Map Projection:"
    pack $l -side left -anchor w -fill both

    set rb $f3.pc
    radiobutton $rb -text "Platte Carree" -variable Utility_maptype -value pc -anchor w
    $rb config -command "$f3.refEntry config -state disabled ; $f3.refBrowse config -state disabled"
    pack $rb -side left -anchor w -fill both
    
    set rb $f3.utm
    radiobutton $rb -text "UTM" -variable Utility_maptype -value utm -anchor w
    $rb config -command "$f3.refEntry config -state disabled ; $f3.refBrowse config -state disabled"
    pack $rb -side left -anchor w -fill both
    
    set rb $f3.reference
    radiobutton $rb -text "Reference" -variable Utility_maptype -value ref -anchor w -command "$f3.refEntry config -state normal ; $f3.refBrowse config -state normal"
    pack $rb -side left -anchor w -fill both
    
    set e $f3.refEntry
    entry $e -textvariable Utility_referenceProjection -width 40 -state disabled

    lappend Utility_fileEntries [list Utility_referenceProjection "Projection Reference Image"]

    global Utility_referenceProjection
    set Utility_referenceProjection ""
    pack $e -side left -anchor w -fill both -expand true

    set mb $f3.refBrowse
    button $mb -text "Browse ..." -command "getFileForOpen Utility Utility_referenceProjection vicarGeoTIFF" -state disabled
    pack $mb -side left -anchor w -fill both

    # Resample Method
    set f4 $subf.f4
    frame $f4
    pack $f4 -side top -anchor w -fill x

    set l $f4.label${row}
    label $l -text "Resample Method:"
    pack $l -side left -anchor w -fill both

    set rb $f4.bilin
    radiobutton $rb -text "Bilinear" -variable Utility_resampleMethod -value bilin -anchor w
    pack $rb -side left -anchor w -fill both

    set rb $f4.noin
    radiobutton $rb -text "Nearest Neighbor" -variable Utility_resampleMethod -value noin -anchor w
    pack $rb -side left -anchor w -fill both

    global Utility_resampleMethod
    set Utility_resampleMethod bilin

    # Raster Type
    set f5 $subf.f5
    frame $f5
    pack $f5 -side top -anchor w -fill x

    set l $f5.label${row}
    label $l -text "Raster Type:"
    pack $l -side left -anchor w -fill both

    set rb $f5.area
    radiobutton $rb -text "Area" -variable Utility_rasterType -value area -anchor w
    pack $rb -side left -anchor w -fill both

    set rb $f5.point
    radiobutton $rb -text "Point" -variable Utility_rasterType -value point -anchor w
    pack $rb -side left -anchor w -fill both

    global Utility_rasterType
    set Utility_rasterType area

    # Coverage Selection
    set f6 $subf.f6
    frame $f6
    pack $f6 -side top -anchor w -fill x

    set l $f6.label${row}
    label $l -text "Coverage Selection:"
    pack $l -side left -anchor w -fill both

    set rb $f6.area
    radiobutton $rb -text "Cover Input" -variable Utility_coverageSelection -value coverinp -anchor w
    pack $rb -side left -anchor w -fill both

    set rb $f6.point
    radiobutton $rb -text "Cover Reference" -variable Utility_coverageSelection -value coverref -anchor w
    pack $rb -side left -anchor w -fill both

    global Utility_coverageSelection
    set Utility_coverageSelection coverinp

    # Utility Execution
    set l $f.labelSpace_2
    label $l -text " "
    pack $l -side top -anchor w

    set l $f.label_42
    label $l -text "UTILITY EXECUTION"
    pack $l -side top -anchor w

    global utilityType
    set utilityType rpcToGeoTIFFMap
    set processingFrame [createMosaicProcessingFrame $f Utility]
    pack $processingFrame -side top -fill x

    set b $f.close
    button $b -text "Close" -command "destroy $f"
    pack $b -side top
}

set afidsValueHistory [ValueHistory \#auto -persist ~/.afidsHistory]
$afidsValueHistory register rpcToGeoTIFFMapInputImageHistory 5
$afidsValueHistory register rpcToGeoTIFFMapElevationImageHistory 5
$afidsValueHistory register rpcToGeoTIFFMapOutputImageHistory 5
