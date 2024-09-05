load tiffTcl.so

source "$env(AFIDS_TCL)/classes.tcl"
source "$env(AFIDS_TCL)/ngclasses.tcl"

proc createTvDialog {argv} {
    set title "CTV2"
    wm title . $title
    wm iconname . $title
    wm protocol . WM_DELETE_WINDOW "exit"
    wm geometry . +200+200

    grid columnconfigure . 0 -weight 0
    grid columnconfigure . 1 -weight 1
    grid columnconfigure . 2 -weight 0

    set pathInit1 ""
    set pathInit2 ""
    set pathInit3 ""
    switch [llength $argv] {
	0 {
	}
	1 {
	    set pathInit1 [lindex $argv 0]
	}
	2 {
	    set pathInit1 [lindex $argv 0]
	    set pathInit2 [lindex $argv 1]
	}
	3 {
	    set pathInit1 [lindex $argv 0]
	    set pathInit2 [lindex $argv 1]
	    set pathInit3 [lindex $argv 2]
	}
	default {
	    error "only 3 or fewer input files allowed"
	}
    }

    set row 0

    set inputImageEntry [Entry \#auto "" \
	    -init $pathInit1 \
	    -width 40 \
	    -label "Input RED/BW:" \
	    -browse "Open -filetypes [FileUtils::fileTypes all nitf geoTiff vicar fits]"]

    $inputImageEntry grid -row $row -column 0

    incr row
    
    set inputImageEntry2 [Entry \#auto "" \
	    -init $pathInit2 \
	    -width 40 \
	    -label "Input GREEN:" \
	    -browse "Open -filetypes [FileUtils::fileTypes all nitf geoTiff vicar fits]"]

    $inputImageEntry2 grid -row $row -column 0

    incr row
    
    set inputImageEntry3 [Entry \#auto "" \
	    -init $pathInit3 \
	    -width 40 \
	    -label "Input BLUE:" \
	    -browse "Open -filetypes [FileUtils::fileTypes all nitf geoTiff vicar fits]"]

    $inputImageEntry3 grid -row $row -column 0

    global entriesControlledByRgb
    set entriesControlledByRgb [list $inputImageEntry2 $inputImageEntry3]

    incr row

    set bf .buttonFrame0
    frame $bf
    grid $bf -row $row -column 0
    grid configure $bf -columnspan 3 -sticky w

    global widgetsControlledByRgb

    set lab1 $bf.lab1
    label $lab1 -text "NITF Image Index"
    pack $lab1 -side left
    lappend widgetsControlledByRgb $lab1

    set ent1 $bf.ent1
    entry $ent1 -width 3 -textvariable imageIndex
    pack $ent1 -side left
    global imageIndex
    set imageIndex 1
    lappend widgetsControlledByRgb $ent1

    set lab2 $bf.lab2
    label $lab2 -text "NITF Band Index"
    pack $lab2 -side left
    lappend widgetsControlledByRgb $lab2

    set ent2 $bf.ent2
    entry $ent2 -width 3 -textvariable imageBand
    pack $ent2 -side left
    global imageBand
    set imageBand 1
    lappend widgetsControlledByRgb $ent2

    set cb $bf.cb
    checkbutton $cb -text "RGB" -variable imageIsRgb -anchor w -command "chooseImageIsRgb" -variable cbValue
    pack $cb -side left

    incr row
    
    set bf .buttonFrame
    frame $bf
    grid $bf -row $row -column 0
    grid configure $bf -columnspan 3

    set b $bf.viewImage
    global env
    button $b -text "View Image" -command [list callCtvd $inputImageEntry $inputImageEntry2 $inputImageEntry3 $ent1 $ent2]
    pack $b -side left -fill x -expand true

    set b $bf.viewMetaData
    button $b -text "View Meta Data" -command "viewMetaData \[$inputImageEntry getValue\]"
    pack $b -side left -fill x -expand true

    set b $bf.exit
    button $b -text "Exit" -command "exit"
    pack $b -side left -fill x -expand true
}

proc callCtvd {i1 i2 i3 image band} {
    global cbValue

    set i1 [$i1 getValue]
    set i2 [$i2 getValue]
    set i3 [$i3 getValue]

    set i1 [file normalize $i1]
    set i2 [file normalize $i2]
    set i3 [file normalize $i3]

    set rgb $cbValue
    set image [$image get]
    set band [$band get]

    set pdfPath "/tmp/[exec whoami]_ctv2tmp.pdf"
    set f [open $pdfPath w]

    puts $f "procedure"
    puts $f "body"
    puts $f "ctvd inp=\"$i1\" +"
    puts $f "     inp2=\"$i2\" +"
    puts $f "     inp3=\"$i3\" +"
    puts $f "     ftd=\"$pdfPath\" +"
    if {$rgb} {
	puts $f "     rgb=y +"
    } else {
	puts $f "     rgb=n +"
    }
    puts $f "     ntfimg=$image +"
    puts $f "     ntfbnd=$band"
    puts $f "end-proc"

    close $f

    set cwd [pwd]
    cd /tmp
    exec taetm -s $pdfPath &
    cd $cwd
}

proc chooseImageIsRgb {} {
    global widgetsControlledByRgb
    global entriesControlledByRgb
    global cbValue

    foreach widget $widgetsControlledByRgb {
	if {$cbValue} {
	    $widget configure -state disabled
	} else {
	    $widget configure -state normal
	}	
    }

    foreach widget $entriesControlledByRgb {
	if {$cbValue} {
	    $widget disable
	} else {
	    $widget enable
	}	
    }
}

proc bin2txt {path} {
    set split [split $path .]
    set end [lindex $split end]
    if {$end == "bin"} {
	set split [lrange $split 0 end-1]
	lappend split "txt"
	set path [join $split .]
    }
    return $path
}

proc viewMetaData {path} {
    set index 0
    set w .metaData_$index
    while {[llength [info commands $w]]} {
	incr index
	set w .metaData_$index
    }

    set title "$path Meta Data"
    toplevel $w
    wm title $w $title
    wm iconname $w $title
    wm protocol $w WM_DELETE_WINDOW "destroy $w"

    set f $w.textFrame
    frame $f
    pack $f -side top -expand true -fill both

    text $f.text -relief sunken -bd 2 -yscrollcommand "$f.scroll set" -setgrid 1 -height 1
    scrollbar $f.scroll -command "$f.text yview"
    pack $f.scroll -side right -fill y
    pack $f.text -expand yes -fill both
    $f.text insert 0.0 [getMetaData $path]
    $f.text mark set insert 0.0

    set b $w.closeButton
    button $b -text "Close" -command "destroy $w"
    pack $b -side bottom

    wm geometry $w 100x20+200+200
}

proc getMetaData {path} {
    set type [tiffTcl::fileType $path]
    if {$type == "TIFF" || $type == "GEOTIFF"} {
	return [getTiffMetaData $path]
    } elseif {$type == "NITF"} {
	return [getNitfMetaData $path]
    } elseif {$type == "VICAR"} {
	return [getVicarMetaData $path]
    } else {
	set data "To review this image's metadata,\nselect \"View Image.\"\nAfter the image displays,\nclick \"Tools-Label Display\"\nfrom the upper left menu."
	set txt [bin2txt $path]
	if {$txt != $path} {
	    catch {
		set file [open $txt r]
		set data [read $file]
		close $file
	    }
	}
	return $data
    }
}

proc getVicarMetaData {path} {
    exec vicarb "label-list $path" > /tmp/[exec whoami]_gvmd.tmp
    set f [open /tmp/[exec whoami]_gvmd.tmp r]
    set d [read $f]
    close $f
    file delete -force /tmp/[exec whoami]_gvmd.tmp
    return $d
}

proc getNitfMetaData {path} {
    return [ exec nitfinfo $path ]
}

proc splitUse00a {use00a} {
    set split ""
    #  ANGLE_TO_NORTH  X 00003 Field 1
    set split "${split}\nANGLE_TO_NORTH: [string range $use00a 0 2]"
    #  MEAN_GSD        X 00005 Field 2
    set split "${split}\nMEAN_GSD: [string range $use00a 3 7]"
    #  FIELD3(RESERV)  X 00001 Field 3
    set split "${split}\nFIELD3: [string range $use00a 8 8]"
    #  DYNAMIC_RANGE   X 00005 Field 4
    set split "${split}\nDYNAMIC_RANGE: [string range $use00a 9 13]"
    #  FIELD5(RESERV)  X 00003 Field 5
    set split "${split}\nFIELD5: [string range $use00a 14 16]"
    #  FIELD6(RESERV)  X 00001 Field 6
    set split "${split}\nFIELD6: [string range $use00a 17 17]"
    #  FIELD7(RESERV)  X 00003 Field 7
    set split "${split}\nFIELD7: [string range $use00a 18 20]"
    #  OBL_ANG         X 00005 Field 8
    set split "${split}\nOBL_ANG: [string range $use00a 21 25]"
    #  ROLL_ANG        X 00006 Field 9
    set split "${split}\nROLL_ANG: [string range $use00a 26 31]"
    #  FIELD10(RESERV) X 00012 Field 10
    set split "${split}\nFIELD10: [string range $use00a 32 43]"
    #  FIELD11(RESERV) X 00015 Field 11
    set split "${split}\nFIELD11: [string range $use00a 44 58]"
    #  FIELD12(RESERV) X 00004 Field 12
    set split "${split}\nFIELD12: [string range $use00a 59 62]"
    #  FIELD13(RESERV) X 00001 Field 13
    set split "${split}\nFIELD13: [string range $use00a 63 63]"
    #  FIELD14(RESERV) X 00003 Field 14
    set split "${split}\nFIELD14: [string range $use00a 64 66]"
    #  FIELD15(RESERV) X 00001 Field 15
    set split "${split}\nFIELD15: [string range $use00a 67 67]"
    #  FIELD16(RESERV) X 00001 Field 16
    set split "${split}\nFIELD16: [string range $use00a 68 68]"
    #  N_REF           X 00002 Field 17
    set split "${split}\nN_REF: [string range $use00a 69 70]"
    #  REV_NUM         X 00005 Field 18
    set split "${split}\nREV_NUM: [string range $use00a 71 75]"
    #  N_SEG           X 00003 Field 19
    set split "${split}\nN_SEG: [string range $use00a 76 78]"
    #  MAX_LP_SEG      X 00006 Field 20
    set split "${split}\nMAX_LP_SEG: [string range $use00a 79 84]"
    #  FIELD20(RESERV) X 00006 Field 21
    set split "${split}\nFIELD20: [string range $use00a 85 90]"
    #  FIELD21(RESERV) X 00006 Field 22
    set split "${split}\nFIELD21: [string range $use00a 91 96]"
    #  SUN_EL          X 00005 Field 23
    set split "${split}\nSUN_EL: [string range $use00a 97 101]"
    #  SUN_AZ          X 00005 Field 24
    set split "${split}\nSUN_AZ: [string range $use00a 102 106]"

    return $split
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

proc splitStdidc {stdidc} {
    set split ""
    #  ACQ_DATE        X 00014
    set split "${split}\nACQ_DATE: [string range $stdidc 0 13]"
    #  MISSION         X 00014
    set split "${split}\nMISION: [string range $stdidc 14 27]"
    #  PASS            X 00002
    set split "${split}\nPASS: [string range $stdidc 28 29]"
    #  OP_NUM          X 00003
    set split "${split}\nOP_NUM: [string range $stdidc 30 32]"
    #  START_SEGMENT   X 00002
    set split "${split}\nSTART_SEGMENT: [string range $stdidc 33 34]"
    #  REPRO_NUM       X 00002
    set split "${split}\nREPRO_NUM: [string range $stdidc 35 36]"
    #  REPLAY_REGEN    X 00003
    set split "${split}\nREPLAY_REGEN: [string range $stdidc 37 39]"
    #  BLANK_FILL      X 00001
    set split "${split}\nBLANK_FILL: [string range $stdidc 40 40]"
    #  START_COLUMN    X 00003
    set split "${split}\nSTART_COLUMN: [string range $stdidc 41 43]"
    #  START_ROW       X 00005
    set split "${split}\nSTART_ROW: [string range $stdidc 44 48]"
    #  END_SEGMENT     X 00002
    set split "${split}\nEND_SEGMENT: [string range $stdidc 49 50]"
    #  END_COLUMN      X 00003
    set split "${split}\nEND_COLUMN: [string range $stdidc 51 53]"
    #  END_ROW         X 00005
    set split "${split}\nEND_ROW: [string range $stdidc 54 58]"
    #  COUNTRY         X 00002
    set split "${split}\nCOUNTRY: [string range $stdidc 59 60]"
    #  W_A_C           X 00004
    set split "${split}\nW_A_C: [string range $stdidc 61 64]"
    #  LOCATION        X 00011
    set split "${split}\nLOCATION: [string range $stdidc 65 75]"
    #  FIELD_17(RESV)  X 00005
    set split "${split}\nFIELD_17: [string range $stdidc 76 80]"
    #  FIELD_18(RESV)  X 00008
    set split "${split}\nFIELD_18: [string range $stdidc 81 88]"

    return $split
}

proc splitSensra {sensra} {
    set split ""
    #  REF_ROW         X 00008
    set split "${split}\nREF_ROW: [string range $sensra 0 7]"
    #  REF_COL         X 00008
    set split "${split}\nREF_COL: [string range $sensra 8 15]"
    #  SENSOR_MODEL    X 00006
    set split "${split}\nSENSOR_MODEL: [string range $sensra 16 21]"
    #  SENSOR_MOUNT    X 00003
    set split "${split}\nSENSOR_MOUNT: [string range $sensra 22 24]"
    #  SENSOR_LOC      X 00021
    set split "${split}\nSENSOR_LOC: [string range $sensra 25 45]"
    #  SENSOR_ALT_SRC  X 00001
    set split "${split}\nSENSOR_ALT_SRC: [string range $sensra 46 46]"
    #  SENSOR_ALT      X 00006
    set split "${split}\nSENSOR_ALT: [string range $sensra 47 52]"
    #  SENSOR_ALT_UNIT X 00001
    set split "${split}\nSENSOR_ALT_UNIT: [string range $sensra 53 53]"
    #  SENSOR_AGL      X 00005
    set split "${split}\nSENSOR_AGL: [string range $sensra 54 58]"
    #  SENSOR_PITCH    X 00007
    set split "${split}\nSENSOR_PITCH: [string range $sensra 59 65]"
    #  SENSOR_ROLL     X 00008
    set split "${split}\nSENSOR_ROLL: [string range $sensra 66 73]"
    #  SENSOR_YAW      X 00008
    set split "${split}\nSENSOR_YAW: [string range $sensra 74 81]"
    #  PLATFORM_PITCH  X 00007
    set split "${split}\nPLATFORM_PITCH: [string range $sensra 82 88]"
    #  PLATFORM_ROLL   X 00008
    set split "${split}\nPLATFORM_ROLL: [string range $sensra 89 96]"
    #  PLATFORM_HDG    X 00005
    set split "${split}\nPLATFORM_HDG: [string range $sensra 97 101]"
    #  GROUND_SPD_SRC  X 00001
    set split "${split}\nGROUND_SPD_SRC: [string range $sensra 102 102]"
    #  GROUND_SPEED    X 00006
    set split "${split}\nGROUND_SPEEN: [string range $sensra 103 108]"
    #  GROUND_SPD_UNIT X 00001
    set split "${split}\nGROUND_SPD_UNIT: [string range $sensra 109 109]"
    #  GROUND_TRACK    X 00005
    set split "${split}\nGROUND_TRACK: [string range $sensra 110 114]"
    #  VERTICAL_VEL    X 00005
    set split "${split}\nVERTICAL_VEL: [string range $sensra 115 119]"
    #  VERT_VEL_UNIT   X 00001
    set split "${split}\nVERT_VEL_UNIT: [string range $sensra 120 120]"
    #  SWATH_FRAMES    X 00004
    set split "${split}\nSWATH_FRAMES: [string range $sensra 121 124]"
    #  N_SWATHS        X 00004
    set split "${split}\nN_SWATHS: [string range $sensra 125 128]"
    #  SPOT_NUM        X 00003
    set split "${split}\nSPOT_NUM: [string range $sensra 129 131]"

    return $split
}

proc splitEntryGeo {geo} {
    set split [split $geo \n]

    set keep ""
    foreach line $split {
	if {[string first "coeff" $line] < 0 && ([string first "lat" $line] >= 0 || [string first "lon" $line] >= 0 || [string first "line" $line] >= 0 || [string first "samp" $line] >= 0)} {
	    set keep "${keep}${line}\n"
	}
    }

    return $keep
}

proc joinNameValueSet {set} {
    set result ""

    foreach {name value} $set {
	set result "${result}${name}: ${value}\n"
    }

    return $result
}

proc getTiffMetaData {path} {
    set data ""

    set data "${data}Image Width: [tiffTcl::getWidth $path]\n"
    set data "${data}Image Height: [tiffTcl::getHeight $path]\n"

    set data "${data}Is Tiled: "
    if {[tiffTcl::isTiled $path]} {
	set data "${data}yes\n"
	set data "${data}   Tile Width: [tiffTcl::getTileWidth $path]\n"
	set data "${data}   Tile Height: [tiffTcl::getTileHeight $path]\n"
    } else {
	set data "${data}no\n"
    }

    set desc [tiffTcl::getDescription $path]
    if {$desc != {}} {
	set data "${data}Image Description:\n"

	set split [split $desc "()"]
	if {[lindex $split 0] == "AFIDS_RESOURCE"} {
	    set desc [lindex $split 1]
	    set split [split $desc ","]
	    foreach item $split {
		set nv [split $item "="]
		set name [lindex $nv 0]
		set value [lindex $nv 1]
		set data "${data}   $name = $value\n"
	    }
	} else {
	    set data "${data}${desc}\n"
	}
    }

    set gtags [tiffTcl::tiffTclGetGeoTiffTags $path]
    if {$gtags != {}} {
	set data "${data}GeoTIFF Tags:\n"

	foreach {key value} $gtags {
	    set data "${data}   $key = $value\n"
	}
    }
    
    set tp [tiffTcl::tiffTclGetGeoTiffTiepoints $path]
    if {$gtags != {}} {
	set data "${data}GeoTIFF Tiepoints:\n"
	foreach {x1 y1 z1 x2 y2 z2} $tp {
	    set data "${data}   X1: $x1  Y1: $y1  Z1: $z1  X2: $x2  Y2: $y2  Z2: $z2\n"
	}
    }

    set ps [tiffTcl::tiffTclGetGeoTiffPixelScale $path]
    if {$ps != {}} {
	foreach {x y z} $ps {
	    set data "${data}GeoTIFF Pixel Scale:\n   X: $x  Y: $y  Z: $z\n"
	}
    }

    set mt [tiffTcl::tiffTclGetGeoTiffModelTransformation $path]
    if {$mt != {}} {
	set data "${data}GeoTIFF Model Transformation:\n"
	foreach {x y z} $mt {
	    set data "${data}   $x  $y  $z\n"
	}
    }

    return $data
}

createTvDialog $argv
