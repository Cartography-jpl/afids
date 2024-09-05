package provide TMASRV 1.0
package require dbtcl

namespace eval vpf2Vec {
    namespace export getVpfParms
    namespace export genVec genVecCommandParms

    variable dbPath
    variable dbName
    variable libName
    variable pgmdataName vpf2vec.pgm
    variable genVecCommandParms
}

proc vpf2Vec::getVpfParms {} {
    set w .vpfParmsDialog
    catch {destroy $w}
    toplevel $w
    wm title $w "VPF Selection"

    label $w.label -text "Enter CDROM Path"
    pack $w.label
    entry $w.entry
    $w.entry insert 0 /cdrom/cdrom0
    pack $w.entry -fill x -expand 1 -padx 3 -pady 3

    frame $w.buttons
    pack $w.buttons -fill x
    button $w.buttons.cancel -text Cancel -command "destroy $w"
    pack $w.buttons.cancel -side left -fill x -expand 1 -padx 3 -pady 3
    button $w.buttons.ok -text Ok -command vpf2Vec::saveGenVecCommandParms
    pack $w.buttons.ok -side left -fill x -expand 1 -padx 3 -pady 3
}

proc vpf2Vec::saveGenVecCommandParms {} {
    global inputDataType
    global scaleValue
    variable genVecCommandParms

    set genVecCommandParms ""

    set w .vpfParmsDialog
    set dbPath [$w.entry get]

    # dbPath is the cdrom path
    # we expect to see one of the following at the top level:
    # vmaplv0  => Level 0
    # eurnasia => Level 1
    # noamer   => Level 1
    # sasaus   => Level 1
    # soamafr  => Level 1
    # tads     => VITD
    # uvmap    => UVMAP

    if [file isdirectory $dbPath/vmaplv0] {
	set formatType vmaplv0
	set dbName vmaplv0

	if [file isdirectory $dbPath/vmaplv0/eurnasia] {
	    set libName eurnasia
	} elseif [file isdirectory $dbPath/vmaplv0/noamer] {
	    set libName noamer
	} elseif [file isdirectory $dbPath/vmaplv0/sasaus] {
	    set libName sasaus
	} elseif [file isdirectory $dbPath/vmaplv0/soamafr] {
	    set libName soamafr
	} else {
	    set libdirs "eurnasia noamer sasaus soamafr directories"
	    tk_messageBox -type ok -default ok -title "VMAP Level 0 Library Not Found" -message \
		    "VMAP Level 0 library not found in CDROM at \"${dbPath}/vmaplv0\". Expected one of: $libdirs"
	    return
	}
    } elseif [file isdirectory $dbPath/eurnasia] {
	set formatType vmaplv1
	set dbName eurnasia
    } elseif [file isdirectory $dbPath/noamer] {
	set formatType vmaplv1
	set dbName noamer
    } elseif [file isdirectory $dbPath/sasaus] {
	set formatType vmaplv1
	set dbName sasaus
    } elseif [file isdirectory $dbPath/soamafr] {
	set formatType vmaplv1
	set dbName soamafr
    } elseif [file isdirectory $dbPath/tads] {
	set formatType vitd
	set dbName tads
    } elseif [file isdirectory $dbPath/uvmap] {
	set formatType uvmap
	set dbName uvmap
    } else {
	set formatType unknown
    }

    if {! [string compare $formatType "unknown"]} {
	if [file isdirectory $dbPath/] {
	    set topdirs "vmaplv0 vmaplv1 eurnasia noamer sasaus soamafr tads uvmap directories"
	    tk_messageBox -type ok -default ok -title "VMAP Data Not Found" -message "VMAP data not found in CDROM at \"$dbPath\". Expected one of: $topdirs"
	} else {
	    tk_messageBox -type ok -default ok -title "CDROM Not Found" -message "CDROM not found at \"$dbPath\""
	}
	return
    }

    # get vmaplv1 libName
    if {! [string compare $formatType "vmaplv1"]} {
	set libName [glob -nocomplain $dbPath/$dbName/lib_*]
	set libName [lindex [split $libName /] end]

	if {! [string compare $libName ""]} {
	    tk_messageBox -type ok -default ok -title "VMAP Level 1 Library Not Found" -message \
		    "VMAP Level 1 library not found in CDROM at \"${dbPath}/${dbName}/lib_*\""
	    return
	}
    }

    if {! [string compare $formatType "vitd"] || ! [string compare $formatType "uvmap"]} {
	# get vitd libName
	if {! [string compare $formatType "vitd"]} {
	    set libPath $dbPath/tads
	}
	
	# get uvmap libName
	if {! [string compare $formatType "uvmap"]} {
	    set libPath $dbPath/uvmap
	}
	
	set dirs [exec ls -l $libPath | grep "^d"]
	set libs {}
	set dirs [split $dirs \n]
	foreach dir $dirs {
	    lappend libs [lindex $dir end]
	}

	if {[llength $libs] == 1} {
	    set libName [lindex $libs 0]
	} else {
	    # choose lib
	    chooseLib $libs $dbPath $dbName $formatType

	    destroy $w
	    return
	}
    }

    set genVecCommandParms "$dbPath $dbName $libName $formatType"

    destroy $w

    set scaleValue(VMAP) "$formatType, $dbName, $libName"
    MUSESRV_SetVMAP .museCon
}

proc vpf2Vec::chooseLib {libs dbPath dbName formatType} {
    set w .vpfLibsDialog
    catch {destroy $w}
    toplevel $w
    wm title $w "VPF $formatType Library Selection"

    label $w.label -text "Select $formatType Library"
    pack $w.label
    eval tk_optionMenu $w.options libChoice $libs
    pack $w.options

    frame $w.buttons
    pack $w.buttons -fill x
    button $w.buttons.cancel -text Cancel -command "destroy $w"
    pack $w.buttons.cancel -side left -fill x -expand 1 -padx 3 -pady 3
    button $w.buttons.ok -text Ok -command [list vpf2Vec::saveGenVecLibChoice $w $dbPath $dbName $formatType]
    pack $w.buttons.ok -side left -fill x -expand 1 -padx 3 -pady 3
}
proc vpf2Vec::saveGenVecLibChoice {w dbPath dbName formatType} {
    global scaleValue
    global libChoice
    variable genVecCommandParms

    set genVecCommandParms "$dbPath $dbName $libChoice $formatType"

    destroy $w

    set scaleValue(VMAP) "$formatType, $dbName, $libChoice"
    MUSESRV_SetVMAP .museCon
}

# The VPF export command looks like:
# vpfimpcl -feat '/cdrom/cdrom0,vmaplv0,rference,placenam,Place Names,placenam.pft,*,,,,,,,,,,,' -outt 7 -ext -120 20 -80 50 -o test.arc
# The parameters will be stored in a file vpfParms.dat and have the form:
# File :== Parmdeslist
# Parmdeslist :== '{' Parmdes {Parmdes} '}'
# Parmdes :== '{' db_name Covlist} '}'
# Covlist :== '{' Cov {Cov} '}'
# Cov :== '{' cov_name Fclasslist '}'
# Fclasslist :== '{' Fclassdes {Fclassdes} '}'
# Fclassdes :== '{' fclass qexp pgmval '}'
# The generated files will be named XXX_YYY where XXX is unique for each file and YYY is its pgm
# value. 
proc vpf2Vec::genVec {x1 y1 x2 y2 dbPath dbName libName dbType regionName xpix ypix} {
    global env

    if {! [file exists $env(TMA_DATA)/vpfParms.dat]} {
	puts "Failed opening $env(TMA_DATA)/vpfParms.dat"
	return
    }
    set parmDesFile [open $env(TMA_DATA)/vpfParms.dat r]
    set parmDesList [read $parmDesFile]
    close $parmDesFile

    set covList {}
    foreach parmDes $parmDesList {
	if {[lindex $parmDes 0] == $dbType} {
	    set covList [lindex $parmDes 1]
	    break
	}
    }

    # clear out existing file in case it exists
    foreach themeType {Roads Rivers TerrainVec} {
	close [open $env(TMA_DATA)/${regionName}${themeType}.tvf w]
    }

    foreach cov $covList {
	set covName [lindex $cov 0]
	set fclassList [lindex $cov 1]
	foreach fclassDes $fclassList {
	    set fclass [lindex $fclassDes 0]
	    set qexp [lindex $fclassDes 1]
	    set pgmval [lindex $fclassDes 2]
	    set themeType [lindex $fclassDes 3]
	    if {[string compare $themeType "Roads"] && [string compare $themeType "Rivers"] && [string compare $themeType "TerrainVec"]} {
		puts "Unknown theme type \"$themeType\""
		return
	    }
	    set outfileName $env(TMA_DATA)/${regionName}${themeType}.tvf
	    set cmd "exec -- vpfimpcl \
		    -feat ${dbPath},${dbName},${libName},${covName},${covName},${fclass},${qexp},,,,,,,,,,, \
		    -outt 7 -tvfcode ${pgmval} -ext $x1 $y1 $x2 $y2 -o ${outfileName}"

	    set fullCovPath ${dbPath}/${dbName}/${libName}/${covName}
	    if [file isdirectory $fullCovPath] {
		if [file exists /tmp/tmacmd.log] {
		    set file [open /tmp/tmacmd.log a]
		} else {
		    set file [open /tmp/tmacmd.log w]
		}
		puts $file $cmd
		close $file
		
		puts $cmd

		catch {eval $cmd}
	    } else {
		puts "Coverage ${covName} not available in ${libName} library of ${dbName}"
	    }
	}

    }

    switch $dbType {
	vmaplv0 {
	    set dataSet VMAP_0
	}
	vmaplv1 {
	    set dataSet VMAP_1
	}
	vitd {
	    set dataSet VITD
	}
	uvmap {
	    set dataSet UVMAP
	}
	default {
	    set dataSet unknown
	}
    }

    #Update the REGION_THEMES file
    foreach themeType {Roads Rivers TerrainVec} {
	Data_Delete tma_regionThemes [list \
          [list REGION_NAME                $regionName] \
          [list THEME_TYPE                 $themeType]]
        set values [list                   \
          [list REGION_NAME                $regionName] \
	  [list DATA_SET                   $dataSet] \
          [list THEME_TYPE                 $themeType] \
          [list COUNT_HEIGHT               $ypix] \
          [list COUNT_WIDTH                $xpix] \
          [list TEXT_FILENAME              ${regionName}${themeType}.tvf] \
          [list TEXT_FORMAT                TVF] \
	  [list REGISTRATION_LAT           0] \
	  [list REGISTRATION_LON           0] \
          ]
        Data_Insert tma_regionThemes $values
    }

    set appId mil

    # create terrain pgm
    set cmd "exec -- rasterize $env(TMA_DATA)/${regionName}TerrainVec.tvf $env(TMA_DATA)/${regionName}Terrain.pgm $x1 $y1 $x2 $y2 $xpix $ypix ${regionName} 0 $dataSet $appId"
    if [file exists /tmp/tmacmd.log] {
	set file [open /tmp/tmacmd.log a]
    } else {
	set file [open /tmp/tmacmd.log w]
    }
    puts $file $cmd
    close $file

    puts $cmd

    catch {eval $cmd}

    #Update the REGION_THEMES file
    Data_Delete tma_regionThemes [list \
	    [list REGION_NAME                $regionName] \
	    [list THEME_TYPE                 Terrain]]
    set values [list                   \
	    [list REGION_NAME                $regionName] \
	    [list DATA_SET                   $dataSet] \
	    [list THEME_TYPE                 Terrain] \
	    [list COUNT_HEIGHT               $ypix] \
	    [list COUNT_WIDTH                $xpix] \
	    [list TEXT_FILENAME              ${regionName}Terrain.pgm] \
	    [list TEXT_FORMAT                PGM] \
	    [list REGISTRATION_LAT           0] \
	    [list REGISTRATION_LON           0] \
	    ]

    Data_Insert tma_regionThemes $values
    
    # create road intersection file
    set cmd "exec -- intersect $env(TMA_DATA)/${regionName}Roads.tvf $env(TMA_DATA)/${regionName}Roads.rif"
    if [file exists /tmp/tmacmd.log] {
	set file [open /tmp/tmacmd.log a]
    } else {
	set file [open /tmp/tmacmd.log w]
    }
    puts $file $cmd
    close $file

    puts $cmd

    catch {eval $cmd}

    #Update the REGION_THEMES file
    Data_Delete tma_regionThemes [list \
	    [list REGION_NAME                $regionName] \
	    [list THEME_TYPE                 Intersection]]
    set values [list                   \
	    [list REGION_NAME                $regionName] \
	    [list DATA_SET                   $dataSet] \
	    [list THEME_TYPE                 Intersection] \
	    [list COUNT_HEIGHT               $ypix] \
	    [list COUNT_WIDTH                $xpix] \
	    [list TEXT_FILENAME              ${regionName}Roads.rif] \
	    [list TEXT_FORMAT                RIF] \
	    [list REGISTRATION_LAT           0] \
	    [list REGISTRATION_LON           0] \
	    ]
    Data_Insert tma_regionThemes $values
}

