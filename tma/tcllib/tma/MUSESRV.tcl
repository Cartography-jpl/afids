# TO DO:
# 12/14/99 Complete VMAP ingestion
#          Move DTED Ouput posting to Save menu item
#          If a "Data Exists" error occurs, give the user a chance to 
#          overwrite it.
#          Save the AOI across processes
# 12/15/99 Make Default CADRG
#          Add coordinate input format radio buttons
#          Load Comm, Data, and Map layers when required
#          Add Map and TMA buttons
# 12/16/99 The Terrain data should be the same size as the elevatio data

package provide TMASRV 1.0

##############################################################################
###############################      DATA      ###############################
##############################################################################

# initial area.  This is the area I use for all my testing.
set MUSESRV_minlat 34.5
set MUSESRV_maxlat 35
set MUSESRV_minlon 126.5
set MUSESRV_maxlon 127.25
# Visual copies.  This is the buffer for adjusting to screen representations
set MUSESRV_vMinlat 34.5
set MUSESRV_vMaxlat 35
set MUSESRV_vMinlon 126.5
set MUSESRV_vMaxlon 127.25

# Initial coordinate input format
set MUSESRV_degreeFormat "dd"
# Visual copy.   This is the buffer for adjusting to screen representations
set MUSESRV_vDegreeFormat "dd"

# Initial Data type.
set inputDataType  "CADRG"

# Initial Input Scales.
set scaleValue(ADRG) "1:50K"
set scaleValue(CADRG) "1:500K"

set scaleValue(DTED) "30"; # This is a posting value an meant as an output.

set scaleValue(VMAP) "1:50K"

##############################################################################
#   COMMAND         : MUSESRV_Init
#   DESCRIPTION     : This Command is the standard function for calling 
#                     any application in the Techbase environment.
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_Init {} {
    
    package require dbtcl
    
    MUSESRV_GetPrefs
    
    MUSESRV_LoadConsole .museCon
    
#    wm withdraw .
}

##############################################################################
#   COMMAND         : MUSESRV_GetPrefs
#   DESCRIPTION     : This Command retrieves the AOI and the coordinate input
#                     format from the Data layer.
#   PARAMETERS      : 
#   RETURNS         : 
#   TABLES          : AMD_MUSEAOI
##############################################################################
proc MUSESRV_GetPrefs {} {
    global env
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global MUSESRV_degreeFormat
    
    package require dbtcl
    
    # Retrieve the values last used by this person.
    set criteria [list  [list USER $env(USER)] ]
    set sel_column [list  MINLAT MAXLAT MINLON MAXLON FORMAT]
    set chk [lindex [Data_Select AMD_MUSEAOI $sel_column $criteria] 0]
    if {([llength $chk] != 0)} {
	set MUSESRV_minlat [lindex $chk 0]
	set MUSESRV_maxlat [lindex $chk 1]
	set MUSESRV_minlon [lindex $chk 2]
	set MUSESRV_maxlon [lindex $chk 3]
	set MUSESRV_degreeFormat [lindex $chk 4]
	MUSESRV_ChangeFormat
    }
}

##############################################################################
#   COMMAND         : MUSESRV_SavePrefs
#   DESCRIPTION     : This Command stores the current user values in the 
#                     database for retrieval later.
#   PARAMETERS      : 
#   RETURNS         : 
#   TABLES          : AMD_MUSEAOI
##############################################################################
proc MUSESRV_SavePrefs {} {
    global env
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global MUSESRV_degreeFormat
    
    # Save the values last used by this person.
    set criteria [list  [list USER $env(USER)] ]
    Data_Delete AMD_MUSEAOI $criteria
    set values [list \
      [list USER             $env(USER)] \
      [list MINLAT           $MUSESRV_minlat] \
      [list MAXLAT           $MUSESRV_maxlat] \
      [list MINLON           $MUSESRV_minlon] \
      [list MAXLON           $MUSESRV_maxlon] \
      [list FORMAT           $MUSESRV_degreeFormat] ]
    Data_Insert  AMD_MUSEAOI $values
}

##############################################################################
#   COMMAND         : MUSESRV_LoadConsole
#   DESCRIPTION     : This Command initializes the interface. 
#   PARAMETERS      : widget - allows the programmer some flexible use of this 
#                              interface in his/her own application.
#   RETURNS         :
##############################################################################
proc MUSESRV_LoadConsole {widget} {
    global inputDataType
    
    # Resticts the Interface to a toplevel window
    set topLevel [MUSESRV_GetTopLevel $widget]
    
    set topLevel .$topLevel
    
    set museConsoleW [toplevel $topLevel]
    
    # This section builds the menubar and all its menus
    MUSESRV_CreateMenubar $museConsoleW
    
    # This section builds the coordinate input form
    set museConsoleF1 [frame $widget.console]
    MUSESRV_CreateConsole $museConsoleF1
    
    # This section sets the environment for the default data type
    MUSESRV_Set$inputDataType $widget
    
    # Display the interface that has been built.
    wm title $widget "Map Creator"
    pack $museConsoleF1 -fill both -expand true
}

##############################################################################
#   COMMAND         : MUSESRV_GetTopLevel
#   DESCRIPTION     : This command peels off the name of the top level widget.
#   PARAMETERS      : widget - any valid widget name.  The widget need not 
#                     exist
#   RETURNS         : Top level widget name
##############################################################################
proc MUSESRV_GetTopLevel {widget} {
    # Extract the top level widget.
    set pagetype [lindex [split $widget .] 1]
    return $pagetype
}

##############################################################################
#   COMMAND         : MUSESRV_CreateMenubar
#   DESCRIPTION     : This command makes the menubar for the window.
#   PARAMETERS      : widget - build menubar in this widget.
#   RETURNS         : None
##############################################################################
proc MUSESRV_CallCreateMap {widget} {
    global MUSESRV_minlat MUSESRV_maxlat MUSESRV_minlon MUSESRV_maxlon scaleValue inputDataType
	    
    MUSESRV_CreateMap $widget \
	    $MUSESRV_minlat $MUSESRV_maxlat \
	    $MUSESRV_minlon $MUSESRV_maxlon \
	    $inputDataType \
	    [MUSESRV_GetMapName]
}
    

proc MUSESRV_CreateMenubar {widget} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global scaleValue 
    global inputDataType
    
    set topLevel .[MUSESRV_GetTopLevel $widget]
    set menubar  $topLevel.menubar
    frame $topLevel.menubar -relief raised -borderwidth 2
    
    # File
    
    set menu $menubar.file
    menubutton $menu \
      -text "File" \
      -menu $menu.menu
    
    set menu $menu.menu
    menu $menu -tearoff 0
    
    $menu add cascade \
      -label "Source.." \
      -menu $menu.datatype
    
    $menu add command \
      -label "Eject" \
      -command {exec eject}
    
    set datatype $menu.datatype
    menu $datatype \
      -tearoff 0
    
    # Attach the Source Menus
    
    # Background data
    MUSESRV_CreateCADRGSourceMenu $datatype
    MUSESRV_CreateADRGSourceMenu $datatype
    
    $datatype add separator
    
    # Elevation Data
    MUSESRV_CreateDTEDSourceMenu $datatype
    
    $datatype add separator
    
    #Terrain Data
    MUSESRV_CreateVMAPSourceMenu $datatype
    
    $menu add separator
    
    # For now, the output scale is an implied value
#    set command "MUSESRV_CreateMap $widget \
#      \$MUSESRV_minlat \$MUSESRV_maxlat \
#      \$MUSESRV_minlon \$MUSESRV_maxlon \
#      \$scaleValue(\$inputDataType) \$inputDataType \
#      \[MUSESRV_GetMapName\]"
    
#    $menu add command \
#      -label "Save As..."  \
#      -command $command

    $menu add command \
      -label "Save As..."  \
      -command "MUSESRV_CallCreateMap $widget"
    
    $menu add command \
      -label "Exit" \
      -command "MUSESRV_Exit $topLevel"
    
    # Edit
    
    set menu $menubar.edit
    menubutton $menu \
      -text "Edit" \
      -menu $menu.menu
    
    set menu $menu.menu
    menu $menu -tearoff 0
    
    $menu add command \
      -label "Undo" \
      -state disabled
    
    $menu add separator
    
    $menu add command \
      -label "Cut" \
      -state disabled
    
    $menu add command \
      -label "Copy" \
      -state disabled
    
    $menu add command \
      -label "Paste" \
      -state disabled
    
    $menu add command \
      -label "Clear" \
      -state disabled
    
    $menu add separator
    
    $menu add command \
      -label "Revert" \
      -command MUSESRV_GetPrefs
    
    # Help
    
    set menu $menubar.help
    menubutton $menu \
      -text "Help" \
      -menu $menu.menu
    
    set menu $menu.menu
    menu $menu -tearoff 0
    
    $menu add command \
      -label "About Map Creator" \
      -command "MUSESRV_Help $widget"
    
    pack $menubar.file \
      $menubar.edit \
      -side left
    
    pack $menubar.help -side right
    
    pack $menubar -side top -fill x
}

##############################################################################
#   COMMAND         : MUSESRV_Exit
#   DESCRIPTION     : This command 
#   PARAMETERS      : window
#   RETURNS         : 
##############################################################################
proc MUSESRV_Exit {widget} {
    global env
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global MUSESRV_degreeFormat
    
    set topLevel ".[MUSESRV_GetTopLevel $widget]"
    
    if {($MUSESRV_minlat != $MUSESRV_maxlat) && \
	    ($MUSESRV_minlon != $MUSESRV_maxlon)} {
	MUSESRV_SavePrefs
    }
    destroy .museCon
}

##############################################################################
#   COMMAND         : MUSESRV_CreateMap
#   DESCRIPTION     : This command is the main function.  It builds and stores
#                     the layers of the Map
#   PARAMETERS      : bottom - lower coordinate of the map
#   
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateMap {widget bottom top left right dataType iName} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global scaleValue
    global inputDataType
    global env
    
    # If a valid name is not passed the procedure will exit.  
    # This method is used to keep functions simple
    #puts "CreateMap $iName"
    if {($iName == {})} {
        return -1
    }
    
    # Determine the image size
    if {$inputDataType != "VMAP"} {
	set size [MUSESRV_CalcImageExtent $inputDataType $scaleValue($inputDataType)]

	if {($size == {})} {
	    return -1
	}
    } else {
	set criteria [list [list REGION_NAME $iName] [list THEME_TYPE Elevation]]
	set sel_column [list COUNT_WIDTH COUNT_HEIGHT]
	set size [Data_Select tma_regionThemes $sel_column $criteria]
	
	if {[llength $size] == 0} {
	    tk_messageBox \
		   -message "Elevation map missing in db for $iName; unable to generate Terrain data" \
		   -parent $widget
	    return -1
	}

	set criteria [list [list REGION_NAME $iName] [list THEME_TYPE Elevation]]
	set sel_column [list NWLAT NWLON SELAT SELON]
	set result [lindex [Data_Select tma_regionThemes $sel_column $criteria] 0]
	set y1x0 [lrange $result 0 1]
	set y0x1 [lrange $result 2 3]
    }
    
    # The image size is returned as a single list
    set xsize [lindex $size 0]
    set ysize [lindex $size 1]
    
    # For not yet implimented code
    set skip 0
    
    # Based on the user input the other values of the layers are determined.
    set themetype "Background"
    set ext "gif"
    switch $inputDataType {
        DTED {
            set themetype "Elevation"
            set ext "raw"
        }
        CADRG -
        ADRG {
            set themetype "Background"
            set ext "gif"
        }
        VMAP {
            set themetype "Terrain"
            set ext "pgm"
            # I am using this as a place holder for unimplimented funtions
            # Currently, TMA requires all 3 layers exist
            set skip 1
        }
    }
    
    set name $iName$themetype
    
    set criteria [list [list REGION_NAME $iName] [list THEME_TYPE $themetype]]
    set sel_column [list REGION_NAME THEME_TYPE]
    set chk [Data_Select tma_regionThemes $sel_column $criteria]
    
    #puts "Data exists: >$chk<"
    
    set path $env(TMA_DATA)
    set oName $path/$name
    if {([llength $chk] > 0) || ([file exists $oName.$ext] == 1)} {
        set answer [tk_messageBox \
          -message "Data exists ($oName.$ext), would you like to overwrite it?" \
          -type yesno -icon question -parent $widget]
        if  {($answer == "no")} {
            return -1
        }
    }
    
    #puts "import $bottom $top $left $right  $xsize $ysize  $inputDataType  $name"
    
    set temp ""
    if {($skip)} { 
        if {! [file exists ${path}/${iName}Elevation.raw]} {
          set answer [tk_messageBox \
            -message "The Elevation file (${path}/${iName}Elevation.raw]) must be created first. \nLayer creation aborted." \
            -type ok -icon info -parent $widget]
          return -1
        }

	set x0 [lindex $y1x0 1]
	set y0 [lindex $y0x1 0]
	set x1 [lindex $y0x1 1]
	set y1 [lindex $y1x0 0]

	# this is necessary because the code below which updates the database reads these
	# variables. non vmap code uses these values, obtained from the map creator dialog,
	# but vmap uses the extent already in the database for the Elevation themetype.
	set MUSESRV_minlon $x0
	set MUSESRV_minlat $y0
	set MUSESRV_maxlon $x1
	set MUSESRV_maxlat $y1

	set cmd [concat vpf2Vec::genVec $x0 $y0 $x1 $y1 $vpf2Vec::genVecCommandParms $iName $xsize $ysize]

	eval $cmd

#          set imageVar [image create photo -file ${path}${iName}Elevation.$ext]
#          set xsize [image width $imageVar]
#          set ysize [image height $imageVar]
#          image delete $imageVar

#          exec pgmcrater -number 1 -xsize $xsize -ysize $ysize > t$name.$ext
#          exec pgmedge t$name.$ext > $name.$ext
#          exec rm t$name.$ext
        
    } else {
        set temp [exec import \
          $bottom $top $left $right \
          $xsize $ysize \
          $inputDataType \
          $name >& /dev/null]
    }
    
    #puts "import $name.$ext"
    if {(($inputDataType != "VMAP") \
	    && ([llength [string trim $temp " \n\t"]] > 0) \
	    && ([string match {*output *names*} $temp] != 1)) \
	    || ([file exists "$env(TMA_DATA)/$name.$ext"] == 0)} {

	set diag "inputDataType: $inputDataType temp: $temp length: [llength [string trim $temp " \n\t"]] match: [string match {*output *names*} $temp] exists ($env(TMA_DATA)/$name.$ext): [file exists "$env(TMA_DATA)/$name.$ext"]"

        tk_messageBox \
          -message "An Error Occured.\n$temp\n$diag" \
          -type ok -icon info -parent $widget
        return -1
    } else {
        
#        set imageVar [image create photo -file $env(TMA_DATA)/$name.$ext]
#        set xsize [image width $imageVar]
#        set ysize [image height $imageVar]
#        image delete $imageVar
        
        #puts " exec mv $name.$ext $oName.$ext "
#        exec mv $env(TMA_DATA)/$name.$ext $oName.$ext
        set criteria [list  [list REGION_NAME $iName] ]
        set sel_column [list  REGION_NAME]
        set chk [Data_Select tma_regionThemes $sel_column $criteria]
        
	if {$inputDataType != "VMAP"} {
	    #Update the REGION_THEMES file
	    Data_Delete tma_regionThemes [list \
		    [list REGION_NAME                $iName] \
		    [list THEME_TYPE                 $themetype]]
	    set values [list                   \
		    [list REGION_NAME                $iName] \
		    [list DATA_SET                   $inputDataType] \
		    [list THEME_TYPE                 $themetype] \
		    [list COUNT_HEIGHT               [lindex [split $ysize .] 0] ] \
		    [list COUNT_WIDTH                [lindex [split $xsize .] 0] ] \
		    [list TEXT_FILENAME              $name.$ext] \
		    [list TEXT_FORMAT                [string toupper $ext]] \
		    [list REGISTRATION_LAT           0] \
		    [list REGISTRATION_LON           0] \
		    ]
	    Data_Insert tma_regionThemes $values
	}
        
        #puts "Map Exists: >$chk<"
        
	if {$inputDataType != "VMAP"} {

	    if {([llength $chk] > 0)} {
		tk_messageBox \
			-message "The data has been saved." \
			-type ok -icon info -parent $widget 
		return 0 
	    }
        
	    #Update the COORDINATE file
	    #Step 1: Install the NW point
	    set values [list                   \
		    [list REGION_NAME                $iName] \
		    [list POINT_NAME                 "NW"] \
		    [list LATITUDE_POINT             $MUSESRV_maxlat] \
		    [list LONGITUDE_POINT            $MUSESRV_minlon] \
		    ]
	    Data_Insert AMD_COORDINATE $values
	    
	    #Step 2: Install the SE point
	    set values [list                   \
		    [list REGION_NAME                $iName] \
		    [list POINT_NAME                 "SE"] \
		    [list LATITUDE_POINT             $MUSESRV_minlat] \
		    [list LONGITUDE_POINT            $MUSESRV_maxlon] \
		    ]
	    Data_Insert AMD_COORDINATE $values
	}

        tk_messageBox \
          -message "The data has been saved." \
          -type ok -icon info -parent $widget 
        return 0
    }
}

##############################################################################
#   COMMAND         : MUSESRV_Help
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_Help {widget} {
    tk_messageBox -message "Map Creator [lindex {\$Revision: 1.3 $} 1]" \
      -type ok -icon info -parent $widget
}

##############################################################################
#   COMMAND         : MUSESRV_CreateConsole
#   DESCRIPTION     : This command 
#   PARAMETERS      : window
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateConsole {widget} {
    global MUSESRV_vMinlat MUSESRV_vMinlon MUSESRV_vMaxlon MUSESRV_vMaxlat
    global MUSESRV_vDegreeFormat
    global scaleValue inputDataType
    
    #
    # Insert the cordinate entry boxes
    #
    
    frame $widget.coords -borderwidth 10
    
    entry $widget.coords.minlon -textvariable MUSESRV_vMinlon -width 9
    entry $widget.coords.minlat -textvariable MUSESRV_vMinlat -width 9
    entry $widget.coords.maxlat -textvariable MUSESRV_vMaxlat -width 9
    entry $widget.coords.maxlon -textvariable MUSESRV_vMaxlon -width 9
    label $widget.coords.space -text "@"
    
    bind $widget.coords.minlat <FocusOut> {MUSESRV_Validate %W}
    bind $widget.coords.minlon <FocusOut> {MUSESRV_Validate %W}
    bind $widget.coords.maxlat <FocusOut> {MUSESRV_Validate %W}
    bind $widget.coords.maxlon <FocusOut> {MUSESRV_Validate %W}
    
    #
    # Insert the Source Data Type and Scale displays
    #
    
    frame $widget.info 
    label $widget.info.inputType -textvariable  inputDataType
    label $widget.info.inputScale -textvariable  scaleValue($inputDataType)
    
    #
    # Insert the Input format selector buttons
    #
    
    frame $widget.format
    radiobutton $widget.format.degree1 \
      -text "Decimal \ndegrees" \
      -variable MUSESRV_degreeFormat \
      -value "dd" \
      -command "MUSESRV_ChangeFormat"
    
    radiobutton $widget.format.degree2 \
      -text "Hours, Minutes, \nSeconds" \
      -variable MUSESRV_degreeFormat \
      -value "hms" \
      -command "MUSESRV_ChangeFormat"
    
    
    pack $widget.coords $widget.format $widget.info -side top
    
    pack $widget.coords.maxlat  -side top
    pack $widget.coords.minlon -side left
    pack $widget.coords.maxlon  -side right
    pack $widget.coords.minlat -side bottom
    pack $widget.coords.space -side top 
    
    pack $widget.format.degree1 $widget.format.degree2 -side left
    
    pack $widget.info.inputType $widget.info.inputScale -side left
    
}

##############################################################################
#   COMMAND         : MUSESRV_ChangeFormat
#   DESCRIPTION     : This command 
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_ChangeFormat {} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global MUSESRV_vMinlat MUSESRV_vMinlon MUSESRV_vMaxlon MUSESRV_vMaxlat
    global MUSESRV_degreeFormat MUSESRV_vDegreeFormat
    package require maptcl
    
    #
    # Convert to decimal for arithimatic calculations
    #
    
    if {($MUSESRV_degreeFormat == $MUSESRV_vDegreeFormat)} {
	# The User invoked a button that was already selected.
	return 
    }
    
    switch $MUSESRV_degreeFormat {
	"dd" {
	    set MUSESRV_vMaxlat $MUSESRV_maxlat
	    set MUSESRV_vMinlat $MUSESRV_minlat
	    set MUSESRV_vMaxlon $MUSESRV_maxlon
	    set MUSESRV_vMinlon $MUSESRV_minlon
	}
	"hms" {
	    set max [Map_CoordToString [list $MUSESRV_maxlat $MUSESRV_maxlon]]
	    set min [Map_CoordToString [list $MUSESRV_minlat $MUSESRV_minlon]]
	    set MUSESRV_vMaxlat [lreplace $max 3 end]
	    set MUSESRV_vMinlat [lreplace $min 3 end]
	    set MUSESRV_vMaxlon [lreplace $max 0 2]
	    set MUSESRV_vMinlon [lreplace $min 0 2]
	}
    }
    set MUSESRV_vDegreeFormat $MUSESRV_degreeFormat
}


##############################################################################
#   COMMAND         : MUSESRV_Validate
#   DESCRIPTION     : This command 
#   PARAMETERS      : widget
#   RETURNS         : 
##############################################################################
proc MUSESRV_Validate {widget} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    global MUSESRV_vMinlat MUSESRV_vMinlon MUSESRV_vMaxlon MUSESRV_vMaxlat
    global MUSESRV_degreeFormat MUSESRV_vDegreeFormat
    package require maptcl
    
    set value [lindex [split $widget .] end]
    
    #
    # Validate the ranges
    #

    switch $value {
	maxlat {
	    switch $MUSESRV_vDegreeFormat {
		"dd" {
		    set MUSESRV_maxlat $MUSESRV_vMaxlat
		}
		"hms" {
		    set MUSESRV_maxlat [lindex [Map_StringToCoord [concat $MUSESRV_vMaxlat $MUSESRV_vMaxlon]] 0]
		}
	    }
	    
	    if  {($MUSESRV_maxlat < $MUSESRV_minlat) || ($MUSESRV_maxlat > 90) } {
		set MUSESRV_maxlat $MUSESRV_minlat
		puts "\007"
	    }
	}
	minlon {
	    switch $MUSESRV_vDegreeFormat {
		"dd" {
		    set MUSESRV_minlon $MUSESRV_vMinlon
		}
		"hms" {
		    set MUSESRV_minlon [lindex [Map_StringToCoord [concat $MUSESRV_vMinlat $MUSESRV_vMinlon]] 1]
		}
	    }

	    if  {($MUSESRV_minlon < -180) || ($MUSESRV_minlon > $MUSESRV_maxlon)} {
		set MUSESRV_minlon $MUSESRV_maxlon
		puts "\007"
	    }
	}
	maxlon {
	    switch $MUSESRV_vDegreeFormat {
		"dd" {
		    set MUSESRV_maxlon $MUSESRV_vMaxlon
		}
		"hms" {
		    set MUSESRV_maxlon [lindex [Map_StringToCoord [concat $MUSESRV_vMaxlat $MUSESRV_vMaxlon]] 1]
		}
	    }

	    if  {($MUSESRV_maxlon > 180) || ($MUSESRV_maxlon < $MUSESRV_minlon)} {
		set MUSESRV_maxlon $MUSESRV_minlon
                puts "\007"
            }
        }
        minlat {
	    switch $MUSESRV_vDegreeFormat {
		"dd" {
		    set MUSESRV_minlat $MUSESRV_vMinlat
		}
		"hms" {
		    set MUSESRV_minlat [lindex [Map_StringToCoord [concat $MUSESRV_vMinlat $MUSESRV_vMinlon]] 0]
		}
	    }

	    if  {($MUSESRV_minlat < -90) || ($MUSESRV_minlat > $MUSESRV_maxlat)} {
                set MUSESRV_minlat $MUSESRV_maxlat
                puts "\007"
            }
        }
    }
    
    #
    # Convert to visual values
    #
    
    switch $MUSESRV_vDegreeFormat {
	"hms" {
	    set max [Map_CoordToString [list $MUSESRV_maxlat $MUSESRV_maxlon]]
	    set min [Map_CoordToString [list $MUSESRV_minlat $MUSESRV_minlon]]
	    set MUSESRV_vMaxlat [lreplace $max 3 end]
	    set MUSESRV_vMinlat [lreplace $min 3 end]
	    set MUSESRV_vMaxlon [lreplace $max 0 2]
	    set MUSESRV_vMinlon [lreplace $min 0 2]
	}
	"dd" {
	    set MUSESRV_vMaxlat $MUSESRV_maxlat
	    set MUSESRV_vMinlat $MUSESRV_minlat
	    set MUSESRV_vMaxlon $MUSESRV_maxlon
	    set MUSESRV_vMinlon $MUSESRV_minlon
	}
    }
    
    
}

##############################################################################
#   COMMAND         : 
#   DESCRIPTION     : This command 
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_CalcImageExtent {dataType scale} {
    
    set output [list 100 100]
    
    switch $dataType {
        "ADRG" {
            set output [MUSESRV_CalcADRGImageExtent $scale]
        }
        "CADRG" {
            set output [MUSESRV_CalcCADRGImageExtent $scale]
        }
        "DTED" {
            set output [MUSESRV_CalcDTEDImageExtent $scale]
        }
    }
    
    return $output
}


##############################################################################
#   COMMAND         : MUSESRV_GetMapName
#   DESCRIPTION     : This command 
#   PARAMETERS      : menu
#   RETURNS         : 
##############################################################################
proc MUSESRV_GetMapName {} {
    global submit scaleValue inputDataType
    package require dbtcl
    package require maptcl
    
    set submit {}
    set sel_column [list NAME]
    set maplist [Data_Select tma_regionThemes $sel_column {}]
    
    set maps [toplevel .fileDialog -class Dialog ]
    wm title $maps "Save Map As.."
    
    set upper [frame ${maps}.upper -relief raised -bd 2]
    pack ${upper} -fill both -side top
    set lower [frame ${maps}.lower -relief raised -bd 2]
    pack ${lower} -fill both -side bottom
    
    set scroll [scrollbar ${upper}.scroll -command "${upper}.list yview"]
    pack ${scroll} -side right -fill y
    set list [listbox ${upper}.list -yscrollcommand "${upper}.scroll set"]
    pack ${list} -fill both -expand true
    
    foreach map ${maplist} {
        ${list} insert end ${map}
    }
    
    if {$inputDataType != "VMAP" && $inputDataType != "DTED"} {
	entry ${lower}.entry
	pack ${lower}.entry -side left
    }
    
    button ${lower}.ok -text "OK"
    button ${lower}.cancel -text "Cancel"
    pack ${lower}.ok ${lower}.cancel
    
    bind ${lower}.ok <Button-1> "set submit ok"
    bind ${lower}.cancel <Button-1> "set submit cancel"
    
    switch $inputDataType {
        DTED {
            set menubar  $lower.menubar
            frame $lower.menubar -relief raised -borderwidth 2
            
            # File
            
            set menu $menubar.posting
            menubutton $menu \
              -text "Posting" \
              -menu $menu.menu
            
            
            set submenu $menu.menu
            menu $submenu -tearoff 0
            
#              $submenu add radio \
#                -label "10 meter" \
#                -variable scaleValue(DTED) \
#                -value "10" \
              
            
#              $submenu add radio \
#                -label "30 meter" \
#                -variable scaleValue(DTED) \
#                -value "30" \
              
            $submenu add radio \
              -label "100 meter" \
              -variable scaleValue(DTED) \
              -value "100" \
              
            $submenu add radio \
              -label "250 meter" \
              -variable scaleValue(DTED) \
              -value "250" \
              
            $submenu add radio \
              -label "500 meter" \
              -variable scaleValue(DTED) \
              -value "500" \
              
            $submenu add radio \
              -label "1000 meter" \
              -variable scaleValue(DTED) \
              -value "1000" \
              
            pack $menubar.posting
            pack $menubar
        }
    }
    
    
    if {$inputDataType != "VMAP" && $inputDataType != "DTED"} {
	set oldfocus [focus]
	focus ${lower}.entry
    }
    grab ${maps}
    
    tkwait variable submit 
    
    if {$inputDataType != "VMAP" && $inputDataType != "DTED"} {
	set name {}
	if {($submit == "ok")} {
	    set name [${lower}.entry get]
	    set stripName {}
	    foreach wrd $name {
		set stripName $stripName$wrd
	    }
	    set name $stripName
	} else {
	    set name {}
	}
    } else {
	# ignore multiple selections
	set selection [lindex [$list curselection] 0]
	if {$selection != ""} {
	    set name [$list get $selection]
	} else {
	    set name ""
	    tk_messageBox -message "Name not selected; no map created" \
		    -type ok -icon info
	}
    }
    
    if {$inputDataType != "VMAP" && $inputDataType != "DTED"} {
	focus ${oldfocus}
    }

    grab release ${maps}
    destroy ${maps}
    
    # puts "GetMapName: $name"
    return $name
}

##############################################################################
#   COMMAND         : MUSESRV_CreateADRGSourceMenu
#   DESCRIPTION     : This command 
#   PARAMETERS      : menu
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateADRGSourceMenu {mnu} {
    global scaleValue
    
    set widget .[MUSESRV_GetTopLevel $mnu]
    
    $mnu add cascade \
      -label "ADRG" \
      -menu $mnu.scaleADRG 
    
    set submenu $mnu.scaleADRG
    menu $submenu \
      -tearoff 0
    
    $submenu add radio \
      -label "1:50K" \
      -variable scaleValue(ADRG) \
      -value "1:50K" \
      -command "MUSESRV_SetADRG $widget"
    
    $submenu add radio \
      -label "1:100K" \
      -variable scaleValue(ADRG) \
      -value "1:100K" \
      -command "MUSESRV_SetADRG $widget"
    
    $submenu add radio \
      -label "1:250K" \
      -variable scaleValue(ADRG) \
      -value "1:250K" \
      -command "MUSESRV_SetADRG $widget"
}

##############################################################################
#   COMMAND         : MUSESRV_SetADRG
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_SetADRG {widget} {
    global inputDataType scaleValue
    
    set inputDataType "ADRG"
    $widget.console.info.inputScale \
      configure -textvariable  scaleValue($inputDataType)
    
    set submenu $widget.menubar.file.menu.oScale
    destroy $submenu
    menu $submenu \
      -tearoff 0
    
    set command "MUSESRV_CreateMap $widget \
      \$MUSESRV_minlat \$MUSESRV_maxlat \
      \$MUSESRV_minlon \$MUSESRV_maxlon \
      $scaleValue($inputDataType) $inputDataType \
      \[MUSESRV_GetMapName\]"
    
    $submenu add command \
      -label "1:50K" \
      -command $command
    
    $submenu add command \
      -label "1:100K" \
      -command $command
    
    $submenu add command \
      -label "1:250K" \
      -command $command
}

##############################################################################
#   COMMAND         : 
#   DESCRIPTION     : This command 
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_CalcADRGImageExtent {scale} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    
    set area [expr [expr $MUSESRV_maxlat - $MUSESRV_minlat] * \
      [expr $MUSESRV_maxlon - $MUSESRV_minlon] ]
    
    # Use static map to screen size
    if {([string match $scale "1:25K"])} {
        if {($area > .005)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return .5
    } elseif {($scale == "1:50K")} {
        if {($area > .01)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return .5
    } elseif {($scale == "1:250K")} {
        if {($area > .25)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return 1
    } elseif {($scale == "1:500K")} {
        if {($area > 1)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return 1
    } elseif {($scale == "1:1M")} {
        if {($area > 4)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return 1
    }
    
    # Screen Pixel sresolution per inch
    set xScreenPixRes 72
    set yScreenPixRes 72
    
    # Get the screen size in inches
    toplevel .mUSESRVtemp
    set winDim [wm maxsize .mUSESRVtemp]
    destroy .mUSESRVtemp
    set xScreenSize [expr [lindex $winDim 0] / $xScreenPixRes]
    set yScreenSize [expr [lindex $winDim 1] / $yScreenPixRes]
    
    # Use NIMA definitions
    set metersPerPixel 5.0
    if {([string match $scale "1:25K"])} {
        set metersPerPixel 2.5
    } elseif {($scale == "1:50K")} {
        set metersPerPixel 5.0
    } elseif {([string match $scale "1:100K"])} {
        set metersPerPixel 10
    } elseif {( $scale == "1:250K")} {
        set metersPerPixel 25
    } elseif {( $scale == "1:500K")} {
        set metersPerPixel 50
    } elseif {( $scale == "1:1M")} {
        set metersPerPixel 100
    }
    
    set sqMeterPerDeg [MUSESRV_CalcMetersPerDeg \
      [expr [expr $MUSESRV_maxlat + $MUSESRV_minlat] / 2] \
      [expr [expr $MUSESRV_maxlon + $MUSESRV_minlon] / 2] ] 
    set pixelPerDeg [expr [expr 1.0 / $metersPerPixel] \
      * [lindex $sqMeterPerDeg 0] ]
    
    #puts "Scale: >$scale<"
    #puts "MPP: $metersPerPixel"
    #puts "M2PD: $sqMeterPerDeg"
    #puts "PPD: $pixelPerDeg"
    
    set x [expr \
      [expr $MUSESRV_maxlon - $MUSESRV_minlon] * $pixelPerDeg]
    set y [expr \
      [expr $MUSESRV_maxlat - $MUSESRV_minlat] * $pixelPerDeg]
    
    #puts "X, Y: $x, $y"
    return [expr 1 / [expr pow(2.0,[expr [expr $MUSESRV_maxlon - $MUSESRV_minlon] * [expr $MUSESRV_maxlat - $MUSESRV_minlat]])]]
    return [list $x $y]
}

##############################################################################
#   COMMAND         : MUSESRV_CreateDTEDSourceMenu
#   DESCRIPTION     : This command 
#   PARAMETERS      : menu
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateDTEDSourceMenu {mnu} {
    global scaleValue
    
    set widget .[MUSESRV_GetTopLevel $mnu]
    
    $mnu add command \
      -label "DTED" \
      -command "MUSESRV_SetDTED $widget"


    # ************************ => RETURN 0 <= ******************************

    return 0
    
    $mnu add cascade \
      -label "DTED" \
      -menu $mnu.scaleDTED \
      -command "MUSESRV_SetDTED $widget"
    
    set submenu $mnu.scaleDTED
    menu $submenu \
      -tearoff 0
    
    $submenu add radio \
      -label "10 meter" \
      -variable scaleValue(DTED) \
      -value "10" \
      -command "MUSESRV_SetDTED $widget"
    
    
    $submenu add radio \
      -label "30 meter" \
      -variable scaleValue(DTED) \
      -value "30" \
      -command "MUSESRV_SetDTED $widget"
    
    $submenu add radio \
      -label "100 meter" \
      -variable scaleValue(DTED) \
      -value "100" \
      -command "MUSESRV_SetDTED $widget"
    
    $submenu add radio \
      -label "250 meter" \
      -variable scaleValue(DTED) \
      -value "250" \
      -command "MUSESRV_SetDTED $widget"
    
    $submenu add radio \
      -label "500 meter" \
      -variable scaleValue(DTED) \
      -value "500" \
      -command "MUSESRV_SetDTED $widget"
    
    $submenu add radio \
      -label "1000 meter" \
      -variable scaleValue(DTED) \
      -value "1000" \
      -command "MUSESRV_SetDTED $widget"
}

##############################################################################
#   COMMAND         : MUSESRV_SetDTED
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_SetDTED {widget} {
    global inputDataType scaleValue
    
    set inputDataType "DTED"
    $widget.console.info.inputScale configure \
      -textvariable  scaleValue($inputDataType)
    
    set scaleValue(DTED) 100
    
    set command "set name \[MUSESRV_GetMapName\]; MUSESRV_CreateMap $widget \
      \$MUSESRV_minlat \$MUSESRV_maxlat \
      \$MUSESRV_minlon \$MUSESRV_maxlon \
      \$scaleValue(DTED) DTED \
      \$name"
    
    set submenu $widget.menubar.file.menu.oScale
    destroy $submenu
    menu $submenu \
      -tearoff 0
    
    $submenu add command \
      -label "10 meter" \
      -command $command
    
    $submenu add command \
      -label "30 meter" \
      -command $command
    
    $submenu add command \
      -label "100 meter" \
      -command $command
    
    $submenu add command \
      -label "250 meter" \
      -command $command
    
    $submenu add command \
      -label "500 meter" \
      -command $command
    
    $submenu add command \
      -label "1000 meter" \
      -command $command
}

##############################################################################
#   COMMAND         : 
#   DESCRIPTION     : This command 
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_CalcDTEDImageExtent {scale} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    
    set x [expr \
      [expr $MUSESRV_maxlon - $MUSESRV_minlon] * \
      [expr 108000 / $scale] ]
    set y [expr \
      [expr $MUSESRV_maxlat - $MUSESRV_minlat] * \
      [expr 108000 / $scale] ]
    
    return [list $x $y]
}

##############################################################################
#   COMMAND         : MUSESRV_CreateCADRGSourceMenu
#   DESCRIPTION     : This command 
#   PARAMETERS      : menu
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateCADRGSourceMenu {mnu} {
    global scaleValue
    
    set widget .[MUSESRV_GetTopLevel $mnu]
    
    $mnu add cascade \
      -label "CADRG" \
      -menu $mnu.scaleCADRG 
    
    set submenu $mnu.scaleCADRG
    menu $submenu \
      -tearoff 0
    
    $submenu add radio \
      -label "1:500K" \
      -variable scaleValue(CADRG) \
      -value "1:500K" \
      -command "MUSESRV_SetCADRG $widget"
    
    $submenu add radio \
      -label "1:1M" \
      -variable scaleValue(CADRG) \
      -value "1:1M" \
      -command "MUSESRV_SetCADRG $widget"
}

##############################################################################
#   COMMAND         : MUSESRV_SetCADRG
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_SetCADRG {widget} {
    global inputDataType
    set inputDataType "CADRG"
    $widget.console.info.inputScale configure \
      -textvariable  scaleValue($inputDataType)
    
    set submenu $widget.menubar.file.menu.oScale
    destroy $submenu
    menu $submenu \
      -tearoff 0
    
    $submenu add command \
      -label "1:500K"  \
      -command "puts 1:500K"
    
    $submenu add command \
      -label "1:1M" \
      -command "puts 1:1M"
}

##############################################################################
#   COMMAND         : 
#   DESCRIPTION     : This command 
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc MUSESRV_CalcCADRGImageExtent {scale} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    
    set area [expr [expr $MUSESRV_maxlat - $MUSESRV_minlat] * \
      [expr $MUSESRV_maxlon - $MUSESRV_minlon] ]
    
    # Use static map to screen size
    if {([string match $scale "1:500K"])} {
        if {($area > 1)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return .5
    } elseif {([string match $scale "1:1M"])} {
        if {($area > 4)} {
            set answer [tk_messageBox \
              -message "Are you sure? This may take a while." \
              -type yesno -icon question ]
            if {($answer == "no")} {
                return {}
            }
        }
        return 1
    }
    
    # Screen Pixel sresolution per inch
    set xScreenPixRes 72
    set yScreenPixRes 72
    
    # Get the screen size in inches
    toplevel .mUSESRVtemp
    set winDim [wm maxsize .mUSESRVtemp]
    destroy .mUSESRVtemp
    set xScreenSize [expr [lindex $winDim 0] / $xScreenPixRes]
    set yScreenSize [expr [lindex $winDim 1] / $yScreenPixRes]
    
    # Use NIMA definitions
    set metersPerPixel 5.0
    if {([string match $scale "1:25K"])} {
        set metersPerPixel 2.5
    } elseif {($scale == "1:50K")} {
        set metersPerPixel 5.0
    } elseif {([string match $scale "1:100K"])} {
        set metersPerPixel 10
    } elseif {( $scale == "1:250K")} {
        set metersPerPixel 25
    } elseif {( $scale == "1:500K")} {
        set metersPerPixel 50
    } elseif {( $scale == "1:1M")} {
        set metersPerPixel 100
    }
    
    set sqMeterPerDeg [MUSESRV_CalcMetersPerDeg \
      [expr [expr $MUSESRV_maxlat + $MUSESRV_minlat] / 2] \
      [expr [expr $MUSESRV_maxlon + $MUSESRV_minlon] / 2] ] 
    set pixelPerDeg [expr [expr 1.0 / $metersPerPixel] \
      * [lindex $sqMeterPerDeg 0] ]
    
    #puts "Scale: >$scale<"
    #puts "MPP: $metersPerPixel"
    #puts "M2PD: $sqMeterPerDeg"
    #puts "PPD: $pixelPerDeg"
    
    set x [expr \
      [expr $MUSESRV_maxlon - $MUSESRV_minlon] * $pixelPerDeg]
    set y [expr \
      [expr $MUSESRV_maxlat - $MUSESRV_minlat] * $pixelPerDeg]
    
    #puts "X, Y: $x, $y"
    return [expr 1 / [expr pow(2.0,[expr [expr $MUSESRV_maxlon - $MUSESRV_minlon] * [expr $MUSESRV_maxlat - $MUSESRV_minlat]])]]
    return [list $x $y]
}

##############################################################################
#   COMMAND         : MUSESRV_CreateVMAPSourceMenu
#   DESCRIPTION     : This command 
#   PARAMETERS      : menu
#   RETURNS         : 
##############################################################################
proc MUSESRV_CreateVMAPSourceMenu {mnu} {
    global scaleValue
    
    set widget .[MUSESRV_GetTopLevel $mnu]
    
    $mnu add command \
      -label "VMAP" \
      -command vpf2Vec::getVpfParms
}

##############################################################################
#   COMMAND         : MUSESRV_SetVMAP
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_SetVMAP {widget} {
    global inputDataType scaleValue
    
    set inputDataType "VMAP"
    $widget.console.info.inputScale \
      configure -textvariable  scaleValue($inputDataType)
}

##############################################################################
#   COMMAND         : MUSESRV_CalcMetersPerDeg
#   DESCRIPTION     : This command
#   PARAMETERS      : window
#   RETURNS         :
##############################################################################
proc MUSESRV_CalcMetersPerDeg {lat lon} {
    global MUSESRV_minlat MUSESRV_minlon MUSESRV_maxlon MUSESRV_maxlat
    
    set x 111133 
    set meridian 100000
    
    set aLat [expr [expr $lat * $lat] / $lat]
    
    if {($aLat < 5)} {
        set meridian {111000}
    } elseif {($aLat < 9)} {
        set meridian {110000}
    } elseif {($aLat < 12)} {
        set meridian {109000}
    } elseif {($aLat < 15)} {
        set meridian {107000}
    } elseif {($aLat < 19)} {
        set meridian {105000}
    } elseif {($aLat < 26)} {
        set meridian {100000}
    } else {
        set meridian {100000}
    }
    return [list $x $meridian]
    
}
