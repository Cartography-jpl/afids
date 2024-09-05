package provide maptcl 1.0

set tcl_precision 17

##############################################################################
###############################      DATA      ###############################
##############################################################################

set MapSRV_anchorX     {}
set MapSRV_anchorY     {}
set MapSRV_modeBox     {off}
set MapSRV_modeClick   {off1}
set MapSRV_current     {}
set MapSRV_overlay     0

##############################################################################
#   COMMAND         : MapSRV_Connect
#   DESCRIPTION     : This command establishes a connection to a map by
#                     prompting user selection from a list of all maps.
#   PARAMETERS      : none
#   RETURNS         : the map name
##############################################################################
proc MapSRV_Connect {} {
    MapSRV_MapOpen
}

##############################################################################
#   COMMAND         : MapSRV_DrawText
#   DESCRIPTION     : This command draws text on the specified map.
#   PARAMETERS      : map      - the map name                   
#                     text     - the text to draw               
#                     position - the location of the text anchor
#                     anchor   - which anchor to use            
#                     color    - the Tk color of the circle     
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawText {map text position anchor color} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    set latpix [MapSRV_LatToPix ${map} [lindex $position 0]]
    set lonpix [MapSRV_LonToPix ${map} [lindex $position 1]]

    $canvas create text $lonpix $latpix -anchor $anchor -text $text -fill $color
}

##############################################################################
#   COMMAND         : MapSRV_DrawEntity
#   DESCRIPTION     : This command draws an entity on the specified map.
#   PARAMETERS      : map      - the map name
#                     coord    - the location coordinate of the entity
#                     entity   - the entity code for the entity
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawEntity {map coord entity} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    set latpix [MapSRV_LatToPix ${map} [lindex ${coord} 0]]
    set lonpix [MapSRV_LonToPix ${map} [lindex ${coord} 1]]

    if {${entity} == "GBCS"} {
        lappend points [expr ${lonpix} - 0] \
                       [expr ${latpix} - 12] \
                       [expr ${lonpix} - 16] \
                       [expr ${latpix} + 6] \
                       [expr ${lonpix} + 16] \
                       [expr ${latpix} + 6]
        eval ${canvas} create polygon ${points} -fill black -width 2
    } else {
        return {}
    }
}

##############################################################################
#   COMMAND         : MapSRV_DrawLine
#   DESCRIPTION     : This command draws lines to points on the specified map.
#   PARAMETERS      : map      - the map name
#                     coords   - the list of coordinates to connect
#                     color    - the Tk color of the line
#                     width    - the width of the line in pixels
#                     arrow    - the Tk arrow format of the line
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawLine {map coords color width arrow} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    foreach coord ${coords} {
        lappend points [MapSRV_LonToPix ${map} [lindex ${coord} 1]] \
                       [MapSRV_LatToPix ${map} [lindex ${coord} 0]]
    }

    eval ${canvas} create line ${points} \
        -arrow ${arrow} -fill ${color} -width ${width} -tags line
}

##############################################################################
#   COMMAND         : MapSRV_DrawOval
#   DESCRIPTION     : This command draws an oval on the specified map.
#   PARAMETERS      : map      - the map name
#                     center   - the center coordinate of the oval
#                     major    - the major semi-axis of the oval in meters
#                     minor    - the minor semi-axis of the oval in meters
#                     angle    - the angle of rotation from north in radians
#                     fill     - the Tk color for filling the oval
#                     outline  - the Tk color for outlining the oval
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawOval {map center major minor angle fill outline} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    if {${major} == ${minor}} {
        set radiusP [expr int([MapSRV_KmToPix ${map} [expr ${major}]])]
	# Limit size of ovals to 5 pixels in radius.
	if {$radiusP > 5} {
	    set radiusP 5
	}
        set centerX [MapSRV_LonToPix ${map} [lindex ${center} 1]]
        set centerY [MapSRV_LatToPix ${map} [lindex ${center} 0]]

        lappend points [expr ${centerX} - ${radiusP}] \
                       [expr ${centerY} - ${radiusP}] \
                       [expr ${centerX} + ${radiusP}] \
                       [expr ${centerY} + ${radiusP}]

        eval ${canvas} create oval ${points} \
          -fill ${fill} -outline ${outline} -width 2 -tags oval
    } else {
        set majorP [expr int([MapSRV_KmToPix ${map} [expr ${major}]])]
        set minorP [expr int([MapSRV_KmToPix ${map} [expr ${minor}]])]
        set centerX [MapSRV_LonToPix ${map} [lindex ${center} 1]]
        set centerY [MapSRV_LatToPix ${map} [lindex ${center} 0]]

        lappend coords [list [expr ${centerY} - ${minorP}] ${centerX}] \
                       [list ${centerY} [expr ${centerX} - ${majorP}]] \
                       [list [expr ${centerY} + ${minorP}] ${centerX}] \
                       [list ${centerY} [expr ${centerX} + ${majorP}]]

        if {${angle} != {}} {
            set offset [list -${centerX} -${centerY}]]
            set coords [MapSRV_Translate ${coords} ${offset}]
            set coords [MapSRV_Rotate ${coords} ${angle}]
            set offset [list ${centerX} ${centerY}]]
            set coords [MapSRV_Translate ${points} ${offset}]
        }

        set points {}
        foreach coord ${coords} {
            set points [concat ${points} ${coord}]
        }

        eval ${canvas} create polygon ${points} \
          -fill ${fill} -outline ${outline} -smooth true -width 2
    }
}

##############################################################################
#   COMMAND         : MapSRV_DrawOverlay
#   DESCRIPTION     : This command draws a color overlay on the specified map.
#   PARAMETERS      : map      - the map name
#                     overlay  - the array of Tk color values
#                     nwpoint  - the northwest coordinate of the covered area
#                     sepoint  - the southeast coordinate of the covered area
#                     height   - the number of gridpoints from north to south
#                     width    - the number of gridpoints from east to west
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawOverlay {map overlay nwpoint sepoint height width} {
    global MapSRV_mapData MapSRV_overlay

    set canvas [lindex $MapSRV_mapData(${map}) 0]
    set tag {}

    if {[expr ${height} * ${width}] == [llength ${overlay}]} {
        set north [MapSRV_LatToPix ${map} [lindex ${nwpoint} 0]]
        set south [MapSRV_LatToPix ${map} [lindex ${sepoint} 0]]
        set east [MapSRV_LonToPix ${map} [lindex ${sepoint} 1]]
        set west [MapSRV_LonToPix ${map} [lindex ${nwpoint} 1]]
        set boxwidth [expr round( (${east} - ${west}) / double(${width}) )]
        set boxheight [expr round( (${south} - ${north}) / double(${height}) )]
        set tag "overlay[incr MapSRV_overlay]"

        for {set x 0} {${x} < ${height}} {incr x} {
            set boxnorth [expr ( ${north} + ( ${x} * ${boxheight} ) )]
            set boxsouth [expr ( ${boxnorth} + ${boxheight} )]
            for {set y 0} {${y} < ${width}} {incr y} {
                set boxwest [expr ( ${west} + ( ${y} * ${boxwidth} ) )]
                set boxeast [expr ( ${boxwest} + ${boxwidth} )]
                set index [expr ( ( ${width} * ${x} ) + ${y} )]
                set color [lindex ${overlay} ${index}]
                if {${color} != {}} {
                    ${canvas} create rect \
                      ${boxwest} ${boxnorth} ${boxeast} ${boxsouth} \
                      -fill ${color} -stipple gray50 -outline {} -tags ${tag}
                }
            }
        }
    }

    set tag
}

##############################################################################
#   COMMAND         : MapSRV_DrawPolygon
#   DESCRIPTION     : This command drwas a polygon on the specified map.
#   PARAMETERS      : map      - the map name
#                     coords   - the list of coordinates to connect
#                     fill     - the Tk color for filling the polygon
#                     outline  - the Tk color for outlining the polygon
#                     width    - the width of the polygon outline in pixels
#                     stippleFile - an optional stipple pattern
#                     stippleColor - color for stipple pattern
#   RETURNS         : the object tag
##############################################################################
proc MapSRV_DrawPolygon {map coords fill outline width {stippleFile ""}} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    foreach coord ${coords} {
        lappend points [MapSRV_LonToPix ${map} [lindex ${coord} 1]] \
                       [MapSRV_LatToPix ${map} [lindex ${coord} 0]]
    }

    if {$stippleFile == ""} {
	eval ${canvas} create polygon ${points} \
		-fill ${fill} -outline ${outline} -width ${width}
    } else {
	eval ${canvas} create polygon ${points} \
		-fill ${fill} -outline ${outline} -width ${width} -stipple @$stippleFile -fill $fill
    }
}

##############################################################################
#   COMMAND         : MapSRV_EraseObject
#   DESCRIPTION     : This command erases the object on the specified map.
#   PARAMETERS      : map      - the map name
#                     object   - the object tag
#   RETURNS         : none
##############################################################################
proc MapSRV_EraseObject {map object} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]
    ${canvas} delete ${object}
}

##############################################################################
#   COMMAND         : MapSRV_KmToPix
#   DESCRIPTION     : This command calculates the number of pixels which scale
#                     to the distance in kilometers on the specified map.
#   PARAMETERS      : map      - the map name
#                     distance - the distance in kilometers
#   RETURNS         : the integer number of pixels
##############################################################################
proc MapSRV_KmToPix {map distance} {
    global MapSRV_mapData

    set data $MapSRV_mapData(${map})
    set height [lindex ${data} 1]
    set upper [lindex ${data} 3]
    set lower [lindex ${data} 4]
    set north [lindex ${upper} 0]
    set south [lindex ${lower} 0]
    expr ( ${distance} * ( ${height} / ( ( ${north} - ${south} ) * 111.0 ) ) )
}

##############################################################################
#   COMMAND         : MapSRV_PickBox
#   DESCRIPTION     : This command retrieves the defining coordinates of a
#                     rectangular region on the specified map by prompting
#                     user selection of the area.
#   PARAMETERS      : map      - the map name
#                     color    - the Tk color of the selected box
#   RETURNS         : the list containing the northwest coordinate and the
#                     southeast coordinate of the selected box where each
#                     coordinate is a list of latitude and longitude values
##############################################################################
proc MAPSRV_ReCoordBox {map s e n w color} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData($map) 0]
    $canvas coords BOX [MapSRV_LonToPix $map $w] [MapSRV_LatToPix $map $s] [MapSRV_LonToPix $map $e] [MapSRV_LatToPix $map $n]
    $canvas itemconfigure BOX -outline $color
}

proc MapSRV_PickBox {map color} {
    global MapSRV_modeBox MapSRV_modeClick MapSRV_mapData

    set canvas [lindex $MapSRV_mapData(${map}) 0]

    # Raise the map window, set box mode, wait for the press and release events
    raise .map${map}
    set MapSRV_modeBox {on}
    set oldcursor [.map${map} cget -cursor]
    .map${map} configure -cursor crosshair
    tkwait variable MapSRV_modeClick
    tkwait variable MapSRV_modeClick
    .map${map} configure -cursor ${oldcursor}
    set MapSRV_modeBox {off}

    # Color the box as specified and caculate lat / lon boundary
    ${canvas} itemconfigure BOX -outline ${color}
    set bounds [${canvas} bbox BOX]
    list [list [MapSRV_PixToLat ${map} [lindex ${bounds} 1]] \
               [MapSRV_PixToLon ${map} [lindex ${bounds} 0]]] \
         [list [MapSRV_PixToLat ${map} [lindex ${bounds} 3]] \
               [MapSRV_PixToLon ${map} [lindex ${bounds} 2]]]
}

##############################################################################
#   COMMAND         : MapSRV_PickCoord
#   DESCRIPTION     : This command retrieves a coordinate on the specified
#                     map by prompting user selection of the coordinate.
#   PARAMETERS      : map      - the map name
#   RETURNS         : the list of latitude and longitude values
##############################################################################
proc MapSRV_PickCoord {map} {
    global MapSRV_anchorX MapSRV_anchorY MapSRV_modeClick

    # Raise the map window and wait for the press and release events
    raise .map${map}
    set oldcursor [.map${map} cget -cursor]
    .map${map} configure -cursor crosshair
    tkwait variable MapSRV_modeClick
    tkwait variable MapSRV_modeClick
    .map${map} configure -cursor ${oldcursor}

    if {${MapSRV_modeClick} == {off1}} {
        set lat [MapSRV_PixToLat ${map} ${MapSRV_anchorY}]
        set lon [MapSRV_PixToLon ${map} ${MapSRV_anchorX}]
        list ${lat} ${lon}
    }
}

##############################################################################
#                                                                            #
#                              PRIVATE COMMANDS                              #
#                                                                            #
##############################################################################

##############################################################################
#   COMMAND         : MapSRV_Exit
#   DESCRIPTION     : This command cleans up the environment and terminates
#                     the Map server process.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
proc MapSRV_Exit {} {
    #Comm_ServerRelease Map
    exit
}

##############################################################################
#   COMMAND         : MapSRV_Init
#   DESCRIPTION     : This command sets up the environment and initiates
#                     the Map server process.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
proc MapSRV_Init {} {
    #package require Comm
    #package require Data

    package require dbtcl

    #Comm_ServerRegister Map

    set mbar [menu .menubar]
    set ctls [menu ${mbar}.ctls]
    set help [menu ${mbar}.help]

    ${mbar} add cascade -label Controls -menu ${ctls}
    ${mbar} add cascade -label Help     -menu ${help}

    ${ctls} add command -label Open  -command {MapSRV_MapOpen}
    ${ctls} add command -label Close  -command {MapSRV_MapClose}
    ${ctls} add separator
    ${ctls} add command -label Exit -command {MapSRV_Exit}

    ${help} add command -label About -command {puts {Read Me}}

    . configure -menu .menubar
}

##############################################################################
#   COMMAND         : MapSRV_LatToPix
#   DESCRIPTION     : This command converts a latitude value to a pixel offset
#                     on the specified map.
#   PARAMETERS      : map      - the map name
#                     lat      - the latitude value
#   RETURNS         : the integer number of pixels
##############################################################################
proc MapSRV_LatToPix {map lat} {
    global MapSRV_mapData

    set data $MapSRV_mapData(${map})
    set height [lindex ${data} 1]
    set upper [lindex ${data} 3]
    set lower [lindex ${data} 4]
    set north [lindex ${upper} 0]
    set south [lindex ${lower} 0]
    expr int( ( ${lat} - ${north} ) * ( ${height} / ( ${south} - ${north} ) ) )
}

##############################################################################
#   COMMAND         : MapSRV_LonToPix
#   DESCRIPTION     : This command converts a longitude value to a pixel offset
#                     on the specified map.
#   PARAMETERS      : map      - the map name
#                     lat      - the latitude value
#   RETURNS         : the integer number of pixels
##############################################################################
proc MapSRV_LonToPix {map lon} {
    global MapSRV_mapData

    set data $MapSRV_mapData(${map})
    set width [lindex ${data} 2]
    set left [lindex ${data} 3]
    set right [lindex ${data} 4]
    set west [lindex ${left} 1]
    set east [lindex ${right} 1]
    expr int( ( ${lon} - ${west} ) * ( ${width} / ( ${east} - ${west} ) ) )
}

##############################################################################
#   COMMAND         : MapSRV_MapClose
#   DESCRIPTION     : This command allows the user to close an open map by
#                     prompting user selection from a list of open maps.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
proc MapSRV_MapClose {} {
    global MapSRV_mapData

    set maplist [array names MapSRV_mapData]
    if {[llength ${maplist}] > 0} {
        set map [MapSRV_Select ${maplist}]
    } else {
        set map {}
    }

    if {${map} != {}} {
        unset MapSRV_mapData(${map})
        destroy .map${map}
    }
}

##############################################################################
#   COMMAND         : MapSRV_MapOpen
#   DESCRIPTION     : This command allows the user to open a map by
#                     prompting user selection from a list of all maps.
#                     This command builds the map window if needed and
#                     establishes the proper bindings for a map window.
#   PARAMETERS      : none
#   RETURNS         : none
##############################################################################
proc MapSRV_MapOpen {} {
    global env MapSRV_mapData MapSRV_latitude MapSRV_longitude

    set maplist \
	    [Data_Select tma_mapRegion NAME {}]
    set map [MapSRV_Select ${maplist}]

    #puts "MapSRV.tcl:MapSRV_MapOpen: User selected $map"

    update idletasks

    if {[lsearch [array names MapSRV_mapData] ${map}] >= 0} {
        wm deiconify .map${map}
        raise .map${map}
    } else {
        set oldcursor [. cget -cursor]
        . configure -cursor watch

        # create the map window
        set top [toplevel .map${map}]
        wm title ${top} ${map}
	wm protocol $top WM_DELETE_WINDOW "puts yo"

        # retrieve the map background data
        set columns [list FILENAME LINES SAMPLES NWLAT NWLON SELAT SELON]
        set criteria [list [list NAME ${map}]]
        set mapdata [Data_Select tma_mapRegion ${columns} ${criteria}]
        set mapdata [lindex ${mapdata} 0]
        set mapfile "$env(TMA_DATA)/[lindex ${mapdata} 0]"
        set mapheight [lindex ${mapdata} 1]
        set mapwidth [lindex ${mapdata} 2]
        set nwcorner [lrange $mapdata 3 4]
        set secorner [lrange $mapdata 5 6]

        # create the MapSRV_mapData(${map}) entry
        set mapdata [concat [lrange $mapdata 0 2] [list ${nwcorner}] [list ${secorner}]]
	puts "mapdata = $mapdata"
        set MapSRV_mapData(${map}) [lreplace ${mapdata} 0 0 ${top}.display]

        # create the map canvas window
        canvas ${top}.display \
            -width ${mapwidth} \
            -height ${mapheight} \
            -scrollregion [list 0 0 ${mapwidth} ${mapheight}] \
            -xscrollcommand "${top}.hscr set" \
            -yscrollcommand "${top}.vscr set" \

        # load the map canvas with the map image
        set mapimage [image create photo -file ${mapfile}]
        ${top}.display create image 0 0 -anchor nw -image ${mapimage} \
		-tags map

        # create the scrollbars
        scrollbar ${top}.hscr \
            -command "${top}.display xview" \
            -orient horizontal \

        scrollbar ${top}.vscr \
            -command "${top}.display yview" \

        # Create the status area
        frame ${top}.status \
            -relief sunken \
            -borderwidth 2 \

        # Create lat / long display in status area
        label ${top}.status.latlabel -text Latitude
        entry ${top}.status.latentry -textvariable MapSRV_latitude(${map})
        label ${top}.status.lonlabel -text Longitude
        entry ${top}.status.lonentry -textvariable MapSRV_longitude(${map})

        # Pack the widgets
        pack ${top}.status.latlabel -side left -fill both
        pack ${top}.status.latentry -side left -fill both -expand true
        pack ${top}.status.lonlabel -side left -fill both
        pack ${top}.status.lonentry -side left -fill both -expand true
        pack ${top}.status -side bottom -fill x
        pack ${top}.hscr -side bottom -fill x
        pack ${top}.vscr -side right -fill y
        pack ${top}.display -side top -fill both -expand true

        # Create the draggable box
        ${top}.display create rectangle 0 0 0 0 -width 2 -tags BOX

        # Bind motion to calculate lat / lon and drag box when appropriate
        bind ${top}.display <Motion> {
            if {"%W" == ".map${MapSRV_current}.display"} {
                MapSRV_Update \
                    ${MapSRV_current} [%W canvasx %x] [%W canvasy %y]
                if {${MapSRV_modeClick} == {on} && ${MapSRV_modeBox} == {on}} {
                    %W itemconfigure BOX -outline red
                    puts "(MAP) %W coords BOX ${MapSRV_anchorX} ${MapSRV_anchorY} [%W canvasx %x] [%W canvasy %y]"
                    %W coords BOX ${MapSRV_anchorX} ${MapSRV_anchorY} \
                                  [%W canvasx %x] [%W canvasy %y]
                }
            }
        }  

        # Bind buttons to set anchors and click mode appropriately
        bind ${top}.display <ButtonPress> {
            set MapSRV_modeClick {on}
            set MapSRV_anchorX [%W canvasx %x]
            set MapSRV_anchorY [%W canvasy %y]
        }
        bind ${top}.display <ButtonRelease-1> {
            set MapSRV_modeClick {off1}
        }
        bind ${top}.display <ButtonRelease-3> {
            set MapSRV_modeClick {off3}
        }

        # Bind toplevel to mark itself current when receiving focus
        bind ${top}.display <FocusIn> {
            regsub ".map" %W "" map
            set MapSRV_current ${map}
        }

        . configure -cursor ${oldcursor}
    }

	set map
}

##############################################################################
#   COMMAND         : MapSRV_PixToLat
#   DESCRIPTION     : This command converts a pixel offset to a latitude value
#                     on the specified map.
#   PARAMETERS      : map      - the map name
#                     pixel    - the pixel offset
#   RETURNS         : the latitude value
##############################################################################
proc MapSRV_PixToLat {map pixel} {
    global MapSRV_mapData

    set data $MapSRV_mapData(${map})

    #puts "MapSRV.tcl:MapSRV_PixToLat:MapSRV_mapData(${map}): $data"

    set height [lindex ${data} 1]
    set upper [lindex ${data} 3]
    set lower [lindex ${data} 4]
    set north [lindex ${upper} 0]
    set south [lindex ${lower} 0]
    expr ( ( ( ( ${south} - ${north} ) / ${height} ) * ${pixel} ) + ${north} )
}

##############################################################################
#   COMMAND         : MapSRV_PixToLon
#   DESCRIPTION     : This command converts a pixel offset to a longitude value
#                     on the specified map.
#   PARAMETERS      : map      - the map name
#                     pixel    - the pixel offset
#   RETURNS         : the longitude value
##############################################################################
proc MapSRV_PixToLon {map pixel} {
    global MapSRV_mapData

    set data $MapSRV_mapData(${map})

    #puts "MapSRV.tcl:MapSRV_PixToLon:MapSRV_mapData(${map}): $data"

    set width [lindex ${data} 2]
    set left [lindex ${data} 3]
    set right [lindex ${data} 4]
    set west [lindex ${left} 1]
    set east [lindex ${right} 1]
    expr ( ( ( ( ${east} - ${west} ) / ${width} ) * ${pixel} ) + ${west} )
}

##############################################################################
#   COMMAND         : MapSRV_Rotate
#   DESCRIPTION     : This command rotates a list of points through an angel.
#   PARAMETERS      : points   - the list of points
#                     angle    - the angle of rotation in radians
#   RETURNS         : the list of rotated points
##############################################################################
proc MapSRV_Rotate {points angle} {
    set length [llength ${points}]

    for {set index 0} {${index} < ${length}} {incr index} {
        set oldP [lindex ${points} ${index}]
        set oldX [lindex ${oldP} 0]
        set oldY [lindex ${oldP} 1]
        set newY [expr (${oldY} * cos(${angle})) - (${oldX} * sin(${angle}))]
        set newX [expr (${oldX} * cos(${angle})) + (${oldY} * sin(${angle}))]
        set newP [list [expr int(${newX})] [expr int(${newY})]]
        set points [lreplace ${points} ${index} ${index} ${newP}]
    }

    set points
}

##############################################################################
#   COMMAND         : MapSRV_Select
#   DESCRIPTION     : This command provides the functionality to prompt for
#                     user selection of a map from the specified list of maps.
#   PARAMETERS      : maplist  - the list of maps
#   RETURNS         : the map name
##############################################################################
proc MapSRV_Select {maplist} {
    global MapSRV_current

    set maps [toplevel .maps -class Dialog]
    wm title ${maps} {Select Map}

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

    bind ${list} <Button-1> {
        set index [%W nearest %y]
        set MapSRV_current [%W get ${index}]
    }

    set oldfocus [focus]
    grab ${maps}
    focus ${maps}
    tkwait variable MapSRV_current
    focus ${oldfocus}
    grab release ${maps}
    destroy ${maps}

    set MapSRV_current
}

##############################################################################
#   COMMAND         : MapSRV_Translate
#   DESCRIPTION     : This command translates a list of points by a vector.
#   PARAMETERS      : points   - the list of points
#                     vector   - the x,y vector of translation
#   RETURNS         : the list of translated points
##############################################################################
proc MapSRV_Translate {points vector} {
    set length [llength ${points}]
    set moveX [lindex ${vector} 0]
    set moveY [lindex ${vector} 1]

    for {set index 0} {${index} < ${length}} {incr index} {
        set oldP [lindex ${points} ${index}]
        set oldX [lindex ${oldP} 0]
        set oldY [lindex ${oldP} 1]
        set newY [expr ${oldX} + ${moveX}]
        set newX [expr ${oldY} + ${moveY}]
        set newP [list ${newX} ${newY}]
        set points [lreplace ${points} ${index} ${index} ${newP}]
    }

    set points
}

##############################################################################
#   COMMAND         : MapSRV_Update
#   DESCRIPTION     : This command updates the status area of the current map.
#   PARAMETERS      : map      - the map name
#                     pixX     - the pixel offset in the horizontal direction
#                     pixY     - the pixel offset in the vertical direction
#   RETURNS         : none
##############################################################################
proc MapSRV_Update {map pixX pixY} {
    global MapSRV_latitude MapSRV_longitude

    lappend coord [MapSRV_PixToLat ${map} ${pixY}]
    lappend coord [MapSRV_PixToLon ${map} ${pixX}]

    #puts "MapSRV.tcl:MapSRV_Update: Pixel (${pixX},${pixY}) maps to $coord"

    set lat [lindex ${coord} 0]
    if {${lat} < 0} {
        set dir S
        set lat [expr -1.0 * ${lat}]
    } else {
        set dir N
    }
    set displayLat $lat
    set degrees [expr int( ${lat} )]
    set lat [expr ${lat} - ${degrees}]
    set minutes [expr int( 60.0 * ${lat} )]
    set lat [expr ${lat} - ( ${minutes} / 60.0 )]
    set seconds [expr int( 3600.0 * ${lat} )]
    set MapSRV_latitude(${map}) \
	    "${dir}${degrees} ${minutes}' ${seconds}'' \
	    ([format "%6.4f" $displayLat])"

    set lon [lindex ${coord} 1]
    if {${lon} < 0} {
        set dir W
        set lon [expr -1.0 * ${lon}]
    } else {
        set dir E
    }
    set displayLon $lon
    set degrees [expr int( ${lon} )]
    set lon [expr ${lon} - ${degrees}]
    set minutes [expr int( 60.0 * ${lon} )]
    set lon [expr ${lon} - ( ${minutes} / 60.0 )]
    set seconds [expr int( 3600.0 * ${lon} )]
    set MapSRV_longitude(${map}) \
	    "${dir}${degrees} ${minutes}' ${seconds}'' \
	    ([format "%7.4f" $displayLon])"
}



