package provide maptcl 1.0

##############################################################################
#   COMMAND         : Map_Connect
#   DESCRIPTION     : This command establishes a connection to a map by
#                     prompting user selection from a list of all maps.
#   PARAMETERS      : none
#   RETURNS         : the map name
##############################################################################
proc Map_Connect {} {
    MapSRV_Connect
}

##########################################################
proc Map_GetLines {map} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData($map) 0]
    lindex [$canvas cget -scrollregion] 3
}

##########################################################
proc Map_GetSamples {map} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData($map) 0]
    lindex [$canvas cget -scrollregion] 2
}

##########################################################
proc max {a b} {
    if {$a > $b} {
	return $a
    } else {
	return $b
    }
}

proc min {a b} {
    if {$a < $b} {
	return $a
    } else {
	return $b
    }
}

##########################################################
proc Map_OverlayBitmap {map bitmapFilename w n fg} {
    global MapSRV_mapData

    set x [MapSRV_LonToPix $map $w]
    set y [MapSRV_LatToPix $map $n]

    set canvas [lindex $MapSRV_mapData($map) 0]
    $canvas create bitmap $x $y -bitmap @$bitmapFilename -anchor nw -foreground $fg
}

##########################################################
proc Map_OverlayPhoto {map imageFilename w n} {
    global MapSRV_mapData

    set image [image create photo -file $imageFilename]
    #puts "Created image in Map_OverlayPhoto"
    set x [MapSRV_LonToPix $map $w]
    set y [MapSRV_LatToPix $map $n]

    set canvas [lindex $MapSRV_mapData($map) 0]
    set imageId [$canvas create image $x $y -image $image -anchor nw \
	    -tags overlay]
    # Do this so display list (which contains the object stacking order)
    # has overlay just above the map.
    # This will not hide the text with leash so it can be moved.
    $canvas raise $imageId map
    #puts "Done create image in canvas in Map_OverlayPhoto"
    list $imageId $image
}

##########################################################
proc Map_CreatePhoto {map tmpImage w n dotBloat} {
    global MapSRV_mapData

    set height [image height $tmpImage]
    set width [image width $tmpImage]
    set maxSample [expr $width - 1]
    set maxLine [expr $height - 1]

    set image [image create photo -height $height -width $width]
    $image blank

    for {set line 0} {$line < $height} {incr line} {
	for {set sample 0} {$sample < $width} {incr sample} {
	    set pixel [$tmpImage get $sample $line]
	    if {[string compare $pixel "0 0 0"]} {
		$image put [eval format "#%02x%02x%02x" $pixel] -to \
			[max 0 [min $maxSample [expr $sample - $dotBloat]]] \
			[max 0 [min $maxLine [expr $line - $dotBloat]]] \
			[max 0 [min $maxSample [expr $sample + $dotBloat + 1]]] \
			[max 0 [min $maxLine [expr $line + $dotBloat + 1]]]
	    }
	}
    }

    set x [MapSRV_LonToPix $map $w]
    set y [MapSRV_LatToPix $map $n]

    set canvas [lindex $MapSRV_mapData($map) 0]
    set imageId [$canvas create image $x $y -image $image -anchor nw]
    list $imageId $image
}

##########################################################
proc Map_CreateBitmap {map bmpPath w n fg} {
    global MapSRV_mapData

    set x [MapSRV_LonToPix $map $w]
    set y [MapSRV_LatToPix $map $n]

    set canvas [lindex $MapSRV_mapData($map) 0]
    $canvas create bitmap $x $y -bitmap @$bmpPath -anchor nw -foreground $fg
}

##########################################################
proc Map_ClearAll {map} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData($map) 0]

    $canvas delete withtag hiliteRects
    $canvas delete withtag text
    $canvas delete withtag line
    $canvas delete withtag oval
}

##########################################################
proc Map_HiliteLabel {map tag color} {
    global MapSRV_mapData
    global canvasInfo
    set canvas [lindex $MapSRV_mapData($map) 0]
    set id [$canvas find withtag $tag]
    puts "id = $id"
#    $canvas itemconfigure $id -fill $color

    global textRects
    if {[array get textRects $id] == {}} {
	set bbox [$canvas bbox $id]
	eval "set textRects($id) \[$canvas create rectangle $bbox -fill $color -tags hiliteRects\]"
	$canvas lower $textRects($id) $id
    }
}

proc Map_UnHiliteLabel {map tag} {
    global MapSRV_mapData
    global canvasInfo
    set canvas [lindex $MapSRV_mapData($map) 0]
    set id [$canvas find withtag $tag]
#    $canvas itemconfigure $id -fill $canvasInfo($canvas,$id,color)

    global textRects
    $canvas destroy $textRects($id)
    array unset textRect $id
}

proc Map_UnHiliteAllLabels {map} {
    global MapSRV_mapData
    global canvasInfo
    set canvas [lindex $MapSRV_mapData($map) 0]
#    foreach name [array names canvasInfo "$canvas,*,color"] {
#	set id [lindex [split $name "\,"] 1]
#	$canvas itemconfigure $id -fill $canvasInfo($name)
#    }

    $canvas delete hiliteRects
    global textRects
    array unset textRects
}

##############################################################################
#   COMMAND         : Map_Text2
#   DESCRIPTION     : This command writes text to map.
#                     If moved the text has a leash to where it was,
#                     and the text has a tag.
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc Map_Text2 {map text coord color tag} {
    global MapSRV_mapData
    global canvasInfo

    set canvas [lindex $MapSRV_mapData($map) 0]

    set x [MapSRV_LonToPix $map [lindex $coord 1]]
    set y [MapSRV_LatToPix $map [lindex $coord 0]]
    
    set id [$canvas create text $x $y -fill $color \
	    -anchor w -width 200 \
	    -font "-*-helvetica-bold-*-*-*-18-*-*-*-*-*-*-*" \
	    -text "$text" -tags [list text $tag]]

    #puts "id = $id"
    set canvasInfo($canvas,$id,originalPosition,x) $x
    set canvasInfo($canvas,$id,originalPosition,y) $y
    set canvasInfo($canvas,$id,color) $color

    set leashId [$canvas create line 0 0 0 0 -fill black -width 2 -tags line]
    set canvasInfo($canvas,$id,leash) $leashId
    set canvasInfo($canvas,$id,leash,x) $x
    set canvasInfo($canvas,$id,leash,y) $y
    $canvas bind text <Button-1>  {canvasMark2 %x %y %W}
    $canvas bind text <B1-Motion> {canvasDrag2 %x %y %W}

    list $id $leashId
}

##############################################################################
proc canvasMark2 { x y w } {
    global canvasInfo
    # Remember the canvas object and its location.
    global lastX lastY
    set lastX [$w canvasx $x]
    set lastY [$w canvasy $y]
    set canvasInfo($w,obj) [$w find closest $lastX $lastY]
    puts "tags = [$w gettags $canvasInfo($w,obj)]"
    #set canvasInfo($w,x) $x
    #set canvasInfo($w,y) $y
}
##############################################################################
proc canvasDrag2 { x y w } {
    # Move the current object.
    global canvasInfo
    set objId $canvasInfo($w,obj)
    if {[lsearch -exact [$w gettags $objId] text] != -1} {
	set x [$w canvasx $x]
	set y [$w canvasy $y]

	global lastX lastY
	set deltaX [expr $x - $lastX]
	set deltaY [expr $y - $lastY]
	set lastX $x
	set lastY $y

	#set dx [expr $x - $canvasInfo($w,x)]
	#set dy [expr $y - $canvasInfo($w,y)]
	#$w move $objId $dx $dy
	#set canvasInfo($w,x) $x
	#set canvasInfo($w,y) $y

	$w coords $objId $x $y

	# move hilite, if any
	global textRects
	if {[array get textRects $objId] != {}} {
	    eval "$w coords $textRects($objId) [$w bbox $objId]"
	}

	# Change leash.
	$w coords $canvasInfo($w,$objId,leash) $x $y \
		$canvasInfo($w,$objId,leash,x) $canvasInfo($w,$objId,leash,y)
    }
}

##############################################################################
#   COMMAND         : Map_Text
#   DESCRIPTION     : This command writes text to map.
#   PARAMETERS      : 
#   RETURNS         : 
##############################################################################
proc Map_Text {map text coord color} {
    global MapSRV_mapData

    set canvas [lindex $MapSRV_mapData($map) 0]

    set x [MapSRV_LonToPix $map [lindex $coord 1]]
    set y [MapSRV_LatToPix $map [lindex $coord 0]]
    
    $canvas create text $x $y -fill $color \
	    -anchor w -width 200 \
	    -font "-*-helvetica-bold-*-*-*-18-*-*-*-*-*-*-*" \
	    -text "$text" -tags [list text]

    $canvas bind text <Button-1>  {canvasMark %x %y %W}
    $canvas bind text <B1-Motion> {canvasDrag %x %y %W}
}
##############################################################################
proc canvasMark { x y w } {
    global canvasInfo
    # Remember the canvas object and its location.
    #set obj [$w find closest $x $y]
    set x [$w canvasx $x]
    set y [$w canvasy $y]
    set canvasInfo($w,obj) [$w find closest $x $y]
    set canvasInfo($w,x) $x
    set canvasInfo($w,y) $y
}
##############################################################################
proc canvasDrag { x y w } {
    # Move the current object.
    global canvasInfo
    if {[lsearch -exact [$w gettags $canvasInfo($w,obj)] text] != -1} {
	set x [$w canvasx $x]
	set y [$w canvasy $y]
	set dx [expr $x - $canvasInfo($w,x)]
	set dy [expr $y - $canvasInfo($w,y)]
	$w move $canvasInfo($w,obj) $dx $dy
	set canvasInfo($w,x) $x
	set canvasInfo($w,y) $y
    }
}

##############################################################################
#   COMMAND         : Map_CoordToString
#   DESCRIPTION     : This command converts a two item list consisting of
#                     floating point latitude and longitude values into a
#                     string representation suitable for display.
#   PARAMETERS      : coord    - the list of latitude and longitude values
#   RETURNS         : the string representation
##############################################################################
proc Map_CoordToString {coord} {
    set lat [lindex ${coord} 0]
    if {${lat} < 0} {
        set dir S
        set lat [expr -1.0 * ${lat}]
    } else {
        set dir N
    }
    set degrees [expr int( ${lat} )]
    set lat [expr ${lat} - ${degrees}]
    set minutes [expr int( 60.0 * ${lat} )]
    set lat [expr ${lat} - ( ${minutes} / 60.0 )]
    set seconds [expr int( 3600.0 * ${lat} )]
    set strlat "${dir}${degrees} ${minutes}' ${seconds}''"
 
    set lon [lindex ${coord} 1]
    if {${lon} < 0} {
        set dir W
        set lon [expr -1.0 * ${lon}]
    } else {
        set dir E
    }
    set degrees [expr int( ${lon} )]
    set lon [expr ${lon} - ${degrees}]
    set minutes [expr int( 60.0 * ${lon} )]
    set lon [expr ${lon} - ( ${minutes} / 60.0 )]
    set seconds [expr int( 3600.0 * ${lon} )]
    set strlon "${dir}${degrees} ${minutes}' ${seconds}''"
 
    concat ${strlat} ${strlon}
}


##############################################################################
#   COMMAND         : Map_DrawArrow
#   DESCRIPTION     : This command draws an arrow on the specified map.
#   PARAMETERS      : map      - the map name
#                     base     - the base coordinate of the arrow
#                     head     - the head coordinate of the arrow
#                     color    - the Tk color for the arrow
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawArrow {map base head color} { 
    set points [list ${base} ${head}]
    MapSRV_DrawLine ${map} ${points} ${color} 2 last
}

##############################################################################
#   COMMAND         : Map_DrawText
#   DESCRIPTION     : This command draws text on the specified map.
#   PARAMETERS      : map      - the map name
#                     text     - the text to draw
#                     position - the location of the text anchor
#                     anchor   - which anchor to use
#                     color    - the Tk color of the circle     
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawText {map text position anchor color} { 
    MapSRV_DrawText $map $text $position $anchor $color
}

##############################################################################
#   COMMAND         : Map_DrawEntity
#   DESCRIPTION     : This command draws an entity on the specified map.
#   PARAMETERS      : map      - the map name
#                     coord    - the location coordinate of the entity
#                     entity   - the entity code for the entity
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawEntity {map coord entity} { 
    MapSRV_DrawEntity ${map} ${coord} ${entity}
}

##############################################################################
#   COMMAND         : Map_DrawFilledCircle
#   DESCRIPTION     : This command draws a filled circle on the specified map.
#   PARAMETERS      : map      - the map name
#                     center   - the center coordinate of the circle
#                     radius   - the radius of the circle in meters
#                     color    - the Tk color of the circle
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawFilledCircle {map center radius color} { 
    MapSRV_DrawOval \
        ${map} ${center} ${radius} ${radius} {{}} ${color} {{}}
}

##############################################################################
#   COMMAND         : Map_DrawHollowCircle
#   DESCRIPTION     : This command draws a hollow circle on the specified map.
#   PARAMETERS      : map      - the map name
#                     center   - the center coordinate of the circle
#                     radius   - the radius of the circle in meters
#                     color    - the Tk color of the circle
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawHollowCircle {map center radius color} { 
    MapSRV_DrawOval \
        ${map} ${center} ${radius} ${radius} {{}} {{}} ${color}
}

##############################################################################
#   COMMAND         : Map_DrawLine
#   DESCRIPTION     : This command draws a line on the specified map.
#   PARAMETERS      : map      - the map name
#                     point1   - the first coordinate of the line
#                     point2   - the second coordinate of the line
#                     color    - the Tk color of the line
#                     width    - the width of the line in pixels
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawLine {map point1 point2 color width} {
    set points [list ${point1} ${point2}]
    MapSRV_DrawLine ${map} ${points} ${color} ${width} none
}

##############################################################################
#   COMMAND         : Map_DrawPolygon
#   DESCRIPTION     : This command draws a polygon on the specified map.
#   PARAMETERS      : map      - the map name
#                     points   - the coordinate list of the polygon
#                     color    - the Tk color of the polygon
#                     stippleFile - an optional stipple pattern
#                     stippleColor - color for stipple pattern
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawPolygon {map points color {stippleFile ""} {stippleColor {{}}}} {
    MapSRV_DrawPolygon ${map} ${points} $stippleColor ${color} 2 $stippleFile
}

##############################################################################
#   COMMAND         : Map_DrawPolyline
#   DESCRIPTION     : This command draws a polyline on the specified map.
#   PARAMETERS      : map      - the map name
#                     points   - the coordinate list of the polyline
#                     color    - the Tk color of the polyline
#                     width    - the width of the polyline in pixels
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawPolyline {map points color width} {
    MapSRV_DrawLine ${map} ${points} ${color} ${width} none
}

##############################################################################
#   COMMAND         : Map_DrawSurface
#   DESCRIPTION     : This command draws a surface on the specified map as an
#                     overlay by mapping surface values to color values in a
#                     color table indexed by surface value ranges. Each element
#                     of the color table is a three item list. The first item
#                     is a minimum value. The second item is a maximum value.
#                     The third item is the Tk color used for this value range.
#   PARAMETERS      : map      - the map name
#                     surface  - the list of surface values
#                     colortbl - the Tk color table indexed by value ranges
#                     nwpoint  - the northwest coordinate of the covered area
#                     sepoint  - the southeast coordinate of the covered area
#                     height   - the number of gridpoints from north to south
#                     width    - the number of gridpoints from east to west
#   RETURNS         : the object tag
##############################################################################
proc Map_DrawSurface {map surface colortbl nwpoint sepoint height width} {
    foreach value ${surface} {
        set color {}
        foreach entry ${colortbl} {
            set rangemin [lindex ${entry} 0]
            set rangemax [lindex ${entry} 1]
            if {${rangemin} <= ${value} && ${rangemax} >= ${value}} {
                set color [lindex ${entry} 2]
                break
            }
        }
        lappend overlay ${color}
    }

    MapSRV_DrawOverlay \
        ${map} ${overlay} ${nwpoint} ${sepoint} ${height} ${width}
}

##############################################################################
#   COMMAND         : Map_EraseObject
#   DESCRIPTION     : This command erases the object on the specified map.
#   PARAMETERS      : map      - the map name
#                     object   - the object tag
#   RETURNS         : none
##############################################################################
proc Map_EraseObject {map object} { 
    MapSRV_EraseObject ${map} ${object}
}

##############################################################################
#   COMMAND         : Map_KmToPix
#   DESCRIPTION     : This command calculates the number of pixels which scale
#                     to the distance in kilometers on the specified map.
#   PARAMETERS      : map      - the map name
#                     distance - the distance in kilometers
#   RETURNS         : the integer number of pixels
##############################################################################
proc Map_KmToPix {map distance} { 
    MapSRV_KmToPix ${map} ${distance}
}

##############################################################################
#   COMMAND         : Map_PickBox
#   DESCRIPTION     : This command retrieves the defining coordinates of a
#                     rectangular region on the specified map by prompting
#                     user selection of the area.
#   PARAMETERS      : map      - the map name
#                     color    - the Tk color of the selected box
#   RETURNS         : the list containing the northwest coordinate and the
#                     southeast coordinate of the selected box where each
#                     coordinate is a list of latitude and longitude values
##############################################################################
proc Map_PickBox {map color} {
    MapSRV_PickBox ${map} ${color}
}

##############################################################################
#   COMMAND         : Map_PickCoord
#   DESCRIPTION     : This command retrieves a coordinate on the specified
#                     map by prompting user selection of the coordinate.
#   PARAMETERS      : map      - the map name
#   RETURNS         : the list of latitude and longitude values
##############################################################################
proc Map_PickCoord {map} {
    MapSRV_PickCoord ${map}
}

proc Map_Grab {map} {
    grab .map$map
}

proc Map_Release {map} {
    grab release .map$map
}


##############################################################################
#   COMMAND         : Map_StringToCoord
#   DESCRIPTION     : This command converts a string representation of
#                     a coordinate into a two item list consisting of
#                     floating point latitude and longitude values
#                     suitable for calculations.
#   PARAMETERS      : string   - the string representation
#   RETURNS         : the list of latitude and longitude values
##############################################################################
proc Map_StringToCoord {string} {
    if {[regexp {^([N|S])([0-9]?[0-9]) ([0-9]?[0-9])' ([0-9]?[0-9])'' ([W|E])([0-9]?[0-9]?[0-9]) ([0-9]?[0-9])' ([0-9]?[0-9])''$} ${string} match \
        nsprefix latdegrees latminutes latseconds \
        ewprefix londegrees lonminutes lonseconds] == 0} {
        return -1
    }

#puts "$latdegrees $latminutes $latseconds"
#puts "$londegrees $lonminutes $lonseconds"

    set lat [expr $latdegrees + $latminutes / 60.0 + $latseconds / 3600.0]
    if {$nsprefix == "S"} {
        set lat [expr -1.0 * $lat]
    }
    set lon [expr $londegrees + $lonminutes / 60.0 + $lonseconds / 3600.0]
    if {$ewprefix == "W"} {
        set lon [expr -1.0 * $lon]
    }

    list $lat $lon
}

##############################################################################
#                                                                            #
#                              PRIVATE COMMANDS                              #
#                                                                            #
##############################################################################

##############################################################################
#   COMMAND         : Map_VerifyCoords
#   DESCRIPTION     : This command determines whether each coordinate in the
#                     list is contained within the bounding rectangular area.
#   PARAMETERS      : coords   - the list of coordinates
#                     area     - the bounding rectangular area
#   RETURNS         : the list of coordinates not contained within the area
##############################################################################
proc Map_VerifyCoords {coords area} {
    set badcoords {}
    set northwest [lindex ${area} 0]
    set southeast [lindex ${area} 1]
    set north [lindex ${northwest} 0]
    set south [lindex ${southeast} 0]
    set east [lindex ${southeast} 1]
    set west [lindex ${northwest} 1]

    foreach coord ${coords} {
        set lat [lindex ${coord} 0]
        set lon [lindex ${coord} 1]
        if {${lat} > ${north} ||
            ${lat} < ${south} ||
            ${lon} > ${east}  ||
            ${lon} < ${west}} {
            lappend badcoords ${coord}
        }
    }

    set badcoords
}

