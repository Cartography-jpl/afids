#   DESCRIPTION     : This module provides the functionality required by
#                     the TMA Client as defined in the TMA Segment API.

package provide TMA 1.0

##############################################################################
#   COMMAND         : TMA_Contours
#   DESCRIPTION     : This command ...
#   PARAMETERS      : area     - the ...
#                     startpt  - the ...
#                     time     - the ...
#                     interval - the ...
#                     count    - the ...
#                     asset    - the ...
#   RETURNS         : ...
##############################################################################
proc TMA_Contours  {area startpt time interval count asset} {
	lappend columns MOVER_TYPE
	set criteria [list ASSET_TYPE ${asset}]
    set results  [Data_Select AMD_ASSET_TYPE_MOVER ${columns} ${criteria}]
    set mover    [lindex ${results} 0]
    set nwpoint  [Map_CoordToString [lindex ${area} 0]]
    set sepoint  [Map_CoordToString [lindex ${area} 1]]
    set startpt  [Map_CoordToString ${startpt}]

    Comm_ServerCommand TMA tma_Contours \
        ${nwpoint} ${sepoint} ${startpt} \
        ${time} ${interval} ${count} ${mover}
}

##############################################################################
#   COMMAND         : TMA_MinPath
#   DESCRIPTION     : This command ...
#   PARAMETERS      : area     - the ...
#                     startpt  - the ...
#                     endpt    - the ...
#                     asset    - the ...
#   RETURNS         : the time in decimal hours
##############################################################################
proc TMA_MinPath {area startpt endpt asset} { 
	lappend columns MOVER_TYPE
	set criteria [list ASSET_TYPE ${asset}]
    set results  [Data_Select AMD_ASSET_TYPE_MOVER ${columns} ${criteria}]
    set mover    [lindex ${results} 0]
    set nwpoint  [Map_CoordToString [lindex ${area} 0]]
    set sepoint  [Map_CoordToString [lindex ${area} 1]]
    set startpt  [Map_CoordToString ${startpt}]
    set endpt    [Map_CoordToString ${endpt}]

    Comm_ServerCommand TMA minPath2 \
	${nwpoint} ${sepoint} ${startpt} ${endpt} ${mover}
}

##############################################################################
#   COMMAND         : TMA_MinPaths
#   DESCRIPTION     : This command ...
#   PARAMETERS      : area     - the ...
#                     startpt  - the ...
#                     endpts   - the ...
#                     asset    - the ...
#   RETURNS         : the list of times in deciaml hours
##############################################################################
proc TMA_MinPaths {area startpt endpts asset} {
    foreach endpt ${endpts} {
        lappend results [TMA_MinPath ${area} ${startpt} ${endpt} ${asset}]
    }
	return ${results}
}

