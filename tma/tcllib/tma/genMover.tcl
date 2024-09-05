package provide helpers 1.0
package require dbtcl

proc getMoverValue {moverData terrainId} {
    foreach pair $moverData {
	if {[lindex $pair 0] == $terrainId} {
	    return [lindex $pair 1]
	}
    }
    return 0
}

proc genMover {appId dataSet moverId} {
    global env

    # Get data for mover first.
    # It is faster than getting each value separately.
    set moverData [Data_Select tma_mover {TERRAIN_ID VALUE} \
	    [list [list APPLICATION_ID $appId] \
	    [list MOVER_ID $moverId]]]
#puts $moverData
    set lines {}
    # Do slope first.
    set numId 700
    set terrainIds [Data_Select tma_terrain {TERRAIN_ID} \
	    [list [list APPLICATION_ID $appId] \
	    [list TYPE SLOPE]]]
    foreach terrainId $terrainIds {
	set value [getMoverValue $moverData $terrainId]
	#set value [Data_Select tma_mover {VALUE} \
	#	[list [list APPLICATION_ID $appId] \
	#	[list MOVER_ID $moverId] \
	#	[list TERRAIN_ID $terrainId]]]
	if {$value == 0} { set value 0.001 }
	set bounds [lindex [Data_Select tma_slope {LOWER UPPER}  \
		[list [list APPLICATION_ID $appId] \
		[list TERRAIN_ID $terrainId]]] 0]
	lappend lines \
		[list $numId slope $value [lindex $bounds 0] [lindex $bounds 1]]
	incr numId
    }
    # Next do terrain data.
    set results [Data_Select tma_terrainMapping {FEATURE_VALUE TERRAIN_ID PGM_VALUE} \
	    [list [list APPLICATION_ID $appId] \
	    [list DATA_SET $dataSet]]]
    foreach result $results {
	set featureValue [lindex $result 0]
	set terrainId    [lindex $result 1]
	set pgmValue     [lindex $result 2]
	set value [getMoverValue $moverData $terrainId]
	#set value [Data_Select tma_mover {VALUE} \
	#	[list [list APPLICATION_ID $appId] \
	#	[list MOVER_ID $moverId] \
	#	[list TERRAIN_ID $terrainId]]]
	if {$value == 0} { set value 0.001 }
	if {$pgmValue == -2} {
	    lappend lines [list $featureValue vdelay $value]
	} elseif {$pgmValue == -1} {
	    lappend lines [list $featureValue vspeed $value]
	} else {
	    lappend lines [list $featureValue rspeed $value $pgmValue]
	}
    }
    set outFile [open "$env(TMA_DATA)/tempMoverData" w]
    puts $outFile [llength $lines]
    foreach line $lines {
	#puts $line
	puts $outFile $line
    }
    close $outFile
    return 
}

#genMover pdc PDC_SHAPE 3
#exit
