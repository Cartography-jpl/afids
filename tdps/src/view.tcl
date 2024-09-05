proc getViewCenterPercentH {} {
    global c

    set xview [$c xview]

    set leftPercent [lindex $xview 0]
    set rightPercent [lindex $xview 1]

    set widthPercent [expr $rightPercent - $leftPercent] 

    expr $leftPercent + $widthPercent / 2.0
}

proc setViewCenterPercentH {hCenter} {
    global c

    set xview [$c xview]

    set leftPercent [lindex $xview 0]
    set rightPercent [lindex $xview 1]

    set widthPercent [expr $rightPercent - $leftPercent] 

    $c xview moveto [expr $hCenter - $widthPercent / 2.0]
}

proc getViewCenterPercentV {} {
    global c

    set yview [$c yview]

    set topPercent [lindex $yview 0]
    set bottomPercent [lindex $yview 1]

    set heightPercent [expr $bottomPercent - $topPercent] 

    expr $topPercent + $heightPercent / 2.0
}

proc setViewCenterPercentV {vCenter} {
    global c

    set yview [$c yview]

    set topPercent [lindex $yview 0]
    set bottomPercent [lindex $yview 1]

    set heightPercent [expr $bottomPercent - $topPercent] 

    $c yview moveto [expr $vCenter - $heightPercent / 2.0]
}

proc centerViewPercent {x y} {
    setViewCenterPercentH $x
    setViewCenterPercentV $y
}

proc zoominPercentCenter {hCenter vCenter} {
    global c scalePower maxZoom

    if {$scalePower == $maxZoom} {
  	return
    }

    $c scale all 0 0 2.0 2.0
    set sr [lindex [$c configure -scrollregion] 4]
    set newsr [list [expr [lindex $sr 0] * 2.0] [expr [lindex $sr 1] * 2.0] [expr [lindex $sr 2] * 2.0] [expr [lindex $sr 3] * 2.0]]
    $c configure -scrollregion $newsr
    incr scalePower 1

    setViewCenterPercentH $hCenter
    setViewCenterPercentV $vCenter

    global mapImage
    if {$mapImage >= 0} {
	genLoadMapImage
    }
}

proc zoomoutPercentCenter {hCenter vCenter} {
    global c scalePower

    if {$scalePower == 1} {
	return
    }

    $c scale all 0 0 [expr 1.0 / 2.0] [expr 1.0 / 2.0]
    set sr [lindex [$c configure -scrollregion] 4]
    set newsr [list [expr [lindex $sr 0] / 2.0] [expr [lindex $sr 1] / 2.0] [expr [lindex $sr 2] / 2.0] [expr [lindex $sr 3] / 2.0]]
    $c configure -scrollregion $newsr
    incr scalePower -1

    setViewCenterPercentH $hCenter
    setViewCenterPercentV $vCenter

    checkCanvasSize true

    global mapImage
    if {$mapImage >= 0} {
	genLoadMapImage
    }
}

proc zoominCenter {} {
    zoominPercentCenter [getViewCenterPercentH] [getViewCenterPercentV]
}

proc zoomoutCenter {} {
    zoomoutPercentCenter [getViewCenterPercentH] [getViewCenterPercentV]
}

proc mouseXYtoViewPercentXY {x y} {
    global c

    # convert to scrollregion space
    set cx [$c canvasx $x]
    set cy [$c canvasy $y]

    set northPole [lindex [lindex [$c configure -scrollregion] 4] 3]

    list [expr ($cx + 2.0 * $northPole) / (4.0 * $northPole)] [expr ($cy + $northPole) / (2.0 * $northPole)]
}

proc zoominXY {x y} {
    eval zoominPercentCenter [mouseXYtoViewPercentXY $x $y]
}

proc zoomoutXY {x y} {
    eval zoomoutPercentCenter [mouseXYtoViewPercentXY $x $y]
}

proc lremove {list item} {
    set index [lsearch $list $item]

    if {$index >= 0} {
	return [lreplace $list $index $index]
    }

    return $list
}

proc zoomToFit {tag} {
    global c

    set all [$c find withtag $tag]

    global mapImage
    set all [lremove $all $mapImage]

    if {$all == {}} {
	if {[$c find all] == {} } {
	    tk_messageBox -message "There are no vectors to zoom"
	} else {
	    tk_messageBox -message "There is no selected vector to zoom"
	}
	return
    }

    # get close
    zoomToFit2 $all

    update idletasks

    # pull it in a bit closer
    zoomToFit2 $all
}

proc zoomToFit2 {all} {
    global c

    set bbox [eval "$c bbox $all"]

    zoomToFit3 $bbox
}

proc zoomToAoi {} {
    global c scalePower
    global playbox_aoiMinLat playbox_aoiMinLon playbox_aoiMaxLat playbox_aoiMaxLon

    set x1 [expr $playbox_aoiMinLon * pow (2, $scalePower)]
    set x2 [expr $playbox_aoiMaxLon * pow (2, $scalePower)]
    set y1 [expr - $playbox_aoiMinLat * pow (2, $scalePower)]
    set y2 [expr - $playbox_aoiMaxLat * pow (2, $scalePower)]
    zoomToFit3 [list $x1 $y2 $x2 $y1]
}

proc zoomToFit3 {bbox} {
    global c scalePower maxZoom

    set bbLeft [lindex $bbox 0]
    set bbTop [lindex $bbox 1]
    set bbRight [lindex $bbox 2]
    set bbBottom [lindex $bbox 3]
    set bbWidth [expr $bbRight - $bbLeft]
    #    puts "bbWidth $bbWidth"
    set bbHeight [expr $bbBottom - $bbTop]
    #    puts "bbHeight $bbHeight"
    set bbCenterX [expr $bbLeft + $bbWidth / 2.0]
    set bbCenterY [expr $bbTop + $bbHeight / 2.0]

    set sr [lindex [$c configure -scrollregion] 4]
    set srLeft [lindex $sr 0]
    set srTop [lindex $sr 1]
    set srRight [lindex $sr 2]
    set srBottom [lindex $sr 3]
    set srWidth [expr $srRight - $srLeft]
    set srHeight [expr $srBottom - $srTop]

    set bbCenterXPercent [expr ($bbCenterX + $srRight) / $srWidth]
    set bbCenterYPercent [expr ($bbCenterY + $srBottom) / $srHeight]

    set targetViewWidthPercent [expr $bbWidth / $srWidth]
    set targetViewHeightPercent [expr $bbHeight / $srHeight]

    set xview [$c xview]
    set yview [$c yview]
    set leftPercent [lindex $xview 0]
    set rightPercent [lindex $xview 1]
    set topPercent [lindex $yview 0]
    set bottomPercent [lindex $yview 1]
    set widthPercent [expr $rightPercent - $leftPercent] 
    set heightPercent [expr $bottomPercent - $topPercent] 

    set neededVZoomFactor [expr $targetViewHeightPercent / $heightPercent]
    set neededHZoomFactor [expr $targetViewWidthPercent / $widthPercent]

    set neededVZoomPower [expr int (floor (- log ($neededVZoomFactor)) / log (2.0))]
    set neededHZoomPower [expr int (floor (- log ($neededHZoomFactor)) / log (2.0))]
    
    set xPower [expr $scalePower + $neededHZoomPower]
    set yPower [expr $scalePower + $neededVZoomPower]

    if {$xPower < $yPower} {
	set power $xPower
    } else {
	set power $yPower
    }

    if {$power > $maxZoom} {
	set power $maxZoom
    }

    zoomTo $power

    centerViewPercent $bbCenterXPercent $bbCenterYPercent

    updateViewDoc
}

proc zoomTo {power} {
    global c maxZoom scalePower

    if {$power < 0} {
	set power 0
    }
    if {$power > $maxZoom} {
	set power $maxZoom
    }

    $c scale all 0 0 [expr pow (2, $power - $scalePower)] [expr pow (2, $power - $scalePower)]

    set scalePower $power

    set scrollregion "[expr -180 * pow (2, $scalePower)] [expr -90 * pow (2, $scalePower)] [expr 180 * pow (2, $scalePower)] [expr 90 * pow (2, $scalePower)]"
    $c configure -scrollregion $scrollregion

    updateViewDoc
}

