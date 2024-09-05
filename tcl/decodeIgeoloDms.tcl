proc news2factor {news} {
    switch $news {
	N - E {
	    return 1
	}
	S - W {
	    return -1
	}
	default {
	    return 0
	}
    }
}

proc lat2dd {lat} {
    # lat like ddmmss
    set dd [string trimleft [string range $lat 0 1] 0]
    if {$dd == ""} {
	set dd 0
    }
    set mm [string trimleft [string range $lat 2 3] 0]
    if {$mm == ""} {
	set mm 0
    }
    set ss [string trimleft [string range $lat 4 5] 0]
    if {$ss == ""} {
	set ss 0
    }
    expr $dd + $mm / 60.0 + $ss / 3600.0
}

proc lon2dd {lon} {
    # lat like dddmmss
    set dd [string trimleft [string range $lon 0 2] 0]
    if {$dd == ""} {
	set dd 0
    }
    set mm [string trimleft [string range $lon 3 4] 0]
    if {$mm == ""} {
	set mm 0
    }
    set ss [string trimleft [string range $lon 5 6] 0]
    if {$ss == ""} {
	set ss 0
    }
    expr $dd + $mm / 60.0 + $ss / 3600.0
}

# decode string like 340334N0441625E340430N0442831E335611N0442830E335514N0441626E
#                    012345678901234567890123456789012345678901234567890123456789
#                              11111111112222222222333333333344444444445555555555
proc igeolo2dms {igeolo} {
    set lat1   [string range $igeolo 0 5]
    set lat1ns [string range $igeolo 6 6]
    set lon1   [string range $igeolo 7 13]
    set lon1ew [string range $igeolo 14 14]
    set lat2   [string range $igeolo 15 20]
    set lat2ns [string range $igeolo 21 21]
    set lon2   [string range $igeolo 22 28]
    set lon2ew [string range $igeolo 29 29]
    set lat3   [string range $igeolo 30 35]
    set lat3ns [string range $igeolo 36 36]
    set lon3   [string range $igeolo 37 43]
    set lon3ew [string range $igeolo 44 44]
    set lat4   [string range $igeolo 45 50]
    set lat4ns [string range $igeolo 51 51]
    set lon4   [string range $igeolo 52 58]
    set lon4ew [string range $igeolo 59 59]

    puts "Lat/Lon 1 [expr [news2factor $lat1ns] * [lat2dd $lat1]]/[expr [news2factor $lon1ew] * [lon2dd $lon1]]"
    puts "Lat/Lon 2 [expr [news2factor $lat2ns] * [lat2dd $lat2]]/[expr [news2factor $lon2ew] * [lon2dd $lon2]]"
    puts "Lat/Lon 3 [expr [news2factor $lat3ns] * [lat2dd $lat3]]/[expr [news2factor $lon3ew] * [lon2dd $lon3]]"
    puts "Lat/Lon 4 [expr [news2factor $lat4ns] * [lat2dd $lat4]]/[expr [news2factor $lon4ew] * [lon2dd $lon4]]"
}

proc igeolo2dmsI {igeolo i} {
    set lat1   [string range $igeolo 0 5]
    set lat1ns [string range $igeolo 6 6]
    set lon1   [string range $igeolo 7 13]
    set lon1ew [string range $igeolo 14 14]
    set lat2   [string range $igeolo 15 20]
    set lat2ns [string range $igeolo 21 21]
    set lon2   [string range $igeolo 22 28]
    set lon2ew [string range $igeolo 29 29]
    set lat3   [string range $igeolo 30 35]
    set lat3ns [string range $igeolo 36 36]
    set lon3   [string range $igeolo 37 43]
    set lon3ew [string range $igeolo 44 44]
    set lat4   [string range $igeolo 45 50]
    set lat4ns [string range $igeolo 51 51]
    set lon4   [string range $igeolo 52 58]
    set lon4ew [string range $igeolo 59 59]

    switch $i {
	0 { return [expr [news2factor $lat1ns] * [lat2dd $lat1]] }
	1 { return [expr [news2factor $lon1ew] * [lon2dd $lon1]] }
	2 { return [expr [news2factor $lat2ns] * [lat2dd $lat2]] }
	3 { return [expr [news2factor $lon2ew] * [lon2dd $lon2]] }
	4 { return [expr [news2factor $lat3ns] * [lat2dd $lat3]] }
	5 { return [expr [news2factor $lon3ew] * [lon2dd $lon3]] }
	6 { return [expr [news2factor $lat4ns] * [lat2dd $lat4]] }
	7 { return [expr [news2factor $lon4ew] * [lon2dd $lon4]] }
    }
}

proc min {a b} {
    if {$a < $b} {
	return $a
    } else {
	return $b
    }
}

proc max {a b} {
    if {$a > $b} {
	return $a
    } else {
	return $b
    }
}

proc min4 {a b c d} {
    min $a [min $b [min $c $d]]
}

proc max4 {a b c d} {
    max $a [max $b [max $c $d]]
}

#igeolo2dms $argv
