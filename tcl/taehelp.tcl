proc ls2lon {line samp} {
    set f [open linesamplonlat.dat "r"]
    set data [read $f]
    close $f

    set lines [split $data \n]
    foreach inline $lines {
	if {$inline == ""} {
	    continue
	}
	set l [lindex $inline 0]
	set s [lindex $inline 1]
	if {$l == $line && $s == $samp} {
	    return [lindex $inline 2]
	}
    }
    puts "ls2lon $line $samp returned nothing from linesamplonlat.dat"
}

proc ls2lat {line samp} {
    set f [open linesamplonlat.dat "r"]
    set data [read $f]
    close $f

    set lines [split $data \n]
    foreach inline $lines {
	if {$inline == ""} {
	    continue
	}
	set l [lindex $inline 0]
	set s [lindex $inline 1]
	if {$l == $line && $s == $samp} {
	    return [lindex $inline 3]
	}
    }
    puts "ls2lat $line $samp returned nothing from linesamplonlat.dat"
}

proc ls2class {line samp} {
    # (lat lon date1 date2 bv)
    set f [open linesamplonlat.dat "r"]
    set data [read $f]
    close $f

    set lines [split $data \n]
    foreach inline $lines {
	if {$inline == ""} {
	    continue
	}
	set l [lindex $inline 0]
	set s [lindex $inline 1]
	if {$l == $line && $s == $samp} {
	    return [lindex $inline 4]
	}
    }
    puts "ls2class $lon $lat returned nothing from thumbdat"
}

proc isodd {arg} {
    expr $arg % 2 == 1
}

proc tush {args} {
    if {[catch {tush_1 $args} msg]} {
	set name [clock format [clock seconds] -format "%b%d-%H:%M:%S"].err
	set file [open /tmp/$name w]
	puts $file "tush error: $msg"
	close $file

	return "see /tmp/$name"
    }

    return $msg
}	

proc tush_1 {_args} {
    set cmd [lindex $_args 0]
    set _args [lrange $_args 1 end]
    switch $cmd {
	rm {
	    if {[llength $_args] != 1} {
		error "usage: rm file"
	    }
	    foreach file [glob -nocomplain $_args] {
		file delete -force $file
	    }
	}
	echo {
	    set length [llength $_args]

	    if {$length == 2} {
		if {[lindex $_args 0] != ">"} {
		    error "usage: echo string > toFile\n       echo string >> toFile"
		}

		file delete -force [lindex $_args 1]
		set file [open [lindex $_args 1] w]
		close $file
		return
	    }

	    if {$length < 3} {
		error "usage: echo string > toFile\n       echo string >> toFile"
	    }

	    set toFile [lrange $_args [expr $length - 1] [expr $length - 1]]

	    if {[lindex $_args [expr $length - 2]] == ">"} {
		if {[lindex $_args [expr $length - 3]] == ">"} {
		    set op "a"
		    set string [lrange $_args 0 [expr $length - 4]]
		} else {
		    set op "w"
		    set string [lrange $_args 0 [expr $length - 3]]
		}
	    } elseif {[lindex $_args [expr $length - 2]] == ">>"} {
		set op "a"
		set string [lrange $_args 0 [expr $length - 3]]
	    } else {
		error "usage: echo string > toFile\n       echo string >> toFile"
	    }

	    set file [open $toFile $op]
	    puts $file "$string"
	    close $file
	}
	cp {
	    if {[llength $_args] != 2} {
		error "usage: cp fromFile toFile"
	    }
	    file copy -force [lindex $_args 0] [lindex $_args 1]
	}
	date {
	    if {[llength $_args] != 3 || [lindex $_args 0] != ">" || [lindex $_args 1] != ">"} {
		error "usage: date >> toFile"
	    }
	    set toFile [lindex $_args 2]
	    set file [open $toFile a]
	    puts $file [clock format [clock seconds]]
	    close $file
	}
	cat {
	    set length [llength $_args]
	    if {$length == 1} {
		set file [lindex $_args 0]
		set file [open $file r]
		set data [read $file]
		close $file
		puts -nonewline $data
		return
	    }

	    if {$length != 4 || [lindex $_args 1] != ">"  || [lindex $_args 2] != ">"} {
		error "usage: cat fromFile >> toFile"
	    }
	    set file1 [lindex $_args 0]
	    set file2 [lindex $_args 3]
	    set file [open $file1 r]
	    set data [read $file]
	    close $file
	    set file [open $file2 a]
	    puts -nonewline $file $data
	    close $file
	}
	grep {
	    if {[llength $_args] != 7 || [lindex $_args 0] != "-e" || [lindex $_args 2] != "-e" || [lindex $_args 5] != ">" } {
		error "usage: grep -e pat1 -e pat2 searchFile > toFile"
	    }
	    set pat1 [lindex $_args 1]
	    set pat2 [lindex $_args 3]
	    set file1 [lindex $_args 4]
	    set file1 [open $file1 r]
	    set data [read $file1]
	    close $file1
	    set lines [split $data '\n']
	    set file2 [lindex $_args 6]
	    set file2 [open $file2 w]
	    foreach line $lines {
		if {[string first $pat1 $line] >= 0 || [string first $pat2 $line] >= 0} {
		    puts $file2 $line
		}
	    }
	    close $file2
	}
	sleep {
	    if {[llength $_args] != 1} {
		error "usage: sleep seconds"
	    }
	    set seconds [lindex $_args 0]
	    set doneTime [expr [clock clicks -milliseconds] + $seconds * 1000]
	    while {[clock clicks -milliseconds] < $doneTime} {
	    }
	}
	mkdir {
	    if {[llength $_args] != 1} {
		error "usage: mkdir dirName"
	    }
	    file mkdir [lindex $_args 0]
	}
	ln {
	    if {[llength $_args] != 3 || [lindex $_args 0] != "-s"} {
		error "usage: ln -s target linkName"
	    }
	    set target [lindex $_args 1]
	    set linkName [lindex $_args 2]
	    file link $linkName $target
	}
	pwd {
	    pwd
	}
	mkfifo {
	    if {[llength $_args] != 1} {
		error "usage: mkfifo pipeName"
	    }
	    global env
	    load notifyTcl.so
	    mkfifo [lindex $_args 0]
	}
	vifClient {
	    if {[llength $_args] != 3} {
		error "usage: vifClient ip port filename"
	    }
	    global env
	    load notifyTcl.so
	    vifClient [lindex $_args 0] [lindex $_args 1] [lindex $_args 2]
	}
	    
	default {
	    error "Unsupported tush command: $cmd"
	}
    }
}

proc saneFile {dir old new} {
    foreach file [glob -nocomplain "${dir}/*"] {
	set replace $file
	set first [string first $old $file]
	if {$first >= 0} {
	    set replace [string replace $file $first [expr $first + [string length $old] - 1] $new]
	}

	set first [string first " " $replace]
	while {$first >= 0} {
	    set replace [string replace $replace $first $first "_"]
	    set first [string first " " $replace]
	}

	if {$file != $replace} {
	    puts "renaming \"$file\" to \"$replace\""
	    file rename $file $replace
	}
    }
}

proc removeEolComments {data {char "#"}} {
    set split [split $data "\n"]
    set collection {}
    foreach line $split {
	if {[string index $line 0] != $char} {
	    lappend collection $line
	}
    }
    join $collection "\n"
}

proc wildFile {file} {
    if { [catch { globOneFile $file } match ] } {
	return "NOMATCH"
    } else {
	return $match
    }
}	

proc globOneFile {file} {
    set set [glob -nocomplain $file]

    if { [llength $set] > 1 } {
	puts "too many files match: $file"
	error "too many files match: $file"
    }

    if { $set == {} } {
	puts "no such file: $file"
	error  "no such file: $file"
    }

    return [lindex $set 0]
}

proc fileVar2ValQ {file var {q "'"}} {
    set file [globOneFile $file]

    set f [open $file "r"]
    set data [removeEolComments [read $f]]
    close $f
    set pos [string first $var $data]
    if {$pos < 0} {
	return $errVal
    }
    incr pos [string length $var]
    if { [string range $data $pos $pos] == "=" } {
	incr pos ; # skip equals sign
    }
    if { [string range $data $pos $pos] == "'" } {
	incr pos ; # skip apostrophe
	set quotedInput 1
    } else {
	set quotedInput 0
    }
    if { $quotedInput } {
	set pos2 [expr [string first "'" $data $pos] - 1]
    } else {
	set pos2 [expr [string first "\n" $data $pos] - 1]
    }
    if { $pos2 < 0 } {
	set pos2 [string length $data]
    }
    set val "$q[string range $data $pos $pos2]$q"
    return $val
}

proc fileVar2Val {file var} {
    fileVar2ValQ $file $var ""
}

proc fileVar2DecVal {file var {errVal 0}} {
    set file [globOneFile $file]

    set f [open $file "r"]
    set data [removeEolComments [read $f]]
    close $f
    set pos [string first $var $data]
    if {$pos < 0} {
	return $errVal
    }
    incr pos [string length $var]
    if {[string index $data $pos] == "'"} {
	incr pos ; # skip apostrophe
	set pos2 [expr [string first "'" $data $pos] - 1]
    } else {
	set pos2 [expr [string first "\n" $data $pos] - 1]
    }
    set val [string range $data $pos $pos2]
    # protect 0 padded ints from being treated as octal
    set num [string trimleft $val "0"]
    return $num
}

proc strVal2RealVar {val} {
    set num [string trimleft $val "0"]
    return $num
}

proc fileVar2RealVal {file var {errVal 0}} {
    set file [globOneFile $file]

    set f [open $file "r"]
    set data [removeEolComments [read $f]]
    close $f
    set pos [string first $var $data]
    if {$pos < 0} {
	return $errVal
    }
    incr pos [string length $var]
    if {[string index $data $pos] == "'"} {
	incr pos ; # skip apostrophe
	set pos2 [expr [string first "'" $data $pos] - 1]
    } else {
	set pos2 [expr [string first "\n" $data $pos] - 1]
    }
    set val [string range $data $pos $pos2]
    # remove leading zeros
    set num [string trimleft $val "0"]
    return $num
}

proc removePath {pathAndFile} {
    set split [split $pathAndFile "/"]
    lindex $split end
}

# getFirstMatchingFileRemovingSuffix
proc getFMFRS {pathPattern} {
    set list [glob -nocomplain $pathPattern]
    if {$list == {}} {
	return "NOMATCH"
    }
    set sort [lsort -ascii $list]
    set match [lindex $sort 0]
    if {[string first "." $match] < 0} {
	return $match
    } else {
	set split [split $match "."]
	set range [lrange $split 0 [expr [llength $split] - 2]]
	set join [join $range "."]
	return $join
    }
}

proc oldStringFind {needle haystack} {
    expr [string first $needle $haystack] >= 0
}

# interpret asterisk as wildcard
proc stringFind {pattern haystack} {
    regsub -all {\*} $pattern {.*} fixed
    expr [llength [regexp -inline -indices $fixed $haystack]] > 0
}

# treat pattern as first class advanced extended regular expression
proc stringFindRe {pattern haystack} {
    expr [llength [regexp -inline -indices $fixed $haystack]] > 0
}

# gets the 75 from "ALV0_PRC_LASTFILE_20000101120028922_F00075_VB40ABCD.bin"
proc getLastFileNumber {name} {
    set lastfile [string first "LASTFILE" $name]
    if {$lastfile < 0} {
	return 0
    } else {
	set split [split $name "_"]
	set numField [lindex $split 4]
	if {[string index $numField 0] != "F"} {
	    return 0
	} else {
	    set field [string range $numField 1 end]
	    set num [string trimleft $field "0"]
	    return $num
	}
    }    
}

# match the float-float substring in a file like
# ACAL_DEC_DOFFS-PRC-C8-G1_00005.013-00104.988_20070105123003063_VSN107TT1.rel
# look in dir for files with embedded signature where value is float-float range
proc matchRange {dir signature value} {
    cd $dir

    foreach file [glob -nocomplain *] {
	if { [string first $signature $file] >= 0 } {
	    if { [regexp -indices {_[0-9]+\.[0-9]+-[0-9]+\.[0-9]+} $file index] } {
		set first [lindex $index 0]
		incr first ; # skip _
		set last [lindex $index 1]
		set range [string range $file $first $last]
		set hyphenIndex [string first "-" $range]
		set firstFloat [string trim [string range $range 0 [expr $hyphenIndex - 1]] "0"]
		set secondFloat [string trim [string range $range [expr $hyphenIndex + 1] end] "0"]

		if { $value >= $firstFloat && $value <= $secondFloat } {
		    return $file
		}
	    }
	}
    }

    return "NOMATCH"
}

# returns current GMT in YYYYMMDDHHMMSS000000 format
proc gmtDateTime {} {
    return [clock format [clock seconds] -format %Y%m%d%H%M%S -gmt true]000000
}

proc ntf2idatim {path} {
    set data [ossim::getImageHeaderFields $path 0]
    set data [split $data "\n"]
    foreach line $data {
	if { [string first "IDATIM" $line] >= 0 } {
	    set split [split $line]
	    set len [llength $split]
	    set last [expr $len - 1]
	    set idatim [lindex $split $last]
	    return $idatim
	}
    }
    return "IDATIM_NOT_FOUND"
}

proc getNamedValue {nameValueList name} {
    set split [split $nameValueList '\n']
    foreach line $split {
	if {$name == [lindex $line 0]} {
	    return [lrange $line 1 end]
	}
    }
}

proc getTagField {path index tag fieldName} {
    set tagValue [ossim::printTag $path $index $tag]
    getNamedValue $tagValue $fieldName
}
