#load ossimTcl.so

proc cibmos2 {dir1 dir2 dir3 dir4 outputPath {master ""} {N 90.0} {E 180.0} {W -180.0} {S -90.0}} {
    cibmos $dir1 $dir2 $dir3 $dir4 $outputPath $N $E $W $S 0 $master
}

# accept up to four source CIB datasets
# extract raw files, labels, gtlabels, and mosiacs to outputPath
proc cibmos {dir1 dir2 dir3 dir4 outputPath {N 90.0} {E 180.0} {W -180.0} {S -90.0} {xvd 0} {master ""}} {
    puts "cibmos $dir1 $dir2 $dir3 $dir4"
    if {$N < $S || $E < $W} {
	tk_messageBox -message "N ($N) < S ($S) or E ($E) < W ($W) in call to cibmos"
	return
    }

    set sourceList {}
    if {$dir1 != "" && $dir1 != "_"} {
	lappend sourceList $dir1
	puts "processing \"${dir1}\""
    }
    if {$dir2 != "" && $dir2 != "_"} {
	lappend sourceList $dir2
	puts "processing \"${dir2}\""
    }
    if {$dir3 != "" && $dir3 != "_"} {
	lappend sourceList $dir3
	puts "processing \"${dir3}\""
    }
    if {$dir4 != "" && $dir4 != "_"} {
	lappend sourceList $dir4
	puts "processing \"${dir4}\""
    }

    if {$sourceList == {}} {
	tk_messageBox -message "No CIB source identified for mosaic"
	return
    }

    set tempFileNum 0
    set tempFileList {}

    foreach source $sourceList {
	set imageCount [ossim::getEntryListSize $source]
	puts "imageCount $imageCount"
	for {set index 0} {$index < $imageCount} {incr index} {
	    if {0} {
		set geom [ossim::getEntryGeometry $source $index]
		array set geomA $geom
		set nl $geomA(number_lines:)
		set ns $geomA(number_samples:)
		set n $geomA(ul_lat:)
		set e $geomA(ur_lon:)
		set w $geomA(ul_lon:)
		set s $geomA(ll_lat:)

		# check corner assumptions
		if {$n != $geomA(ur_lat:)} {
		    tk_messageBox -message "ul_lat != ur_lat in $source"
		}
		if {$e != $geomA(lr_lon:)} {
		    tk_messageBox -message "ur_lon != lr_lon in $source"
		}
		if {$w != $geomA(ll_lon:)} {
		    tk_messageBox -message "ul_lon != ll_lon in $source"
		}
		if {$s != $geomA(lr_lat:)} {
		    tk_messageBox -message "ll_lat != lr_lat in $source"
		}

		if {$geomA(number_input_bands:) != 1 ||
		    $geomA(number_output_bands:) != 1} {
		    tk_messageBox -message "band count assertion failed for $source"
		}
	    }

	    set ll [exec gdalinfo $source | grep "Lower Left"]
	    set ur [exec gdalinfo $source | grep "Upper Right"]
	    set size [exec gdalinfo $source | grep "Size is"]

	    # Lower Left  ( -73.5108033,  19.4869565) ( 73d30'38.89"W, 19d29'13.04"N)
	    # Upper Right ( -72.9872576,  20.0117647) ( 72d59'14.13"W, 20d 0'42.35"N)
	    # Size is 53760, 58368
	    # 012345678990123456789012345678901234567
	    set n [string range $ur 27 37]
	    set e [string range $ur 13 24]
	    set w [string range $ll 13 24]
	    set s [string range $ll 27 37]

	    set size [split $size ","]
	    set nl [string trim [lindex $size 1]]
	    set ns [lindex [lindex $size 0] 2]
	    set NL $nl
	    set NS $ns

	    puts "rectsOverlap $n $e $w $s $N $E $W $S ? [rectsOverlap $n $e $w $s $N $E $W $S]"

	    if {[rectsOverlap $n $e $w $s $N $E $W $S]} {
		set sl [expr round( $nl * ($n - $N) / ($n - $s) )]
		set ss [expr round( $ns * ($W - $w) / ($e - $w) )]
		set el [expr round( $nl * ($n - $S) / ($n - $s) )]
		set es [expr round( $ns * ($E - $w) / ($e - $w) )]

		if {$sl < 0} {
		    set sl 0
		}
		if {$ss < 0} {
		    set ss 0
		}
		if {$el > $nl} {
		    set el $nl
		}
		if {$es > $ns} {
		    set es $ns
		}

		set nl [expr $el - $sl]
		set ns [expr $es - $ss]
		incr sl ; # one-based
		incr ss ; # one-based

		set tempFile cibTemp_${tempFileNum}.img
		incr tempFileNum
		lappend tempFileList $tempFile

		puts "ossim::toVicar $source $tempFile $index 0 $sl $ss $nl $ns $NL $NS $n $e $w $s"
		ossim::toVicar $source $tempFile $index 0 $sl $ss $nl $ns $NL $NS $n $e $w $s
	    }
	}
    }

    puts "Mosaicking cib pieces ..."

    set tempPdf cibrun.pdf
    set file [open $tempPdf "w"]
    puts $file "procedure"
    puts $file "body"
    puts $file "setlib-add library=(\$R2LIB)"
    puts $file "local afidsroot type=(string,128)"
    puts $file "translog AFIDS_ROOT afidsroot"
    puts $file "setlib-delete library=(\$R2LIB)"
    puts $file "setlib-add library=(&afidsroot/vdev,\$R2LIB)\n"

    if {$master == ""} {
	set firstInput [lindex $tempFileList 0]
	set sizedInput $firstInput
	foreach tempFile [lrange $tempFileList 1 end] {
	    set sizedOutput ${tempFile}.sz
	    puts $file "gtsize inp=( +"
	    puts $file "${tempFile},${firstInput} +"
	    puts $file ") out=${sizedOutput} 'coverinp"
	    lappend sizedInput $sizedOutput
	}
    } else {
	set sizedInput {}
	foreach tempFile $tempFileList {
	    set sizedOutput ${tempFile}.sz
	    puts $file "gtsize inp=( +"
	    puts $file "${tempFile},${master} +"
	    puts $file ") out=${sizedOutput} 'coverinp"
	    lappend sizedInput $sizedOutput
	}
    }

    puts $file "fthfastmos +"

    set index 1
    foreach tempFile $sizedInput {
	puts $file "inp${index}=$tempFile +"
	incr index
    }

    puts $file "out=${outputPath} +"
    puts $file "toler=.001"
    
    puts $file {write "MSG: PROCESSING COMPLETED"}

    foreach tempFile $tempFileList {
	puts $file "ush rm -f $tempFile"
    }

    foreach tempFile [lrange $sizedInput 1 end] {
	puts $file "ush rm -f $tempFile"
    }

    puts $file "ush rm -f cibrun.pdf"

    puts $file "\nend-proc"
    close $file
}

proc rectsOverlap {n1 e1 w1 s1 n2 e2 w2 s2} {
    if {$s1 > $n2 || $n1 < $s2 || $e1 < $w2 || $w1 > $e2} {
	return 0
    } else {
	return 1
    }
}

#cibmos "/data/cibSamples/cib05/RPF/A.TOC" "" "" "" foo.img 47.2 -116 -121 46.8
