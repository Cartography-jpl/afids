# Filename tempalte predate-postdate_presrc-postsrc_aoi_vSWversion.csv
# Example filename 20100928-20101001_WV2-WV2_AF1B_v1.75.csv

#        REQUIRED FIELDS: (parentheticals are notes vice part of field name)
#                Date1       (Date of Image1)
#                Time1       (Time of image1)
#                Date2       (Date of Image2)
#                Time2       (Time of Image2)
#                Lat
#                Lon
#                Sensor1     (Name of Sensor1)
#                Sensor2     (Name of Sensor2)
#                BV          (Brightness Value)
#                BG
#                NDVI
       
#                Please also include values for the individual bands. As discussed substitute the [Bx] for the band descriptor
#             (R/G/B/NIR/SWIR/Etc):
#                [B1]_BV
#                [B1]_BG
#                [B1]_NDVI
#                [B2]_BV
#                [B2]_BG
#                [B2]_NDVI
#                <!-- Etc -->

#        FORMATTING:
#                Dates will be in the format of: yyyymmdd (i.e. 20100416)
#                Times will be in the format of: hh:mm:ss (i.e. 15:02:26)
#                Coordinates will be degree decimal to eight decimal places.
#                Leave off the hemisphere letter and use negative values for West and South
#                Abbreviations: LS5, LS7, SP4, SP5, RE, WV1, WV2, QB, etc
#                "Run" number in filename is incremented by one when an image pair is reprocessed.  
#                This will prevent confusion over which is current.

# rapideye case
# pre="2009-11-04T072343_RE2_1B-NAC_2572874_72469_" +
# post="2009-11-10T070802_RE4_1B-NAC_2570700_71771_" +

# asc file contents:
#      1     64.42303241    31.79317130       1
#      3     64.55858796    31.78150463       1
#      4     64.55488426    31.78025463       1
#      5     64.42307870    31.74136574       1
#      6     64.55034722    31.71752315       1
#      7     64.40456019    31.68261574       1
#      8     64.40442130    31.68247685       1
#     13     63.99062500    31.64020833       1
#     14     63.98840278    31.64011574       1
#     15     63.99048611    31.64011574       1

#load ossimTcl.so

proc wv2asc2fade {subdir prefile postfile aoi} {

    set inpcoords outfadecoords
    set inpcols outfade

    # NITF_IDATIM=20100911065617
    #     set predatim [exec gdalinfo $prefile | grep IDATIM]
    #     set split [split $predatim =]
    #     set datim [lindex $split 1]
    set datim [ossim::getImageHeaderFields $prefile 0]
    set fieldLabel "IDATIM (Image Date & Time): "
    set i [string first $fieldLabel $datim]
    set len [string length $fieldLabel]
    set datim [string range $datim [expr $i + $len] [expr $i + $len + 13]]
    set datim [conformIdatimTo2500C $datim]

    set Date1 [string range $datim 0 7]
    set Time1 "[string range $datim 8 9]:[string range $datim 10 11]:[string range $datim 12 13]"

    #     set postdatim [exec gdalinfo $postfile | grep IDATIM]
    #     set split [split $postdatim =]
    #    set datim [lindex $split 1]
    set datim [ossim::getImageHeaderFields $postfile 0]
    set fieldLabel "IDATIM (Image Date & Time): "
    set i [string first $fieldLabel $datim]
    set len [string length $fieldLabel]
    set datim [string range $datim [expr $i + $len] [expr $i + $len + 13]]
    set datim [conformIdatimTo2500C $datim]

    set Date2 [string range $datim 0 7]
    set Time2 "[string range $datim 8 9]:[string range $datim 10 11]:[string range $datim 12 13]"

    set Sensor1 "WV2"
    set Sensor2 "WV2"

    global env
    set svnVer $env(AFIDS_VERSION)
    set outf [open ${subdir}/${Date1}-${Date2}_${Sensor1}-${Sensor2}_${aoi}_v${svnVer}.csv w]

    set f [open $inpcoords "r"]
    set data [read $f]
    close $f

    set coordslines [split $data "\n"]

    set f [open $inpcols "r"]
    set data [read $f]
    close $f

    set colslines [split $data "\n"]

    puts $outf "Date1,Time1,Date2,Time2,Lat,Lon,Sensor1,Sensor2,BV,B1_BV,B2_BV,B3_BV,B4_BV,B5_BV,B6_BV,B7_BV,B8_BV"

    set BV ""
    set BG ""
    set NDVI ""
    set B1_BV ""
    set B1_BG ""
    set B1_NDVI ""
    set B2_BV ""
    set B2_BG ""
    set B2_NDVI ""
    set B3_BV ""
    set B3_BG ""
    set B3_NDVI ""
    set B4_BV ""
    set B4_BG ""
    set B4_NDVI ""
    set B5_BV ""
    set B5_BG ""
    set B5_NDVI ""

    set index 0
    foreach line $coordslines {
	if {$line == ""} {
	    continue
	}

	set Lon [lindex $line 0]
	set Lat [lindex $line 1]

	set cols [lindex $colslines $index]

	set BV [lindex $cols 0]
	#set BG [lindex $cols 1]
	#set NDVI [lindex $cols 2]
	set B1_BV [lindex $cols 1]
	set B2_BV [lindex $cols 2]
	set B3_BV [lindex $cols 3]
	set B4_BV [lindex $cols 4]
	set B5_BV [lindex $cols 5]
	set B6_BV [lindex $cols 6]
	set B7_BV [lindex $cols 7]
	set B8_BV [lindex $cols 8]

	puts $outf "$Date1,$Time1,$Date2,$Time2,$Lat,$Lon,$Sensor1,$Sensor2,$BV,$B1_BV,$B2_BV,$B3_BV,$B4_BV,$B5_BV,$B6_BV,$B7_BV,$B8_BV"

	incr index
    }

    close $outf
}

# NITF_IDATIM=21184840ZMAY10 (Seen in WV2)
#             DDhhmmssZMMMYY
proc dateTimeFromZ {idatim} {
    set mon [string range $idatim 9 11]
    switch $mon {
	"JAN" {
	    set MM "01"
	}
	"FEB" {
	    set MM "02"
	}
	"MAR" {
	    set MM "03"
	}
	"APR" {
	    set MM "04"
	}
	"MAY" {
	    set MM "05"
	}
	"JUN" {
	    set MM "06"
	}
	"JUL" {
	    set MM "07"
	}
	"AUG" {
	    set MM "08"
	}
	"SEP" {
	    set MM "09"
	}
	"OCT" {
	    set MM "10"
	}
	"NOV" {
	    set MM "11"
	}
	"DEC" {
	    set MM "12"
	}
    }

    # DDhhmmssZMMMYY
    set DD [string range $idatim 0 1]
    set hh [string range $idatim 2 3]
    set mm [string range $idatim 4 5]
    set ss [string range $idatim 6 7]
    set YY [string range $idatim 12 13]

    # want YYYYMMDDhhmmss
    return "20${YY}${MM}${DD}${hh}${mm}${ss}"
}

proc conformIdatimTo2500C {idatim} {
    if {[string first "Z" $idatim] > 0} {
	return [dateTimeFromZ $idatim]
    } else {
	return $idatim
    }
}

proc wv2date {file} {
    if [catch {ossim::getImageHeaderFields $file 0} datim] {
	# Don't worry if we have trouble reading the field, just return
	# a blank.
	return "BLANK_IDATIM"
    } else {
	set fieldLabel "IDATIM (Image Date & Time): "
	set i [string first $fieldLabel $datim]
	set len [string length $fieldLabel]
	set datim [string range $datim [expr $i + $len] [expr $i + $len + 13]]

	if {[string trim $datim] == ""} {
	    return "BLANK_IDATIM"
	}

	set datim [conformIdatimTo2500C $datim]

	return [string range $datim 0 7]
    }
}

proc wv2time {file} {
    if [catch {ossim::getImageHeaderFields $file 0} datim] {
	# Don't worry if we have trouble reading the field, just return
	# a blank.
	return "BLANK_IDATIM"
    } else {
	set fieldLabel "IDATIM (Image Date & Time): "
	set i [string first $fieldLabel $datim]
	set len [string length $fieldLabel]
	set datim [string range $datim [expr $i + $len] [expr $i + $len + 13]]

	if {[string trim $datim] == ""} {
	    return "BLANK_IDATIM"
	}

	set datim [conformIdatimTo2500C $datim]
	return "[string range $datim 8 9]:[string range $datim 10 11]:[string range $datim 12 13]"
    }
}

proc wv2asc2fade2 {subdir prefile postfile aoi sensor1 sensor2} {

    set inpcoords ${subdir}/outtabcoords
    set inpcols ${subdir}/outfade

    # NITF_IDATIM=20100911065617 (MIL-STD-1500C)
    #             YYYYMMDDhhmmss
    # NITF_IDATIM=21184840ZMAY10 (Seen in WV2)
    #             DDhhmmssZMMMYY
    #     set predatim [exec gdalinfo $prefile | grep IDATIM]
    #     set split [split $predatim =]
    #     set datim [lindex $split 1]
    set Date1 [wv2date $prefile]
    set Time1 [wv2time $prefile]

    set Date2 [wv2date $postfile]
    set Time2 [wv2time $postfile]

    set sensor1 "WV2"
    set sensor2 "WV2"

    global env
    set svnVer $env(AFIDS_VERSION)
    set outf [open ${subdir}/${Date1}-${Date2}_${sensor1}-${sensor2}_${aoi}_v${svnVer}.csv w]

    set f [open $inpcoords "r"]
    set data [read $f]
    close $f

    set coordslines [split $data "\n"]

    set f [open $inpcols "r"]
    set data [read $f]
    close $f

    set colslines [split $data "\n"]

    puts $outf "Date1,Time1,Date2,Time2,Lat,Lon,sensor1,sensor2,BV"

    set BV ""

    set index 0
    foreach line $coordslines {
	if {$line == ""} {
	    continue
	}

	set Lon [lindex $line 0]
	set Lat [lindex $line 1]

	set cols [lindex $colslines $index]

	set BV [lindex $cols 0]

	puts $outf "$Date1,$Time1,$Date2,$Time2,$Lat,$Lon,$sensor1,$sensor2,$BV"

	incr index
    }

    close $outf
}
