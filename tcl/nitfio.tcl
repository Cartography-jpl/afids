#! /usr/bin/env tclsh
# -*-Tcl-*-
#

set fileHeaderFieldSizes {
    FHDR 4
    FVER 5
    CLEVEL 2
    STYPE 4
    OSTAID 10
    FDT 14
    FTITLE 80
    FSCLAS 1
    FSCLSY 2
    FSCODE 11
    FSCTLH 2
    FSREL 20
    FSDCTP 2
    FSDCDT 8
    FSDCXM 4
    FSDG 1
    FSDGDT 8
    FSCLTX 43
    FSCATP 1
    FSCAUT 40
    FSCRSN 1
    FSSRDT 8
    FSCTLN 15
    FSCOP 5
    FSCPYS 5
    ENCRYP 1
    FBKGC 3
    ONAME 24
    OPHONE 18
    FL 12
    HL 6
    NUMI 3
    LISH0 6
    LI0 10
}

array set aFileHeaderFieldSizes $fileHeaderFieldSizes

array set aFileHeaderFieldOffsets {}

set offset 0
foreach {name size} $fileHeaderFieldSizes {
    array set aFileHeaderFieldOffsets [list $name $offset]
    incr offset $size
}

set imageHeaderFieldSizes {
    IM 2
    IID1 10
    IDATIM 14
    TGTID 17
    IID2 80
    ISCLAS 1
    ISCLSY 2
    ISCODE 11
    ISCTLH 2
    ISREL 20
    ISDCTP 2
    ISDCDT 8
    ISDCXM 4
    ISDG 1
    ISDGDT 8
    ISCLTX 43
    ISCATP 1
    ISCAUT 40
    ISCRSN 1
    ISSRDT 8
    ISCTLN 15
    ENCRYP 1
    ISORCE 42
    NROWS 8
    NCOLS 8
    PVTYPE 3
    IREP 8
    ICAT 8
    ABPP 2
    PJUST 1
    ICORDS 1
    IGEOLO 60
}

array set aImageHeaderFieldSizes $imageHeaderFieldSizes

array set aImageHeaderFieldOffsets {}

set offset 0
foreach {name size} $imageHeaderFieldSizes {
    array set aImageHeaderFieldOffsets [list $name $offset]
    incr offset $size
}

# This could be generalized to handle optional fields
proc fileHeaderFieldOffset {field} {
    global aFileHeaderFieldOffsets

    return $aFileHeaderFieldOffsets($field)
}

proc getFileHeaderField {path field} {
    global aFileHeaderFieldSizes

    set f [open $path r]
    seek $f [fileHeaderFieldOffset $field]

    set data [read $f $aFileHeaderFieldSizes($field)]

    close $f
    return $data
}

proc setFileHeaderField {path field value} {
    global aFileHeaderFieldSizes

    set f [open $path r+]
    seek $f [fileHeaderFieldOffset $field]

    puts -nonewline $f $value

    close $f
}

proc printFileHeaderFields {path} {
    global fileHeaderFieldSizes

    foreach {name size} $fileHeaderFieldSizes {
	puts "$name [getFileHeaderField $path $name]"
    }
}

# This could be generalized to handle optional fields
proc imageHeaderFieldOffset {field} {
    global aImageHeaderFieldOffsets

    return $aImageHeaderFieldOffsets($field)
}

proc getImageHeaderField {path field} {
    global aImageHeaderFieldSizes

    set fileHeaderLength [getFileHeaderField $path HL]
    set fileHeaderLength [string trimleft $fileHeaderLength "0"]

    set f [open $path r]
    seek $f [expr $fileHeaderLength + [imageHeaderFieldOffset $field]]

    set data [read $f $aImageHeaderFieldSizes($field)]

    close $f
    return $data
}

proc setImageHeaderField {path field value} {
    global aImageHeaderFieldSizes

    set fileHeaderLength [getFileHeaderField $path HL]
    set fileHeaderLength [string trimleft $fileHeaderLength "0"]

    set f [open $path r+]
    seek $f [expr $fileHeaderLength + [imageHeaderFieldOffset $field]]

    puts -nonewline $f $value

    close $f
}

proc printImageHeaderFields {path} {
    global imageHeaderFieldSizes

    foreach {name size} $imageHeaderFieldSizes {
	puts "$name [getImageHeaderField $path $name]"
    }
}

proc copyFileHeaderFields {fields from to} {
    foreach field $fields {
	setFileHeaderField $to $field [getFileHeaderField $from $field]
    }
}

proc copyImageHeaderFields {fields from to} {
    foreach field $fields {
	setImageHeaderField $to $field [getImageHeaderField $from $field]
    }
}

proc copyNitfHeaders {from to} {
    set fileItems2Keep {
	FHDR
	FVER
	CLEVEL
	STYPE
	OSTAID
	FDT
	FTITLE
	FSCLAS
	FSCLSY
	FSCODE
	FSCTLH
	FSREL
	FSDCTP
	FSDCDT
	FSDCXM
	FSDG
	FSDGDT
	FSCLTX
	FSCATP
	FSCAUT
	FSCRSN
	FSSRDT
	FSCTLN
	FSCOP
	FSCPYS
	ENCRYP
	FBKGC
	ONAME
	OPHONE
    }

    set imageItems2Keep {
	IDATIM
	TGTID
	IID2
	ISCLAS
	ISCLSY
	ISCODE
	ISCTLH
	ISREL
	ISDCTP
	ISDCDT
	ISDCXM
	ISDG
	ISDGDT
	ISCLTX
	ISCATP
	ISCAUT
	ISCRSN
	ISSRDT
	ISCTLN
	ENCRYP
	ISORCE
    }

    set tmp {}
    foreach field $fileItems2Keep {
	lappend tmp $field
    }
    set fileItems2Keep $tmp

    set tmp {}
    foreach field $imageItems2Keep {
	lappend tmp $field
    }
    set imageItems2Keep $tmp

    copyFileHeaderFields $fileItems2Keep $from $to
    copyImageHeaderFields $imageItems2Keep $from $to
}

if {[info vars argv] != {}} {
    foreach path $argv {
	puts "*** FILE HEADER ***"
	printFileHeaderFields $path

	puts "*** IMAGE HEADER ***"
	printImageHeaderFields $path
    }
}