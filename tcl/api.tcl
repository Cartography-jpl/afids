set LOCAL_DIR [pwd]
set VDEV_DIR "$env(AFIDS_ROOT)/vdev"

proc setupUtilityRun {} {
    global VDEV_DIR
    global utilityType
    global utilityInputImage
    global utilityReferenceImage
    global utilityOutputImage
    global typref
    global Utility_GoreWidth
    global interp
    global polyfit
    global Utility_fftSize
    global Utility_magnify
    global Utility_NAH
    global Utility_NAV
    global Utility_tolerance
    global viewutilitywhenComplete
    global UTILITY_AOI_MIN_LAT
    global UTILITY_AOI_MAX_LAT
    global UTILITY_AOI_MIN_LON
    global UTILITY_AOI_MAX_LON

    set file [open utilityrun.pdf w]

    puts $file "procedure"
    puts $file "body"
    puts $file "! Have to make sure there is an R2LIB for translog"
    puts $file "setlib-add library=(\$R2LIB)"
    puts $file "local afidsroot type=(string,128)"
    puts $file "translog AFIDS_ROOT afidsroot"
    puts $file "! Now move R2LIB after vdev"
    puts $file "setlib-delete library=(\$R2LIB)"
    puts $file "setlib-add library=(&afidsroot/vdev,\$R2LIB)\n"

    switch $utilityType {
	fixRpc {
	    puts $file "fixrpc.pdf +"
	}
	nlcLogNtm {
	    puts $file "run_ntm.pdf +"
	}
	chipPercentage {
	    puts $file "rchipper.pdf +"
	}
	chipSelected {
	    puts $file "tchipper.pdf +"
	}
	chipEntireImage {
	    puts $file "chipper.pdf +"
	}
	ridca {
	    puts $file "cncmp.pdf +"
	}
	cutSubimageVicarOut {
	    puts $file "util1.pdf +"
	}
	cutSubimageGeoTIFFOut {
	    puts $file "util2.pdf +"
	}
	cutToGeoTIFFReference {
	    puts $file "util3.pdf +"
	}
	correlationCorrection {
	    puts $file "util4.pdf +"
	}
	rmsPixels {
	    puts $file "util5.pdf +"
	}
	accuracyVsBaseOrMaster {
	    puts $file "util6.pdf +"
	}
	imageVsWaterMask {
	    puts $file "util7.pdf +"
	}
	geotiffLabel {
	    puts $file "util8.pdf +"
	}
	vicarMapping {
	    puts $file "util9.pdf +"
	}
	displayImage {
	    puts $file "utila.pdf +"
	}
	rms90Vectors {
	    puts $file "utilb.pdf +"
	}
	imageWithRms90Vectors {
	    puts $file "utilc.pdf +"
	}
	cepPixels {
	    puts $file "utild.pdf +"
	}
	satPointError {
	    puts $file "utile.pdf +"
	}
	ntmToGeoTiff {
	    puts $file "utilf.pdf +"
	}
	rpcToGeoTIFFMap {
	    puts $file "utilg.pdf +"
	}
    }

    if {$utilityType == "fixRpc"} {
	#  parm inp      type=(string,99)
	#  parm elev     type=(string,99)
	#  parm linesamp real def=(-999.0,-999.0)  !optional point loc in raw
	#  parm lonlat   real def=(-999.0,-999.0)  !optional point lonlat in raw
	global chipperFixrpcInp chipperFixrpcElev
	global chipperTiepointLine chipperTiepointSamp chipperTiepointLon chipperTiepointLat

	puts $file "inp=$chipperFixrpcInp +"
	puts $file "elev=$chipperFixrpcElev +"
	puts $file "linesamp=($chipperTiepointLine,$chipperTiepointSamp) +"
	puts $file "lonlat=($chipperTiepointLon,$chipperTiepointLat)"
    }

    if {$utilityType == "nlcLogNtm"} {
	global utilityInputImage runNtmTiff nlcLogNtmOutputPrefix

	global SESSION_ID
	puts $file "outn=$SESSION_ID/$nlcLogNtmOutputPrefix +"
	puts $file "tiff=$runNtmTiff +"
    }

    if {$utilityType == "chipPercentage"} {
	global utilityInputImage utilityOutputImage
	global chipPercentage_elevInputImage
	global targetSelectionMode
	global SESSION_ID
	global nlcLogNtmOutputPrefix

	# parm     inname    string   !dir and full filename of input vicar gt image 
	puts $file "inname=${SESSION_ID}/${nlcLogNtmOutputPrefix}.img +"
	# parm     outname   string   !Output filename; "_&K_&M.hlf" will be appended
	puts $file "outname=$SESSION_ID/${nlcLogNtmOutputPrefix} +"
	# parm     elev      string   !Input elevation DEM file covering input image
	puts $file "elev=$chipPercentage_elevInputImage +"
	# parm     percimg   integer  def=0   !Percent of image's theoretical total # of chips
	global percentOfImage
	if {$targetSelectionMode == "percentOfImage" && $percentOfImage != ""} {
	    puts $file "percimg=$percentOfImage +"
	}
	# parm     numchip   integer  def=0   !Max number of chips to be generated
	global numberOfChips
	if {$targetSelectionMode == "numberOfChips" && $numberOfChips != ""} {
	    puts $file "numchip=$numberOfChips +"
	}
	# parm     seed      integer  def=0   !If not 0, overwrights the Random Seed calculation 
	global useDefaultRandomSeed randomSeed
	if {! $useDefaultRandomSeed && $randomSeed != ""} {
	    puts $file "seed=$randomSeed +"
	}
	global chipperOutputLines chipperOutputSamples chipperOverlap
	# parm     outl      integer  def=1024  !num of output lines
	puts $file "outl=$chipperOutputLines +"
	# parm     outs      integer  def=1024  !num of output samps
	puts $file "outs=$chipperOutputSamples +"
	# parm     olap      integer  def=128 !amount of overlap for both lines and samples
	puts $file "olap=$chipperOverlap"
    }

    if {$utilityType == "chipSelected"} {
	global utilityInputImage utilityOutputImage
	global chipSelected_elevInputImage
	global targetXYSelectionMode targetFile
	global xSample yLine
	global SESSION_ID
	global nlcLogNtmOutputPrefix
	# parm     inname  string   !dir and full filename of input vicar gt image 
	puts $file "inname=${SESSION_ID}/${nlcLogNtmOutputPrefix}.img +"
	# parm     outname string   !Output filename; "_&K_&M.hlf" will be appended
	puts $file "outname=$SESSION_ID/${nlcLogNtmOutputPrefix} +"
	# parm     txtname string   def="none" !Input text file with x,y or lat/long coordinates
	if {$targetXYSelectionMode == "tf" && $targetFile != ""} {
	    puts $file "txtname=$targetFile +"
	}
	# parm     tsamp   integer  def=0     !target sample (target mode); X
	if {$targetXYSelectionMode == "xy" && $xSample != ""} {
	    puts $file "tsamp=$xSample +"
	}
	# parm     tline   integer  def=0     !target line (target mode); Y
	if {$targetXYSelectionMode == "xy" && $yLine != ""} {
	    puts $file "tline=$yLine +"
	}
	global chipperOutputLines chipperOutputSamples chipperOverlap
	# parm     outl      integer  def=1024  !num of output lines
	puts $file "outl=$chipperOutputLines +"
	# parm     outs      integer  def=1024  !num of output samps
	puts $file "outs=$chipperOutputSamples +"
	# parm     olap      integer  def=128 !amount of overlap for both lines and samples
	puts $file "olap=$chipperOverlap +"
	# parm     elev    string   !Input elevation DEM file covering input image
	puts $file "elev=$chipSelected_elevInputImage"
    }

    if {$utilityType == "chipEntireImage"} {
	global nlcLogNtmOutputPrefix
	global utilityInputImage utilityOutputImage runNtmTiff

	global SESSION_ID
	puts $file "inname=${SESSION_ID}/${nlcLogNtmOutputPrefix}.img +"
	puts $file "outname=$SESSION_ID/${nlcLogNtmOutputPrefix} +"
	global chipEntireImage_elevInputImage
	puts $file "elev=$chipEntireImage_elevInputImage +"

	global UTILITY_AOI_MIN_LAT
	global UTILITY_AOI_MAX_LAT
	global UTILITY_AOI_MIN_LON
	global UTILITY_AOI_MAX_LON

	set sl $UTILITY_AOI_MAX_LAT
	set ss $UTILITY_AOI_MIN_LON
	set ll $UTILITY_AOI_MIN_LAT
	set ls $UTILITY_AOI_MAX_LON

	if {$sl == ""} {
	    set sl 1
	}
	if {$ss == ""} {
	    set ss 1
	}
	if {$ll == ""} {
	    set ll 0
	}
	if {$ls == ""} {
	    set ls 0
	}

	puts $file "sl=$sl +"
	puts $file "ss=$ss +"
	puts $file "nl=[expr $ll - $sl + 1] +"
	puts $file "ns=[expr $ls - $ss + 1] +"

	global chipperOutputLines chipperOutputSamples chipperOverlap
	puts $file "outl=$chipperOutputLines +"
	puts $file "outs=$chipperOutputSamples +"
	puts $file "olap=$chipperOverlap"
    }

    if {$utilityType == "chipper"} {
	global utilityInputImage utilityOutputImage runNtmTiff

	puts $file "run_ntm.pdf +"

	puts $file "inp=$utilityInputImage +"
	global SESSION_ID
	puts $file "outn=$SESSION_ID/$utilityOutputImage +"
	puts $file "tiff=$runNtmTiff\n"

	puts $file "chipper.pdf +"

	puts $file "inname=${utilityOutputImage}.img +"
	puts $file "outname=$utilityOutputImage +"
	global chipper_elevInputImage
	puts $file "elev=$chipper_elevInputImage"

	global UTILITY_AOI_MIN_LAT
	global UTILITY_AOI_MAX_LAT
	global UTILITY_AOI_MIN_LON
	global UTILITY_AOI_MAX_LON

	set sl $UTILITY_AOI_MAX_LAT
	set ss $UTILITY_AOI_MIN_LON
	set ll $UTILITY_AOI_MIN_LAT
	set ls $UTILITY_AOI_MAX_LON

	if {$sl == ""} {
	    set sl 1
	}
	if {$ss == ""} {
	    set ss 1
	}
	if {$ll == ""} {
	    set ll 0
	}
	if {$ls == ""} {
	    set ls 0
	}

	puts $file "sl=$sl +"
	puts $file "ss=$ss +"
	puts $file "nl=[expr $ll - $sl + 1] +"
	puts $file "ns=[expr $ls - $ss + 1] +"

	global chipperOutputLines chipperOutputSamples chipperOverlap chipperTargetLine chipperTargetSample
	puts $file "outl=$chipperOutputLines +"
	puts $file "outs=$chipperOutputSamples +"
	puts $file "olap=$chipperOverlap +"
	puts $file "tsamp=$chipperTargetSample +"
	puts $file "tline=$chipperTargetLine"
    }

    if {$utilityType == "ridca"} {
	#  	parm inp1 type=string
	#  	parm inp2 type=string
	global ridcaFirstDate ridcaSecondDate
	puts $file "  inp1=$ridcaFirstDate +"
	puts -nonewline $file "  inp2=$ridcaSecondDate"

	#  	parm thresh type=int default=40
	global lightThreshold
	#  	parm mthresh type=int default=-999999
	global darkThreshold
	#  	parm shadow type=int default=-999999
	global shadowThreshold
	if {$lightThreshold != ""} {
	    puts -nonewline $file " +\n  thresh=$lightThreshold"
	}
	if {$darkThreshold != ""} {
	    puts -nonewline $file " +\n  mthresh=$darkThreshold"
	}
	if {$shadowThreshold != ""} {
	    puts -nonewline $file " +\n  shadow=$shadowThreshold"
	}

	#  	parm minarea type=int default=1
	global areaMin
	#  	parm maxarea type=int default=999999999
	global areaMax
	#  	parm mindiam type=int default=1
	global diameterMin
	#  	parm maxdiam type=int default=999999999
	global diameterMax
	#  	parm minadratio type=real default=0.0
	global ratioMin
	#  	parm maxadratio type=real default=999999.0
	global ratioMax
	if {$areaMin != ""} {
	    puts -nonewline $file " +\n  minarea=$areaMin"
	}
	if {$areaMax != ""} {
	    puts -nonewline $file " +\n  maxarea=$areaMax"
	}
	if {$diameterMin != ""} {
	    puts -nonewline $file " +\n  mindiam=$diameterMin"
	}
	if {$diameterMax != ""} {
	    puts -nonewline $file " +\n  maxdiam=$diameterMax"
	}
	if {$ratioMin != ""} {
	    puts -nonewline $file " +\n  minadratio=$ratioMin"
	}
	if {$ratioMax != ""} {
	    puts -nonewline $file " +\n  maxadratio=$ratioMax"
	}


	#  	parm minimg1 type=int default=0
	global minImg1
	#  	parm maximg1 type=int default=999999
	global maxImg1
	#  	parm minimg2 type=int default=0
	global minImg2
	#  	parm maximg2 type=int default=999999
	global maxImg2
	if {$minImg1 != ""} {
	    puts -nonewline $file " +\n  minimg1=$minImg1"
	}
	if {$maxImg1 != ""} {
	    puts -nonewline $file " +\n  maximg1=$maxImg1"
	}
	if {$minImg2 != ""} {
	    puts -nonewline $file " +\n  minimg2=$minImg2"
	}
	if {$maxImg2 != ""} {
	    puts -nonewline $file " +\n  maximg2=$maxImg2"
	}

	#  	parm bkminimg1 type=int default=0
	global bkminImg1
	#  	parm bkmaximg1 type=int default=999999
	global bkmaxImg1
	#  	parm bkminimg2 type=int default=0
	global bkminImg2
	#  	parm bkmaximg2 type=int default=999999
	global bkmaxImg2
	if {$bkminImg1 != ""} {
	    puts -nonewline $file " +\n  bkminimg1=$bkminImg1"
	}
	if {$bkmaxImg1 != ""} {
	    puts -nonewline $file " +\n  bkmaximg1=$bkmaxImg1"
	}
	if {$bkminImg2 != ""} {
	    puts -nonewline $file " +\n  bkminimg2=$bkminImg2"
	}
	if {$bkmaxImg2 != ""} {
	    puts -nonewline $file " +\n  bkmaximg2=$bkmaxImg2"
	}

	global ridcaSpectralFilterMirror
	if {$ridcaSpectralFilterMirror} {
	    puts -nonewline $file " +\n  mirror=\"y\""
	} else {
	    puts -nonewline $file " +\n  mirror=\"n\""
	}

	#  	parm skipto type=int default=0
	global ridcaSkip

	puts -nonewline $file " +\n  skipto=$ridcaSkip"

	#  	parm tool type=int default=0
	global ridcaTool
	puts -nonewline $file " +\n  tool=$ridcaTool"

	#  	parm toolval type=int default=1
	global ridcaComponentId
	if {$ridcaComponentId != ""} {
	    puts -nonewline $file " +\n  toolval=$ridcaComponentId"
	}

	puts $file " "
    }

    if {$utilityType == "cutSubimageVicarOut" || \
	    $utilityType == "cutSubimageGeoTIFFOut" || \
	    $utilityType == "cutToGeoTIFFReference" || \
	    $utilityType == "correlationCorrection" || \
	    $utilityType == "ntmToGeoTiff"} {
	puts $file "out=$utilityOutputImage +"
    }

    if {$utilityType == "ntmToGeoTiff"} {
	set split [split $utilityOutputImage "/"]
	set last [lindex $split [expr [llength $split] - 1]]
	set split [split $last "."]
	set last [lindex $split [expr [llength $split] - 1]]
	if {$last == "tif" || $last == "TIF"} {
	    set split [lrange $split 0 [expr [llength $split] - 2]]
	}
	puts $file "scr_key=[join $split .] +"
    }

    if {$utilityType == "cutToGeoTIFFReference" || $utilityType == "correlationCorrection"} {
	puts $file "ref=$utilityReferenceImage +"
    }

    if {$utilityType == "cutSubimageVicarOut" || $utilityType == "cutSubimageGeoTIFFOut"} {
	global UTILITY_AOI_MIN_LAT
	global UTILITY_AOI_MAX_LAT
	global UTILITY_AOI_MIN_LON
	global UTILITY_AOI_MAX_LON

	set sl $UTILITY_AOI_MAX_LAT
	set ss $UTILITY_AOI_MIN_LON
	set ll $UTILITY_AOI_MIN_LAT
	set ls $UTILITY_AOI_MAX_LON

	puts $file "aoi=(${sl},${ss},${ll},${ls}) +"
    }

    if {$utilityType == "correlationCorrection"} {
	puts $file "typref=$typref +"
	puts $file "interp=$interp +"
	puts $file "nah=$Utility_NAH nav=$Utility_NAV +"
	puts $file "gorewid=$Utility_GoreWidth +"
	puts $file "fftsize=$Utility_fftSize + "
	puts $file "magnify=$Utility_magnify +"
	puts $file "toler=$Utility_tolerance +"
	if {$polyfit == "pwl"} {
	    puts $file "polyfit=\"\" +"
	} else {
	    puts $file "polyfit=$polyfit +"
	}
    }

    if {$utilityType == "cutSubimageVicarOut" ||
    $utilityType == "cutSubimageGeoTIFFOut" ||
    $utilityType == "cutToGeoTIFFReference" ||
    $utilityType == "correlationCorrection" ||
    $utilityType == "utmToGeoTiff" ||
    $utilityType == "ntmToGeoTiff" ||
    $utilityType == "nlcLogNtm"} {

	if {$viewutilitywhenComplete} {
	    puts $file "xvd=y +"
	} else {
	    puts $file "xvd=n +"
	}
    }

    if {$utilityType != "ridca" &&
    $utilityType != "fixRpc" &&
    $utilityType != "chipper" &&
    $utilityType != "chipEntireImage" &&
    $utilityType != "chipSelected" &&
    $utilityType != "chipPercentage" &&
    $utilityType != "rpcToGeoTIFFMap"} {

	puts $file "inp=$utilityInputImage"
    }

    if {$utilityType == "rpcToGeoTIFFMap"} {
	# parm inp type=(string,99)
	global rpcToGeoTIFFMapInputImageEntry
	$rpcToGeoTIFFMapInputImageEntry updateHistory
	puts $file "  inp=[$rpcToGeoTIFFMapInputImageEntry getValue] +"

	# parm dted type=(string,99)
	global rpcToGeoTIFFMapElevationImageEntry
	$rpcToGeoTIFFMapElevationImageEntry updateHistory
	puts $file "  dted=[$rpcToGeoTIFFMapElevationImageEntry getValue] +"

	# parm out type=(string,99)
	global rpcToGeoTIFFMapOutputImageEntry
	$rpcToGeoTIFFMapOutputImageEntry updateHistory
	puts $file "  out=[$rpcToGeoTIFFMapOutputImageEntry getValue] +"

	# parm mpix real def=1.0
	global Utility_MPIX
	puts $file "  mpix=$Utility_MPIX +"

	# parm maptype type=string valid=("pc","utm","ref") default="utm"
	global Utility_maptype
	puts $file "  maptype=$Utility_maptype +"

	# parm usermapref type=(string,99) default=""
	global Utility_referenceProjection
	puts $file "  usermapref=\"$Utility_referenceProjection\" +"

	# parm interp type=string valid=("bilin","noin") default="bilin"
	global Utility_resampleMethod
	puts $file "  interp=$Utility_resampleMethod +"

	# parm rastype type=string valid=("area","point") default="point"
	global Utility_rasterType
	puts $file "  rastype=$Utility_rasterType +"

	# parm typref type=keyword count=(0:1) valid=(coverref,coverinp) default=coverinp
	global Utility_coverageSelection
	puts $file "  typref=$Utility_coverageSelection +"

	# parm xvd string valid=("n","y") default="n"
	global viewutilitywhenComplete
	if {$viewutilitywhenComplete} {
	    puts $file "  xvd=y +"
	} else {
	    puts $file "  xvd=n +"
	}

	# parm nah int def=1000
	global Utility_NAH
	puts $file "  nah=$Utility_NAH +"

	# parm nav int def=1000
	global Utility_NAV
	puts $file "  nav=$Utility_NAV"
    }

    puts $file "\nend-proc"
    close $file
}

proc setupMosaicRun {Type} {
    set type [string tolower $Type]
    set TYPE [string toupper $Type]

    global env
    if {$Type != "Cib"} {
	if {$Type == "Afidswcibdb"} {
	    global cibMosaicMode

	    switch $cibMosaicMode {
		cib1And5 {
		    set file [open "$env(AFIDS_DATA)/api/afidswcibbothdbTemplate.pdf" r]
		}
		cib1Only {
		    set file [open "$env(AFIDS_DATA)/api/afidswcib1dbTemplate.pdf" r]
		}
		cib5Only {
		    set file [open "$env(AFIDS_DATA)/api/afidswcib5dbTemplate.pdf" r]
		}
		default {
		    error "unknown cib mosaic mode $cibMosaicMode"
		}
	    }
	} else {
	    set file [open "$env(AFIDS_DATA)/api/${type}Template.pdf" r]
	}

	set data [read $file]
	close $file
    }

    global VDEV_DIR

    if {$Type == "Dted"} {
	global SESSION_ID dtedLevel
	global guiFlavor
	global DTEDQUAD
	if {$guiFlavor == "afids"} {
	    set DTEDQUAD dtedquad
	} else {
	    set DTEDQUAD dtedquad2
	}
    }

    if {$Type == "Landsat"} {
	global Landsat_sourceFile_1 Landsat_metadataFile_1
	global Landsat_sourceFile_2 Landsat_metadataFile_2
	global Landsat_sourceFile_3 Landsat_metadataFile_3
	global Landsat_sourceFile_4 Landsat_metadataFile_4

	set files [concat $Landsat_sourceFile_1 $Landsat_sourceFile_2 $Landsat_sourceFile_3 $Landsat_sourceFile_4]
	set numfile [llength $files]
	set imageFileParms {}
	set i 1
	foreach file $files {
	    if {$i == 1} {
		set imageFileParms "file${i}=$file"
	    } else {
		set imageFileParms "$imageFileParms +\n    file${i}=$file"
	    }
	    incr i
	}

	set files [concat $Landsat_metadataFile_1 $Landsat_metadataFile_2 $Landsat_metadataFile_3 $Landsat_metadataFile_4]
	set metaFileParms {}
	set i 1
	foreach file $files {
	    if {$i == 1} {
		set metaFileParms "met${i}=$file"
	    } else {
		set metaFileParms "$metaFileParms +\n    met${i}=$file"
	    }
	    incr i
	}
    }

    global ${TYPE}_OUT_IMAGE_NAME
    
    if {$Type == "Afidswcibdb"} {
	global CIB1_DISKIMG_DIR CIB5_DISKIMG_DIR
    }

    if {$Type == "Srtm" || $Type == "Afidswdb"} {
	set split [split [set ${TYPE}_OUT_IMAGE_NAME] /]
	if {[llength $split] == 1} {
	    set outName $split
	    set outDir "./"
	} else {
	    set outName [lindex $split [expr [llength $split] - 1]]
	    set dir [lrange $split 0 [expr [llength $split] - 2]]
	    set outDir [join $dir /]
	}
	global guiFlavor
	if {$guiFlavor != "afids"} {
	    global SESSION_ID
	    set outDir $SESSION_ID
	}
    }

    if {$Type == "Cib"} {
	global cibRes

	global env
	if {true || $env(OS_NAME) == "Linux"} {
	    for {set i 0} {$i < 4} {incr i} {
		global ${TYPE}_DISKIMG_DIR_${i}
		set DISKIMG_DIR_${i} [set [set TYPE]_DISKIMG_DIR_${i}]
	    }
	}
    }

    if {$Type != "Landsat"} {
	global ${TYPE}_AOI_MIN_LAT ${TYPE}_AOI_MAX_LAT ${TYPE}_AOI_MIN_LON ${TYPE}_AOI_MAX_LON LOCAL_DIR
	global ${TYPE}_DISKIMG_DIR

	global env
	if {($Type != "Cib" && $Type != "Afidswcibdb") || (false && $env(OS_NAME) != "Linux")} {
	    set DISKIMG_DIR [set [set TYPE]_DISKIMG_DIR]
	}
	set AOI_MIN_LAT [set [set TYPE]_AOI_MIN_LAT]
	set AOI_MAX_LAT [set [set TYPE]_AOI_MAX_LAT]
	set AOI_MIN_LON [set [set TYPE]_AOI_MIN_LON]
	set AOI_MAX_LON [set [set TYPE]_AOI_MAX_LON]
    }

    global VDEV_DIR
    global guiFlavor
    if {$guiFlavor == "afids"} {
	set OUT_IMAGE_NAME [set [set TYPE]_OUT_IMAGE_NAME]
    } else {
	global SESSION_ID
	set OUT_IMAGE_NAME ${SESSION_ID}/[set [set TYPE]_OUT_IMAGE_NAME]
    }

    global env
    if {$Type == "Cib" && (true || $env(OS_NAME) == "Linux")} {
	global view${type}whenComplete
	cibmos $DISKIMG_DIR_0 $DISKIMG_DIR_1 $DISKIMG_DIR_2 $DISKIMG_DIR_3 $CIB_OUT_IMAGE_NAME [set ${TYPE}_AOI_MAX_LAT] [set ${TYPE}_AOI_MAX_LON] [set ${TYPE}_AOI_MIN_LON] [set ${TYPE}_AOI_MIN_LAT] [set view${type}whenComplete]

    } else {	    
	set file [open "${type}run.pdf" w]
	
	puts [subst $data]
	puts $file [subst $data]
	close $file
    }
}

proc setupCoregistrationRun {Type xvdOnly RTYPE} {
	set preamble {procedure

body

setlib-add library=(\$R2LIB)
local afidsroot type=(string,128)
translog AFIDS_ROOT afidsroot
setlib-delete library=(\$R2LIB)
setlib-add library=(&afidsroot/vdev,\$R2LIB)
}

        set epilogue {end-proc
}

    set type [string tolower $Type]
    set TYPE [string toupper $Type]

    global SESSION_ID
    global ${Type}_sourceFile
    global ${Type}_metadataFile
    global ${Type}_outimg

    global controlPointLongLat
    set controlPointLongLat ""

    if {$Type == "Landsat"} {
	global Landsat_coregElevInputImage Landsat_coregBaseInputImage
	
	if {$Landsat_coregElevInputImage == ""} {
	    set Landsat_coregElevInputImage {""}
	}

	if {$Landsat_coregBaseInputImage == ""} {
	    set Landsat_coregBaseInputImage {""}
	}
    }

    if {$Type == "Aster"} {
	global Aster_coregElevInputImage Aster_coregBaseInputImage
	
	if {$Aster_coregElevInputImage == ""} {
	    set Aster_coregElevInputImage {""}
	}

	if {$Aster_coregBaseInputImage == ""} {
	    set Aster_coregBaseInputImage {""}
	}
    }

    if {$Type == "ALI"} {
	set split [split $ALI_sourceFile "."]
	set length [llength $split]
	if {[lindex $split [expr $length - 1]] == "M1R"} {
	    set chop [lrange $split 0 [expr $length - 2]]
	    set ALI_sourceFile_prefix [join $chop "."]
	} else {
	    set ALI_sourceFile_prefix $ALI_sourceFile
	}
    }

    if {$Type == "ALI2"} {
	set split [split $ALI2_sourceFile "."]
	set length [llength $split]
	if {[lindex $split [expr $length - 1]] == "M1R"} {
	    set chop [lrange $split 0 [expr $length - 2]]
	    set ALI2_sourceFile_prefix [join $chop "."]
	} else {
	    set ALI2_sourceFile_prefix $ALI2_sourceFile
	}
    }

    if {[string first "2" $Type] > 0} {
	global ${Type}_previousCoregImage
	global ${Type}2_outimg
    }

    if {$Type == "Aster2"} {
	global Aster_band
	global Aster_sensor

	switch $Aster_sensor {
	    VNIR - SWIR - TIR {
		set btype $Aster_sensor
	    }
	    SE-05 {
		set btype L2-5
	    }
	    SKT-08 {
		set btype L2-8
	    }
	    SRTIR-09 {
		set btype L2-9
	    }
	}
    }

    if {$Type == "Hyperion" || $Type == "ALI"} {
	global ${Type}_fftSize

	if {[set ${Type}_fftSize] == "128"} {
	    global ${Type}_tiepoints
	    set tp0 [lindex [set ${Type}_tiepoints] 0]
	    set tp1 [lindex [set ${Type}_tiepoints] 1]
	    set tp2 [lindex [set ${Type}_tiepoints] 2]
	    set tp3 [lindex [set ${Type}_tiepoints] 3]
	    set tp4 [lindex [set ${Type}_tiepoints] 4]
	    set tp5 [lindex [set ${Type}_tiepoints] 5]
	    set tp6 [lindex [set ${Type}_tiepoints] 6]
	    set tp7 [lindex [set ${Type}_tiepoints] 7]
	    set tiePoints "linesamp=(${tp0},${tp1},${tp4},${tp5}) +\n     lsatls=(${tp2},${tp3},${tp6},${tp7})"
	} else {
	    set tiePoints "linesamp=(-999.0,-999.0,-999.0,-999.0) +\n     lsatls=(-999.0,-999.0,-999.0,-999.0)"
	}
    }

#      if {$Type == "Aster"} {
#  	global ${Type}_tiepoints
#  	set tp0 [lindex [set ${Type}_tiepoints] 0]
#  	set tp1 [lindex [set ${Type}_tiepoints] 1]
#  	set tp2 [lindex [set ${Type}_tiepoints] 2]
#  	set tp3 [lindex [set ${Type}_tiepoints] 3]
#  	set tp4 [lindex [set ${Type}_tiepoints] 4]
#  	set tp5 [lindex [set ${Type}_tiepoints] 5]
#  	set tp6 [lindex [set ${Type}_tiepoints] 6]
#  	set tp7 [lindex [set ${Type}_tiepoints] 7]
#  	set tiePoints "linesamp=(${tp0},${tp1},${tp4},${tp5}) +\n     lsatls=(${tp2},${tp3},${tp6},${tp7})"
#      }

    global ${Type}_coregBaseInputImage
    if {($Type == "Ikonos" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Aster")} {
	catch {
	    # aster doesn't have rpc
	    global ${Type}_outrpctiff outrpc
	    if {[set ${Type}_outrpctiff] != ""} {
		set outrpc "outrpc=[set ${Type}_outrpctiff]"
	    } else {
		set outrpc "outrpc=\"\""
	    }

	    global optionalOutRpcformat

	    set outrpc "${outrpc} outrpctp=\"${optionalOutRpcformat}\""
	}

	if {[set ${Type}_coregBaseInputImage] == {""}} {
	    set tiePoints ""
	} else {
	    global ${Type}_tiepoints

	    set tp0 [lindex [set ${Type}_tiepoints] 0]
	    set tp1 [lindex [set ${Type}_tiepoints] 1]
	    set tp2 [lindex [set ${Type}_tiepoints] 2]
	    set tp3 [lindex [set ${Type}_tiepoints] 3]
	    set tp4 [lindex [set ${Type}_tiepoints] 4]
	    set tp5 [lindex [set ${Type}_tiepoints] 5]
	    set tp6 [lindex [set ${Type}_tiepoints] 6]
	    set tp7 [lindex [set ${Type}_tiepoints] 7]
	    set tp8 [lindex [set ${Type}_tiepoints] 8]
	    set tp9 [lindex [set ${Type}_tiepoints] 9]
	    set tp10 [lindex [set ${Type}_tiepoints] 10]
	    set tp11 [lindex [set ${Type}_tiepoints] 11]
	    set tiePoints "linesamp=(${tp0},${tp1},${tp4},${tp5},${tp8},${tp9}) +\n     lsatls=(${tp2},${tp3},${tp6},${tp7},${tp10},${tp11})"
	}
    }

    if {[string first "Ikonos" $Type] >= 0 || [string first "Quickbird" $Type] >= 0} {
	global ${Type}_rawtype
	if {[isTiff [set ${Type}_sourceFile]]} {
	    set ${Type}_rawtype tif
	} else {
	    set ${Type}_rawtype nitf
	}
    }

    if {$Type == "ALI"} {
	global ${Type}_sensor
    }

    if {$Type == "Aster" || $Type == "Landsat" || $Type == "Hyperion" || $Type == "ALI" || $Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Ikonos" || $Type == "Modis"} {
	global ${Type}_referenceProjection
	global ${Type}_resampleMethod
	global ${Type}_rasterType
    }

    if {$Type == "Hyperion2"} {
	global Hyperion_band
	global Hyperion_nband
    }

    if {$Type == "Quickbird2"} {
	global Quickbird_band
	global Quickbird_nband
    }

    if {$Type == "ALI2"} {
	global ALI_band
	global ALI_nband

	set aliStartBand $ALI_band
    }

    if {$Type == "Ikonos"} {
	global Ikonos_headerFile
    }

    global env
    if {[string first "landsat" $type ] < 0} {
	global env
	if {$Type == "Cib" && (true || $env(OS_NAME) == "Linux")} {
	    set file [open "$env(AFIDS_DATA)/api/${type}LinuxTemplate.pdf" r]
	} elseif {$Type == "SPOT" && $RTYPE == "secondary"} {
	    set file [open "$env(AFIDS_DATA)/api/${type}2Template.pdf" r]
	} else {
	    set file [open "$env(AFIDS_DATA)/api/${type}Template.pdf" r]
	}
    } else {
	set file [open "$env(AFIDS_DATA)/api/${type}CoregTemplate.pdf" r]
    }
    set data [read $file]
    close $file

#    set useAOILimits 1

    if {$Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Ikonos" || $Type == "Aster" || $Type == "Landsat" || $Type == "Modis"} {
	global ${TYPE}_AOI_MIN_LON ${TYPE}_AOI_MAX_LON
	set AOI_MIN_SAMP [set [set TYPE]_AOI_MIN_LON]
	set AOI_MAX_SAMP [set [set TYPE]_AOI_MAX_LON]
  	if {$AOI_MIN_SAMP == "" || $AOI_MAX_SAMP == ""} {
#  	    set useAOILimits 0
	    set AOI_MIN_SAMP ""
	    set AOI_MAX_SAMP ""
  	}
    }

    if {[string first "2" $Type] < 0} {
	if {$RTYPE == "master"} {
	    set refimg ""
	} else {
	    global ${Type}_coregMasterFile
	    set refimg "refimg=[set ${Type}_coregMasterFile]"
	}
    }

    if {$Type == "Quickbird" || $Type == "SPOT" || $Type == "NTM" || $Type == "Ikonos"} {
	global ${Type}_MPIX ${Type}_MPIXF
	# ignored values
	if {$RTYPE == "secondary"} {
	    set ${Type}_MPIX 0.0
	    set ${Type}_MPIXF 0.0
	} else {
	    if {[set ${Type}_MPIX] <= 1.5} {
		set ${Type}_MPIXF [expr 2.0 * [set ${Type}_MPIX]]
	    } else {
		set ${Type}_MPIXF [set ${Type}_MPIX]
	    }
	}
    }

    global ${Type}_coregBaseInputImage
    global controlPointLine controlPointSamp controlPointLat controlPointLon
    if {$Type == "NTM" && [set ${Type}_coregBaseInputImage] == "" && $controlPointLine != "" && $controlPointSamp != "" && $controlPointLon != "" && $controlPointLat != ""} {
	set ${Type}_coregBaseInputImage "\"\""

	global controlPointLongLat
	set controlPointLongLat "longlat=($controlPointLine,$controlPointSamp,$controlPointLon,$controlPointLat,-999.0,-999.0)"
    }

    global beList
    global bfes

    if {$RTYPE == "secondary" && ($Type == "NTM" || $Type == "Quickbird" || $Type == "SPOT") && [$beList getValue] != ""} {
	global beOutType
	global outputChipPrefix
	global beRefRpc
	global chipPixelSize
	global chipWindowSize

	set chipPixelSize_fixed 1.0
	set chipWindowSize_fixed 4096

	if {[string is double $chipPixelSize] || [string is integer $chipPixelSize]} {
	    set chipPixelSize_fixed $chipPixelSize
	}

	if {[string is integer $chipWindowSize]} {
	    set chipWindowSize_fixed $chipWindowSize
	}

	set bfes "belist=\"[$beList getValue]\" +"
	set bfes "${bfes}\n     beouttype=\"$beOutType\" +"
	set bfes "${bfes}\n     beoutroot=\"$outputChipPrefix\" +"
	set bfes "${bfes}\n     bepixsize=$chipPixelSize_fixed +"
	set bfes "${bfes}\n     bewinsize=$chipWindowSize_fixed +"
	set bfes "${bfes}\n     berefrpc=\"[$beRefRpc getValue]\""
    } else {
	set bfes " "
    }

    # if not adband
    if {[string first "2" $Type] < 0} {
	global ${TYPE}_AOI_MIN_LAT ${TYPE}_AOI_MAX_LAT
	set AOI_MIN_LINE [set [set TYPE]_AOI_MIN_LAT]
	set AOI_MAX_LINE [set [set TYPE]_AOI_MAX_LAT]
  	if {$AOI_MIN_LINE == "" || $AOI_MAX_LINE == ""} {
#  	    set useAOILimits 0
	    set AOI_MIN_LINE ""
	    set AOI_MAX_LINE ""
  	}

	global XVDOnlyFlag
	set XVDOnlyFlag $xvdOnly
	global ${Type}_coregBaseInputImage

	if {[catch {
	    if {[set ${Type}_coregBaseInputImage] == ""} {
		set coregBaseInputImage "\"\""
	    } else {
		set coregBaseInputImage [set ${Type}_coregBaseInputImage]
	    }
	}]} { set coregBaseInputImage "\"\"" }

	global ${Type}_coregElevInputImage
	global ${Type}_NAV ${Type}_NAH 
	global ${Type}_maptype

	# ignored values
	if {$RTYPE == "secondary"} {
	    set ${Type}_NAV 0
	    set ${Type}_NAH 0
	    set ${Type}_maptype pc
	    set ${Type}_referenceProjection "ignore"
	    set ${Type}_resampleMethod noin
	    set ${Type}_rasterType area
	}
    }

    if {$RTYPE == "secondary" && $Type == "Landsat"} {
	global useBaseForMasterRegistration
	if {$useBaseForMasterRegistration} {
	    set baseForMaster "mosmst=y"
	} else {
	    set baseForMaster "mosmst=n"
	}
    } else {
	set baseForMaster ""
    }

#    if {$useAOILimits} {
    switch $Type {
	"Quickbird" - "NTM" - "Ikonos" - "Aster" - "Landsat" - "Modis" - "SPOT" {
	    set aoiLimits "line_upper=\"$AOI_MAX_LINE\" line_lower=\"$AOI_MIN_LINE\" samp_left=\"$AOI_MIN_SAMP\" samp_right=\"$AOI_MAX_SAMP\""
	}
	"Hyperion" - "ALI" {
	    set aoiLimits "line_upper=\"$AOI_MAX_LINE\" line_lower=\"$AOI_MIN_LINE\""
	}
    }
#      } else {
#  	set aoiLimits ""
#      }

    if {$Type == "Modis2"} {
	global Modis2_sourceFile_2
	global Modis2_metadataFile_2
	global Modis2_time
	global Modis_band

	if {$Modis2_sourceFile_2 != ""} {
	    set rawimg2 "rawimg2=$Modis2_sourceFile_2"
	    set rawgeo2 "rawgeo2=$Modis2_metadataFile_2"
	} else {
	    set rawimg2 ""
	    set rawgeo2 ""
	}
    }

    if {$Type == "Modis"} {
	global Modis_time
	global Modis_RTYPE
	global Modis_sourceFile_2
	global Modis_metadataFile_2

	if {$Modis_sourceFile_2 != ""} {
	    set rawimg2 "rawimg2=$Modis_sourceFile_2"
	    set rawgeo2 "rawgeo2=$Modis_metadataFile_2"
	} else {
	    set rawimg2 ""
	    set rawgeo2 ""
	}

	if {$Modis_time == "day"} {
	    set Modis_RTYPE $RTYPE
	} else { ; # nighttime
	    if {$RTYPE == "master"} {
		set Modis_RTYPE nightmaster
	    } elseif {$RTYPE == "secondary"} {
		set Modis_RTYPE nightsecondary
	    }
	}
    }

    global ${Type}_siteref
    global ${Type}_siteout

    global VDEV_DIR

    if {[string first "landsat" $type] < 0} {
	set file [open "${type}run.pdf" w]
    } else {
	set file [open "${type}coregrun.pdf" w]
    }
    
    if {$Type == "Quickbird2"} {
	global Quickbird_band
	global Quickbird_nband

	set band $Quickbird_band
	set nband $Quickbird_nband

	set Quickbird_nband 4

        puts $preamble
        puts $file $preamble

	for {set thisBand $band} {$thisBand < $band + $nband} {incr thisBand} {
	    set Quickbird_band $thisBand

	    puts [subst $data]
	    puts $file [subst $data]
	}

        puts $epilogue
	puts $file $epilogue

	set Quickbird_band $band
	set Quickbird_nband $nband

    } elseif {$Type == "SPOT" && $RTYPE == "secondary"} {
	global SPOT_band
	global SPOT_nband

	set band [expr $SPOT_band - 1]
	set nband $SPOT_nband

        puts $preamble
        puts $file $preamble

	for {set thisBand $band} {$thisBand < $band + $nband} {incr thisBand} {
	    set outimg ${SPOT_outimg}_b[expr $thisBand + 1].img

	    if {$thisBand == 3} {
		set magfac "magfac=2.0"
	    } else {
		set magfac ""
	    }

	    set rawband $thisBand

	    puts [subst $data]
	    puts $file [subst $data]
	}

        puts $epilogue
	puts $file $epilogue

    } elseif {$Type == "Aster"} {

        puts $preamble
        puts $file $preamble

	puts [subst $data]
	puts $file [subst $data]

	global runAllAdditionalBands

	if {$runAllAdditionalBands} {
	    global env
	    set t2file [open "$env(AFIDS_DATA)/api/${type}2autoTemplate.pdf" r]
	    set data [read $t2file]
	    close $t2file

	    # ASTCALL2
	    # rawimg=$Aster_sourceFile
	    # outimg=$Aster_outimg

	    # ASTWARPAD2 PARAMETERS
	    # rawaster=$Aster2_sourceFile
	    # fnamein=$Aster2_previousCoregImage
	    # outimg=$Aster2_outimg
	    # band=$Aster_band
	    # btype=$btype

	    global Aster_sourceFile Aster_outimg
	    set Aster2_sourceFile $Aster_sourceFile
	    set Aster2_previousCoregImage $Aster_outimg

	    #VNIR 1 3N
	    set btype VNIR

	    foreach Aster_band {1 3N} {
		set Aster2_outimg ${Aster_outimg}_${btype}${Aster_band}
		puts [subst $data]
		puts $file [subst $data]
	    }

	    #SWIR 4 5 6 7 8 9
	    set btype SWIR

	    foreach Aster_band {4 5 6 7 8 9} {
		set Aster2_outimg ${Aster_outimg}_${btype}${Aster_band}
		puts [subst $data]
		puts $file [subst $data]
	    }

	    #TIR 10 11 12 13 14
	    set btype TIR

	    foreach Aster_band {10 11 12 13 14} {
		set Aster2_outimg ${Aster_outimg}_${btype}${Aster_band}
		puts [subst $data]
		puts $file [subst $data]
	    }
	}

        puts $epilogue
	puts $file $epilogue

    } else {
	puts [subst $data]
	puts $file [subst $data]
    }

    close $file
}

proc isTiff {path} {
    set fh [open $path r]
    set fourBytes [read $fh 4]
    close $fh

    set littleEndianHeader "\x49\x49\x2A\x00"
    set bigEndianHeader "\x4D\x4D\x00\x2A"

    if {$fourBytes == $littleEndianHeader || $fourBytes == $bigEndianHeader} {
	return 1
    }

    return 0
}
