lappend auto_path $env(ITCL_LIBRARY)
package require Itcl
package require Tix

image create photo filledTriangle -file $env(AFIDS_DATA)/gui/filledTriangle.gif

# FileUtils CLASS DESCRIPTION

# FileUtils is a static Itcl class collecting file manipulation utilities.

# STATIC METHODS

#  fileTypes type1 type2 ... typen
#   Returns a list appropriate for the -filetypes option to tk_getOpenFile
#   and tk_getSaveFile. The supported types can be determined by calling
#   proc supportedFileTypes.

#  supportedFileTypes
#   Returns a list of type supported by proc fileTypes.

#  makeTemp prefix {suffix ""}
#   Creates a name for a temporary file, prefixed by prefix and
#   suffixed by suffix.

#  isReadableNonEmptyFile file
#   Returns 0 or 1

itcl::class FileUtils {
    proc isReadableNonEmptyFile {file} {
	if {[file isfile $file]} {
	    if {[file readable $file]} {
		if {[file size $file] > 0} {
		    return 1
		}
	    }
	}

	return 0
    }

    public proc makeTemp {prefix {suffix ""}} {
	set num [expr int(rand() * 1000000)]
	set name ${prefix}temp_${num}${suffix}
	while {[glob -nocomplain $name] != ""} {
	    set num [expr int(rand() * 1000000)]
	    set name ${prefix}temp_${num}${suffix}
	}

	return $name
    }

    public proc fileTypes {args} {
	if {! $initialized} {
	    initialize
	}

	set list {}

	foreach type $args {
	    if {[catch { set types($type) } ]} {
		error "FileUtils::fileTypes: \"$type\" is an unsupported type"
	    }
	    ListUtils::lappendList list [list [string map {\n " "} $types($type)]]
	}

	return [list $list]
    }

    public proc supportedFileTypes {} {
	if {! $initialized} {
	    initialize
	}

	lsort [array names types]
    }

    # PRIVATE

    private common types
    private common initialized 0

    private proc initialize {} {
	array set types {
	    text {
		{"Text" {.txt} }
	    }
	    filter {
		{"Filters" {.flt} }
	    }
	    pdf {
		{"TAE Scripts" {.pdf} }
	    }
	    vicar {
		{"VICAR Images"   {.img} }
		{"VICAR Images"   {.hlf} }
		{"VICAR Images"   {.IMG} }
		{"VICAR Images"   {.HLF} }
	    }
	    fits {
		{"FITS Images" {.fit} }
		{"FITS Images" {.fits} }
		{"FITS Images" {.FIT} }
		{"FITS Images" {.FITS} }
	    }
	    tiff {
		{"TIFF Images" {.tif} }
		{"TIFF Images" {.TIF} }
	    }
	    geoTiff {
		{"GeoTIFF Images" {.tif} }
		{"GeoTIFF Images" {.tiff} }
		{"GeoTIFF Images" {.TIF} }
		{"GeoTIFF Images" {.TIFF} }
	    }
	    nitf {
		{"NITF Image" {.ntf}  }
		{"NITF Image" {.nitf} }
		{"NITF Image" {.NTF}  }
		{"NITF Image" {.NITF} }
	    }
	    all {
		{"All files" * }
	    }
	}
	set initialized 1
    }
}


# ListUtils CLASS DESCRIPTION

# ListUtils is a static Itcl class collecting list utilities.

# STATIC METHODS

#  first list
#   Returns first item in list
#  last list
#   Returns last item in list
#  removeLast list
#   Returns list with last item removed
#  appendList list items
#   Like append with {expand} expansion
#  lappendList list items
#   Like lappend with {expand} expansion

itcl::class ListUtils {
    public proc first {list} {
	lindex $list 0
    }

    public proc last {list} {
	lindex $list [expr [llength $list] - 1]
    }

    public proc removeLast {list} {
	set length [llength $list]
	set index [expr $length - 1]
	lreplace $list $index $index
    }

    public proc appendList {list items} {
	foreach item $items {
	    lappend list $item
	}
	return $list
    }

    public proc lappendList {list items} {
	foreach item $items {
	    eval "uplevel \{ lappend $list $item \}"
	}
	return [eval "uplevel \{ set $list \}"]
    }

    public proc import {} {
	uplevel #0 {
	    namespace eval ListUtils { namespace export first last removeLast}
	    namespace import ListUtils::first ListUtils::last ListUtils::removeLast
	}
    }
}



# Debug CLASS DESCRIPTION

# Debug is a static Itcl class collecting debugging utilities.

# STATIC METHODS

#  dumpStack
#   Prints stadk dump to stdout

itcl::class Debug {
    public proc dumpStack {} {
	for {set x [expr [info level] - 1]} {$x > 0} {incr x -1} {
	    puts "$x: [info level $x]"
	}
    }
}


# ProgressDialog CLASS DESCRIPTION

# ProgressDialog is an Itcl class that creates progress dialogs.

# CONSTRUCTOR OPTIONS

#  -title title
#   Initializes the dialog title.
#  -description description
#   Sets the dialog's description label.
#  -status status
#   Initializes the dialog's status label.
#  -meter value
#   Initializes the dialog's meter widget. Value must be in the range 0.0 to 1.0.
#  -meterLabel label
#   Initializes the meter's text label.
#  -terminate command
#   Places a command button labeled "Terminate" in the dialog. If it
#   is pressed, the command is called with this object as its
#   argument.
#  -userData data
#   Initializes public instance variable userData.

# INSTANCE METHODS

#  setStatus status
#   Updates the dialog's status field. If -status was not passed to
#   the constructor, setStatus messages will be ignored.

#  setMeter value
#   Updates the dialog's meter to the specified value, which must be
#   in the range 0.0 to 1.0. If -meter was not passed to the
#   constructor, setMeter messages will be ignored.

itcl::class ProgressDialog {
    private variable dialog
    private variable statusLabel ""
    private variable meter ""
    private variable commandButton ""
    private variable terminateCommand ""
    public variable userData ""

    public destructor {
	destroy $dialog
    }

    public constructor {args} {
	# check for -title value
	set optionIndex [lsearch $args "-title"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set title [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	} else {
	    set title ""
	}

	set dialogName [split $this ":"]
	set dialogName [lindex $dialogName 2]

	set dialog .progressDialog_$dialogName
	toplevel $dialog

	wm title $dialog $title
	wm protocol $dialog WM_DELETE_WINDOW "$this wmDeleteWindow"
	wm geometry $dialog +200+200

	# check for -description value
	set optionIndex [lsearch $args "-description"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set description [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]

	    set l $dialog.description
	    label $l -text $description
	    pack $l -side top
	}

	# check for -status value
	set optionIndex [lsearch $args "-status"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set status [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]

	    set sf $dialog.statusFrame
	    frame $sf
	    pack $sf -side top

	    set l $sf.label
	    label $l -text "Status:"
	    pack $l -side left

	    set statusLabel $sf.statusLabel
	    label $statusLabel -text $status -bg green
	    pack $statusLabel -side left
	}

	# check for -meter value
	set optionIndex [lsearch $args "-meter"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set meterValue [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]

	    # check for -meterLabel label
	    set optionIndex [lsearch $args "-meterLabel"]
	    if {$optionIndex >= 0} {
		set labelIndex [expr $optionIndex + 1]
		set meterLabel [lindex $args $labelIndex]
		set args [lreplace $args $optionIndex $labelIndex]
	    } else {
		set meterLabel " "
	    }

	    set meter $dialog.meter
	    tixMeter $dialog.meter -text $meterLabel -value $meterValue
	    pack $dialog.meter -side top
	}

	# check for -terminate value
	set optionIndex [lsearch $args "-terminate"]
	if {$optionIndex >= 0} {
	    set commandIndex [expr $optionIndex + 1]
	    set terminateCommand [lindex $args $commandIndex]
	    set args [lreplace $args $optionIndex $commandIndex]

	    set commandButton $dialog.commandButton
	    button $commandButton -text "Terminate" -command "$terminateCommand $this"
	    pack $commandButton -side top
	}

	# check for -userData value
	set optionIndex [lsearch $args "-userData"]
	if {$optionIndex >= 0} {
	    set userDataIndex [expr $optionIndex + 1]
	    set userData [lindex $args $userDataIndex]
	    set args [lreplace $args $optionIndex $userDataIndex]
	}

	if {$args != {}} {
	    error "ProgressDialog::constructor: unknown options \"$args\""
	}
    }

    public method wmDeleteWindow {} {
	if {$terminateCommand == ""} {
	    itcl::delete object $this
	} else {
	    $terminateCommand $this
	}
    }

    public method setStatus {status} {
	if {$statusLabel == ""} {
	    return
	}

	$statusLabel configure -text $status
    }

    public method setMeter {value {label ""}} {
	if {$meter == ""} {
	    return
	}

	$meter configure -value $value

	if {$label != ""} {
	    $meter configure -text $label
	}
    }
}



# ValueHistory CLASS DESCRIPTION

# ValueHistory is an Itcl class that maintains a database of
# application name/valueList pairs. The values in valueList are stored
# in most-recently-set-first order. The length of the valueList is
# limited to a historyLength specified for each name.

# CONSTRUCTOR OPTIONS
#  -persist file
#  Keeps database in file, which is updated whenever the database
#  changes. Method register below ignored its initValueList arg if the
#  name was read from file.

# INSTANCE METHODS

# register name historyLength [init]
#  Registers a name, initializing to init, limiting the value list to
#  historyLength.

# unregister name
#  Removes name from database.

# getNames
#  Returns registered names.

# setValue name value
#  If name hasn't been registered, a warning will be issued, and the
#  setValue will be otherwise ignored. Value is pushed on front of
#  value list. List is truncated to history length limit.

# getValue name
#  Returns first element in name's value list.  If name hasn't been
#  registered, an error will be raised.

# setHistory name valueList
#  Sets valueList of name. List is truncated to history length limit.
#  If name hasn't been registered, a warning will be issued, and the
#  setValue will be otherwise ignored.

# getHistory name
#  Returns name's value list.  If name hasn't been registered, an
#  error will be raised.

itcl::class ValueHistory {
    # array mapping name to history length
    private variable historyLengths
    # array mapping name to value lists
    private variable valueLists
    private variable file ""
    # array mapping name to value lists
    private variable lastHistory

    public constructor {args} {
	# check for -persist file
	set optionIndex [lsearch $args "-persist"]
	if {$optionIndex >= 0} {
	    set fileIndex [expr $optionIndex + 1]
	    set file [lindex $args $fileIndex]
	    set args [lreplace $args $optionIndex $fileIndex]

	    if {[glob -nocomplain $file] != ""} {
		set fh [open $file "r"]
		set data [read $fh]
		close $fh
		array set lastHistory $data
	    }
	}

	if {$args != {}} {
	    error "ValueHistory::constructor: unknown options \"$args\""
	}
    }

    public method register {name historyLength {initValueList {}}} {
	set historyLengths($name) $historyLength

	if {[array get lastHistory $name] != ""} {
	    set valueLists($name) $lastHistory($name)
	} else {
	    set valueLists($name) $initValueList
	}

	updatePersistantDb
    }

    private method updatePersistantDb {} {
	if {$file != ""} {
	    set fh [open $file "w"]
	    puts $fh [array get valueLists]
	    close $fh
	}
    }

    public method getNames {} {
	array names valueLists
    }

    public method setValue {name value} {
	set names [array names valueLists]
	if {[lsearch $names $name] < 0} {
	    puts "\"$name\" not in ValueHistory; setValue ignored"
	} else {
	    set valueLists($name) [moveToFrontOfQueue $valueLists($name) $value $historyLengths($name)]
	}

	updatePersistantDb
    }

    public method getValue {name} {
	lindex $valueLists($name) 0
    }

    public method setHistory {name valueList} {
	set names [array names valueLists]
	if {[lsearch $names $name] < 0} {
	    puts "\"$name\" not in ValueHistory; setValue ignored"
	} else {
	    set valueLists($name) $valueList
	}

	updatePersistantDb
    }

    public method getHistory {name} {
	set valueLists($name)
    }

    private proc moveToFrontOfQueue {list item maxLength} {
	set index [lsearch $list $item]
	if {$index >= 0} {
	    set list [lreplace $list $index $index]
	}

	set list [concat $item $list]

	return [lrange $list 0 [expr $maxLength - 1]]
    }
}



# Entry CLASS DESCRIPTION

# Entry is an Itcl class that provides a composite widget including a
# Tk entry, and optionally, a label, a history button, and a file
# browse button.

# CONSTRUCTOR OPTIONS

# Any constructor options not listed below are forwarded to the Tk
# entry function.

# -init initialEntryValue

# -state normal | disabled

# -label entryLabelString

# -history historyDb historyName
#  historyDb must be an instance of ValueHistory.  historyName must be
#  either a name registered with ValueHistory::register, or the string
#  "gridPlaceHolder", meaning that no history button will be created,
#  but any following widgets will be gridded with an empty grid cell
#  created in place of the history button. The historyDb will be
#  ignored for gridPlaceHolders.

# -browse args

#  Provides a file browse button. args must begin with either Open,
#  Save, or SelectDir. Remaining args are forwarded to tk_getOpenFile
#  or tk_getSaveFile.

# INSTANCE METHODS

# grid args
#  ars are forwarded to ::grid for each component widget, except
#  -column, which is fowarded only for the leftmost component
#  widget. The column value is incremented for each successive
#  component widget.

# updateHistory
#  Add current value to history. Must be used with -history option.

# -updateHistoryOnReturn
#  Call method updateHistory whenever <Return> is pressed in the entry

# getValue
# setValue value
# enable
# disable

itcl::class Entry {
    private common widgetNum 0
    private variable entryPath ""
    private variable textVariable ""
    private variable labelPath ""
    private variable historyButtonPath ""
    private variable browseButtonPath ""
    private variable historyDb ""
    private variable historyName ""
    private variable updateHistoryOnReturn 0

    public constructor {parent args} {
	set entryPath ${parent}.entry_${widgetNum}
	incr widgetNum
	set textVariable entryTextVariable_$entryPath
	set initValueSet 0

	# check for -init value
	set optionIndex [lsearch $args "-init"]
	if {$optionIndex >= 0} {
	    set initValueSet 1
	    set valueIndex [expr $optionIndex + 1]
	    setValue [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	}

	# check for -updateHistoryOnReturn
	set optionIndex [lsearch $args "-updateHistoryOnReturn"]
	if {$optionIndex >= 0} {
	    set updateHistoryOnReturn 1
	    set args [lreplace $args $optionIndex $optionIndex]
	}

	# check for -state value
	set state normal
	set optionIndex [lsearch $args "-state"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set state [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	}

	# check for -label value
	set optionIndex [lsearch $args "-label"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set labelText [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	    set labelPath ${parent}.entry_${widgetNum}_label
	    label $labelPath -text $labelText -state $state -anchor e
	}

	# check for -history db name
	set optionIndex [lsearch $args "-history"]
	if {$optionIndex >= 0} {
	    set dbIndex [expr $optionIndex + 1]
	    set historyDb [lindex $args $dbIndex]
	    set nameIndex [expr $optionIndex + 2]
	    set historyName [lindex $args $nameIndex]
	    set args [lreplace $args $optionIndex $nameIndex]

	    if {$historyName != "gridPlaceHolder"} {
		set historyButtonPath ${parent}.entry_${widgetNum}_historyButton

		menubutton $historyButtonPath -image filledTriangle -padx 0 -direction left -menu $historyButtonPath.m -relief raised
		# .tixBalloon bind $historyButtonPath -msg "Click for previous values"

		loadHistoryMenu

		# if no -init given, use history, if any
		if {$initValueSet == 0} {
		    setValue [$historyDb getValue $historyName]
		}
	    }
	}

	# check for -initialDir value
	set initialDir [pwd]
	set optionIndex [lsearch $args "-initialDir"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set initialDir [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	    setValue $initialDir
	}

	# check for -browse value
	set optionIndex [lsearch $args "-browse"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set browse [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]

	    set browseType [lindex $browse 0]
	    set browseArgs [lrange $browse 1 end]

	    set browseButtonPath ${parent}.entry_${widgetNum}_browseButton
	    switch $browseType {
		Open - Save {
		    set browseCommand "$this setWhenNonEmptyValue $textVariable \[[concat tk_get${browseType}File -parent . $browseArgs -initialdir $initialDir]\]"
		}
		SelectDir {
		    # the dirSelectDialog needs to be created with a null command, then configured, because it erroneously executes the command on creation
		    set browseCommand "\
			    catch {destroy .dirSelectDialog} ; \
                            set dir \[pwd\] ; \
                            cd $initialDir ; \
			    tixDirSelectDialog .dirSelectDialog ; \
			    .dirSelectDialog configure -command \"set $textVariable\" ; \
			    .dirSelectDialog popup ; \
                            cd \$dir"
		}
		default {
		    error "bad browsing value \"$browse\": must be a list starting with Open, Save, or SelectDir"
		}
	    }

	    button $browseButtonPath -text "Browse ..." -state $state -command $browseCommand

	}

	eval [concat entry $entryPath -state $state -textvariable $textVariable $args]

	if {$updateHistoryOnReturn} {
	    bind $entryPath <Return> "$this updateHistory"
	}
    }

    public method setWhenNonEmptyValue {var val} {
	if {$val != ""} {
	    global $var
	    set $var $val
	}
    }

    public method loadHistoryMenu {} {
	catch {destroy $historyButtonPath.m}
	menu $historyButtonPath.m -tearoff 0 
	foreach choice [$historyDb getHistory $historyName] {
	    $historyButtonPath.m add command -label $choice -command "$this setValue $choice"
	}
    }

    public method updateHistory {} {
	if {$historyName == "" || $historyName == "gridPlaceHolder"} {
	    error "Entry::updateHistory: entry has no history to update"
	} else {
	    $historyDb setValue $historyName [getValue]
	    loadHistoryMenu
	}
    }

    public method grid {args} {
	set column 0

	# check for -column value
	set optionIndex [lsearch $args "-column"]
	if {$optionIndex >= 0} {
	    set valueIndex [expr $optionIndex + 1]
	    set column [lindex $args $valueIndex]
	    set args [lreplace $args $optionIndex $valueIndex]
	}

	if {$labelPath != ""} {
	    eval [concat ::grid $labelPath -sticky e -column $column $args]
	    incr column
	}

 	eval [concat ::grid $entryPath -sticky news -column $column $args]

	if {$historyName != ""} {
	    incr column

	    if {$historyName != "gridPlaceHolder"} {
		eval [concat ::grid $historyButtonPath -sticky news -column $column $args]
	    }
	}

	if {$browseButtonPath != ""} {
	    incr column
	    eval [concat ::grid $browseButtonPath -sticky news -column $column $args]
	}
    }

    public method getValue {} {
	global $textVariable
 	set $textVariable
    }

    public method setValue {value} {
	global $textVariable
 	set $textVariable $value
    }

    public method enable {} {
 	$entryPath configure -state normal
	if {$labelPath != ""} {
	    $labelPath configure -state normal
	}
	if {$historyButtonPath != ""} {
	    $historyButtonPath configure -state normal
	}
	if {$browseButtonPath != ""} {
	    $browseButtonPath configure -state normal
	}
    }

    public method disable {} {
 	$entryPath configure -state disabled
	if {$labelPath != ""} {
	    $labelPath configure -state disabled
	}
	if {$historyButtonPath != ""} {
	    $historyButtonPath configure -state disabled
	}
	if {$browseButtonPath != ""} {
	    $browseButtonPath configure -state disabled
	}
    }
}
