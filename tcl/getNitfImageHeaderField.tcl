load ossimTcl.so

# imageIndex is zero-based
# fieldName is one of {IGEOLO}
proc getNitfImageHeaderField {path imageIndex fieldName } {

    set fields [ossim::getImageHeaderFields $path $imageIndex]

    set lines [split $fields "\n"]

    set fieldValue "getNitfImageHeaderField: field $fieldName not found in image header $imageIndex of $path"
    foreach line $lines {
	if {[lindex $line 0] == $fieldName} {
	    set colon [expr 2 + [string first ":" $line]]
	    set fieldValue [string range $line $colon end]
	    break
	}
    }

    return $fieldValue
}
