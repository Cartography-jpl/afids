package require Itcl

# MiscUtils CLASS DESCRIPTION

# MiscUtils is a static Itcl class collecting misc utilities that have
# no relationship with each other.

# STATIC METHODS

#  splitRpc rpc
#   Returns a list of fieldName fieldValue pairs extracted from a
#   given NITF RPC TRE.

itcl::class MiscUtils {
    public proc splitRpc {rpc} {
	set split {}
	#  FIELD1          X 00001
	lappend split "FIELD1" [string range $rpc 0 0]
	#  FIELD2          X 00007
	lappend split "FIELD2" [string range $rpc 1 7]
	#  FIELD3          X 00007
	lappend split "FIELD3" [string range $rpc 8 14]
	#  FIELD4          X 00006
	lappend split "FIELD4" [string range $rpc 15 20]
	#  FIELD5          X 00005
	lappend split "FIELD5" [string range $rpc 21 25]
	#  FIELD6          X 00008
	lappend split "FIELD6" [string range $rpc 26 33]
	#  FIELD7          X 00009
	lappend split "FIELD7" [string range $rpc 34 42]
	#  FIELD8          X 00005
	lappend split "FIELD8" [string range $rpc 43 47]
	#  FIELD9          X 00006
	lappend split "FIELD9" [string range $rpc 48 53]
	#  FIELD10         X 00005
	lappend split "FIELD10" [string range $rpc 54 58]
	#  FIELD11         X 00008
	lappend split "FIELD11" [string range $rpc 59 66]
	#  FIELD12         X 00009
	lappend split "FIELD12" [string range $rpc 67 75]
	#  FIELD13         X 00005
	lappend split "FIELD13" [string range $rpc 76 80]
	#  [LOOP][000000000000020][00001]
	#  FIELD14         X 00012
	#  [LOOP][000000000000020][00001]
	#  FIELD15         X 00012
	#  [LOOP][000000000000020][00001]
	#  FIELD16         X 00012
	#  [LOOP][000000000000020][00001]
	#  FIELD17         X 00012
	for {set i 0} {$i < 80} {incr i} {
	    lappend split "FIELD1[expr 4 + $i/20][expr $i%20 + 1]" [string range $rpc [expr 81 + $i * 12] [expr 92 + $i * 12]]
	}

	return $split
    }
}
