proc fixCbMiniProc {prefix} {

    set f [open mosaic_elv.pdf r]
    set d [read $f]
    close $f

    set fout [open mosaic_elv.pdf.tmp w]

    set lines [split $d "\n"]

    foreach line $lines {
	set line [string trim $line]
	if {[string first "img" $line] >= 0} {
	    set line "${prefix}${line}"
	}
	puts $fout $line
    }
    close $fout

    exec rm mosaic_elv.pdf
    exec mv mosaic_elv.pdf.tmp mosaic_elv.pdf
}