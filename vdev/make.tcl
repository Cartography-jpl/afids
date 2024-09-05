set V2TOP $env(V2TOP)

foreach dir [lsort [glob *]] {
    puts "\nProcessing $dir ...\n"
    if {$dir != "vextract" && $dir != "xvd"} {
	set app $dir

	set csh [open make.csh w]

	puts $csh "\#!/bin/csh"
	puts $csh "source $V2TOP/vicset1.csh"
	puts $csh "source $V2TOP/vicset2.csh"

	puts $csh "if (! -d ./bin) mkdir ./bin"
	puts $csh "chmod 755 $app/*"
	puts $csh "if (-f ./bin/$app) rm ./bin/$app"
	puts $csh "cd $app"
	puts $csh "if (-f ${app}.imake) then"
	puts $csh "  vimake $app"
	puts $csh "endif"
	puts $csh "if (-f ${app}.make) then"
	puts $csh "  rm *.o"
	puts $csh "  make -f ${app}.make"
	puts $csh "  cp $app ../bin/"
	puts $csh "endif"
	puts $csh "cd .."

	close $csh

	exec chmod u+x make.csh

	catch {exec ./make.csh} ervar

	puts $ervar
    }
}

puts "Done."
