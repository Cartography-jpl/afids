#load ossimTcl.so

set datepre  "20091104072343"
set datepost "20091110070802"
set inp afg2.asc

set shp [shp::create $inp SHPT_POINT]
set dbf [dbf::create $inp]

dbf::addField $dbf predate 14
dbf::addField $dbf postdate 14

set f [open $inp "r"]
set data [read $f]
close $f

set lines [split $data "\n"]

set index 0
foreach line $lines {
    if {$line == ""} {
	continue
    }

    set lon [lindex $line 1]
    set lat [lindex $line 2]

    shp::writePoint $shp $lon $lat
    dbf::writeString $dbf $index 0 $datepre
    dbf::writeString $dbf $index 1 $datepost

    incr index
}

shp::close $shp
dbf::close $dbf


set shp [shp::open $inp]
set dbf [dbf::open $inp]

set info [shp::getInfo $shp]
set cnt [lindex $info 0]

set fields [dbf::getFieldCount $dbf]
set recs [dbf::getRecordCount $dbf]

puts "$cnt shp records"
puts "$recs dbf records"
puts "$fields fields:"
for {set i 0} {$i < $fields} {incr i} {
    puts "field $i: [dbf::getFieldInfo $dbf $i]"
}

for {set i 0} {$i < 5} {incr i} {
    puts "$i: [shp::read $shp $i]"
    puts "[dbf::readString $dbf $i 0] to [dbf::readString $dbf $i 1]"
}

