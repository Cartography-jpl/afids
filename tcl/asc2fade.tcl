# Example filename 20100501-20100530_LS4-LS5_run01.csv

# New example 20100928-20101001_WV2-WV2_AF1B_v1.75.csv
# where AF1B is an AOI identifier and v1.75 identifies the software version

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

set inpcoords outfadecoords
set inpcols outfade

set Date1  "20091104"
set Time1  "07:23:43"
set Date2 "20091110"
set Time2 "07:08:02"
set run "01"

set Sensor1 "RE2"
set Sensor2 "RE4"

puts "Filename: ${Date1}-${Date2}_${Sensor1}-${Sensor2}_run${run}.csv"

set f [open $inpcoords "r"]
set data [read $f]
close $f

set coordslines [split $data "\n"]

set f [open $inpcols "r"]
set data [read $f]
close $f

set colslines [split $data "\n"]

puts "Date1,Time1,Date2,Time2,Lat,Lon,Sensor1,Sensor2,BV,BG,NDVI,B1_BV,B1_BG,B1_NDVI,B2_BV,B2_BG,B2_NDVI,B3_BV,B3_BG,B3_NDVI,B4_BV,B4_BG,B4_NDVI,B5_BV,B5_BG,B5_NDVI"

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
    set BG [lindex $cols 1]
    set NDVI [lindex $cols 2]
    set B1_BV [lindex $cols 3]
    set B2_BV [lindex $cols 4]
    set B3_BV [lindex $cols 5]
    set B4_BV [lindex $cols 6]
    set B5_BV [lindex $cols 7]

    puts "$Date1,$Time1,$Date2,$Time2,$Lat,$Lon,$Sensor1,$Sensor2,$BV,$BG,$NDVI,$B1_BV,$B1_BG,$B1_NDVI,$B2_BV,$B2_BG,$B2_NDVI,$B3_BV,$B3_BG,$B3_NDVI,$B4_BV,$B4_BG,$B4_NDVI,$B5_BV,$B5_BG,$B5_NDVI"

    incr index
}
