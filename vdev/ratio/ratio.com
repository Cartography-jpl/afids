$!****************************************************************************
$!
$! Build proc for MIPL module ratio
$! VPACK Version 1.9, Wednesday, August 17, 2016, 12:31:19
$!
$! Execute by entering:		$ @ratio
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module ratio ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ratio.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ratio.imake") .nes. ""
$   then
$      vimake ratio
$      purge ratio.bld
$   else
$      if F$SEARCH("ratio.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ratio
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ratio.bld "STD"
$   else
$      @ratio.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ratio.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ratio.com -mixed -
	-i ratio.imake -
	-p ratio.pdf -
	-t tstratio.pdf tstratio.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ratio.imake
#define  PROCEDURE ratio 
#define R2LIB 

$ Return
$!#############################################################################
$PDF_File:
$ create ratio.pdf
PROCEDURE help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING DEFAULT=""
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM CENTER TYPE=KEYWORD VALID=(CENTER,NOCENTER) DEFAULT=CENTER
PARM MODE TYPE=KEYWORD VALID=(RATIO,DIFFEREN) DEFAULT=RATIO
PARM MODE2 TYPE=KEYWORD VALID=(NOLOG,LOG) DEFAULT=NOLOG
PARM AREA TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SAMPLE TYPE=REAL DEFAULT=5.0
PARM LINC TYPE=INTEGER DEFAULT=20
PARM INCLUDE TYPE=REAL COUNT=2 DEFAULT=(0.0,5.0)
PARM THRESHOL TYPE=REAL DEFAULT=0.0
PARM MODE3 TYPE=KEYWORD VALID=(FILTER,NOFILTER) DEFAULT=FILTER
PARM PERCENT TYPE=REAL DEFAULT=2.0
PARM MODE4 TYPE=KEYWORD VALID=(DISPLAY,NODISPLA) DEFAULT=DISPLAY
PARM ATM1 TYPE=REAL DEFAULT=0.0
PARM ATM2 TYPE=REAL DEFAULT=0.0
LOCAL LFUNC TYPE=STRING 
LOCAL SIZF TYPE=STRING
LOCAL AREF TYPE=STRING
LOCAL FLD1 TYPE=STRING
LOCAL FLD2 TYPE=STRING
BODY
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&&ATM2"
RATIO0 &INP &OUT &SIZF &AREF &FLD1 &FLD2 CENTER=&CENTER MODE=&MODE +
 MODE2=&MODE2 MODE3=&MODE3 MODE4=&MODE4 FUNC=LFUNC
IF (OUT<>"") F2 &INP &OUT &SIZF FUNC=&LFUNC
END-PROC
.TITLE
Vicar Procedure ratio
.HELP
PURPOSE:
ratio operates on two input pictures to generate a third (comparison) picture.
There are four modes of operation:

	1. RATIO            OUT = GAIN*(IN1/IN2)+OFFSET
	2. LOG RATIO        OUT = GAIN*LN(IN1/IN2)+OFFSET
	3. DIFFERENCE       OUT = GAIN*(IN1-IN2)+OFFSET
	4. LOG DIFFERENCE   OUT = GAIN*LN(IN1-IN2+256)+OFFSET

The GAIN and OFFSET values are calculated by RATIO so that either (centering
option, default) the mean DN of the output picture will be 128, and the 
saturation on each tail will be at most at the user-specified level, or 
(nocentering option) the saturation on each tail will be at the user-specified
level. The program F2 is used by RATIO to form the output picture.

.PAGE
EXECUTION:

Examples

Basic Form

ratio INP=(PIC1,PIC2) OUT=RATIO

This is the simplest call to ratio.  Output picture ratio will be made using
the ratio of the two pictures, as in mode 1 above.  Ratioing, then, is the
default mode.  To use differencing, instead, specify 'DIFFEREN.  To get a
natural log output of either the ratio or the difference, use 'LOG or
MODE2=LOG.  (Note: It is possible to not give an output data set name.  In
this case, the program will print the gain and offset, but won't write an
output data set.)

.PAGE
Sampling Parameters

ratio INP=(PIC1,PIC2) OUT=RATIO AREA=(1,1,1024,1024) LINC=5

In this example, only the upper-left corner will be used for sampling, and
further, only every fifth line within this area.

ratio INP=(PIC1,PIC2) OUT=RATIO SAMPLE=20.0

Here, the SAMPLE parameter is used to specify that 20% of the lines in the
image are to be used for sampling.  The default value for SAMPLE is 5.0, so
normally, sampling is done using only 5% of the image lines.

.PAGE
Offset and Gain Parameters

ratio INP=(PIC1,PIC2) OUT=DIFFPIC 'DIFF INCLUDE=(-200.0,200.0)

The INCLUDE option is used here to specify that only difference values between
-200.0 and 200.0 are to be used for statistics.  The default for this range
when calculating differences is [-249.5,250.5].  This option can also be
used to exclude certain ratio values; the default range for ratios is [0.,5.].
If the LOG mode is used, INCLUDE applies to the value before the logarithm
is calculated.

ratio INP=(PIC1,PIC2) OUT=DIFFPIC 'DIFF THRESHOL=5.0

Here, the THRESHOLd option is used to toss out any difference values which
occur less than 5.0 percent as much as the most-frequent value; these are then
excluded from the calculations.  This option can be used for ratios, as well.
The default value is 0.0, that is, to never exclude values only on the basis
of frequency.  

ratio INP=(PIC1,PIC2) OUT=RATIO 'NOFILTER PERCENT=2.0

Normally, a five-element box filter is used to smooth the histogram of ratio
and difference values.  'NOFILTER prevents filtering of the histogram.
    The PERCENT keyword specifies the desired saturation level (in percent).
Since the mean is pegged at DN=128, and the stretch is linear, only one end
of the histogram will be saturated at the specified percentage.  The saturation
at the other end will be less.  The default is 2.0 percent, as used in the
example.

.PAGE
Miscellaneous Parameters

ratio INP=(PIC1,PIC2) OUT=LOGRAT 'LOG 'NODISPLA ATM1=-0.5 ATM2=-0.5

This last example illustrates the remaining commands.  The output picture
will be calculated from the logs of the ratios using the whole image (but
only five percent of the lines) as the sampling area.  Since we are calculating
a ratio, values outside of the range [0.0,5.0] will be excluded, but no
values will be excluded solely on the basis of frequency.  The box filtering
will be done as normal to smooth the histogram, and the program will saturate
the output image to two percent.
    Normally, the ratio (or difference) histogram is printed. 'NODISPLA
prevents this.  Finally, ATM1 and ATM2 are real numbers which will be added to
IN1 and IN2, respectively, before any ratios or differences are calculated. 
These values are generally used as atmospheric correction terms and are usually
negative.  The default is ATM1 = ATM2 = 0.0. 

.PAGE
OPERATION:
The region between the two INCLUDE values is divided into 500 equal parts.
These become the permissible histogram values.  The input pictures are sampled
according to the AREA, SAMPLE, and LINC parameters to form the histogram.
Values beyond the INCLUDE range are ignored.
    The histogram is filtered, then levels populated below the THRESHOLD are
set to zero.  The mean of this histogram is calculated .  Gain and offset 
values are chosen so that the mean is transformed to 128 and the higher of
the dark and bright saturation levels is equal to the requested level of
saturation.
    The histogram is displayed, and the gain and offset passed to the program 
F2, which generates the output picture.

WRITTEN BY:  A.R. Gillespie, 4 January 1974
COGNIZANT PROGRAMMER:  Ron Alley
REVISION:  4		4 June 1986
Made Portable for UNIX      CRI      10-JUL-95

.LEVEL1
.VARIABLE INP
Input files
.VARIABLE OUT
Output data set, if any
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE CENTER
Force output mean=128?
(CENTER,NOCENTER) 
.VARIABLE MODE
Calculation type 
(RATIO,DIFFEREN)
.VARIABLE MODE2
Log mode selection 
(NOLOG,LOG)
.VARIABLE AREA
Sampling area
.VARIABLE SAMPLE
Sampling percentage
.VARIABLE LINC
Line-increment
.VARIABLE INCLUDE
Range of values for 
statistical inclusion
.VARIABLE THRESHOLD
Value-frequency exclusion 
threshold
.VARIABLE MODE3
Filtering selection 
(FILTER,NOFILTER)
.VARIABLE PERCENT
Desired saturation level
.VARIABLE MODE4
Display control 
(DISPLAY,NODISPLA)
.VARIABLE ATM1
IN1 correction
.VARIABLE ATM2
IN2 correction
.LEVEL2
.VARIABLE INP
INP specifies the input files, of which there must be two.
.VARIABLE OUT
OUT specifies the output data set, if any.  If none is specified, the gain and
offset and computed and printed, but F2 is not called and no output data set
is written.
.VARIABLE CENTER
Specifying NOCENTER forces the saturation level of the output image to be at
the requested level on both ends. Specifying CENTER (the default) forces the
output mean to be at 128 DN, and the saturation level to be at the requested
level at one end, and less at the other.
.VARIABLE MODE
MODE may be used to specify whether calculations are to be based on differences
or ratios between the two input images.  If the user asserts MODE=DIFF or 'DIFF
the differences mode will be invoked.  Otherwise the program will use ratioing.
.VARIABLE MODE2
MODE2=LOG and 'LOG both specify that the natural log of the difference or ratio
should be calculated.
.VARIABLE AREA
AREA causes the program to sample only within the region specified.  The
default is to use the whole image for sampling.
.VARIABLE SAMPLE
SAMPLE indicates the percentage of lines within the sampling area which will be
sampled.  All samples on a chosen line are sampled.  The default is 5 percent.
.VARIABLE LINC
LINC specifies the line increment to use in sampling lines.
.VARIABLE INCLUDE
The INCLUDE option may be used to include only a certain range of values for
statistical calculations.  The default range for ratios is [0.0,5.0], and for
differences, [-249.5,250.5].  If the LOG option is used, INCLUDE applies
to the value before the logarithm is calculated.
.VARIABLE THRESHOL
Ratio (and difference) values occuring less than THRESHOLd percent as much
as the most frequent value will be excluded from the calculations.  The
default is THRESHOLd = 0.0, that is, to not exclude any values based only on
frequency.
.VARIABLE MODE3
MODE3 may be used to disable filtering.  Typing 'NOFILTER or MODE3=NOFILTER
will prevent the program from performing its normal 5-element, smoothing
box-filter on the histogram.
.VARIABLE PERCENT
PERCENT is a real number specifying the desired saturation level (in percent).
Since the mean is pegged at DN=128, and the stretch is linear, only one end
of the histogram will be saturated at this percentage.  The saturation at the
other end will be less.  The default is saturation of 2.0 percent.
.VARIABLE MODE4
MODE4 may be used to disable the normal histogram output.  Set it equal to 
NODISPLA or usr 'NODISPLA.
.VARIABLE ATM1
ATM1 specifies a real number to be added to IN1 before computing ratios or
differences.  This value is normally used as an atmospheric correction term,
and is usually negative.  The default is 0.0.
.VARIABLE ATM2
ATM2 specifies a real number to be added to IN1 before computing ratios or
differences.  This value is normally used as an atmospheric correction term,
and is usually negative.  The default is 0.0.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstratio.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! RATIO TEST SCRIPT
! SET TERMINAL WIDTH=132 BEFORE RUNNING THIS SCRIPT
!
! CREATE 4 TEST IMAGES
gen 1 20 20
gen 2 20 20 IVAL=1
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                     18,19,20,21,22,23,24,25)
size tmp1 3 zoom=4 'noin
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31,+
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45,+
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59,+
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73,+
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87,+
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
size tmp2 4 zoom=2 'noin
!
! RATIO OF 1 AND 2 WITH HISTOGRAM DISPLAY
ratio (1,2) C 
list C
!
! LOG RATIO OF 1 AND 2 WITH HISTOGRAM
ratio (1,2) C 'LOG
list C
!
! DIFFERENCE OF 2 AND 3 WITH HISTOGRAM
ratio (2,3) C 'DIFF
list C
!
! LOG DIFFERENCE OF 1 AND 4 WITH HISTOGRAM
ratio (1,4) C 'LOG 'DIFF
list C
!
! COMPUTATION ONLY (NO OUTPUT SET OR HISTOGRAM)
ratio (3,4) 'NODISP
! 
! COMPUTATION ONLY AND NO FILTERING
ratio (3,4) 'NODISP 'NOFILTER
!
! SAMPLING AREA SPECIFIED, ALONG WITH ATM CORRECTIONS
ratio (3,4) C AREA=(1,1,4,4) SAMPLE=100.0 ATM1=0.5 ATM2=0.5 'NODISP
label-list C
!
! ABOVE, WITH ALL LINES SAMPLES
ratio (3,4) C LINC=1 ATM1=0.5 ATM2=0.5 'NODISP 
label-list C
!
! RATIO BETWEEN 1 AND 2 WITH BOTTOM 10 PERCENT EXCLUDED
ratio (1,2) C THRESH=10.0
list C
!
! AS ABOVE, BUT INCLUDING ONLY THOSE RATIO'S ABOVE .90
ratio (1,2) C INCLUDE=(.90,5.0)
list C
!
! AS ABOVE, BUT WITH 10 PERCENT SATURATION
ratio (1,2) C INCLUDE=(.90,5.0) PERCENT=10.0
list C
! 
! END-SCRIPT
end-proc
$!-----------------------------------------------------------------------------
$ create tstratio.log
tstratio
gen one 20 20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen two 20 20 IVAL=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                     18,19,20,21,22,23,24,25)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp1 three zoom=4 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    5,    5)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      4*NL,      4*NS
 SIZE task completed
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31, +
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45, +
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59, +
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73, +
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87, +
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp2 four zoom=2 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
write "RATIO OF 1 AND 2 WITH HISTOGRAM DISPLAY"
RATIO OF 1 AND 2 WITH HISTOGRAM DISPLAY
ratio (one,two) C
let $echo="no"
RATIO0 (one,two) C     CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=+
FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

     IF (OUT<>"") F2 (one,two) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (one,two) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
LOG RATIO OF 1 AND 2 WITH HISTOGRAM
RATIO0 (one,two) C     CENTER=CENTER MODE=RATIO   MODE2=LOG MODE3=FI+
LTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

L O G   R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
      -0.705    0DN  ***************     +         +         +         +         +         +         +         +         +
      -0.698         ***************     +         +         +         +         +         +         +         +         +
      -0.692         ***************     +         +         +         +         +         +         +         +         +
      -0.686         ***************     +         +         +         +         +         +         +         +         +
      -0.679         ***************     +         +         +         +         +         +         +         +         +

      -0.415         ***************     +         +         +         +         +         +         +         +         +
      -0.409         ***************     +         +         +         +         +         +         +         +         +
      -0.402         ***************     +         +         +         +         +         +         +         +         +
      -0.396         ***************     +         +         +         +         +         +         +         +         +
      -0.389         ***************     +         +         +         +         +         +         +         +         +

      -0.299         ***************     +         +         +         +         +         +         +         +         +
      -0.293         ***************     +         +         +         +         +         +         +         +         +
      -0.286         ***************     +         +         +         +         +         +         +         +         +
      -0.280         ***************     +         +         +         +         +         +         +         +         +
      -0.274         ***************     +         +         +         +         +         +         +         +         +

      -0.235         ***************     +         +         +         +         +         +         +         +         +
      -0.229         ***************     +         +         +         +         +         +         +         +         +
      -0.222         ***************     +         +         +         +         +         +         +         +         +
      -0.216         ***************     +         +         +         +         +         +         +         +         +
      -0.209         ***************     +         +         +         +         +         +         +         +         +

      -0.196         ***************     +         +         +         +         +         +         +         +         +
      -0.190         ***************     +         +         +         +         +         +         +         +         +
      -0.183         ***************     +         +         +         +         +         +         +         +         +
      -0.177         ***************     +         +         +         +         +         +         +         +         +
      -0.171         ***************     +         +         +         +         +         +         +         +         +
      -0.164         ***************     +         +         +         +         +         +         +         +         +
      -0.158   MEAN  ***************     +         +         +         +         +         +         +         +         +
      -0.151         ***************     +         +         +         +         +         +         +         +         +
      -0.145         ***************************** +         +         +         +         +         +         +         +
      -0.138         ***************************** +         +         +         +         +         +         +         +
      -0.132         ***************************** +         +         +         +         +         +         +         +
      -0.126         ***************************** +         +         +         +         +         +         +         +
      -0.119         *******************************************       +         +         +         +         +         +
      -0.113         ***************************** +         +         +         +         +         +         +         +
      -0.106         *******************************************       +         +         +         +         +         +
      -0.100         *******************************************       +         +         +         +         +         +
      -0.093         **********************************************************  +         +         +         +         +
      -0.087         **********************************************************  +         +         +         +         +
      -0.080         **************************************************************************************    +         +
      -0.074         **************************************************************************************    +         +
      -0.068         *****************************************************************************************************
      -0.061         *****************************************************************************************************
      -0.055         **************************************************************************************    +         +
      -0.048         **********************************************************  +         +         +         +         +
      -0.042         *******************************************       +         +         +         +         +         +
      -0.035         ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  233.0006*ALOG(IN1/IN2)+165.0"

     IF (OUT<>"") F2 (one,two) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
 F2 (one,two) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153
      2       3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154
      3      71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154
      4      98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155
      5     113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155
      6     123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155
      7     129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156
      8     134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156
      9     138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157
     10     140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157
     11     143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157
     12     145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157
     13     146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158
     14     148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158
     15     149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158
     16     150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158
     17     151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158
     18     152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159
     19     152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159
     20     153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159 159
DIFFERENCE OF 2 AND 3 WITH HISTOGRAM
RATIO0 (two,three) C     CENTER=CENTER MODE=DIFFEREN   MODE2=NOLOG M+
ODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
      -2.000         ***************     +         +         +         +         +         +         +         +         +
      -1.000    0DN  ***************************** +         +         +         +         +         +         +         +
       0.000         *******************************************       +         +         +         +         +         +
       1.000         ************************************************************************        +         +         +
       2.000         **************************************************************************************    +         +
       3.000         **************************************************************************************    +         +
       4.000         *****************************************************************************************************
       5.000         *****************************************************************************************************
       6.000         **************************************************************************************    +         +
       7.000         *****************************************************************************************************
       8.000   MEAN  *****************************************************************************************************
       9.000         **************************************************************************************    +         +
      10.000         *****************************************************************************************************
      11.000         *****************************************************************************************************
      12.000         **************************************************************************************    +         +
      13.000         **************************************************************************************    +         +
      14.000         ************************************************************************        +         +         +
      15.000         *******************************************       +         +         +         +         +         +
      16.000         ***************************** +         +         +         +         +         +         +         +
      17.000  255DN  ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  14.16667*(IN1-IN2)+21.2"

     IF (OUT<>"") F2 (two,three) C  FUNC="  14.16667*(IN1-IN2)+21.2"
 F2 (two,three) C  FUNC="  14.16667*(IN1-IN2)+21.2"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 176 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
      2      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
      3      50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220 220 234 248 255
      4      64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234 234 248 255 255
      5       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
      6      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
      7      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
      8      50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220 220 234 248 255
      9       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     10       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
     11      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
     12      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
     13       0   0   7  21  21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191
     14       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     15       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
     16      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
     17       0   0   0   7   7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177
     18       0   0   7  21  21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191
     19       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     20       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
LOG DIFFERENCE OF 1 AND 4 WITH HISTOGRAM
RATIO0 (one,four) C     CENTER=CENTER MODE=DIFFEREN   MODE2=LOG MODE+
3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

L O G   D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
       5.526    0DN  ****************    +         +         +         +         +         +         +         +         +
       5.535         ************************************    +         +         +         +         +         +         +
       5.544         ******************************************************************    +         +         +         +
       5.552         **************************************************************************************    +         +
       5.561   MEAN  *****************************************************************************************************
       5.570         **************************************************************************************    +         +
       5.579         ******************************************************************    +         +         +         +
       5.587         ************************************    +         +         +         +         +         +         +
       5.596  255DN  ****************    +         +         +         +         +         +         +         +         +


The ratio function is:

"  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"

     IF (OUT<>"") F2 (one,four) C  FUNC="  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"
 F2 (one,four) C  FUNC="  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 301 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      71  85  85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212
      2      85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212 212 225
      3       0   0   0   0   0   0   0  13  13  28  28  42  42  57  57  71  71  85  85  99
      4       0   0   0   0   0  13  13  28  28  42  42  57  57  71  71  85  85  99  99 114
COMPUTATION ONLY (NO OUTPUT SET OR HISTOGRAM)
RATIO0 (three,four)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE+
3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB
The ratio function is:

"  296.5116*(IN1/IN2)-47.4"

     IF (OUT<>"") F2 (three,four)   FUNC="  296.5116*(IN1/IN2)-47.4"
let $echo="no"
COMPUTATION ONLY AND NO FILTERING
RATIO0 (three,four)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE+
3=NOFILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB
The ratio function is:

"  310.9756*(IN1/IN2)-56.0"

     IF (OUT<>"") F2 (three,four)   FUNC="  310.9756*(IN1/IN2)-56.0"
let $echo="no"
SAMPLING AREA SPECIFIED, ALONG WITH ATM CORRECTIONS
RATIO0 (three,four) C  AREA=(1,1,4,4)  SAMPLE=1.000000000000e+02  AT+
M1=5.000000000000e-01 ATM2=5.000000000000e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB
The ratio function is:

"  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"

     IF (OUT<>"") F2 (three,four) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
 F2 (three,four) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
let $echo="no"
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
---- Task: SIZE -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
FUNCTION='  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8'
 
************************************************************
ABOVE, WITH ALL LINES SAMPLES
RATIO0 (three,four) C    LINC=1  ATM1=5.000000000000e-01 ATM2=5.0000+
00000000e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB
The ratio function is:

"  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"

     IF (OUT<>"") F2 (three,four) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
 F2 (three,four) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
let $echo="no"
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
---- Task: SIZE -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: rjb -- Wed Aug 17 12:29:58 2016 ----
FUNCTION='  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1'
 
************************************************************
RATIO BETWEEN 1 AND 2 WITH BOTTOM 10 PERCENT EXCLUDED
RATIO0 (one,two) C    THRESH=1.000000000000e+01  CENTER=CENTER MODE=+
RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

     IF (OUT<>"") F2 (one,two) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (one,two) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
AS ABOVE, BUT INCLUDING ONLY THOSE RATIO'S ABOVE .90
RATIO0 (one,two) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00+
)  CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961         *********************************************     +         +         +         +         +         +
       0.970  255DN  ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3109.758*(IN1/IN2)-2747.8"

     IF (OUT<>"") F2 (one,two) C  FUNC="  3109.758*(IN1/IN2)-2747.8"
 F2 (one,two) C  FUNC="  3109.758*(IN1/IN2)-2747.8"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206
      2       0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214
      3       0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221
      4       0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227
      5       0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232
      6       0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238
      7       0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242
      8       0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247
      9      16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251
     10      51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255
     11      79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255
     12     103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255
     13     123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255
     14     140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255
     15     155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255
     16     168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255
     17     179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255
     18     189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255
     19     198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255
     20     206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255 255
AS ABOVE, BUT WITH 10 PERCENT SATURATION
RATIO0 (one,two) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00+
)  PERCENT=1.000000000000e+01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
** RATIO version 19-JUL-2016 - (64bit) - RJB

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961  255DN  *********************************************     +         +         +         +         +         +
       0.970         ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3887.196*(IN1/IN2)-3466.6"

     IF (OUT<>"") F2 (one,two) C  FUNC="  3887.196*(IN1/IN2)-3466.6"
 F2 (one,two) C  FUNC="  3887.196*(IN1/IN2)-3466.6"
Beginning VICAR task F2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
let $echo="no"
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
 Task:F2        User:rjb       Date_Time:Wed Aug 17 12:29:58 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226
      2       0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235
      3       0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244
      4       0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252
      5       0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255
      6       0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255
      7       0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255
      8       0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255
      9       0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255
     10      32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255
     11      67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255
     12      97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255
     13     122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255
     14     143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255
     15     161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255
     16     178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255
     17     192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255
     18     205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     19     216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     20     226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
exit
slogoff
$ Return
$!#############################################################################
