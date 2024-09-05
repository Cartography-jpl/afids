$!****************************************************************************
$!
$! Build proc for MIPL module shady
$! VPACK Version 1.9, Tuesday, August 16, 2016, 20:45:03
$!
$! Execute by entering:		$ @shady
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module shady ***"
$!
$ Create_Source = ""
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
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to shady.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("shady.imake") .nes. ""
$   then
$      vimake shady
$      purge shady.bld
$   else
$      if F$SEARCH("shady.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake shady
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @shady.bld "STD"
$   else
$      @shady.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create shady.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack shady.com -mixed -
	-s shady.f -
	-i shady.imake -
	-p shady.pdf -
	-t tstshady.pdf tstshady.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create shady.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR program SHADY - Introduces shading and contour lines into an image
C        SHADY  IN  OUT  user-parameters...
C
      SUBROUTINE MAIN44

c      IMPLICIT INTEGER(A-Z)
        implicit none 
       EXTERNAL SHADY
       INTEGER*2 CTBL(256),STBL(512)
       INTEGER*4 A,B,COUNT,DEF,INTERVAL,INUNIT,OUTUNIT,IVAL
       INTEGER*4 I,J,J1,J2,L,M,N,LOOP,NL,NLI,NREG,NS,NSI
       INTEGER*4 NSP,NSQ,NSTART,SL,SS,STATUS
       INTEGER*4 C(3),S(2)
       INTEGER*8 N8NS,N8NSQ,N8NSP
       LOGICAL*4 XVPTST,NOSHAD,NOCONT,DBUG

      COMMON/C1/SL,SS,NL,NS,J1,J2,CTBL,STBL,DBUG,NOSHAD,NOCONT
      COMMON/C2/INUNIT,OUTUNIT

      CALL XVEACTION('SA',' ')
      CALL XVMESSAGE('** SHADY version  19-JUL-2016 - (64bit) - RJB',' ')

      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE',
     +'O_FORMAT','BYTE','U_FORMAT','HALF',' ')

C     ....Begin parameter processing
      CALL XVPARM('SHADE',N,COUNT,DEF,0)		!N=direction of sun

      CALL XVPARM('CONTOUR',C,COUNT,DEF,0)
      NSTART = C(1)	!Lowest DN of 2nd contour region
      NREG = C(2)	!Number of regions
      INTERVAL = C(3)	!DN spacing between regions

      CALL XVPARM('SCALE',S,COUNT,DEF,0)
      A = S(1)		!Shading scale
      B = S(2)		!Shading offset

      NOSHAD = XVPTST('NOSHADE')	!Suppress shading?
      NOCONT = XVPTST('NOCONTOU')	!Suppress contours?
      DBUG = XVPTST('DBUG')		!Print diagnostics?

C     ....Generate contour look-up table
      IF(NOCONT) GO TO 30
      IVAL = 0
      IF(NSTART.GT.0) CALL MVE(-6,NSTART,0,CTBL,0,1)
      LOOP = NREG - 2
      J = NSTART + 1
C
      DO  L=1,LOOP
            IVAL = IVAL + 1
        DO  I=1,INTERVAL
              IF(J.GT.256) GO TO 30
              CTBL(J) = IVAL
              J = J + 1
        END DO
      END DO
C
      IF(J .LT. 256) CALL MVE(-6,257-J,IVAL+1,CTBL(J),0,1)
C
C     ....Set shade indices
   30 NSP = NS + 2
      IF(NOSHAD) GO TO 36
      M = N
      IF(M.GT.3) M = M - 4
      IF(M.NE.0) GO TO 301
      J1 = NSP + 3
      J2 = NSP + 1
      GO TO 303
  301 J1 = 3
      J2 = 2*NSP + 1
      DO I=1,3
           IF(M.EQ.I) GO TO 303
           J1 = J1 - 1
           J2 = J2 + 1
      END DO
  303 IF(M.EQ.N) GO TO 304
      M = J1
      J1 = J2
      J2 = M
  304 CONTINUE

C************************************
C                                   *
C     GENERATE SCALE LOOK-UP TABLE  *
C                                   *
C************************************         
      DO  I=1,512
            IVAL = (I-256)*A + B
            IF(IVAL.LT.0) IVAL = 0
            IF(IVAL.GT.255) IVAL = 255
            STBL(I) = IVAL
      END DO
      GO TO 37
   36 IF(.NOT.NOCONT) GO TO 37
        CALL XVMESSAGE('** BOTH SHADING AND CONTOURS SUPPRESSED',' ')
        CALL XVMESSAGE('** TASK CANCELLED',' ')
C      CALL QPRINT('0**BOTH SHADING AND CONTOURS SUPPRESSED',39)
C      CALL QPRINT('0**TASK CANCELLED',17)
      CALL ABEND
   37 NSQ = NSP
      IF(NOCONT) NSQ = 2

      N8NS = int8(2*NS)
      N8NSQ = int8(6*NSQ)
      N8NSP = int8(6*NSP)
      CALL STACKA_BIG(5,SHADY,3,N8NS,N8NSQ,N8NSP)

      RETURN
      END
C****************************************************************************
      SUBROUTINE SHADY(PIC,NXX,CBUF,NXY,BUF,NXZ)

C      IMPLICIT INTEGER(A-Z)
      implicit none 

      INTEGER*2 BUF(*),CBUF(*),PIC(*)
      LOGICAL*4 DBUG,NOCONT,NOSHAD
      INTEGER*2 STBL(512),CTBL(256)
      INTEGER*4 I,I1,I2,I3,J1,J2,K2,K3,L,C0,C1,INUNIT,OUTUNIT
      INTEGER*4 ISAVE,NL,NS,NS2,NSP,NSP3,SL,SS,STATUS
      INTEGER*8 NXX,NXY,NXZ

      COMMON/C1/SL,SS,NL,NS,J1,J2,CTBL,STBL,DBUG,NOSHAD,NOCONT
      COMMON/C2/INUNIT,OUTUNIT
C
      NSP = NS + 2
      NSP3 = 3*NSP
      IF(NXZ.LT.6*NSP) GOTO 980
      I1 = 2
      I2 = I1 + NSP
      I3 = I2 + NSP
      NS2 = NS*2
      IF (.NOT.DBUG) GOTO 38
      CALL XVMESSAGE(' ',' ')
      CALL PRNT(2,256,CTBL,'CTBL=.')
      CALL XVMESSAGE(' ',' ')
      CALL PRNT(2,256,STBL,'STBL=.')

C***************************************************
C                                                  * 
C     READ IN FIRST LINE AND REFLECT AT MARGINS    *
C                                                  *
C***************************************************

   38 CALL XVREAD(INUNIT,BUF(I2),STATUS,'SAMP',SS,
     +            'NSAMPS',NS,' ')
      BUF(I2-1) = BUF(I2)
      BUF(I2+NS) = BUF(I2+NS-1)
      CALL MVE(2,NS+2,BUF(I2-1),BUF(I1-1),1,1)
      IF(.NOT.NOCONT) CALL CTAB(BUF(I2),CBUF(I2),CTBL,NS)

C*********************
C                    * 
C     MAIN LOOP      *
C                    *
C*********************

      DO  L=1,NL
            IF(L.NE.NL) GO TO 391
            CALL MVE(2,NS+2,BUF(I2-1),BUF(I3-1),1,1)
            IF(.NOT.NOCONT) CALL MVE(2,NS,CBUF(I2),CBUF(I3),1,1)
            GO TO 40
  391       CALL XVREAD(INUNIT,BUF(I3),STATUS,'SAMP',SS,
     +                  'NSAMPS',NS,' ')
   40       IF(NOSHAD) GO TO 50
            BUF(I3-1) = BUF(I3)
            BUF(I3+NS) = BUF(I3+NS-1)
            CALL SHADER(BUF(J2),BUF(J1),PIC,STBL,NS)
   50       IF(NOCONT) GO TO 60
            IF(NOSHAD) CALL MVE(2,NS,BUF(I2),PIC,1,1)
            CALL CTAB(BUF(I3),CBUF(I3),CTBL,NS)
            K2 = I2
            K3 = I3
            C1 = CBUF(I2)
            DO I=1,NS
                    C0 = CBUF(K2)
                    IF(C0.NE.C1.OR.C0.NE.CBUF(K3)) PIC(I) = 255
                    K2 = K2 + 1
                    K3 = K3 + 1
                    C1 = C0
            END DO  
   60       ISAVE = I1
            I1 = I2
            I2 = I3
            I3 = ISAVE
            J1 = J1 + NSP
            J2 = J2 + NSP
            IF(J1.GT.NSP3) J1 = J1 - NSP3
            IF(J2.GT.NSP3) J2 = J2 - NSP3
            CALL XVWRIT(OUTUNIT,PIC,STATUS,'NSAMPS',NS,' ')
      END DO
C
      RETURN
CCCCCC
  980 CALL QPRINT('0**NOT ENOUGH CORE',18)
      CALL XVMESSAGE(' ',' ')
      CALL PRNT(4,1,(6*NSP-NXZ+1023)/1024,' **INCREASE REGION SIZE BY.')
C          UNEXPECTED EOF
      CALL ABEND
      END
C********************************************************************
	SUBROUTINE SHADER(BUF1,BUF2,PIC,STBL,NS)

C***************************************************
C                                                  *
C-----CONVERTED FROM THE IBM ASSEMBLER BY GMY 1-84 * 
C                                                  *
C***************************************************
        implicit none
	INTEGER*2 BUF1(*),BUF2(*),PIC(*),STBL(512)
        INTEGER*4 I,J,NS
C
	DO I=1,NS
      	     J = BUF1(I) - BUF2(I) + 256
  	     PIC(I) = STBL(J)
        END DO
C
	RETURN
	END
C***************************************************************************
	SUBROUTINE CTAB(BUF,CBUF,CTBL,NS)

C***************************************************
C                                                  *
C-----CONVERTED FROM THE IBM ASSEMBLER BY GMY 1-84 * 
C                                                  *
C***************************************************
        implicit none
	INTEGER*2 BUF(*),CBUF(*),CTBL(*)
        INTEGER*4 I,J,NS
C
	DO  I=1,NS
              J = BUF(I) + 1
     	      CBUF(I) = CTBL(J)
        END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create shady.imake
#define  PROGRAM   shady

#define MODULE_LIST shady.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create shady.pdf
process help=*
 PARM INP   TYPE=STRING
 PARM OUT   TYPE=STRING
 PARM SIZE  TYPE=INTEGER    COUNT=4  DEFAULT=(1,1,0,0)
PARM SL	    TYPE=INTEGER    DEFAULT=1
PARM SS	    TYPE=INTEGER	DEFAULT=1
PARM NL	    TYPE=INTEGER	DEFAULT=0
PARM NS 	TYPE=INTEGER	DEFAULT=0
 PARM SHADE TYPE=INTEGER    DEFAULT=1  VALID=(0:7)
 PARM SCALE TYPE=INTEGER    COUNT=2  DEFAULT=(6,128)
 PARM CONTOUR   TYPE=INTEGER  COUNT=3  DEFAULT=(1,16,16) VALID=(0:256)
PARM NOSHADE	TYPE=KEYWORD VALID="NOSHADE" COUNT=(0:1) DEFAULT=--
PARM NOCONTOU	TYPE=KEYWORD VALID="NOCONTOU" COUNT=(0:1) DEFAULT=--
PARM DBUG	TYPE=KEYWORD    VALID="DBUG" COUNT=(0:1) DEFAULT=--
 END-PROC
.TITLE
 shady
.HELP
 PURPOSE:

 shady is a VICAR applications program which introduces shading
 and contour lines on a picture.  Shading is created by computing
 the spatial derivative of the image in a specified direction.  
 Shading and contours may be suppressed, if desired.

 EXECUTION:

  	 shady  INP  OUT  user-parameters... 
 where
	INP is the input picture (byte format).
	OUT is the output picture (byte format).
.page
 OPERATION:

 shady uses a simple-minded algorithm to introduce solar shading into
 an image. (Note:  A more general algorithm is available in the VICAR
 program shady2.) 

 The direction of the sun is specified by the SHADE parameter.
 Shading is introduced by computing the spatial derivative between
 neighboring pixels.  Consider a 3x3 pixel area of the input image,

			x3   x2   x1

			x4   x    x0

		        x5   x6   x7

 where x1,x2,x3,... are the pixels surrounding pixel x.  Let the direction
 of the sun be specified by the parameter SHADE=3.  If contour lines are
 suppressed, then the output pixel y (corresponding to input pixel x) is
 computed as follows:

		y = A*(x7 - x3) + B

 where A and B are specified by the SCALE parameter.

 Contour lines are generated by first dividing the DN scale
 into the R specified regions (see Figure 2).


        1        2        3       ...       R
   |________|________|________|________|________|
   0        L       L+I     L+2I    L+(R-2)I   255

                Figure 2
.page
 DN values from 1 to L-1 are assigned to region 1, etc.
 Let M = (m  ) be a region picture, where m   is the region
           ij                              ij
 number associated with input picture element x  .  An element
                                               ij
 m    is considered a boundary point if 
  ij

         m    = m         or    m   = m
          ij     i,j-1           ij    i-1,j

 If m   is a boundary point, then element y   of the output is 
     ij                                    ij
 assigned a DN value of 255.
.page
TIMING: 
  For a 500x500 Byte image, on a Vax-8600, the following CPU times
  were measured:

  For contouring ('NOSHADE):  3.30 sec.
  For shading ('NOCONTOU):    2.64 sec.
  For both:                   4.29 sec.

HISTORY:

 WRITTEN BY:             Gary Yagi,  18 April 1973
 COGNIZANT PROGRAMMER:   Ray Bambery  

 REVISIONS:
 15 Jan 1984  C Avis	Convert from IBM to VAX
 17 Feb 1986  F Moss	VICAR2 conversion
 22 SEP 1987  S Pohorsky  Corrected xvptst call for nocontou.
 02 Nov 1988  G Yagi	Changes to help file.  Reverse sense of SHADE parameter
 16 Jul 1993  G Madrid	Ported to Unix.
 19 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARIABLE INP
 STRING - INPUT FILE NAME
.VARIABLE OUT
 STRING - OUTPUT FILE NAME
.VARIABLE SIZE
 4 INTEGERS - STANDARD
 VICAR SIZE FIELD
.VARIABLE SL
 INTEGER - STARTING LINE
.VARIABLE SS
 INTEGER - STARTING SAMPLE
.VARIABLE NL
 INTEGER - NUMBER OF LINES
.VARIABLE NS
 INTEGER - NUMBER OF SAMPLES
.VARIABLE SHADE
 INTEGER - OPTIONAL
 Direction of the sun.
.VARIABLE SCALE
 2 INTEGERS - OPTIONAL - SCALE
 AND OFFSET CONSTANTS
.VARIABLE CONTOUR
 3 INTEGERS - OPTIONAL - CONTOUR
 REGION SPECIFICATIONS
.VARIABLE NOSHADE
 STRING - OPTIONAL - SUPPRESSES
 SHADING
.VARIABLE NOCONTOU
 STRING - OPTIONAL - SUPPRESSES
 CONTOUR LINES
.VARIABLE DBUG
 KEYWORD - OPTIONAL - GIVE DEBUG PRINTOUT
.LEVEL2
.VARIABLE INP
 Input file name - restricted to byte data
.VARIABLE OUT
 Output file name - restricted to byte data
.VARIABLE SIZE
 The standard VICAR size field:  Starting Line, Starting
 Sample, Number of Lines, Number of Samples
.VARIABLE SL
 Starting line number
.VARIABLE SS
 Starting sample number
.VARIABLE NL
 Number of lines
.VARIABLE NS
 Number of samples
.VARIABLE SHADE
 Integer between 1 and 7 specifying the direction of the sun
 Default is SHADE = 1.
              
                         2
                    3         1
               
                  4      *      0  

                    5        7
                         6


.VARIABLE SCALE
 Integers specifying scale and offset constants to be applied to the 
 output image.  Default is SCALE = (6,128).
.VARIABLE CONTOUR
 Integers specifying the lowest DN in second contour region, the number
 of contour regions, and the interval width of each region.
 Default is CONT = (1,16,16).
.VARIABLE NOSHADE
 Suppresses shading.  Contour lines are overlaid on the input
 image.  Default mode is a shaded picture.
.VARIABLE NOCONTOU
 Suppresses contour lines.  Default is a shaded picture with
 contours.
.VARIABLE DBUG
 Generates programmer debug printout.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstshady.pdf
procedure
! Jul 25, 2016- RJB
! TEST SCRIPT FOR SHADY
! tests BYTE images
!
! Vicar Programs:
!       gen list
!
! External Programs;
!   <none>
!           
! parameters:
!   <none>
!
! Requires no external test data: 
!
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="stop"
let $echo="no"
write "THIS IS A TEST OF MODULE SHADY"
write "FIRST WE WILL TEST THE CONTOUR OPTION"
let $echo="yes"
gen a NL=500 NS=500
let $echo="no"
write "GENERATE CONTOURS"
let $echo="yes"
shady a b 'NOSHADE
let $echo="no"
write "LIST THE RESULT OF THE DEFAULT PARAMETERS"

let $echo="yes"
list b (1,1,5,10)
list b (401,401,5,10)
let $echo="no"

write "TRY OTHER PARAMETERS"
let $echo="yes"
shady a c 'NOSHADE CONT=(0,10,3)
list c  (1,1,5,10)
list c (401,401,5,10)
let $echo="no"
write "TRY THE SHADING OPTION"
write "THE RESULT SHOULD BE FLAT 128 EXCEPT AT THE EDGES"
write "FOR ILLUMINATION AT 45 DEGREES FROM UP"
let $echo="yes"
shady a d 'NOCONTOU SHADE=5
list d  (1,1,5,10)
list d (401,401,5,10)
let $echo="no"
write "TRY BOTH TOGETHER"
let $echo="yes"
shady a e CONT=(0,10,3) SHADE=0 'DBUG SCALE=(6,100)
list e (1,1,5,10)
list e (401,401,5,10)
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstshady.log
tstshady
THIS IS A TEST OF MODULE SHADY
FIRST WE WILL TEST THE CONTOUR OPTION
gen a NL=500 NS=500
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
GENERATE CONTOURS
shady a b 'NOSHADE
Beginning VICAR task shady
** SHADY version  19-JUL-2016 - (64bit) - RJB
let $echo="no"
LIST THE RESULT OF THE DEFAULT PARAMETERS
list b (1,1,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp     1       3       5       7       9
   Line
      1     255 255   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
list b (401,401,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp   401     403     405     407     409
   Line
    401     255 255  34  35  36  37  38  39  40  41
    402     255  34  35  36  37  38  39  40  41  42
    403      34  35  36  37  38  39  40  41  42  43
    404      35  36  37  38  39  40  41  42  43  44
    405      36  37  38  39  40  41  42  43  44  45
let $echo="no"
TRY OTHER PARAMETERS
shady a c 'NOSHADE CONT=(0,10,3)
Beginning VICAR task shady
** SHADY version  19-JUL-2016 - (64bit) - RJB
list c  (1,1,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp     1       3       5       7       9
   Line
      1       0   1 255 255   4 255 255   7 255 255
      2       1 255 255   4 255 255   7 255 255  10
      3     255 255   4 255 255   7 255 255  10 255
      4       3   4 255 255   7 255 255  10 255 255
      5       4 255 255   7 255 255  10 255 255  13
list c (401,401,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp   401     403     405     407     409
   Line
    401      32  33  34  35  36  37  38  39  40  41
    402      33  34  35  36  37  38  39  40  41  42
    403      34  35  36  37  38  39  40  41  42  43
    404      35  36  37  38  39  40  41  42  43  44
    405      36  37  38  39  40  41  42  43  44  45
let $echo="no"
TRY THE SHADING OPTION
THE RESULT SHOULD BE FLAT 128 EXCEPT AT THE EDGES
FOR ILLUMINATION AT 45 DEGREES FROM UP
shady a d 'NOCONTOU SHADE=5
Beginning VICAR task shady
** SHADY version  19-JUL-2016 - (64bit) - RJB
list d  (1,1,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp     1       3       5       7       9
   Line
      1     128 134 134 134 134 134 134 134 134 134
      2     122 128 128 128 128 128 128 128 128 128
      3     122 128 128 128 128 128 128 128 128 128
      4     122 128 128 128 128 128 128 128 128 128
      5     122 128 128 128 128 128 128 128 128 128
list d (401,401,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
     Samp   401     403     405     407     409
   Line
    401     128 128 128 128 128 128 128 128 128 128
    402     128 128 128 128 128 128 128 128 128 128
    403     128 128 128 128 128 128 128 128 128 128
    404     128 128 128 128 128 128 128 128 128 128
    405     128 128 128 128 128 128 128 128 128 128
let $echo="no"
TRY BOTH TOGETHER
shady a e CONT=(0,10,3) SHADE=0 'DBUG SCALE=(6,100)
Beginning VICAR task shady
** SHADY version  19-JUL-2016 - (64bit) - RJB

CTBL=
               1     1     1     2     2     2     3     3     3     4     4     4     5     5     5     6     6     6     7     7
               7     8     8     8     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9
               9     9     9     9     9     9     9     9     9     9     9     9     9     9     9     9

STBL=
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
               0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     4
              10    16    22    28    34    40    46    52    58    64    70    76    82    88    94   100
list e (1,1,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:49 2016
     Samp     1       3       5       7       9
   Line
      1      94  88 255 255  88 255 255  88 255 255
      2      94 255 255  88 255 255  88 255 255  88
      3     255 255  88 255 255  88 255 255  88 255
      4      94  88 255 255  88 255 255  88 255 255
      5      94 255 255  88 255 255  88 255 255  88
list e (401,401,5,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:20:48 2016
 Task:SHADY     User:rjb       Date_Time:Wed Jul 27 01:20:49 2016
     Samp   401     403     405     407     409
   Line
    401      88  88  88  88  88  88  88  88  88  88
    402      88  88  88  88  88  88  88  88  88  88
    403      88  88  88  88  88  88  88  88  88  88
    404      88  88  88  88  88  88  88  88  88  88
    405      88  88  88  88  88  88  88  88  88  88
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
