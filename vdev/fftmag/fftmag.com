$!****************************************************************************
$!
$! Build proc for MIPL module fftmag
$! VPACK Version 1.9, Tuesday, August 16, 2016, 16:48:06
$!
$! Execute by entering:		$ @fftmag
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
$ write sys$output "*** module fftmag ***"
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
$ write sys$output "Invalid argument given to fftmag.com file -- ", primary
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
$   if F$SEARCH("fftmag.imake") .nes. ""
$   then
$      vimake fftmag
$      purge fftmag.bld
$   else
$      if F$SEARCH("fftmag.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftmag
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftmag.bld "STD"
$   else
$      @fftmag.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftmag.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftmag.com -mixed -
	-s fftmag.f -
	-i fftmag.imake -
	-p fftmag.pdf -
	-t tstfftmag.pdf tstfftmag.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftmag.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C**********************************************************************
C       PROGRAM FFTMAG
C**********************************************************************
C
      INCLUDE 'VICMAIN_FOR'
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 13 OCT 1983
C     CONVERTED TO VICAR2 I/O BY FLORANCE MOSS, 28 AUG 1987
C     MSTP S/W CONVERSION (VICAR PORTING) A. Scop (CRI) 1 JUL 1994
C
      SUBROUTINE MAIN44
C      IMPLICIT INTEGER(A-Z)
        implicit none
      INTEGER*4 INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
      INTEGER*4 COUNT,DEF,N,MULT,SL,SS,STATUS
      INTEGER*8 I8NBYTES
      EXTERNAL WORK


      COMMON INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
C
      CALL IFMESSAGE('FFTMAG version 25-JUL-2016 - (64bit) - RJB')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')

      CALL XVSIZE(SL,SS,NXO,NSO,NX,NS)


      CALL XVPARM('BYS',MULTS,COUNT,DEF,1)
      CALL XVPARM('BYL',MULTL,COUNT,DEF,1)

      CALL XVPARM('BY',MULT,COUNT,DEF,1)
      IF (DEF .EQ. 0 ) THEN
          MULTL=MULT
          MULTS=MULT
      END IF      

C       
      IF (MULTL.EQ.0) MULTL=NXO/NX
      N=LOG(FLOAT(MULTL))/LOG(2.) + 1.0E-5
      MULTL = MAX(1.,2.**N)

      IF (MULTS.EQ.0) MULTS=NSO/NS
      N=LOG(FLOAT(MULTS))/LOG(2.) + 1.0E-5
      MULTS = MAX(1.,2.**N)

      NXO=MULTL*NX
      NSO=MULTS*NS

      CALL XVMESSAGE(' ',' ')
      CALL PRNT(4,1,MULTL,' LINE EXPANSION FACTOR = ')
      CALL PRNT(4,1,MULTS,' SAMPLE EXPANSION FACTOR = ')
      CALL PRNT(4,1,NXO,' OUTPUT LINES = ')
      CALL PRNT(4,1,NSO,' OUTPUT SAMPLES = ')

      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE',
     +       'U_NL',NXO,'U_NS',NSO,'U_FORMAT','COMP',' ')

      NBYTES = NSO * 8
      I8NBYTES = int8(NBYTES)
      CALL STACKA_BIG(3,WORK,1,I8NBYTES)
      RETURN

      END
C
C
C
C
C**********************************************************************
C
      SUBROUTINE WORK(C,LENC)

      implicit none

      INTEGER*4 INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
      INTEGER*4 I,J,K,L
      INTEGER*8 LENC

      COMMON INUNIT,OUTUNIT,NXO,NSO,NX,NS,MULTL,MULTS,NBYTES
      INTEGER*4 STATUS
      REAL*4 SCALE
      COMPLEX C(NBYTES)

      IF (LENC.LT.NBYTES) THEN
          CALL MABEND('??E - INSUFFICIENT MEMORY')
      END IF

      SCALE=MULTL*MULTS
      DO I=1,NSO
          C(I) = (0,0)
      END DO
      DO L=1,NXO
          IF (L.LE.NX/2+1 .OR. L.GE.NXO-NX/2+2) THEN
              K=-1
              CALL XVREAD(INUNIT,C,STATUS,'NSAMPS',NS,' ')
              IF (MULTS.GT.1) THEN 
                  DO J=NSO,NSO-NS/2+2,-1
                      K=K+1
                      C(J)=C(NS-K)
                      C(NS-K)=(0,0)
                  END DO
              END IF
          ELSE IF (L.EQ.NX/2+2) THEN
              DO I=1,NSO
                  C(I) = (0,0)
              END DO
          END IF
C       
          DO I=1,NSO
              C(I) = C(I)*SCALE
          END DO
          CALL XVWRIT(OUTUNIT,C,STATUS,'NSAMPS',NSO,' ')
C       

      END DO

      CALL XLDEL(OUTUNIT,'SYSTEM','NL',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','NL',NXO,STATUS,'FORMAT','INT',' ')

      CALL XLDEL(OUTUNIT,'SYSTEM','FORMAT',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','FORMAT','COMP',STATUS,'FORMAT',
     &'STRING',' ')

      CALL XLDEL(OUTUNIT,'SYSTEM','NS',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','NS',NSO,STATUS,'FORMAT','INT',' ')

      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftmag.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftmag

   To Create the build file give the command:

		$ vimake fftmag			(VMS)
   or
		% vimake fftmag			(Unix)


************************************************************************/


#define PROGRAM	fftmag
#define R2LIB

#define MODULE_LIST fftmag.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftmag.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM BY TYPE=INTEGER DEFAULT=0
PARM BYS TYPE=INTEGER DEFAULT=0
PARM BYL TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
"fftmag"
.HELP
PURPOSE:
"fftmag" will expand a picture's size by a factor of 2^N by enlarging
the Fourier Transform of the picture.  This technique provides correct 
interpolation using the Sampling Theorem.

 
EXECUTION:

Examples

fftmag  INP=FFTIN  OUT=FFTOUT  BY=2

This command will enlarge input transform FFTIN by a factor of two in both the
line and sample directions.

fftmag  INP=FFTIN  OUT=FFTOUT  BYL=5  BYS=2

This command will enlarge the transform by a factor of two in the sample
direction, but by a factor of four in the line direction.  This is because 
expansion factors must be powers of 2, and are rounded down to the next-lower 
power, as necessary.

Note that ALLOC must be used to allocate space for the output transform before
"fftmag" is run.


OPERATION:
"fftmag" expands a picture by partitioning the Fourier Tranform and placing the 
four quadrants into a larger transform array.  The details of the partitioning
are illustrated in the figure.  The expansion of the spatial dimension is
accomplished by a reduction in the apparent frequency (apparent because of the
larger format of the new FFT) according to the Similarity Theorem:

					       1      fx fy
If FT(g(x,y)) = G(fx,fy), then FT(g(ax,by)) = ---- G (--,--)
					      |ab|     a  b

where a = the sample magnification and b = the line magnification as specified
by the magnification keywords.

The interpolation algorithm which results in the above operation is the
Sampling Theorem, or optimal interpolation method for band limited data.  By
partitioning the FFT we equivalently band limit the larger image of the scene
until it contains the same frequencies we actually possess.  For BY=2, for 
example:
				    ____
	FT(I(i,j)) = FT(I(i,j)) * __|  |__0.25 cps
	  actual       double

Taking the FFT of both sides results in the Sampling Theorem, which is superior
to all other interpolation theorems.


+--+------------+--+------------+--+	\
|DC|		|  |		|DC|	|
+--+------------+--+------------+--+	|
|  |            |  |		|	|
|  | FFT        |  |		|	| N/2+1
|  | INPUT      |  |		|	|
|  | FORMAT     |  |		|	|
|  |            |  |		|	|
|  |            |  |		|	|
+--+------------+--+------------+	|	
|  |		|XX|		|	/
+--+------------+--+------------+
|  |            |  |		|	\
|  |            |  |		|	|
|  |            |  |		|	| N/2-1
|  |            |  |		|	|
|  |            |  |		|	|
|  |            |  |		|	/
+--+------------+--+------------+--+
|DC|				|DC|
+--+                            +--+

\-----------------/ \-----------/
      8(M/2+1)         8(M/2-1)


+--+------------+--+----------------------------+---------------+
|DC|		|  |				|		|
+--+------------+--+                            +---------------+
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
+--+------------+--+                            +---------------+
|  |            |XX|                            |               |
+--+------------+--+				+---------------+
|								|
|			FFT OUTPUT FORMAT			|
|								|
|								|
|								|
|								|
+--+------------+--+                            +---------------+
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
|  |            |  |				|		|
+--+------------+--+----------------------------+---------------+

WRITTEN BY:  J. J. Lorre, 1 April 1980
COGNIZANT PROGRAMMER:  Ray Bambery
REVISION:  New
Made portable for UNIX: A. Scop (CRI) 1 July 1994
  Apr 27 1998.....RRP.....Changed COMPLEX format keyword to COMP as per AR-9582.

  25 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARIABLE INP
STRING - Input FFT
.VARIABLE OUT
STRING - Output FFT
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE BY
INTEGER - Line/sample magnification
.VARIABLE BYL
INTEGER - Line magnification
.VARIABLE BYS
INTEGER - Sample magnification
.LEVEL2
.VARIABLE BY
BY specifies the desired expansion factor of the output picture.  It should be
equal to 2^N where N is an integer.  If BY is not an integer power of two, it
will be rounded down as necessary. If BY is defaulted, it will be computed from
the SIZE specification; if BY and SIZE are specified, BY will take precedence.
.VARIABLE BYL
BYL is the same as BY, except only in the line direction. The default if BY
wasn't specified is to calculate BYL from the SIZE specification.
.VARIABLE BYS
BYS is the same as BY, except only in the sample direction. The default if BY
wasn't specified is to calculate BYS from the SIZE specification.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftmag.pdf
procedure
! Jul 25, 2016- RJB
! TEST SCRIPT FOR FFTMAG
! tests BYTE images
!
! Vicar Programs:
!       gen list fft22 label-list
!
! External Programs;
!   <none>
!           
! parameters:
!   <none>
!
! Requires no external test data: 
!

refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
gen A 16 16
list A SIZE=(1,1,10,10)
fft22 A B
fftmag B C BY=2
label-list C
fft22 C D 'INVERSE
list D SIZE=(1,1,10,10)

fftmag B C BYL=2
label-list C
fft22 C D 'INVERSE
list D SIZE=(1,1,10,10)

fftmag B C BYS=2
label-list C
fft22 C D 'INVERSE
list D SIZE=(1,1,10,10)

let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstfftmag.log
tstfftmag
gen A 16 16
Beginning VICAR task gen
GEN Version 6
GEN task completed
list A SIZE=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
fft22 A B
Beginning VICAR task fft22
** FFT22 version 25-JUL-2016 - (64bit) - RJB
FORWARD TRANSFORM
TRANSFORM COMPLETED
fftmag B C BY=2
Beginning VICAR task fftmag
FFTMAG version 25-JUL-2016 - (64bit) - RJB

 LINE EXPANSION FACTOR =          2
 SAMPLE EXPANSION FACTOR =          2
 OUTPUT LINES =         32
 OUTPUT SAMPLES =         32
label-list C
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMP format from a X86-LINUX host
                1 bands
                32 lines per band
                32 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: FFT22 -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
---- Task: FFTMAG -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
 
************************************************************
fft22 C D 'INVERSE
Beginning VICAR task fft22
** FFT22 version 25-JUL-2016 - (64bit) - RJB
INVERSE TRANSFORM
TRANSFORM COMPLETED
list D SIZE=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
 Task:FFT22     User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
     Samp     1       3       5       7       9
   Line
      1       0   0   1   3   2   2   3   4   4   4
      2       0   0   0   1   0   0   1   2   2   3
      3       1   0   2   4   3   3   4   5   5   5
      4       3   1   4   5   5   4   6   7   7   7
      5       2   0   3   5   4   4   5   6   6   6
      6       2   0   3   4   4   4   5   6   6   6
      7       3   1   4   6   5   5   6   7   7   7
      8       4   2   5   7   6   6   7   8   8   8
      9       4   2   5   7   6   6   7   8   8   8
     10       4   3   5   7   6   6   7   8   8   8
fftmag B C BYL=2
Beginning VICAR task fftmag
FFTMAG version 25-JUL-2016 - (64bit) - RJB

 LINE EXPANSION FACTOR =          2
 SAMPLE EXPANSION FACTOR =          1
 OUTPUT LINES =         32
 OUTPUT SAMPLES =         16
label-list C
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMP format from a X86-LINUX host
                1 bands
                32 lines per band
                16 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: FFT22 -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
---- Task: FFTMAG -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
 
************************************************************
fft22 C D 'INVERSE
Beginning VICAR task fft22
** FFT22 version 25-JUL-2016 - (64bit) - RJB
INVERSE TRANSFORM
TRANSFORM COMPLETED
list D SIZE=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
 Task:FFT22     User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
     Samp     1       3       5       7       9
   Line
      1       0   0   1   3   2   2   3   4   4   4
      2       1   0   2   4   3   3   4   5   5   5
      3       2   0   3   5   4   4   5   6   6   6
      4       3   1   4   6   5   5   6   7   7   7
      5       4   2   5   7   6   6   7   8   8   8
      6       5   3   6   8   7   7   8   9   9   9
      7       6   4   7   9   8   8   9  10  10  10
      8       7   5   8  10   9   9  10  11  11  11
      9       8   6   9  11  10  10  11  12  12  12
     10       9   7  10  12  11  11  12  13  13  13
fftmag B C BYS=2
Beginning VICAR task fftmag
FFTMAG version 25-JUL-2016 - (64bit) - RJB

 LINE EXPANSION FACTOR =          1
 SAMPLE EXPANSION FACTOR =          2
 OUTPUT LINES =         16
 OUTPUT SAMPLES =         32
label-list C
Beginning VICAR task label
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMP format from a X86-LINUX host
                1 bands
                16 lines per band
                32 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: FFT22 -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
---- Task: FFTMAG -- User: rjb -- Tue Jul 26 23:14:58 2016 ----
 
************************************************************
fft22 C D 'INVERSE
Beginning VICAR task fft22
** FFT22 version 25-JUL-2016 - (64bit) - RJB
INVERSE TRANSFORM
TRANSFORM COMPLETED
list D SIZE=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
 Task:FFT22     User:rjb       Date_Time:Tue Jul 26 23:14:58 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       0   0   0   1   2   3   4   5   6   7
      3       1   2   3   4   5   6   7   8   9  10
      4       3   4   5   6   7   8   9  10  11  12
      5       2   3   4   5   6   7   8   9  10  11
      6       2   3   4   5   6   7   8   9  10  11
      7       3   4   5   6   7   8   9  10  11  12
      8       4   5   6   7   8   9  10  11  12  13
      9       4   5   6   7   8   9  10  11  12  13
     10       4   5   6   7   8   9  10  11  12  13
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
