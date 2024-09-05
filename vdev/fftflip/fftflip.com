$!****************************************************************************
$!
$! Build proc for MIPL module fftflip
$! VPACK Version 1.9, Tuesday, August 16, 2016, 16:45:27
$!
$! Execute by entering:		$ @fftflip
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
$ write sys$output "*** module fftflip ***"
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
$ write sys$output "Invalid argument given to fftflip.com file -- ", primary
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
$   if F$SEARCH("fftflip.imake") .nes. ""
$   then
$      vimake fftflip
$      purge fftflip.bld
$   else
$      if F$SEARCH("fftflip.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftflip
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftflip.bld "STD"
$   else
$      @fftflip.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftflip.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftflip.com -mixed -
	-s fftflip.f -
	-i fftflip.imake -
	-p fftflip.pdf -
	-t tstfftflip.pdf tstfftflip.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftflip.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C*********************************************************************
C  PROGRAM FFTFLIP:  TRANSPOSE QUADRANTS OF BYTE/HALFWORD FFT 
C
C   JAN.'84  ...JJL...  ORIGINAL VERSION
C   MAY '84  ...ASM...  CONVERTED TO VAX
C   AUG.'85  ...LWK...  CONVERTED TO VICAR2, FIXED BUGS
C   SEP.'94  ...AMS...  CRI MSTP S/W CONVERSION (VICAR PORTING)
C
C**********************************************************************
C
      SUBROUTINE MAIN44
	implicit none
      EXTERNAL WORK
C 
C
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1,IST
      INTEGER*4 OUT1,icode       !BUF(90)
      INTEGER*8 i8in,i8save,i8save2
      real*4 A 
      LOGICAL*4 FLIP
        character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      character*8 format
C
C PARAMETERS...
C       NLW =FFT SIZE,DEFAULT =32
C       LINES =LINE AND SAMPLE WIDTH,BE INTERPOLATED
C       (PSUEDO APODIZE),DEFAULT =1
C       DC =NUMBER OF PIXELS SURROUNDING THE DC PIXEL THAT
C       ARE,BE EXCLUDED FROM 'LINES'INTERPOLATION.
C       DEFAULT =1 (DC PIXEL ONLY)
C       FLIP =TRANSPOSES OUTPUT FFT ALONG THE DIAGONAL AXIS
C       FOR USE WHEN INPUT FFT WAS CREATED BY FFT22.
C
C      LOGICAL*4 FMT,MODE
C 
      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     & NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1

      DATA FLIP/.FALSE./

      CALL IFMESSAGE('** FFTFLIP version 25-JUL-2016 - (64-bit) rjb')
      CALL XVEACTION('SA',' ')
C 
C DEFINE EXTERNAL SUBROUTINE CALLING SEQUENCES 
C 
      CALL XVUNIT( IN1, 'INP', 1, IST,' ')
      CALL XVOPEN( IN1, IST, 'U_FORMAT', 'REAL', ' ')
C
      CALL XVGET( IN1, IST, 'PIX_SIZE', KEY,'FORMAT',format,' ')

      call xvclose(in1,ist,' ')
        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
             call xvmessage('??E - Unknown data format for input image',' ')
             call abend
        endif

        call xvopen(in1,ist,'OPEN_ACT','SA','IO_ACT','SA',
     1          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')            !FMT(INCODE),' ')


        KEY = 2-KEY
C
      CALL PPARM	!PROCESS PARAMETERS
C
      CALL XVUNIT( OUT1, 'OUT', 1, IST,' ')
      CALL XVOPEN( OUT1, IST, 'OP', 'WRITE', 'U_FORMAT',fmt(4),
     &     'O_FORMAT',fmt(icode),'U_NL', NLO, 'U_NS', NSO,' ')
C
      i8save2 = int8(NLW*NLW*4)
      i8save  = int8(NLW/2*4)
      i8in    = int8(NLW*NPIX*4)         
c      CALL STACKA(5,WORK,3,NLW*NLW*4,NLW/2*4,NLW*NPIX*4)
      CALL STACKA_BIG(5,WORK,3,i8save2,i8save,i8in)
      RETURN
      END
C 
C
C
C**********************************************************************
C
      SUBROUTINE WORK(SAV2,LSAV2,SAVE,LSAVE,IN,LIN)
c
      implicit none
      INTEGER*4 BLOCK,FFT,OUT1
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1,IST
      integer*4 J,L,LINE,N10,N11,N3,N4,N9
      integer*8 LSAV2,LSAVE,LIN
      real*4 IN(NPIX,NLW),SAV2(NLW,NLW),SAVE(NLW/2)
      real*4 A,A1,OF,SL
      LOGICAL*4 FLIP

      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     +    NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1
c
      IF (LSAV2.LT.NLW*NLW*2 .OR. LSAVE.LT.NLW/2*2 .OR.
     +    LIN.LT.NLW*NPIX*2) THEN
          CALL XVMESSAGE('??E - STACKA ERROR',' ')
          CALL ABEND
      END IF
C
C FFT ROW LOOP
C 
      DO BLOCK=1,NROWS
C
C READ IN A ROW OF FFT'S
C
          DO LINE=1,NLW
              CALL XVREAD( IN1, IN(1,LINE), IST, 'NSAMPS', NSO,' ')
          END DO
C 
C FFT COLUMN LOOP
C
          DO FFT=1,NCOLS
              N3 =FFT*NLW-N2+1
              N4 =(FFT-1)*NLW+1
              N9 =N4-1
              N10 =N9+N5
              N11 =N9+N6
C
C FLIP FFT QUADRANTS
C
              DO J =1,N2
                  CALL MVE(7,N2,IN(N3,N2+J),SAVE,1,1)
                  CALL MVE(7,N2,IN(N4,J),IN(N3,N2+J),1,1)
                  CALL MVE(7,N2,SAVE,IN(N4,J),1,1)
C 
                  CALL MVE(7,N2,IN(N4,N2+J),SAVE,1,1)
                  CALL MVE(7,N2,IN(N3,J),IN(N4,N2+J),1,1)
                  CALL MVE(7,N2,SAVE,IN(N3,J),1,1)
              END DO
              IF (FLIP) THEN
                  DO J=1,NLW
                      DO L =1,NLW
                          SAV2(L,J) =IN(L+N9,J)
                      END DO
                  END DO
                  DO J =1,NLW
                      DO L =1,NLW
                          IN(J+N9,L) =SAV2(L,J)
                      END DO
                  END DO
              END IF
C 
C HORIZONTAL INTERPOLATION
C
              IF (LINES.GT.0) THEN
                  DO L=1,NLW
                      IF (L.LE.N7 .OR. L.GE.N8) THEN
                          A1=IN(N10,L)
                          SL =(IN(N11,L) -A1)/A
                          OF =A1 -SL*N10 +0.5
                          DO J =N10+1,N11-1
                              IN(J,L) =SL*J+OF
                          END DO
                      END IF
                  END DO
C
C VERTICAL INTERPOLATION
C
                  DO J =N4,N9+NLW
                      IF (J.LE.N9+N7 .OR. J.GE.N9+N8) THEN
                          A1 =IN(J,N5)
                          SL =(IN(J,N6)-A1)/A
                          OF =A1 -SL*N5+0.5
                          DO L =N12,N13
                              IN(J,L) =SL*L+OF
                          END DO
                      END IF
                  END DO
              END IF
          END DO
          DO LINE=1,NLW
C
C WRITE OUT ROW OF FFT'S
C
              CALL XVWRIT( OUT1, IN(1,LINE), IST, 'NSAMPS', NSO,' ')
          END DO
      END DO
      RETURN
      END
C 
C
C
C**********************************************************************
C
      SUBROUTINE PPARM
C 
C      INTEGER IPAR(100)
      implicit none
      INTEGER*4 OUT1
      integer*4 NLO,NSO,NX,NS,NLW,LINES,NLDC,NROWS,NPIX,NCOLS
      integer*4 SLIN,SSAM,N2,N5,N6,N7,N8,N12,N13,KEY,IN1
      integer*4 IC,ID
      real*4 A
      LOGICAL*4 FLIP,XVPTST
C      REAL RPAR(100)
C      LOGICAL*4 KPAR(100)
C      EQUIVALENCE (IPAR,RPAR)
C      EQUIVALENCE (IPAR,KPAR)
      COMMON NLO,NSO,NX,NS,SLIN,SSAM,NLW,LINES,NLDC,FLIP,
     +    NROWS,NPIX,NCOLS,N2,N5,N6,N7,N8,A,N12,N13,KEY,IN1,OUT1
      
C
      CALL XVSIZE( SLIN, SSAM, NLO, NSO, NX, NS)
C
      CALL XVPARM( 'NLW', NLW, IC, ID, 1)
      IF (ID.EQ.1) NLW = MIN0(NLO,NSO)
C
      CALL XVPARM( 'LINE', LINES, IC, ID, 1)
C
      CALL XVPARM( 'DC', NLDC, IC, ID, 1)
C
      FLIP = XVPTST( 'FLIP')
C
      NROWS =NLO/NLW
      NPIX=NSO
      NCOLS=NPIX/NLW
      N2=NLW/2
      N5 =N2-LINES/2
      N6 =N2+(LINES+1)/2+1
      N7 =N2-NLDC/2
      N8 =N2+(NLDC+1)/2+1
      A =LINES+1
      N12=N5+1
      N13=N6-1
C 
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftflip.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftflip

   To Create the build file give the command:

		$ vimake fftflip			(VMS)
   or
		% vimake fftflip			(Unix)


************************************************************************/


#define PROGRAM	fftflip
#define R2LIB

#define MODULE_LIST fftflip.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftflip.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM NLW TYPE=INTEGER COUNT=0:1 DEFAULT=--
PARM FLIP TYPE=KEYWORD VALID=(FLIP) COUNT=0:1 DEFAULT=--
PARM LINE TYPE=INTEGER DEFAULT=0
PARM DC TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
Vicar Program "fftflip"
.HELP
PURPOSE:
"fftflip" accepts one FFT or a matrix of FFT outputs from programs "fft2",
or "fft22", and rearranges the internal structure of each FFT
such that DC is moved from the upper-left corner to center.  An optional
transposition can also be made along the diagonal (upper-left to lower-right).
"fftflip" differs from "fftpic" in that no automatic scaling of the FFT is
performed, and no beginning and trailing samples are added.

EXECUTION:

	fftflip INP OUT SIZE PARAMS

where:  INP  is the input FFT or matrix of FFTs.
	OUT  is the output file.
        SIZE specifies a subarea of the input file to process.
	PARAMS includes other parameters descibed below and in Tutor mode.
.page
OPERATION:
"fftflip" performs the operation shown in the following diagrams.
The letters A,B,C,D refer to fft quadrants.
+-------+-------+     +-------+-------+     +-------+-------+
|DC     |       |     |       |       |     |       |       |
|       |       |     |       |       |     |       |       |
|   A   |   B   |     |   D   |   C   |     |   D'  |   B'  |
|       |       |     |       |       |     |       |       |
+-------+-------+ --> +-------+-------+ --> +-------+-------+
|       |       |     |       |DC     |     |       |DC     |
|       |       |     |       |       |     |       |       |
|   C   |   D   |     |   B   |   A   |     |   C'  |   A'  |
|       |       |     |       |       |     |       |       |
+-------+-------+     +-------+-------+     +-------+-------+
  INPUT FFT           DEFAULT OPERATION      WITH FLIP OPTION
                                            (' MEANS TRANSPOSE)

When the input is a matrix of FFTs, then the above operation is
performed on each FFT separately.

Obviously, there is nothing that compells the input to this program 
to be an FFT:  it will work on any arbitrary image;  however, the
operation described is most meaningful for an FFT, so that term is
used here.


LIMITATIONS
    (Removed Warning about slowness on VAX)

    Does not work with "COMP" images

.PAGE
EXAMPLES

	fftflip INP=A OUT=B NLW=32 LINE=0 'FLIP

	In this example, the FFT in file A will be rearranged and then the
	program will perform a transposition along the upper-left to lower-
	right diagonal.  The result will be written to file B.  NLW gives
	the FFT dimension in pixels; the default is 32.  LINE specifies the
	number of lines above and below DC which are to be averaged
	together as a form of pseudo apodizing; no averaging is desired in
	this case.  (If averaging were to be done, the DC parameter could
	also have been specified, which indicates the number of lines
	surrounding DC that are to be excluded from the averaging; the
	default is DC=1, that is, only the DC line is excluded.)

.page
	insert INP=IN OUT=A (1,1,32,32)		(where 'IN' is image data)
	fft22 INP=A OUT=B POW=5 IFMT=BYTE OFMT=COMP
	cform INP=B OUT=A IN=COMP OUT=HALF SO=(9.76525,0.)
	fftflip INP=A OUT=B NLW=32 LINE=0 'FLIP
	f2 INP=B OUT=A 'HALF 'OUTH +
	    FUNC="(32767./ALOG(32767.))*ALOG10(IN1)"
	linear INP=A OUT=B 'HALF 'OUTB STRE=(6000,14000) LIMI=(0,255)
	list B

	This example shows fftflip as used in a typical command sequence

.page
WRITTEN BY:  J.J. Lorre,  23 January 1984

CONVERTED TO VAX BY:  A.S.Mazer, 11 May 1984


CURRENT COGNIZANT PROGRAMMER:  R. J. Bambery

    May 11, 1984 - A.S.Mazer        Converted to VAX 
    Aug  1, 1985 - L. W. Kamp       CONVERTED TO VICAR2, FIXED BUGS
    Sep  5, 1994 - A. Scop (CRI)     Made portable for UNIX 
    Dec 19, 2012 - R. J. Bambery     LINUX 64-bit changes, Internal format
                                     is real, removed BYE,HALF limitation
    Apr 20, 2014 - R. J. Bambery    Removed unused variable
.LEVEL1
.VARIABLE INP
STRING - Input fft(s)
.VARIABLE OUT
STRING - Output fft(s)
.VARIABLE SIZE
INTEGER - Standard size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NL
INTEGER - Number of lines
.VARIABLE NS
INTEGER - Number of samples
.VARIABLE NLW
INTEGER - fft dimension in pixels
.VARIABLE FLIP
KEYWORD - Causes transposition along NW-SE diagonal
.VARIABLE LINE
INTEGER - Number of lines above and below DC to be averaged
.VARIABLE DC
INTEGER - Number of lines around DC to be excluded from averaging
.LEVEL2
.VARIABLE INP
Input fft(s) produced by fft2, fft22, or Rfft2 in halfword or byte format.
.VARIABLE OUT
Rearranged output fft(s).
.VARIABLE NLW
Dimension of fft in pixels; each fft must be square.

Default is NLW is the smaller of NL and NS.
.VARIABLE FLIP
'FLIP causes the program, after rearranging each fft, to perform a 
transposition along the upper-left to lower-right diagonal.
.VARIABLE LINE
LINE specifies the number of lines above and below DC that are to be averaged
together as a form of pseudo apodizing.  

The default is 0, i.e. no averaging is performed.
.VARIABLE DC
DC specifies the number of lines surrounding DC that are to be excluded from
the 'LINEs' averaging.  This is ignored if LINE=0.

The default is 1, that is, only the DC line is excluded.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftflip.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

local   afidsroot   type=string count=1
! Sep 18, 2013 - RJB
! TEST SCRIPT FOR FFTFLIP 
! tests BYTE, HALF, FULL and REAL images
!
! Vicar Programs:
!   gen label-list insect sargonb difpic xvd
!
! External programs
!       <none>
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!           
!
! Requires NO external test data: 
!
! DATA ORIG FROM CEP946.FFT.TEST - 
!     SAME DATA AS USED FOR FFTCLAS
!            
! Output: 

refgbl $autousage
refgbl $echo
   
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
!
!  TEST 1 - Byte input data
!
gen A 512 512  linc=1 sinc=3 format=byte
gen B 256 256  linc=1 sinc=-2 ival=200 format=byte
gen C 256 256  linc=1 sinc=5 ival=10 format=byte modulo=112

!                     (B,B,  B,  B,A,  A)
insect (A,B) W insect=(1,1,256,256,1,257)
insect (W,C) X insect=(1,1,256,256,257,1)
sargonb X  B1 func1=setto const1=1000 vert1=(255,255,257,255,257,257,255,257)
label-list B1

fftflip B1 C1 'flip line=0 

fftflip B1 D1 line=5 

fftflip B1 E1 dc=7

  difpic (B1,C1) BC1
  difpic (B1,D1) BD1
  difpic (B1,E1) BE1
  difpic (D1,E1) DE1

if (mode = "nobatch" or mode = "inter")
  let $echo="no"
 xvd B1
 xvd C1
 xvd D1
 xvd E1
 xvd BE1
 xvd DE1

end-if 
let $echo="yes"
!
!  TEST 2 - Half input data
!
gen A 512 512  linc=1 sinc=3 format=half
gen B 256 256  linc=1 sinc=-2 ival=500 format=half
gen C 256 256  linc=1 sinc=5 ival=20 format=half modulo=412

!                     (B,B,  B,  B,A,  A)
insect (A,B) W insect=(1,1,256,256,1,257)
insect (W,C) X insect=(1,1,256,256,257,1)
sargonb X  B2 func1=setto const1=1000 vert1=(255,255,257,255,257,257,255,257)
label-list B2

fftflip B2 C2 'flip line=0 

fftflip B2 D2 line=5 

fftflip B2 E2 dc=7

  difpic (B2,C2)
  difpic (B2,D2)
  difpic (B2,E2)
  difpic (B2,E2) BE2
  difpic (D2,E2) DE2

if (mode = "nobatch" or mode = "inter")
  let $echo="no"
 xvd B2
 xvd C2
 xvd D2
 xvd E2
 xvd BE2
 xvd DE2

end-if

let $echo="yes"
!
!  TEST 3 - Full input data type
!
gen A 512 512  linc=1 sinc=3 format=full  
gen B 256 256  linc=1 sinc=-2 ival=500 format=full 
gen C 256 256  linc=1 sinc=5 ival=20 format=full modulo=412

!                     (B,B,  B,  B,A,  A)
insect (A,B) W insect=(1,1,256,256,1,257)
insect (W,C) X insect=(1,1,256,256,257,1)
sargonb X  B3  func1=setto const1=10000 vert1=(255,255,257,255,257,257,255,257)
!label-list B3

fftflip B3 C3 'flip line=0 

fftflip B3 D3 line=5  

fftflip B3 E3 dc=7

  difpic (B3,C3) BC3
  difpic (B3,D3) BD3
  difpic (B3,E3) BE3
  difpic (D3,E3) DE3

if (mode = "nobatch" or mode = "inter")
  let $echo="no"
 xvd B3
 xvd C3
 xvd D3
 xvd E3
 xvd BE3
 xvd DE3
end-if
  let $echo="yes"
!
!  TEST 4 - Real Input data type
!
gen A 512 512  linc=1 sinc=3 format=real
gen B 256 256  linc=1 sinc=-2 ival=500 format=real
gen C 256 256  linc=1 sinc=5 ival=20 format=real modulo=412

!                     (B,B,  B,  B,A,  A)
insect (A,B) W insect=(1,1,256,256,1,257)
insect (W,C) X insect=(1,1,256,256,257,1)
sargonb X  B4  func1=setto const1=10000 vert1=(255,255,257,255,257,257,255,257)

label-list B4

fftflip B4 C4 'flip line=0

fftflip B4 D4 line=5

fftflip B4 E4 dc=7

  difpic (B4,C4) BC4
  difpic (B4,D4) BD4
  difpic (B4,E4) BE4
  difpic (D4,E4) DE4

if (mode = "nobatch" or mode = "inter")
  let $echo="no"
 xvd B4
 xvd C4
 xvd D4
 xvd E4
 xvd BE4
 xvd DE4

end-if


let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstfftflip.log
tstfftflip
gen A 512 512  linc=1 sinc=3 format=byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B 256 256  linc=1 sinc=-2 ival=200 format=byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C 256 256  linc=1 sinc=5 ival=10 format=byte modulo=112
Beginning VICAR task gen
GEN Version 6
GEN task completed
insect (A,B) W insect=(1,1,256,256,1,257)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
insect (W,C) X insect=(1,1,256,256,257,1)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
sargonb X  B1 func1=setto const1=1000 vert1=(255,255,257,255,257,257,255,257)
Beginning VICAR task sargonb
SARGONB - Apr 20, 2014 - rjb (64-bit)
label-list B1
Beginning VICAR task label
************************************************************
 
        ************  File B1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                512 lines per band
                512 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:16:10 2016 ----
IVAL=0.0
SINC=3.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:10 2016 ----
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:10 2016 ----
---- Task: SARGONB -- User: rjb -- Tue Jul 26 23:16:10 2016 ----
 
************************************************************
fftflip B1 C1 'flip line=0
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B1 D1 line=5
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B1 E1 dc=7
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
  difpic (B1,C1) BC1
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  74.3229
 NUMBER OF POS DIFF=     129263
 AVE VAL OF NEG DIFFS= -74.3229
 NUMBER OF NEG DIFFS=     129263
 TOTAL NUMBER OF DIFFERENT PIXELS=     258526
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  98.6198
  difpic (B1,D1) BD1
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  86.4544
 NUMBER OF POS DIFF=      67165
 AVE VAL OF NEG DIFFS= -87.4239
 NUMBER OF NEG DIFFS=      66441
 TOTAL NUMBER OF DIFFERENT PIXELS=     133606
 AVE VAL OF DIFFS=-0.693893E-02
 % DIFF PIXELS=  50.9666
  difpic (B1,E1) BE1
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  88.3173
 NUMBER OF POS DIFF=      65541
 AVE VAL OF NEG DIFFS= -88.3173
 NUMBER OF NEG DIFFS=      65541
 TOTAL NUMBER OF DIFFERENT PIXELS=     131082
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  50.0038
  difpic (D1,E1) DE1
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  30.6682
 NUMBER OF POS DIFF=2372
 AVE VAL OF NEG DIFFS= -26.3470
 NUMBER OF NEG DIFFS=2692
 TOTAL NUMBER OF DIFFERENT PIXELS=5064
 AVE VAL OF DIFFS= 0.693893E-02
 % DIFF PIXELS=  1.93176
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="yes"
gen A 512 512  linc=1 sinc=3 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B 256 256  linc=1 sinc=-2 ival=500 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C 256 256  linc=1 sinc=5 ival=20 format=half modulo=412
Beginning VICAR task gen
GEN Version 6
GEN task completed
insect (A,B) W insect=(1,1,256,256,1,257)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
insect (W,C) X insect=(1,1,256,256,257,1)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
sargonb X  B2 func1=setto const1=1000 vert1=(255,255,257,255,257,257,255,257)
Beginning VICAR task sargonb
SARGONB - Apr 20, 2014 - rjb (64-bit)
label-list B2
Beginning VICAR task label
************************************************************
 
        ************  File B2 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                512 lines per band
                512 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:16:11 2016 ----
IVAL=0.0
SINC=3.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:11 2016 ----
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:11 2016 ----
---- Task: SARGONB -- User: rjb -- Tue Jul 26 23:16:11 2016 ----
 
************************************************************
fftflip B2 C2 'flip line=0
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B2 D2 line=5
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B2 E2 dc=7
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
  difpic (B2,C2)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     261226
  difpic (B2,D2)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     262143
  difpic (B2,E2)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     262144
  difpic (B2,E2) BE2
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  617.431
 NUMBER OF POS DIFF=     131072
 AVE VAL OF NEG DIFFS= -617.431
 NUMBER OF NEG DIFFS=     131072
 TOTAL NUMBER OF DIFFERENT PIXELS=     262144
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  100.000
  difpic (D2,E2) DE2
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  254.477
 NUMBER OF POS DIFF=3381
 AVE VAL OF NEG DIFFS= -273.132
 NUMBER OF NEG DIFFS=1705
 TOTAL NUMBER OF DIFFERENT PIXELS=5086
 AVE VAL OF DIFFS=  1.50565
 % DIFF PIXELS=  1.94016
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="yes"
gen A 512 512  linc=1 sinc=3 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B 256 256  linc=1 sinc=-2 ival=500 format=full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C 256 256  linc=1 sinc=5 ival=20 format=full modulo=412
Beginning VICAR task gen
GEN Version 6
GEN task completed
insect (A,B) W insect=(1,1,256,256,1,257)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
insect (W,C) X insect=(1,1,256,256,257,1)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
sargonb X  B3  func1=setto const1=10000 vert1=(255,255,257,255,257,257,255,257)
Beginning VICAR task sargonb
SARGONB - Apr 20, 2014 - rjb (64-bit)
fftflip B3 C3 'flip line=0
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B3 D3 line=5
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B3 E3 dc=7
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
  difpic (B3,C3) BC3
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  612.393
 NUMBER OF POS DIFF=     130613
 AVE VAL OF NEG DIFFS= -612.393
 NUMBER OF NEG DIFFS=     130613
 TOTAL NUMBER OF DIFFERENT PIXELS=     261226
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  99.6498
  difpic (B3,D3) BD3
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  619.492
 NUMBER OF POS DIFF=     130197
 AVE VAL OF NEG DIFFS= -614.272
 NUMBER OF NEG DIFFS=     131946
 TOTAL NUMBER OF DIFFERENT PIXELS=     262143
 AVE VAL OF DIFFS= -1.50565
 % DIFF PIXELS=  99.9996
  difpic (B3,E3) BE3
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  617.985
 NUMBER OF POS DIFF=     131072
 AVE VAL OF NEG DIFFS= -617.985
 NUMBER OF NEG DIFFS=     131072
 TOTAL NUMBER OF DIFFERENT PIXELS=     262144
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  100.000
  difpic (D3,E3) DE3
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  254.477
 NUMBER OF POS DIFF=3381
 AVE VAL OF NEG DIFFS= -273.132
 NUMBER OF NEG DIFFS=1705
 TOTAL NUMBER OF DIFFERENT PIXELS=5086
 AVE VAL OF DIFFS=  1.50565
 % DIFF PIXELS=  1.94016
if (mode = "nobatch" or mode = "inter")
end-if
  let $echo="yes"
gen A 512 512  linc=1 sinc=3 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B 256 256  linc=1 sinc=-2 ival=500 format=real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen C 256 256  linc=1 sinc=5 ival=20 format=real modulo=412
Beginning VICAR task gen
GEN Version 6
GEN task completed
insect (A,B) W insect=(1,1,256,256,1,257)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
insect (W,C) X insect=(1,1,256,256,257,1)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
sargonb X  B4  func1=setto const1=10000 vert1=(255,255,257,255,257,257,255,257)
Beginning VICAR task sargonb
SARGONB - Apr 20, 2014 - rjb (64-bit)
label-list B4
Beginning VICAR task label
************************************************************
 
        ************  File B4 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                512 lines per band
                512 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Tue Jul 26 23:16:12 2016 ----
IVAL=0.0
SINC=3.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:13 2016 ----
---- Task: INSECT -- User: rjb -- Tue Jul 26 23:16:13 2016 ----
---- Task: SARGONB -- User: rjb -- Tue Jul 26 23:16:13 2016 ----
 
************************************************************
fftflip B4 C4 'flip line=0
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B4 D4 line=5
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
fftflip B4 E4 dc=7
Beginning VICAR task fftflip
** FFTFLIP version 25-JUL-2016 - (64-bit) rjb
  difpic (B4,C4) BC4
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  612.393
 NUMBER OF POS DIFF=     130613
 AVE VAL OF NEG DIFFS= -612.393
 NUMBER OF NEG DIFFS=     130613
 TOTAL NUMBER OF DIFFERENT PIXELS=     261226
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  99.6498
  difpic (B4,D4) BD4
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  619.484
 NUMBER OF POS DIFF=     130197
 AVE VAL OF NEG DIFFS= -614.275
 NUMBER OF NEG DIFFS=     131947
 TOTAL NUMBER OF DIFFERENT PIXELS=     262144
 AVE VAL OF DIFFS= -1.51353
 % DIFF PIXELS=  100.000
  difpic (B4,E4) BE4
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  617.985
 NUMBER OF POS DIFF=     131072
 AVE VAL OF NEG DIFFS= -617.985
 NUMBER OF NEG DIFFS=     131072
 TOTAL NUMBER OF DIFFERENT PIXELS=     262144
 AVE VAL OF DIFFS=  0.00000
 % DIFF PIXELS=  100.000
  difpic (D4,E4) DE4
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 AVE VAL OF POS DIFFS=  254.165
 NUMBER OF POS DIFF=3390
 AVE VAL OF NEG DIFFS= -272.642
 NUMBER OF NEG DIFFS=1705
 TOTAL NUMBER OF DIFFERENT PIXELS=5095
 AVE VAL OF DIFFS=  1.51353
 % DIFF PIXELS=  1.94359
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
