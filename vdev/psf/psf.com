$!****************************************************************************
$!
$! Build proc for MIPL module psf
$! VPACK Version 1.9, Tuesday, August 16, 2016, 19:38:23
$!
$! Execute by entering:		$ @psf
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
$ write sys$output "*** module psf ***"
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
$ write sys$output "Invalid argument given to psf.com file -- ", primary
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
$   if F$SEARCH("psf.imake") .nes. ""
$   then
$      vimake psf
$      purge psf.bld
$   else
$      if F$SEARCH("psf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake psf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @psf.bld "STD"
$   else
$      @psf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create psf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack psf.com -mixed -
	-s psf.f -
	-i psf.imake -
	-p psf.pdf -
	-t tstpsf.pdf tstpsf.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create psf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C  95-1-2  ...AS....  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C  85-3-28 ...LWK...  CONVERTED TO VAX VICAR2, ADDED STACKA CALL
C                      & 'SHIFT' KEYWORD
      SUBROUTINE MAIN44
c      IMPLICIT INTEGER (A-Z)
	implicit none
	integer*4 iun,istat,sl,ss,nlo,nso,nl,ns,cnt,def,icode
      	INTEGER*4 AREA(4),insize,psfsize
        integer*8 nb1,nb2
      	EXTERNAL WORK
c
       	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*8 format
C
      CALL IFMESSAGE('PSF version Jul 15, 2016 (64-bit) - rjb')
      CALL XVEACTION('SA',' ')

      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
        call xvopen(iun,istat,'OP', 'READ','OPEN_ACT','SA','IO_ACT','SA',' ')
	call xvsize( sl, ss, nlo, nso, nl, ns)
        call xvget(iun,istat,'FORMAT',format,' ')

        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        call xvclose(iun,istat,' ')

        call xvopen(iun,istat,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')            !FMT(INCODE),' ')

      CALL XVPARM( 'AREA', AREA, CNT, DEF, 4)	! (NO DEFAULT ALLOWED)
      IF (AREA(3).EQ.(AREA(3)/2)*2) THEN
	AREA(3)=AREA(3)+1
	CALL XVMESSAGE('NL OF AREA INCREASED BY 1',' ')
      ENDIF
      IF (AREA(4).EQ.(AREA(4)/2)*2) THEN
	AREA(4)=AREA(4)+1
	CALL XVMESSAGE('NS OF AREA INCREASED BY 1',' ')
      ENDIF
C     pixsize=4
      insize = 4*MAX0( NS, NSO)
      NB1 = int8(insize)		        ! 'IN' BUFFER SIZE (IN BYTES)
      psfsize = 4*AREA(4)
      NB2 = int8(psfsize)			! PSF BUFFER SIZE (IN BYTES)
c	12 parms, name, 2 arrays, arr1 size, arr2 size, other variables...
      CALL STACKA( 13,WORK, 2, NB1, NB2, NL, NS, NLO, NSO, AREA, IUN, 
     . insize, psfsize,icode)

      RETURN
      END

C*****************************************************************
      SUBROUTINE WORK( IN, NB1, PSF, NB2, NL, NS, NLO, NSO, AREA, IUN, 
     . insize, psfsize, icode)
c      IMPLICIT INTEGER (A-Z)
	implicit none
        integer*4  insize,psfsize
      	integer*8 nb1,nb2
      	REAL*4 IN( insize), PSF(psfsize)
      	INTEGER*4 AREA(4)
	integer*4 nl,ns,nlo,nso,iun,icode,j,l,n
	integer*4 icenx,iceny,ilin,ir,lb
	integer*4 il,lt,nlin,nlin2,nlinl,npix,nsa2,nsal
	integer*4 olin,oun,istat
      	LOGICAL*4 XVPTST,negval
	real*4 mean, xn, limit
      	REAL*4 XDN, CENX, CENY
      	CHARACTER*4 format
c
       	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/

	print *,'NB1,NB2, = ',nb1,nb2

	negval = .false.
        LIMIT = 255.
	format = fmt(icode)
      if (format.eq.fmt(1)) then
	LIMIT = 255.
      else if (format .eq. fmt(2)) then
	LIMIT = 32767.
      else if (format .eq. fmt(3)) then
	LIMIT = 65535.
      else if (format .eq. fmt(4)) then
	LIMIT = 3.4028234e38
      endif

      LT = AREA(1)
      LB = AREA(3)+LT-1
      IL = AREA(2)
      IR = AREA(4)+IL-1
      IF (LB.GT.NL) LB=NL
      IF (IR.GT.NS) IR=NS
      IF (IL.GT.NS .OR. LT.GT.NL) THEN
	CALL MABEND('??E - AREA IS OUTSIDE INPUT IMAGE **')
      ENDIF
      NLIN=LB-LT+1
      NPIX=IR-IL+1
      NLIN2=NLIN/2
      NLINL=NLIN-NLIN2
      NSA2=NPIX/2
      NSAL=NPIX-NSA2

      CALL XVUNIT( OUN, 'OUT', 1, istat,' ')
      CALL XVOPEN( OUN, istat, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS',
     .	NSO, 'U_FORMAT', fmt(4), 'O_FORMAT',fmt(icode), ' ')

C  READ BORDER POINTS AND GET BKG DN
C
      N=0
      MEAN=0.0
      CALL XVREAD( IUN, IN, istat, 'LINE', LT,' ')
      DO L=LT,LB
	IF (L.EQ.LT .OR. L.EQ.LB) THEN
	  DO J=IL,IR
	    N=N+1
	    MEAN=MEAN+IN(J)
	    if (IN(J).lt.0) negval=.true.
	  ENDDO
	ELSE
	  N=N+2
	  if (IN(IL).lt.0 .or. IN(IR).lt.0) negval=.true.
	  MEAN=MEAN+IN(IL)+IN(IR)
	ENDIF
	IF (L.LT.LB) CALL XVREAD( IUN, IN, istat,' ')
      ENDDO
      MEAN=NINT(MEAN/FLOAT(N))
      CALL PRNT(7,1,MEAN,'BACKGROUND DN =')
        if (negval) then
           call xvmessage ('??W - WARNING: Image contains negative values',' ' )
        endif
C
C  DETERMINE CENTROID - negative values are zeroed 
C
      XDN = 0.0
      CENX = 0.0
      CENY = 0.0
      xn = 0.0
      CALL XVREAD( IUN, IN, istat, 'LINE', LT,' ')
      DO L=LT,LB
	DO J=IL,IR
	  XN = IN(J)-MEAN
	  IF (XN.LT.0.0) XN=0.0
	  XN = XN*XN
	  XDN = XDN+XN
	  CENX = CENX+FLOAT(J-IL)*XN
	  CENY = CENY+FLOAT(L-LT)*XN
	ENDDO
	IF (L.LT.LB) CALL XVREAD( IUN, IN, istat,' ')
      ENDDO
      ICENX = CENX/XDN+IL+.5	! Round up
      ICENY = CENY/XDN+LT+.5
      CALL PRNT(4,1,ICENX,'X CENTROID =')
      CALL PRNT(4,1,ICENY,'Y CENTROID =')
C
C  SET UP AREA TO CONTAIN PSF
C IF 'SHIFT' WAS SPECIFIED, RETAIN ORIGINAL SIZE, MOVE CENTER.
C ELSE RETAIN ORIGINAL AREA & QUARTER THE PSF UNEQUALLY.
C **> (MAY BE BETTER TO REDUCE SIZE & QUARTER SYMMETRICALLY!)
      IF (XVPTST( 'SHIFT')) THEN
	LB=ICENY+NLIN2		! BOTTOM LINE OF PSF
	LT=ICENY-NLIN2		! TOP LINE OF PSF
	IR=ICENX+NSA2		! RIGHT EDGE OF PSF
	IL=ICENX-NSA2		! LEFT EDGE OF PSF
	IF (LB.GT.NL) LB=NL
	IF (IR.GT.NS) IR=NS
	IF (LT.LT.1) LT=1
	IF (IL.LT.1) IL=1
	NLIN=LB-LT+1		! NL OF PSF
	NPIX=IR-IL+1		! NS OF PSF
	NSA2=NPIX/2
	NLIN2=NLIN/2
      ELSE
	NLIN2 = ICENY-LT+1
	NSA2 = ICENX-IL+1
      ENDIF
      NLINL=NLIN-NLIN2
      NSAL=NPIX-NSA2
C
C  COPY DATA TO OUTPUT AND SUBTRACT BACKGROUND
C
      N=0
      DO OLIN = 1, NLO			! OUTPUT LINE NUMBER
	CALL MVE(7,NSO,0,IN,0,1)	! ZERO THE OUTPUT BUFFER
	call mve(7,NB2/4,0,PSF,0,1)		! ZERO THE PSF
	ILIN = 0			! ZERO THE INPUT LINE NUMBER
	IF (OLIN.LE.NLINL)
     .	 ILIN = OLIN+LT-1+NLIN2  	! BOTTOM OF PSF
	IF (OLIN.GE.NLO-NLIN2+1)
     .	 ILIN = OLIN-NLO+NLIN2+LT-1	! TOP OF PSF
	IF (ILIN.NE.0) THEN
	  CALL XVREAD( IUN, PSF, istat, 'LINE', ILIN, 'SAMP', IL,
     .	   'NSAMPS', NPIX,' ')		! READ THE PSF AREA
	  CALL SUBV(7,NPIX,MEAN,PSF,0,1) ! SUBTRACT BACKGROUND
	  CALL TRUNC(PSF,0.0,LIMIT,NPIX)
	  CALL MVE(7,NSA2,PSF,IN(NSO-NSA2+1),1,1) ! LEFT SIDE OF PSF
	  CALL MVE(7,NSAL,PSF(NSA2+1),IN,1,1)	! RIGHT SIDE OF PSF
	ENDIF

	CALL XVWRIT( OUN, IN, istat,' ')
      ENDDO

	call outparm (negval)
      RETURN
      END
c==============================================================
	subroutine outparm (negval)
c
c	return output parameter negative
c
      include 'pgminc'            ! FOR XQINI...

	integer*4 istat, negative
	integer*4 PARB(xprdim)
	logical*4 negval
c
	negative = 0
	if (negval) negative = 1	
	call xqini(parb, xprdim, xabort)
	call xqintg(parb,'NEGATIVE',1,negative,xadd,istat)      
	call xqout(parb,istat)
	return
	end
c==========================================================
      SUBROUTINE TRUNC( BUF, LO, HI, N)
C  TRUNCATE N BUF VALUES TO RANGE (LO, HI).
      IMPLICIT none 
      integer*4 i,n
      real*4 lo,hi,BUF(1)


      DO I=1,N
	IF (BUF(I).LT.LO) BUF(I) = LO
	IF (BUF(I).GT.HI) BUF(I) = HI
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create psf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM psf

   To Create the build file give the command:

		$ vimake psf			(VMS)
   or
		% vimake psf			(Unix)


************************************************************************/


#define PROGRAM	psf
#define R2LIB

#define MODULE_LIST psf.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create psf.pdf
process help=*
PARM INP    STRING      COUNT=1
PARM OUT    STRING      COUNT=1
PARM SIZE   INTEGER     COUNT=4 DEFAULT=(1,1,0,0)
PARM SL     INTEGER     COUNT=1 DEFAULT=1
PARM SS     INTEGER     COUNT=1 DEFAULT=1
PARM NL     INTEGER     COUNT=1 DEFAULT=0
PARM NS     INTEGER     COUNT=1 DEFAULT=0
PARM AREA   INTEGER     COUNT=4 
PARM SHIFT  KEYWORD     VALID=(SHIFT,NOSHIFT) DEFAULT=SHIFT
local neg   INTEGER     INITIAL=0
PARM NEGATIVE  NAME     DEFAULT=neg

END-PROC
.TITLE
VICAR Program "psf"
.HELP
PURPOSE

Determine point spread function from image

OPERATION

This program copies a point spread function (PSF) contained in the
input image to an output image in a format suitable for the production
of the optical transfer function (OTF) by program "fft22".  The
program was designed to be used in the procedure "restore", which deconvolves
the point spread function from an image, but can also be used in stand-
alone mode.

The input image is normally a star or the output of an appropriate test target
from a camera test.

The AREA parameter restricts where in the image the psf object occurs. For
example, if the PSF is from a starfield there may be other stars in the field
you wish to ignore. Similarly the output from a camera calibration test target
may have a border or other types of targets in the field.
.page
EXECUTION FORMAT:

  psf  IN  OUT  SIZE  AREA  SHIFT

where:	IN	is the input image.
	OUT	is the output point spread function.
	SIZE	is the size of the output file, and should be
		  the same dimension as the image to which the
		  OTF will be applied.
	AREA	is a required parameter defining the location of
		  the point spread function in the input image.
	SHIFT   is an optional parameter determining whether the
		  PSF area will be allowed to shift in the input. SHIFT
          is like what FFTFLIP does for FFTs.
.page
METHOD

First, the program determines the mean of the DNs lying on the border
of the region defined by the AREA parameter.  This mean is used as the
background DN value.  The subimage of size determined by AREA minus the
background value is the PSF.

Next, the program determines the centroid of the PSF.  The subimage is
quartered about the pixel closest to the centroid and is copied into
the four corners of the output data set.  (This process is intended to
eliminate translation of an image when deconvolved with the point spread
function, although subpixel translations will still be present.)  The
rest of the output file is left empty (0 DN).  

The program works with BYTE, HALF, FULL and REAL formats. Its output
format is the same as the input.  

It warns you if negative numbers are found in the image. If so, it
returns a value of 1 in the NEGATIVE return parameter.

RESTRICTIONS

    For REAL images negative values are converted to 0. So centroid is
only computed from positive values. If it is important, you may convert 
images with negative values to all positive by MAXMIN and F2 before
processing with PSF.

.page
HISTORY:

Written by:  J.J.Lorre,  1 Jan. 1978
Converted to VAX by:  L.W.Kamp,  29 Mar. 1985
Current Cognizant Programmer:  Ray Bambery
Revisions:

    2 Jan 1995 - Made portable for UNIX A.Scop (CRI) 
   29 Dec 2012 - RJB - Fixes for 64-bit Linux under gfortran 4.7.2
   24 Aug 2013 - Ray Bambery - Fixed crash when used with BYTE images
                Got message with BYTE images:
*** glibc detected *** /data/rjb/ada/src/psf: free(): invalid next size (fast): 0x00000000011deeb0 ***
======= Backtrace: =========
/lib64/libc.so.6[0x36e8275916]
                Program now accepts BYTE, HALF, FULL and REAL images
                Added NEGATIVE return parameter.
   20 Apr 2014 - Ray Bambery - Fixed uninitialized variable that caused problems
   14 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.vari INP
Input image
.vari OUT
Output PSF
.vari SIZE
Size of output file.
 = (SL,SS,NL,NS)
.vari SL
Starting line
(always 1)
.VARI SS
Starting sample
(always 1)
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of samples
.vari AREA
Area containing PSF.
.VARI SHIFT
Shift PSF about new
centroid?
.VARI NEGATIVE
A return parameter. If 1 then
negative numbers were found in
image.
.LEVEL2
.vari INP
INP is the name of the image file from which the PSF will be extracted,
using the AREA parameter.
.vari OUT
OUT is the name of the output file into which the properly formatted PSF
will be written.
.vari SIZE
SIZE is the standard Vicar2 parameter containing:
 
  (Starting Line, Starting Sample, Number of Lines, Number of Samples)
 
Note that in this program it refers only to the output file, and therefore
the first two elements are ignored.  This parameter is included only for
unformity in Vicar2 application program interface.
.vari SL
See HELP SIZE.
.vari SS
See HELP SIZE.
.VARIABLE NL
NL specifies the size of the image in the line direction, i.e., the
number of lines in the image.
.VARIABLE NS
NS specifies the size of the image in the sample direction, i.e.,
the number of pixels per line in the image.
.vari AREA
AREA specifies the area in the input file (INP) from which the PSF will be
extracted, after subtraction of the background DN value.  It has the
same structure as the SIZE parameter, i.e.:
 
  (Starting Line, Starting Sample, Number of Lines, Number of Samples).
 
Note that the last two elements should be less than or equal to the
corresponding elements in SIZE (or NL and NS) for meaningful results.

If NOSHIFT is specified, then the final PSF will remain strictly inside
this area after determination of the centroid.  Otherwise, the final
PSF will be of the same size as specified by AREA, but centered on the
centroid of the function.
.vari SHIFT
This parameter controls whether the PSF will be allowed to shift outside
the area specified by the AREA parameter after determination of its
centroid.

If SHIFT is specified, then the final PSF will be of the size specified 
by the AREA parameter, but centered on the centroid of the function. (If
part falls outside the image, the size is reduced accordingly.)  This is
the normal choice when the user does know the exact location of the PSF
in the input.

If NOSHIFT is specified, then the area of the PSF remains that specified
by AREA, but the quartering is done about the centroid, so that the four
quarters may be unequal.  This choice is useful if the location of the
PSF in the input is accurately known and it is important that data
outside this area be excluded.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpsf.pdf
procedure
parm    mode    type=string count=(0:1) valid=(batch,nobatch,inter) default=batch
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

LOCAL   INPIC       TYPE=STRING count=1
! Aug 23, 2013 - RJB
! TEST SCRIPT FOR PSF
! tests BYTE, HALF, FULL and REAL images
!
! Vicar Programs:
!       translog, label-list list spot maxmin f2 cform
!       qplot2
! 
! External programs
!       gnuplot 4.6.x
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no display but creates .eps files
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!
!   In batch mode it produces files testx.eps by calling gnuplot
!       to create the encapsulated postscript file which can be
!       later viewed with ghostscript or gimp
!   In interactive or nobatch mode gnuplot is called with a window
!       manager for X11. The gnuplot display is killed by
!       a mouse click anywhere on the plot panel
!
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 

refgbl $echo
refgbl $autousage
!PARM DIR TYPE=STRING DEFAULT="MIPL:[MIPL.VGR]"
!LOCAL INPIC TYPE=STRING
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
!MIPL        
    ush ln -s /project/test_work/testdata/mipl/vgr vg
else
!CARTLAB     
    if (aftestdata = "")
        ush ln -s ../test_data/vicar_test_images/testdata/mipl/vgr vg
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/mipl/vgr vg
    end-if
end-if
let $echo="no"
let _onfail="goto rm"
write "This is a test file for program PSF"
write "A single star image from voyager ISS"
let $echo="yes"
let INPIC= "vg/star.img"

! TEST 1 - Star from the Voyager Spacecraft camera

label-li &INPIC
maxmin &inpic
psf inp=&INPIC out=vgrpsf.vic size=(1,1,50,50) AREA=(10,10,30,30) SHIFT=SHIFT
list vgrpsf.vic (1,1,20,20)

! TEST 2 - PSF from BYTE IMAGE
 
spot   psf1.vic size=(1,1,50,50) shape=gaussian sigmax=5 sigmay=5 dnmax=50
psf  psf1.vic psf1.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift


! TEST 3 - PSF from HALF IMAGE

cform psf1.vic psfh1.vic oform=half
psf  psfh1.vic psf1h.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
list  psf1h.out (1,1,1,20)

! TEST 4 - PSF from FULL IMAGE

spot   psf2.vic size=(1,1,50,50) shape=gaussian sigmax=10 sigmay=10 dnmax=120
cform psf2.vic psff2.vic oform=full
f2 psff2.vic psff2a.vic func=(in1+5000)
psf psff2a.vic psff2a.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
list psff2a.out (1,1,1,20)

! TEST 5 - PSF from increased area

psf psff2a.vic psff2b.out size=(1,1,50,50) AREA=(6,6,44,44) shift=shift
list psff2b.out (1,1,1,20)

qplot2 psff2b.out proc=(1,1,1,1,25, 1,1,1,25,1) +
      title="PSF Test - Line1: Horiz PSF   Line2: Vertical PSF" +
      plotout=psff2b  

if (mode = "nobatch" or mode = "inter") 
    ush gnuplot psff2b.gpi
end-if


! TEST 6 - PSF from REAL IMAGE

cform psf2.vic psfr2.vic oform=real
f2 psfr2.vic psfr2a.vic func=(in1+5000)
psf psfr2a.vic psfr2a.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
list psfr2a.out (1,1,1,20)

! TEST 7 - OFFSET PSF

spot   psf3.vic size=(1,1,50,50) shape=gaussian sigmax=5 sigmay=5 dnmax=120 x0=10 y0=10  
psf   psf3.vic psf3.out size=(1,1,50,50) area=(1,1,19,19) shift=shift

! TEST 8 - UNSYMMETRICAL PSF

spot psf4.vic size=(1,1,50,50) shape=gaussian sigmax=10 sigmay=12 dnmax=120
psf psf4.vic psf4.out size=(1,1,50,50) AREA=(6,6,44,44) shift=shift

qplot2 psf4.out proc=(1,1,1,1,25, 1,1,1,25,1) +
    title="PSF Test - Line1: Horiz PSF   Line2: Vertical PSF" +
    plotout=psf4

if (mode = "nobatch" or mode = "inter")
    ush gnuplot psf4.gpi
end-if

rm>
let  $echo="no" 
ush rm vg
end-proc
$!-----------------------------------------------------------------------------
$ create tstpsf.log
tstpsf
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
else
    if (aftestdata = "")
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/mipl/vgr vg
    end-if
end-if
let $echo="no"
This is a test file for program PSF
A single star image from voyager ISS
let INPIC= "vg/star.img"
label-li vg/star.img
Beginning VICAR task label
************************************************************
 
        ************  File vg/star.img ************
                2 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a VAX-VMS host
                50 lines
                50 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: CONVIM -- User: LWK059 -- Thu Apr 11 11:31:57 1985 ----
LAB01=
'77                    50     100  50 100 I 2                          SC'
LAB02=
'PROJ = SATELLITE SEARCH    OBJECT = NEPTUNE                           UC'
LAB03=
'OBS DATE = 25 MAY  83 UT   CENTRAL WAVELENGTH = 8900A  WIDTH = 400A   UC'
LAB04=
'OBS TIME =    02:24   UT   EXP TIME= 60 SEC   INPUT TAPE= LCO8306     UC'
LAB05=
'INST = LAS CAMPANAS OBS   2.4M TELESCOPE  WITH CCD DETECTOR           UC'
LAB06=
'I4DIV                                                                1HL'
NLABS=6
 
************************************************************
maxmin vg/star.img
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:          -19   at  (    41,    10)
Max. value:         2929   at  (    26,    26)

psf inp=vg/star.img out=vgrpsf.vic size=(1,1,50,50) AREA=(10,10,30,30) SHIFT=SHIFT
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
NL OF AREA INCREASED BY 1
NS OF AREA INCREASED BY 1
BACKGROUND DN =  89.000000
??W - WARNING: Image contains negative values
X CENTROID =         26
Y CENTROID =         27
list vgrpsf.vic (1,1,20,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:CONVIM    User:LWK059    Date_Time:Thu Apr 11 11:31:57 1985
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:31 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1      2759  2525  2015  1391   902   548   290   216    70    25    42    28     0     0     9
      2      2057  1884  1502  1099   727   429   194   183   115    17    25    33     0     5    24
      3      1243  1164   915   713   526   316   212   137   112   107    51    10     0     0    22
      4       595   611   589   424   294   277   105    56    69    25    38     0     0    13     2
      5       400   337   342   338   209   157    90    15   105    51     0    20    69    35     0
      6       183   216   140   145   150   104   113     2    37    17     7     0     0     2     0
      7       130    59    55    56   153    71    52    37     8    18    16    30     4     3     0
      8        29   119    32   136    95    40    53     0     0    34     0     0     0    27    12
      9        65     5    34    88    11     0    89    27     5     0     1    66    23     9     0
     10        26    28    32     0    40    30     0     0    23     0     0     8     0     0     1
     11        35    38     2    50    12    19    13     0    21     0     0     0     0     0     0
     12         0    52     0    33     0     0    39     5     1     0    29    24     0     8     0
     13         0    20     3    25    11     0     0     4     0     0     0     0    20    29    27
     14         0     0     1     0     0     0    33     0     0     0     0    30    19     6     0
     15        24     3     0    35     0     0     0     0     0    14    23     6     0    27     0
     16         2    23    42     0     0     8    27     0     0    20    55     4     0     0    42

   HALF     samples are interpreted as HALFWORD data
 Task:CONVIM    User:LWK059    Date_Time:Thu Apr 11 11:31:57 1985
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:31 2016
     Samp      16    17    18    19    20
   Line
      1        56     0     0     0     0

      3        16     0     0     0     0

      5         6     0     0     0     0

      9        26     0     0     0     0

     11        22     0     0     0     0
     12        43     0     0     0     0

     15        37     0     0     0     0
     16        12     0     0     0     0
spot   psf1.vic size=(1,1,50,50) shape=gaussian sigmax=5 sigmay=5 dnmax=50
Beginning VICAR task spot
SPOT version 04-Nov-2010 - RJB - 64bit
****GAUSSIAN PATTERN GENERATED
psf  psf1.vic psf1.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
BACKGROUND DN =  2.0000000
X CENTROID =         25
Y CENTROID =         25
cform psf1.vic psfh1.vic oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
psf  psfh1.vic psf1h.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
BACKGROUND DN =  2.0000000
X CENTROID =         25
Y CENTROID =         25
list  psf1h.out (1,1,1,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:31 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:31 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        48    47    44    39    34    28    22    16    11     7     4     2     0     0     0
spot   psf2.vic size=(1,1,50,50) shape=gaussian sigmax=10 sigmay=10 dnmax=120
Beginning VICAR task spot
SPOT version 04-Nov-2010 - RJB - 64bit
****GAUSSIAN PATTERN GENERATED
cform psf2.vic psff2.vic oform=full
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = FULL
CONVERSION COMPLETE
f2 psff2.vic psff2a.vic func=(in1+5000)
Beginning VICAR task f2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 calculating every pixel
FUNCTION EVALUATED 2500 TIMES
psf psff2a.vic psff2a.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
BACKGROUND DN =  5028.0000
X CENTROID =         26
Y CENTROID =         26
list psff2a.out (1,1,1,20)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1             90         89         86         82         77         71         65         58         51         44

   FULL     samples are interpreted as FULLWORD data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1             37         30         23         16         10          5          0          0          0          0
psf psff2a.vic psff2b.out size=(1,1,50,50) AREA=(6,6,44,44) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
NL OF AREA INCREASED BY 1
NS OF AREA INCREASED BY 1
BACKGROUND DN =  5006.0000
X CENTROID =         25
Y CENTROID =         25
list psff2b.out (1,1,1,20)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1            114        113        111        108        104         99         94         87         81         74

   FULL     samples are interpreted as FULLWORD data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1             66         59         52         45         39         32         27         22         17         13
qplot2 psff2b.out proc=(1,1,1,1,25, 1,1,1,25,1)  +
      title="PSF Test - Line1: Horiz PSF   Line2: Vertical PSF"  +
      plotout=psff2b
Beginning VICAR task qplot2
qplot2 version 28-Aug-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
cform psf2.vic psfr2.vic oform=real
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = REAL
CONVERSION COMPLETE
f2 psfr2.vic psfr2a.vic func=(in1+5000)
Beginning VICAR task f2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 calculating every pixel
FUNCTION EVALUATED 2500 TIMES
psf psfr2a.vic psfr2a.out size=(1,1,50,50) AREA=(17,17,33,33) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
BACKGROUND DN =  5028.0000
X CENTROID =         26
Y CENTROID =         26
list psfr2a.out (1,1,1,20)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       9.000E+01   8.900E+01   8.600E+01   8.200E+01   7.700E+01   7.100E+01   6.500E+01   5.800E+01   5.100E+01   4.400E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:SPOT      User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
 Task:PSF       User:rjb       Date_Time:Wed Jul 27 01:08:32 2016
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       3.700E+01   3.000E+01   2.300E+01   1.600E+01   1.000E+01   5.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
spot   psf3.vic size=(1,1,50,50) shape=gaussian sigmax=5 sigmay=5 dnmax=120 x0=10 y0=10
Beginning VICAR task spot
SPOT version 04-Nov-2010 - RJB - 64bit
****GAUSSIAN PATTERN GENERATED
psf   psf3.vic psf3.out size=(1,1,50,50) area=(1,1,19,19) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
BACKGROUND DN =  15.000000
X CENTROID =         10
Y CENTROID =         10
spot psf4.vic size=(1,1,50,50) shape=gaussian sigmax=10 sigmay=12 dnmax=120
Beginning VICAR task spot
SPOT version 04-Nov-2010 - RJB - 64bit
****GAUSSIAN PATTERN GENERATED
psf psf4.vic psf4.out size=(1,1,50,50) AREA=(6,6,44,44) shift=shift
Beginning VICAR task psf
PSF version Jul 15, 2016 (64-bit) - rjb
NL OF AREA INCREASED BY 1
NS OF AREA INCREASED BY 1
BACKGROUND DN =  10.000000
X CENTROID =         25
Y CENTROID =         25
qplot2 psf4.out proc=(1,1,1,1,25, 1,1,1,25,1)  +
    title="PSF Test - Line1: Horiz PSF   Line2: Vertical PSF"  +
    plotout=psf4
Beginning VICAR task qplot2
qplot2 version 28-Aug-2013 (64-bit gnuplot) - rjb
if (mode = "nobatch" or mode = "inter")
end-if
let  $echo="no"
exit
slogoff
$ Return
$!#############################################################################
