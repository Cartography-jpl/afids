$!****************************************************************************
$!
$! Build proc for MIPL module insert
$! VPACK Version 1.9, Wednesday, August 17, 2016, 11:46:49
$!
$! Execute by entering:		$ @insert
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
$ write sys$output "*** module insert ***"
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
$ write sys$output "Invalid argument given to insert.com file -- ", primary
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
$   if F$SEARCH("insert.imake") .nes. ""
$   then
$      vimake insert
$      purge insert.bld
$   else
$      if F$SEARCH("insert.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake insert
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @insert.bld "STD"
$   else
$      @insert.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create insert.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack insert.com -mixed -
	-s insert.f -
	-i insert.imake -
	-p insert.pdf -
	-t tstinsert.pdf tstinsert.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create insert.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    02 MAY 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C   INSERT  -  SOURCE, HELP FILE AND PDF
C
C        'INSERT'   LINE INSERTION PROGRAM
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C      IMPLICIT INTEGER (A-Z)
	implicit none
      LOGICAL*4 FILL,AVG,TRUNC,FIX,NOPRNT
      LOGICAL*4 XVPTST
	integer*4  i,j
	integer*4 dndf,idn,linedf,ndn,nl,nli,nb,nline,npxl
	integer*4 ns,nsi,runit,save,sl,ss,status,tnl,wunit 
      	integer*4 MAXCNT, icode
	integer*8 nbytes
	real*4 linear(100)
        character*4 orgin
        character*8 fmt(5)/'BYTE','HALF','FULL','REAL','DOUB'/
        character*8 format

      COMMON /C1/ FILL,AVG,TRUNC,FIX,NOPRNT,TNL,NL,NS,SL,SS,NPXL,NB,
     & NSI,IDN,WUNIT,RUNIT,LINEAR
      EXTERNAL WORK
      DATA MAXCNT /100/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PARAMETERS
C     'LINE',I,J,K..   I,J,K ARE LINES TO BE INSERTED
C     'DN',N         INSERTED LINES TO HAVE DN OF N
C     'AVG'          INSERTED LINE TO BE AVG OF NEIGHBORS
C     'TRUNC'        OUTPUT TRUNCATED TO NL LINES
C     'NOFIX'       SUPPRESSES FIXING OF DROPPED LINES
C     'NOPRINT'     SUPPRESSES PRINTOUT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        DEFAULT PARAMETERS
      FILL= .FALSE.
      AVG= .TRUE.
      TRUNC= .FALSE.
      FIX= .TRUE.
      NOPRNT= .FALSE.
C
C----    GET PARAMETERS
C
      CALL IFMESSAGE ('** INSERT - 12-Jul-2016 (64-bit) - rjb')
c      CALL XVEACTION ('SA',' ')
      CALL XVPARM('LINE',LINEAR,NLINE,LINEDF,MAXCNT)
      IF (LINEDF.EQ.1) NLINE=0
      IF (LINEDF.EQ.1 .OR. NLINE.EQ.1)  GO TO 20
C  SORT LINES BY INCREASING LINE NUMBER:
      DO I = 1,NLINE-1
	DO J = I+1,NLINE
	  IF (LINEAR(I).EQ.LINEAR(J)) THEN
	    CALL XVMESSAGE('??E -"LINE" may not contain repeated numbers',' ')
	    CALL ABEND
	  ELSEIF (LINEAR(I).GT.LINEAR(J)) THEN
	    SAVE = LINEAR(I)
	    LINEAR(I) = LINEAR(J)
	    LINEAR(J) = SAVE
	  ENDIF
	ENDDO
      ENDDO
c
c	linear is now sorted list of replacement lines
20    AVG = .NOT.XVPTST('NOAVG')

      CALL XVPARM('DN',IDN,NDN,DNDF,100)	!dndf = DN default=0
      IF (DNDF.EQ.0)  AVG = .FALSE.

      TRUNC = XVPTST('TRUNC')

      FIX = .NOT.XVPTST('NOFIX')

      NOPRNT= XVPTST('NOPRINT')


C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
	call xvunit(runit,'INP',1,status,' ')
	call xvopen(runit,status,'OPEN_ACT','SA','IO_ACT','SA',' ')
	call xvget(runit,status,'FORMAT',FORMAT,'ORG',orgin,'PIX_SIZE',NPXL,'NS',NSI,' ')
	call xvget(runit,status,'NL',nl,'NS',ns,' ')
	CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      NB = NPXL*NS			!nb = num of bytes not number of bands
      TNL= NL+NLINE
      IF (TRUNC)  TNL= NL


        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (format.eq.'DOUB') icode=5
	if (format.eq.'DOUB'.or.format.eq.'COMPLEX') then
		call xvmessage('??E - DOUB or COMPLEX data is not supported',' ')
		call abend
	endif
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend
        endif
        if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

        call xvclose(runit,status,' ')

c        call xvunit(runit,'INP',1,status,' ')
	call xvopen(runit,status,'OPEN_ACT','SA','IO_ACT','SA',
     +            'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

	call xvunit(wunit,'OUT',1,status,' ')
	call xvopen(wunit,status,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_NL',TNL,'U_NS',NS,'O_FORMAT',
     +		fmt(icode),'U_FORMAT',fmt(4),' ')
      IF (TNL.NE.NL) THEN
	CALL PRNT(4,1,TNL,'OUTPUT NL=.')
	CALL PRNT(4,1,NS,'OUTPUT NS=.')
      ENDIF
	nbytes = int8(nb)*8
	print *, "nb = ",nb
         print *, "nbytes = ",nbytes
      CALL STACKA_big(4, WORK, 2, nbytes, nbytes)
	print *, "after"
      CALL XVCLOSE(RUNIT,STATUS,' ')
      CALL XVCLOSE(WUNIT,STATUS,' ')
      RETURN
      END

C*********************************************************

      SUBROUTINE WORK( BUF, L1, FILBUF, L2)
c      IMPLICIT INTEGER*4 (A-Z)
	implicit none
      real*4 BUF(nb), FILBUF(nb)
      LOGICAL*4 FILL,AVG,EOF,TRUNC,FIX,NOPRNT
      INTEGER*4 LINEAR(100)
	integer*4 idn,j,lcnt,lread,lwrit,nl,nb,npxl
	integer*4 ns,nsi,runit,sl,ss,status,tnl,wunit 
	integer*8 L1,L2

	real*4 dn
      COMMON /C1/ FILL,AVG,TRUNC,FIX,NOPRNT,TNL,NL,NS,SL,SS,NPXL,NB,
     & NSI,IDN,WUNIT,RUNIT,LINEAR

      EOF = .FALSE.
C  LREAD IS THE NUMBER OF THE CURRENT INPUT LINE WRT SL
C  LWRIT IS THE NUMBER OF THE CORRESPONDING OUTPUT LINE
C  LCNT  IS THE NEXT NUMBER IN THE INSERTED LINE LIST
      LCNT= 1
      LREAD = 0
      LWRIT = 1

      IF (AVG)  GO TO 500
      DN = real(IDN)
C  LOAD FILL BUFFER WITH SPECIFIED DN
      CALL MVE(7,NS,DN,FILBUF,0,1)		!-5 FULL to BYTE, 7 real to real	

C  MAIN LOOP:
C	filbuf is either fixed DN values or the buffer from the previous read
c
500   IF (LREAD.LT.NL)  GO TO 510
      IF (LINEAR(LCNT).EQ.LWRIT)  GO TO 520
      RETURN				!this is return when done

510   IF (LREAD.EQ.0 .AND. SL.NE.1) THEN
	CALL XVREAD(RUNIT,BUF,STATUS,'LINE',SL,'SAMP',SS,
     .	 'NSAMPS',NS,' ')
      ELSEIF (SS.NE.1 .OR. NSI.NE.NS) THEN
	CALL XVREAD(RUNIT,BUF,STATUS,'SAMP',SS,'NSAMPS',NS,' ')
      ELSE
	CALL XVREAD(RUNIT,BUF,STATUS,' ')
      ENDIF

      LREAD= LREAD+1
      IF (STATUS.EQ.-30) GO TO 650		!-30 = 'END_OF_FILE' - pad bottom of picture
      IF (STATUS.NE.1.AND. FIX)  GO TO 515	! if other error, insert filler if requested 
      IF (LINEAR(LCNT).NE.LWRIT)  GO TO 700	! if not the inserted line, write the read buffer
      GO TO 520					!else, continue

C  LINE TO BE INSERTED
515   LREAD = LREAD+1
520   LCNT = LCNT+1
      IF (.NOT.AVG)  GO TO 600
c if starting and averaging, put first read line into filbuf 
      IF (LWRIT.EQ.1)  CALL MVE(7,NB,BUF,FILBUF,1,1) 	!1 BYTE to BYTE

c      JJ = 0
      DO 570 J=1,NS
	filbuf(j) = 0.5*(filbuf(j)+buf(j))+0.5	
570   CONTINUE

600   IF (.NOT.NOPRNT) CALL PRNT(4,1,LWRIT,'LINE INSERTED.')
      CALL XVWRIT(WUNIT,FILBUF,STATUS,' ')
      LWRIT= LWRIT+1
      IF (LINEAR(LCNT).EQ.LWRIT)  GO TO 520
      IF (EOF) RETURN
      GO TO 700
C
C        FILL-IN BOTTOM OF PICTURE
650   EOF= .TRUE.
      IF (LINEAR(LCNT).NE.LWRIT)  GO TO 655
      CALL MVE(7,NB,FILBUF,BUF,1,1)			!1 BYTE to BYTE
      GO TO 520

655   FILL= .TRUE.
      CALL MVE(7,NB,FILBUF,BUF,1,1)
675   IF (LWRIT.GT.TNL)  RETURN
      IF (.NOT.NOPRNT) CALL PRNT(4,1,LWRIT,'LINE INSERTED.')

C---- WRITE THE LINE

700   CONTINUE
      CALL XVWRIT(WUNIT,BUF,STATUS,' ')		!write out the line
      LWRIT = LWRIT+1
      IF (FILL)  GO TO 675
      IF (AVG)  CALL MVE(7,NB,BUF,FILBUF,1,1)	!carry over the written line, for averaging
      GO TO 500

      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create insert.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM insert

   To Create the build file give the command:

		$ vimake insert			(VMS)
   or
		% vimake insert			(Unix)


************************************************************************/


#define PROGRAM	insert
#define R2LIB

#define MODULE_LIST insert.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create insert.pdf
process help=*
PARM INP   TYPE=STRING
PARM OUT   TYPE=STRING
PARM SIZE  TYPE=INTEGER COUNT=4                DEFAULT=(1,1,0,0)
PARM SL    TYPE=INTEGER                        DEFAULT=1
PARM SS    TYPE=INTEGER                        DEFAULT=1
PARM NL    TYPE=INTEGER                        DEFAULT=0
PARM NS    TYPE=INTEGER                        DEFAULT=0
PARM LINE  TYPE=INTEGER COUNT=1:100            DEFAULT=0
PARM DN    TYPE=INTEGER                        DEFAULT=0
PARM NOAVG TYPE=KEYWORD VALID=NOAVG COUNT=0:1  DEFAULT=--
PARM TRUNC TYPE=KEYWORD VALID=TRUNC COUNT=0:1  DEFAULT=--
PARM NOFIX TYPE=KEYWORD VALID=NOFIX COUNT=0:1  DEFAULT=--
PARM NOPRINT TYPE=KEYWORD VALID=NOPRINT COUNT=0:1 DEFAULT=--
END-PROC

.TITLE
 INSERT - insert lines into an image.
.HELP

 PURPOSE:
 INSERT is an applications program that will insert
 artificial lines in a picture and rescale the
 remaining lines down the appropriate amount. Insert
 is useful for correcting misregistration due to
 line dropout.

 DESCRIPTION:

 The parameters are generated on the basis that
 the input image has less lines than is desired.
 This could be due to telemetry dropout or other
 corruption process.

 The options of insertion are:
 1) Averaging adjacent lines, the default operation
 2) Inserting a line of specific DN value, 
    (NOAVG or by specifying a NON-ZERO DN)
  
 LINE is an array of line numbers where insertion is
 to occur. They may be successive. A maximum of
 100 lines is permitted. The line numbers are with
 respect to the final output image, not the input.

 TRUNC refers to the final output image. If the 
 resultant image after insertion is bigger than
 desired truncation allows the user to drop the
 excess lines at the bottom of the input image.
 The desired size of the image can be declared
 in the SIZE or NL parameters.

 NOFIX indicates that bad lines will be written as read.
 By default, if a bad line is detected, it is replaced
 by the two options listed above.

 Note: A bad line means a line that generated any
 I/O error STATUS return other than 'END OF FILE' 
 on the internal xvread routine. Basically, this
 is a holdover from when input data was read from
 magnetic tapes.

.PAGE
 EXECUTION:
 The following is the execution statement for INSERT:
        insert  INP  OUT  PARAMS
.PAGE
 EXAMPLES:
 insert INP=A OUT=B SIZE=(1,1,100,200) LINE=(1,10)

 Lines 1 and 10 of the output data set b are artificially
 generated by averaging pixels in adjoining lines.  Lines
 2 through 9 are lines 1 through 8 of the input data set.
 Lines 11 through 102 are lines 9 through 100 of the input
 data set.

 insert INP=A OUT=B SIZE=(1,1,100,200) DN=128 LINE=5 TRUNC=TRUNC

 Line 5 of the output data set, B, will be artificially filled
 with DN value of 128. Lines 1 through 4 are lines 1 through 4
 of the input data set, A. Lines 6 through 100 are lines 5
 through 99 of the input data set, A.
.PAGE
 RESTRICTIONS:

1)  Max. number of lines that can be inserted = 100.

2) If no lines are specified for insertion, the input
 data set is copied to the output dataset.  If an
 unexpected eof occurs, artificial lines are
 added to make up the difference.
 
3) The 2 GB  maximimum image size was removed 1 JUL 2016.
4) Does not support DOUBLE or COMPLEX data sets
5) Input DN is in integer format even if data set
    is REAL data type, cannot enter fraction in DN param.

.PAGE
 ORIGINAL PROGRAMMER:  J. D. ADDINGTON,  19 JUNE 1974
 CONVERTED TO VAX:     J. A. MOSHER,     15 MARCH 1983
 CONVERTED TO VICAR2:  S. POHORSKY,      19 NOV. 1984
 REVISION : 1           L. W. KAMP,      20 JAN. 1985
 Made portable for UNIX Alan Scop (CRI),  2 MAY 1994    

 Revision:              R. J. Bambery,    1 Jul 2016
 Removed 2 GB maximum image size. Updated documentation. 
 Accepts BYTE, HALF, FULL and REAL data sets

     12 Jul 2016 - R. J. Bambery - linked against new stacka_big routine
                Centos-7.2 & gcc-4.8.5
 
 CURRENT COGNIZANT PROGRAMMER:  R. J. BAMBERY
.LEVEL1
.VARIABLE INP
 INPUT DATASET
.VARIABLE OUT
 OUTPUT DATASET
.VARIABLE SIZE
 IMAGE SIZE
.VARI SS
 INTEGER - STARTING SAMPLE
.VARI SL
 INTEGER - STARTING LINE
.VARI NL
 INTEGER - NUMBER OF LINES
.VARI NS
 INTEGER - NUMBER OF SAMPLES
.VARIABLE LINE
 1-100 ARTIFICIAL-LINE NUMBERS
.VARIABLE DN
 DN VALUES OF INSERTED LINE
.VARIABLE NOAVG
 SUPPRESS AVERAGING OF INSERTED PIXELS?
.VARIABLE TRUNC
 OUTPUT LINES TO BE TRUNCATED?
.VARI NOFIX
 SUPPRESS FIXING OF BAD LINES?
.VARI NOPRINT
 SUPPRESS MESSAGES?
.LEVEL2
.VARI INP
 STANDARD VICAR INPUT DATASET PARAMETER
 (ONE DATASET)
.VARI OUT
 STANDARD VICAR1 OUTPUT DATASET PARAMETER
 (ONE DATASET)
.VARI SIZE
 STANDARD VICAR1 SIZE PARAMETER.
.VARIABLE LINE
 NEW LINES WILL BE INSERTED AT THE LINE(S) SPECIFIED.
 LINE NUMBERS ARE WITH RESPECT TO OUTPUT PICTURE.
.VARIABLE DN
 DENOTES THE INSERTED LINE PIXELS HAVE A DN OF VALUE N.
.VARIABLE NOAVG
 VALID KEYWORD VALUE: NOAVG.

 'NOAVG' DENOTES THAT INSERTED LINES WILL HAVE ALL DN'S EQUAL
 TO ZERO (OR TO 'DN', IF THAT PARAMETER IS SPECIFIED).
 DEFAULT IS THAT INSERTED LINE PIXELS ARE GENERATED BY
 AVERAGING THE PIXELS IN NEIGHBORING LINES.

 IF PARAMETER 'DN' IS SPECIFIED, THEN NO AVERAGING WILL BE
 PERFORMED.
.VARIABLE TRUNC
 VALID KEYWORD VALUE: TRUNC.
 THIS DENOTES THE OUTPUT PICTURE WILL BE TRUNCATED TO
 THE NUMBER OF LINES SPECIFIED IN THE SIZE PARAMETER, OR,
 IF NO 'SIZE' SPECIFIED, IN THE INPUT IMAGE.
.VARI NOFIX
 VALID KEYWORD VALUE: NOFIX.

 THIS DENOTES THAT BAD LINES WILL BE WRITTEN AS READ.  DEFAULT IS 
 TO REPLACE A BAD LINE WITH A NEW LINE CONSISTING OF ZEROES OR AN
 AVERAGE OF THE ADJACENT LINES (DEPENDING ON WHETHER 'NOAVG' WAS
 SPECIFIED).

 A BAD LINE MEANS A LINE THAT GENERATED ANY I/O ERROR OTHER THAN
 'END OF FILE' WHEN READ.
.VARI NOPRINT
 VALID KEYWORD VALUE: NOPRINT.

 THIS CAUSES MESSAGES TO BE SUPPRESSED.  IF THIS KEYWORD IS NOT
 SPECIFIED, A MESSAGE WILL BE PRINTED FOR EVERY LINE INSERTED.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstinsert.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="stop"
let $echo="yes"

! Jun 30, 2016 - RJB
! TEST SCRIPT FOR INSERT
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list label-list 
!
! External Programs:
!   <none>
! 
! Parameters:
!   <none>
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check to see if mipl or cartlab for test images
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata

write "afidsroot = &afidsroot"
write "aftestdata = &aftestdata"
if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/carto ct
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/carto ct
    else 
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto ct
    end-if
end-if
let _onfail="goto rm"


! allocate extra line for insertion below!
! BYTE DATA
gen OUT=BYTE.DAT NL=10 NS=10
list INP=BYTE.DAT
label-list INP=BYTE.DAT

!COPY FILE ONLY
insert INP=BYTE.DAT SIZE=(3,3,5,5) OUT=INSBYTE.DAT
list INP=INSBYTE.DAT
label-list INP=INSBYTE.DAT

!INSERT A LINE BY AVERAGING TWO LINES
insert INP=BYTE.DAT OUT=INSBYTE.DAT LINE=5
list INP=INSBYTE.DAT

! SUPPRESS AVERAGING
insert INP=BYTE.DAT OUT=INSBYTE.DAT 'NOAVG LINE=5
list INP=INSBYTE.DAT
label-list INP=INSBYTE.DAT

!INSERT A LINE AT 128 DN; MAINTAIN SAME SIZE
insert INP=BYTE.DAT OUT=INSBYTE.DAT DN=128 LINE=5 'TRUNC
list INP=INSBYTE.DAT
label-list INP=INSBYTE.DAT

! INSERT MULTIPLE LINES, RANDOM ORDER
insert INP=BYTE.DAT OUT=INSBYTE.DAT LINE=(13,3,8,4)
list INP=INSBYTE.DAT
let $echo="no"
write " "
write "-------- HALF DATA ------"
write " "
let $echo="yes"
!
!NOW FOR HALFWORD DATA
gen OUT=HALF.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=HALF
list INP=HALF.DAT
label-list INP=HALF.DAT

!COPY FILE ONLY
insert INP=HALF.DAT SIZE=(3,3,5,6) OUT=INSHALF.DAT
list INP=INSHALF.DAT
label-list INP=INSHALF.DAT

!INSERT A LINE BY AVERAGING TWO LINES
insert INP=HALF.DAT OUT=INSHALF.DAT LINE=5
list INP=INSHALF.DAT
label-list INP=INSHALF.DAT

!INSERT A LINE AT 1234 DN; MAINTAIN SAME SIZE FIELD
insert INP=HALF.DAT OUT=INSHALF.DAT DN=1234 LINE=5 'TRUNC
list INP=INSHALF.DAT
label-list INP=INSHALF.DAT
let $echo="no"
write " "
write "-------- FULL DATA ------"
write " "
let $echo="yes"

!
! FOR FULL DATA
gen OUT=FULL.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=FULL
list INP=FULL.DAT
label-list INP=FULL.DAT

!COPY FILE ONLY
insert INP=FULL.DAT SIZE=(3,3,5,6) OUT=INSFULL.DAT
list INP=INSFULL.DAT
label-list INP=INSFULL.DAT

!INSERT A LINE BY AVERAGING TWO LINES
insert INP=FULL.DAT OUT=INSFULL.DAT LINE=5
list INP=INSFULL.DAT
label-list INP=INSFULL.DAT

!INSERT A LINE AT 1234 DN; MAINTAIN SAME SIZE FIELD
insert INP=FULL.DAT OUT=INSFULL.DAT DN=1234 LINE=5 'TRUNC
list INP=INSFULL.DAT
label-list INP=INSFULL.DAT
let $echo="no"
write " "
write "-------- REAL DATA ------"
write " "
let $echo="yes"

!
! FOR REAL DATA
gen OUT=REAL.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=REAL
list INP=REAL.DAT
label-list INP=REAL.DAT

!COPY FILE ONLY
insert INP=REAL.DAT SIZE=(3,3,5,6) OUT=INSREAL.DAT
list INP=INSREAL.DAT
label-list INP=INSREAL.DAT

!INSERT A LINE BY AVERAGING TWO LINES
insert INP=REAL.DAT OUT=INSREAL.DAT LINE=5
list INP=INSREAL.DAT
label-list INP=INSREAL.DAT

!INSERT A LINE AT 1234 DN; MAINTAIN SAME SIZE FIELD
insert INP=REAL.DAT OUT=INSREAL.DAT DN=1234 LINE=5 'TRUNC
list INP=INSREAL.DAT
label-list INP=INSREAL.DAT

let $echo="no"
write " "
write " New test of image > 2 GB"
write " "
write " test in insert of line 5005 in large image"
write " "
let $echo="yes"
insert ct/pre_reg.hlf bigins.img DN=9234 LINE=5005
list bigins.img  size=(5000,5000,10,10)
!
! clean up
!
rm>
let $echo="no"
ush rm ct
end-proc
$!-----------------------------------------------------------------------------
$ create tstinsert.log
tstinsert
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
write "afidsroot = /home/rjb/vicar_afids/install/afids"
afidsroot = /home/rjb/vicar_afids/install/afids
write "aftestdata = /media/sf_raid3/test_data"
aftestdata = /media/sf_raid3/test_data
if (afidsroot = "")
else
    if (aftestdata = "")
    else
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto ct
    end-if
end-if
let _onfail="goto rm"
gen OUT=BYTE.DAT NL=10 NS=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list INP=BYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
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
label-list INP=BYTE.DAT
Beginning VICAR task label
************************************************************
 
        ************  File BYTE.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
 
************************************************************
insert INP=BYTE.DAT SIZE=(3,3,5,5) OUT=INSBYTE.DAT
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
list INP=INSBYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
     Samp     1       3       5
   Line
      1       4   5   6   7   8
      2       5   6   7   8   9
      3       6   7   8   9  10
      4       7   8   9  10  11
      5       8   9  10  11  12
label-list INP=INSBYTE.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSBYTE.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                5 lines per band
                5 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
 
************************************************************
insert INP=BYTE.DAT OUT=INSBYTE.DAT LINE=5
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         11
OUTPUT NS=         10
LINE INSERTED          5
list INP=INSBYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       4   5   6   7   8   9  10  11  12  13
      7       5   6   7   8   9  10  11  12  13  14
      8       6   7   8   9  10  11  12  13  14  15
      9       7   8   9  10  11  12  13  14  15  16
     10       8   9  10  11  12  13  14  15  16  17
     11       9  10  11  12  13  14  15  16  17  18
insert INP=BYTE.DAT OUT=INSBYTE.DAT 'NOAVG LINE=5
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         11
OUTPUT NS=         10
LINE INSERTED          5
list INP=INSBYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12

      6       4   5   6   7   8   9  10  11  12  13
      7       5   6   7   8   9  10  11  12  13  14
      8       6   7   8   9  10  11  12  13  14  15
      9       7   8   9  10  11  12  13  14  15  16
     10       8   9  10  11  12  13  14  15  16  17
     11       9  10  11  12  13  14  15  16  17  18
label-list INP=INSBYTE.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSBYTE.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                11 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
 
************************************************************
insert INP=BYTE.DAT OUT=INSBYTE.DAT DN=128 LINE=5 'TRUNC
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
LINE INSERTED          5
list INP=INSBYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5     128 128 128 128 128 128 128 128 128 128
      6       4   5   6   7   8   9  10  11  12  13
      7       5   6   7   8   9  10  11  12  13  14
      8       6   7   8   9  10  11  12  13  14  15
      9       7   8   9  10  11  12  13  14  15  16
     10       8   9  10  11  12  13  14  15  16  17
label-list INP=INSBYTE.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSBYTE.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:44 2016 ----
 
************************************************************
insert INP=BYTE.DAT OUT=INSBYTE.DAT LINE=(13,3,8,4)
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         14
OUTPUT NS=         10
LINE INSERTED          3
list INP=INSBYTE.DAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:44 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       2   3   4   5   6   7   8   9  10  11
      5       3   4   5   6   7   8   9  10  11  12
      6       4   5   6   7   8   9  10  11  12  13
      7       5   6   7   8   9  10  11  12  13  14
      8       6   7   8   9  10  11  12  13  14  15
      9       7   8   9  10  11  12  13  14  15  16
     10       8   9  10  11  12  13  14  15  16  17
     11       9  10  11  12  13  14  15  16  17  18
let $echo="no"
 
-------- HALF DATA ------
 
gen OUT=HALF.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list INP=HALF.DAT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0   100   200   300   400   500   600   700   800   900
      2       100   200   300   400   500   600   700   800   900  1000
      3       200   300   400   500   600   700   800   900  1000  1100
      4       300   400   500   600   700   800   900  1000  1100  1200
      5       400   500   600   700   800   900  1000  1100  1200  1300
      6       500   600   700   800   900  1000  1100  1200  1300  1400
      7       600   700   800   900  1000  1100  1200  1300  1400  1500
      8       700   800   900  1000  1100  1200  1300  1400  1500  1600
      9       800   900  1000  1100  1200  1300  1400  1500  1600  1700
     10       900  1000  1100  1200  1300  1400  1500  1600  1700  1800
label-list INP=HALF.DAT
Beginning VICAR task label
************************************************************
 
        ************  File HALF.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
 
************************************************************
insert INP=HALF.DAT SIZE=(3,3,5,6) OUT=INSHALF.DAT
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
list INP=INSHALF.DAT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp       1     2     3     4     5     6
   Line
      1       400   500   600   700   800   900
      2       500   600   700   800   900  1000
      3       600   700   800   900  1000  1100
      4       700   800   900  1000  1100  1200
      5       800   900  1000  1100  1200  1300
label-list INP=INSHALF.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSHALF.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                5 lines per band
                6 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
 
************************************************************
insert INP=HALF.DAT OUT=INSHALF.DAT LINE=5
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         11
OUTPUT NS=         10
LINE INSERTED          5
list INP=INSHALF.DAT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0   100   200   300   400   500   600   700   800   900
      2       100   200   300   400   500   600   700   800   900  1000
      3       200   300   400   500   600   700   800   900  1000  1100
      4       300   400   500   600   700   800   900  1000  1100  1200
      5       350   450   550   650   750   850   950  1050  1150  1250
      6       400   500   600   700   800   900  1000  1100  1200  1300
      7       500   600   700   800   900  1000  1100  1200  1300  1400
      8       600   700   800   900  1000  1100  1200  1300  1400  1500
      9       700   800   900  1000  1100  1200  1300  1400  1500  1600
     10       800   900  1000  1100  1200  1300  1400  1500  1600  1700
     11       900  1000  1100  1200  1300  1400  1500  1600  1700  1800
label-list INP=INSHALF.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSHALF.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                11 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
 
************************************************************
insert INP=HALF.DAT OUT=INSHALF.DAT DN=1234 LINE=5 'TRUNC
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
LINE INSERTED          5
list INP=INSHALF.DAT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0   100   200   300   400   500   600   700   800   900
      2       100   200   300   400   500   600   700   800   900  1000
      3       200   300   400   500   600   700   800   900  1000  1100
      4       300   400   500   600   700   800   900  1000  1100  1200
      5      1234  1234  1234  1234  1234  1234  1234  1234  1234  1234
      6       400   500   600   700   800   900  1000  1100  1200  1300
      7       500   600   700   800   900  1000  1100  1200  1300  1400
      8       600   700   800   900  1000  1100  1200  1300  1400  1500
      9       700   800   900  1000  1100  1200  1300  1400  1500  1600
     10       800   900  1000  1100  1200  1300  1400  1500  1600  1700
label-list INP=INSHALF.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSHALF.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
 
************************************************************
let $echo="no"
 
-------- FULL DATA ------
 
gen OUT=FULL.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list INP=FULL.DAT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0        100        200        300        400        500        600        700        800        900
      2            100        200        300        400        500        600        700        800        900       1000
      3            200        300        400        500        600        700        800        900       1000       1100
      4            300        400        500        600        700        800        900       1000       1100       1200
      5            400        500        600        700        800        900       1000       1100       1200       1300
      6            500        600        700        800        900       1000       1100       1200       1300       1400
      7            600        700        800        900       1000       1100       1200       1300       1400       1500
      8            700        800        900       1000       1100       1200       1300       1400       1500       1600
      9            800        900       1000       1100       1200       1300       1400       1500       1600       1700
     10            900       1000       1100       1200       1300       1400       1500       1600       1700       1800
label-list INP=FULL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File FULL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
 
************************************************************
insert INP=FULL.DAT SIZE=(3,3,5,6) OUT=INSFULL.DAT
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
list INP=INSFULL.DAT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
     Samp            1          2          3          4          5          6
   Line
      1            400        500        600        700        800        900
      2            500        600        700        800        900       1000
      3            600        700        800        900       1000       1100
      4            700        800        900       1000       1100       1200
      5            800        900       1000       1100       1200       1300
label-list INP=INSFULL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSFULL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                5 lines per band
                6 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
 
************************************************************
insert INP=FULL.DAT OUT=INSFULL.DAT LINE=5
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         11
OUTPUT NS=         10
LINE INSERTED          5
list INP=INSFULL.DAT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0        100        200        300        400        500        600        700        800        900
      2            100        200        300        400        500        600        700        800        900       1000
      3            200        300        400        500        600        700        800        900       1000       1100
      4            300        400        500        600        700        800        900       1000       1100       1200
      5            350        450        550        650        750        850        950       1050       1150       1250
      6            400        500        600        700        800        900       1000       1100       1200       1300
      7            500        600        700        800        900       1000       1100       1200       1300       1400
      8            600        700        800        900       1000       1100       1200       1300       1400       1500
      9            700        800        900       1000       1100       1200       1300       1400       1500       1600
     10            800        900       1000       1100       1200       1300       1400       1500       1600       1700
     11            900       1000       1100       1200       1300       1400       1500       1600       1700       1800
label-list INP=INSFULL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSFULL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                11 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
 
************************************************************
insert INP=FULL.DAT OUT=INSFULL.DAT DN=1234 LINE=5 'TRUNC
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
LINE INSERTED          5
list INP=INSFULL.DAT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:45 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0        100        200        300        400        500        600        700        800        900
      2            100        200        300        400        500        600        700        800        900       1000
      3            200        300        400        500        600        700        800        900       1000       1100
      4            300        400        500        600        700        800        900       1000       1100       1200
      5           1234       1234       1234       1234       1234       1234       1234       1234       1234       1234
      6            400        500        600        700        800        900       1000       1100       1200       1300
      7            500        600        700        800        900       1000       1100       1200       1300       1400
      8            600        700        800        900       1000       1100       1200       1300       1400       1500
      9            700        800        900       1000       1100       1200       1300       1400       1500       1600
     10            800        900       1000       1100       1200       1300       1400       1500       1600       1700
label-list INP=INSFULL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSFULL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:45 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
 
************************************************************
let $echo="no"
 
-------- REAL DATA ------
 
gen OUT=REAL.DAT NL=10 NS=10 LINC=100 SINC=100 FORMAT=REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list INP=REAL.DAT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02
      2       1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03
      3       2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03
      4       3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03
      5       4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03
      6       5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03
      7       6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03
      8       7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03
      9       8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03   1.700E+03
     10       9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03   1.700E+03   1.800E+03
label-list INP=REAL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File REAL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
 
************************************************************
insert INP=REAL.DAT SIZE=(3,3,5,6) OUT=INSREAL.DAT
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
list INP=INSREAL.DAT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
     Samp             1           2           3           4           5           6
   Line
      1       4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02
      2       5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03
      3       6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03
      4       7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03
      5       8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03
label-list INP=INSREAL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSREAL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                5 lines per band
                6 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
 
************************************************************
insert INP=REAL.DAT OUT=INSREAL.DAT LINE=5
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=         11
OUTPUT NS=         10
LINE INSERTED          5
list INP=INSREAL.DAT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:47 2016
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02
      2       1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03
      3       2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03
      4       3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03
      5       3.505E+02   4.505E+02   5.505E+02   6.505E+02   7.505E+02   8.505E+02   9.505E+02   1.050E+03   1.150E+03   1.250E+03
      6       4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03
      7       5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03
      8       6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03
      9       7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03
     10       8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03   1.700E+03
     11       9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03   1.700E+03   1.800E+03
label-list INP=INSREAL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSREAL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                11 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:47 2016 ----
 
************************************************************
insert INP=REAL.DAT OUT=INSREAL.DAT DN=1234 LINE=5 'TRUNC
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
LINE INSERTED          5
list INP=INSREAL.DAT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:19:46 2016
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:47 2016
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02
      2       1.000E+02   2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03
      3       2.000E+02   3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03
      4       3.000E+02   4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03
      5       1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03   1.234E+03
      6       4.000E+02   5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03
      7       5.000E+02   6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03
      8       6.000E+02   7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03
      9       7.000E+02   8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03
     10       8.000E+02   9.000E+02   1.000E+03   1.100E+03   1.200E+03   1.300E+03   1.400E+03   1.500E+03   1.600E+03   1.700E+03
label-list INP=INSREAL.DAT
Beginning VICAR task label
************************************************************
 
        ************  File INSREAL.DAT ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                10 lines per band
                10 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: rjb -- Wed Jul 27 00:19:46 2016 ----
IVAL=0.0
SINC=100.0
LINC=100.0
BINC=1.0
MODULO=0.0
---- Task: INSERT -- User: rjb -- Wed Jul 27 00:19:47 2016 ----
 
************************************************************
let $echo="no"
 
 New test of image > 2 GB
 
 test in insert of line 5005 in large image
 
insert ct/pre_reg.hlf bigins.img DN=9234 LINE=5005
Beginning VICAR task insert
** INSERT - 12-Jul-2016 (64-bit) - rjb
OUTPUT NL=      32886
OUTPUT NS=      38550
LINE INSERTED       5005
list bigins.img  size=(5000,5000,10,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:tllogan   Date_Time:Wed Sep 18 17:16:51 2013
 Task:INSERT    User:rjb       Date_Time:Wed Jul 27 00:19:47 2016
     Samp    5000  5001  5002  5003  5004  5005  5006  5007  5008  5009
   Line
   5000      3158  3158  3158  3162  3170  3183  3183  3179  3192  3179
   5001      3141  3141  3145  3170  3183  3175  3170  3183  3196  3183
   5002      3170  3158  3158  3166  3166  3175  3179  3170  3166  3187
   5003      3145  3162  3153  3162  3170  3170  3170  3166  3162  3179
   5004      3187  3170  3158  3162  3170  3175  3183  3179  3166  3175
   5005      9234  9234  9234  9234  9234  9234  9234  9234  9234  9234
   5006      3170  3162  3153  3158  3166  3170  3179  3179  3166  3175
   5007      3179  3183  3170  3162  3162  3179  3183  3175  3166  3158
   5008      3158  3158  3162  3162  3170  3192  3187  3196  3175  3166
   5009      3162  3162  3162  3162  3166  3166  3175  3179  3179  3166
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
