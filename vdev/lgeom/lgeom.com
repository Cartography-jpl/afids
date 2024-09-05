$!****************************************************************************
$!
$! Build proc for MIPL module lgeom
$! VPACK Version 1.9, Tuesday, August 16, 2016, 19:16:28
$!
$! Execute by entering:		$ @lgeom
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
$ write sys$output "*** module lgeom ***"
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
$ write sys$output "Invalid argument given to lgeom.com file -- ", primary
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
$   if F$SEARCH("lgeom.imake") .nes. ""
$   then
$      vimake lgeom
$      purge lgeom.bld
$   else
$      if F$SEARCH("lgeom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lgeom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lgeom.bld "STD"
$   else
$      @lgeom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lgeom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lgeom.com -mixed -
	-s lgeom.f lgeom_unix.f mvb.c -
	-p lgeom.pdf -
	-t tstlgeom.pdf tstlgeom.log tlgeom.f tlgeom.pdf tlgeom.imake -
	   timelgeom.pdf -
	-i lgeom.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lgeom.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C                       LGEOM
C       28 APR 93  SP           Made portable for UNIX.  Changed code to use 
C                               BYTE2INT AND INT2BYTE for converting 
C                               between BYTE and INTEGER.  Gave control over
C                               progress messages to PRINT parameter.  Added
C                               PMEM parameter.  Changed call to STACKA so 
C                               GDIR and BDIR allocated in one chunk to insure
C                               they are contiguous.
C	07 JUL 92  HJF		CORRECT ERROR WITH 'NOIN WHEN R = IRMAX+.5
C				PROGRAM WAS USING LINE IRMAX+1 WHICH WAS NOT
C				IN MEMORY. CHANGED GSUB, GSUBH, GSUB1, GSUBH1
C				ALSO, ADDED NOIZ OPTION TO NOT INTERPOLATE
C				WHEN DN IS ZERO.
C
C	05 APR 91  HJF		USE STACKA TO ALLOCATE SPACE TO AVOID
C				EXCEEDING PAGE FILE QUOTA FOR SMALL QUOTAS
C				AND ALLOW USE OF LARGER WORKING SETS AND
C				PICTURE SIZES
C
C	08 NOV 90  HJF		USE WORKING SET EXTENT TO ALLOCATE SPACE.
C				ONE PASS VERSION WHEN ENTIRE INPUT PICTURE
C				FITS IN MEMORY
C
C	31 OCT 90  HJF		KEEP TRACK OF IDS BLOCK NUMBERS IN MEMORY.
C				READ IDS IN FORWARD DIRECTION TO IMPROVE
C				EXECUTION TIME.
C      
C      20 APR 90   ...SP...     INCREASED BUFFER SIZES TO ALLOW FOR LARGER
C                               IMAGES.  ADDED PRINT PARAMETER FOR PRINTING
C                               TIEPOINT VALUES AND PERIODIC PROGRESS MESSAGES.
C                               CHANGED FBI,CSD,PBN,MAXNB,NSAMP TO INTEGER*4.
C                               ADDED SOME FLOATs TO PREVENT INTEGER OVERFLOW.
C                               INCREASED NUMBER OF TIEPOINT RECTANGLES ALLOWED
C                               TO 40000.
C      14 OCT 88   ...SP...     CHANGED TO IGNORE FORMAT PARAMETER BECAUSE 
C                               VICAR2 USES ONLY THE FORMAT IN LABEL.
C                               CHANGED GSUB AND GSUBH TO SET NEXSAM FALSE
C                               IF NSAMP LE 0 TO ALLOW MAPTRANS POLECASE TO 
C                               WORK.
C      20 APR 88   ...SP...     CORRECTED ROUNDING FROM ADDING .5 TO USING NINT
C                               IN GSUBH.  THIS AFFECTS THE CASE OF NEGATIVE
C                               HALFWORD DNS.
C       1 SEP 87   ...SP...     CORRECTED CODE WHERE NSI USED INSTEAD OF NWI
C                               FOR NUMBER OF SAMPLES IN SUBROUTINE GSUBH.
C      31 JUL 87   ...FM...     MODIFIED HELP FILE TO GIVE CORRECT INFORMATION 
C                               ABOUT PARAMETERS "PDS" AND "PARMS".
C      14 MAY 87   ...SP...     CORRECTED AN INFREQUENT ROUNDING PROBLEM IN 
C                               GSUB AND GSUBH THAT LED TO AN OVERINDEXED ARRAY.
C                               (R WAS EQUAL TO RMAX AND RFRAC .GE. SMALL, BUT
C                                BIGBUF(IR+1) UNDEFINED.)
C                               ALSO CORRECTED CASE WHERE IP .EQ. NSI AND
C                               LBUF(INDEX+1) UNDEFINED.)
C      16 APR 87   ...SP...     CORRECTED THE COUNT OF RECTANGLES IN THE 
C                               TIEPOINT GRID BY SUBTRACTING 1.  THIS WAS
C                               CAUSING VARIOUS ERRORS.
C      16 APR 86   ...SP...     CORRECTED HANDLING OF SS VALUE FOR HALFWORD.
C       1 APR 87   ...SP...     REDUCED SIZE OF IDS AND UPDATED HELP AS 
C                               SUGGESTED BY LWK.
C      25 JAN 84   ...DFS...    FIX BOUNDARY CHECK WITH RER
C      16 JAN 84   ...DFS...	FIX NSI IN GSUBH
C      15 SEP 83   ...DFS...    CONVERT FOR USE ON VAX/VMS
C      12 APR 81   ...JBS...    CONVERT TO FIXED POINT INTENSITY INTERP
C      12 APR 81   ...JBS...    ALLOW NEGATIVE DN
C       8 OCT 80   ...JBS...    CORRECT RARE SPACE ALLOCATION PROBLEM
C       9 OCT 79   ...JBS...    ADD SAVE AREAS TO GSUB, GSUBH.  OVERLAY.
C      12 OCT 78   ...JBS...    ADD HALFWORD OPTION
C      27 JUN 75   ...DAH...    CHANGES FOR CONVERSION TO 360/OS
C      21 JUN 71     HJF31      VICAR 4-1
C       3 MAY 71     HJF31      REDUCE SIZE FOR RELEASE 7
C
C       THIS PROGRAM WILL REMOVE GEOMETRIC DISTORTION FROM RECORDED
C       PICTURES BASED ON USER INPUT DISPLACEMENT RECTANGLES
C
      PROGRAM LGEOM
C
C       MUST CONFORM TO "MAIN44" STANDARD
      INCLUDE 'VICMAIN_FOR'
C
C--BEGIN MAIN PROGRAM
C
      SUBROUTINE MAIN44

      IMPLICIT NONE
C
C       VARIABLES:
C
C	BLOCK KEEPS TRACK OF BLOCK NUMBER ON IDS
C	BIGBUF IS THE WORKING BUFFER, ALSO KNOWN AS LBUF AND I2BUF.
C	MAXBUF IS THE SIZE (IN BYTES) OF BIGBUF.
C	MAXP2 IS THE SIZE (IN BYTES) OF THE SECOND PASS BUFFER
C	FBI - FIRST BYTE INDEX
C	CSD - CURRENT STRING DISPLACEMENT
C	BBC - BUFFER BYTE COUNT
C	GDIR - GROUP DIRECTORY FOR BLOCKS ON IDS
C	BDIR - BLOCK DIRECTORY FOR GROUPS ON IDS
C	CNT - COUNT OF BLOCKS FOR EACH GROUP
C	LOC - CURRENT LOC FOR EACH GROUP
C
C	IN PASS 1, THE INPUT PICTURE IS READ AND STRINGS OF POINTS FOR THE
C	OUTPUT PICTURE ARE WRITTEN ON THE INTERMEDIATE DATASET (IDS).
C	THE GROUP NUMBER FOR EACH BLOCK ON THE IDS IS SAVED IN GDIR.
C	IN PASS 2, DATA FOR ONE GROUP OF LINES AT A TIME IS READ FROM THE
C	IDS AND WRITTEN TO THE OUTPUT DATASET.
C
C	AT THE END OF PASS 1, GDIR IS TRANSFORMED INTO BDIR SO PASS 2 KNOWS
C	WHICH BLOCKS BELONG TO EACH GROUP AND CAN READ THEM IN THE FORWARD
C	DIRECTION AND TAKE ADVANTAGE OF DISK BLOCKING.
C
C	IF THE ENTIRE PICTURE FITS IN MEMORY, LGEOM OPERATES IN A ONE-PASS
C	MODE AND THE IDS IS NOT USED.
C
C	INPUT,IDS,AND OUT ARE THE UNIT NUMBERS FOR VICAR2
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
	INTEGER*4 INPUT,IDS,OUT
	EXTERNAL LGM ! SUBROUTINE CALLED BY STACKA

      INTEGER*4 MAXNBPAR,BUFLENPAR,COUNT,NAH,NAV,MAXREC
      PARAMETER (MAXNBPAR=1200)     ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.
      PARAMETER (BUFLENPAR=1000)    ! DEFAULT NS FOR IDS FILE.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4 LOC(MAXNBPAR)

      INTEGER*4 MAXBUF,MAXP2,MAXTP,IUSE,IDEL,LPB,ILC,IMAX
      INTEGER*4 IND,ILB
      INTEGER*4 I,NSO,NSI
      INTEGER*4 NB,K,K1,MEMSIZE
      REAL*4 FSSI,FLNT
C
C--EQUIVALENT AREA
      INTEGER*4 SLI,SSI,NLO,NBO,NLI,NBI
      INTEGER*4 LOCAT(10)
C
      INTEGER*4 CBBC,CCSD,CFBI,ICR,ISAMP,LD,MAXBBC,MAXB1
      INTEGER*4 SC,BLOCK,BUFLEN,MAXCSD
      INTEGER*4 B,FBI(MAXNBPAR), PIXSIZ
      INTEGER*4 MAXNB,BBC(MAXNBPAR)
      INTEGER*8 i8maxbuf,i8maxrec,i8k
      LOGICAL*4 HALFW,XVPTST,NOIN,NOIZ, PRINT
      REAL*4 LARGE,SMALL,FLN,FSM,FSMT,RMIN
      REAL*4 RMAX, RATIO
      INTEGER*4 XVPIXSIZEU
      CHARACTER*8 FMT
      CHARACTER*256 IDSNAM
	integer*8 totmem
C
C--INCLUDE COMMON BLOCKS 

      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT

      COMMON/P1/ HALFW,NOIN,NOIZ,ILC,ILB,IUSE,IDEL,LPB,IMAX,NB,FSSI,FLNT

      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NBO,LOCAT(4)),(NLI,LOCAT(5)),(NBI,LOCAT(6))
C
      DATA MAXNB/MAXNBPAR/
      DATA NOIN,NOIZ,HALFW /.FALSE.,.FALSE.,.FALSE./
      DATA LARGE,SMALL,BLOCK /0.99,0.01,0/

C--BEGIN EXECUTION
	call ifmessage ('LGEOM - 26-JUL-2016 (64-bit) - rjb')	
	PRINT = XVPTST( 'PRINT' ) ! PRINT TIEPOINTS AND PROGRESS MESSAGES

c	CALL XVEACTION('SA',' ') ! SET XV ERROR ACTION
	CALL XVUNIT(INPUT,'INP',1,IND,' ')
	CALL XVOPEN(INPUT,IND,' ')
	CALL XVPARM('IDSNS',BUFLEN,I,K,0)
	BUFLEN=(BUFLEN/4)*4
	IF (BUFLEN .LT. 100) THEN
	  CALL XVMESSAGE('??E - IDSNS too small, 1000 bytes used',' ')
	  BUFLEN = 1000
	ENDIF
C
C--Get the name and unit number of the IDS 
	CALL XVPARM('IDSNAM',IDSNAM,I,K,0)
	CALL XVUNIT(IDS,'XX',1,IND,'U_NAME',IDSNAM,' ')

C--Get image size
      CALL XVSIZE(SLI,SSI,NLO,NSO,NLI,NSI)
      IF (NSO .GT. 32767)
     .     CALL MABEND( '??E - ERROR: NSO > 32767')

      CALL XVGET( INPUT, IND, 'FORMAT', FMT, ' ') 
      IND = XVPIXSIZEU( PIXSIZ, FMT, INPUT)  !PIXEL SIZE IN BYTES

      IF ( PIXSIZ .NE. 1 .AND. PIXSIZ .NE. 2 )
     .     CALL MABEND('??E - ERROR:INP. IMAGE IS NOT BYTE OR HALF FORMAT')
C
C--GET NUMBER OF BYTES PER LINE
      NBO = NSO*PIXSIZ
      NBI = NSI*PIXSIZ
C
C-- SET HALFWORD FLAG IF INPUT IS HALFWORD
      HALFW = PIXSIZ .EQ. 2
C
C--INDEX TO LBUF FOR BYTE 0 OF FIRST OF ILC INPUT LINES
      IUSE = 4*NLI
      IDEL = 4*((NBI+3)/4)

	CALL XVPCNT('INP',COUNT) ! FIND MAX # OF RECTANGLES
	IF(COUNT.EQ.2) THEN ! IF 2 INPUTS, LGEOM WAS CALLED FROM MAP2
	 MAXREC=401 ! ASSUME MAX OF 401 RECTANGLES FROM MAP2
	 MAXTP=441  ! AND MAX OF 441 TIEPOINTS
	ELSE
	 CALL XVP('NAH',NAH,COUNT)
         IF (COUNT .EQ. 0)        CALL MABEND('NAH not specified')
	 CALL XVP('NAV',NAV,COUNT)
         IF (COUNT .EQ. 0)        CALL MABEND('NAV not specified')
	 MAXREC=(NAV*NAH)+1 ! MAX # OF RECTANGLES
	 MAXTP=(NAH+1)*(NAV+1) ! MAX # OF TIEPOINTS
	ENDIF

        CALL GET_MEM_SIZE(MEMSIZE) ! GET PHYSICAL MEMORY SIZE
	MAXBUF = MEMSIZE-48*MAXREC ! SUBTRACT SIZE OF INCRMNTS ->SIZE OF BIGBUF
	MAXB1=4*NLI+IDEL*NLI+NBO ! MAXBUF FOR 1 PASS LGEOM
	IF(MAXBUF.GT.MAXB1) MAXBUF=MAXB1
	IF(MAXBUF.LT.150000) MAXBUF=150000
	IF(MAXBUF.LT.16*MAXTP) MAXBUF=16*MAXTP
C MAKE SURE BUF CAN HOLD THE TIEPOINTS

C
C   SEE IF WE CAN DO IT IN ONE PASS
C
	ILB=0 ! ASSUME 2 PASSES
	ILC=(MAXBUF-4*NLI-NBO)/IDEL
	IF(ILC.GE.NLI) THEN
	  ILB=4*NLI+IDEL*NLI+1
	  LPB=NLO
	  IMAX=IUSE+NLI*IDEL
	  CALL XVUNIT(OUT,'OUT',1,IND,' ')
	  CALL XVOPEN(OUT,IND,'OP','WRITE','U_NS',NSO,'U_NL',NLO,'O_FORMAT',FMT,
     * 'OPEN_ACT','SA','IO_ACT','SA',' ')
	  K=1 ! PROVIDE DUMMY NL FOR IDS
	  GOTO 120 ! WE CAN DO IT IN 1 PASS
	ENDIF
C
	MAXP2=MAXBUF+48*MAXREC ! USE BIGBUF + INCRMNTS IN SECOND PASS
50	LPB=(MAXP2-BUFLEN)/NBO ! LINES PER BUFFER
	NB=(NLO+LPB-1)/LPB ! NUMBER OF BUFFERS
	ILC=(MAXBUF-4*NLI-NB*BUFLEN)/IDEL ! INPUT LINES IN CORE
	IF(NB.LE.MAXNB.AND.ILC.GE.8) GOTO 100 ! IF OK, GOTO 100
	BUFLEN=BUFLEN-100 ! MAKE BUFLEN SMALLER
	IF(BUFLEN.GE.100) GOTO 50 ! IF BUFLEN OK, GO TRY THIS VALUE

	CALL TOOBIG ! EXPLAIN THIS IMAGE IS TOO BIG AND ABEND.

100	CONTINUE

C--CALCULATE NUMBER OF LINES NEEDED FOR IDS.
C  TOTAL SIZE OF IDS = RATIO * OUTPUT IMAGE, DEFAULT RATIO OF 2.0

	CALL XVP('RATIO',RATIO,COUNT)
	K=(RATIO*NBO*NLO)/BUFLEN ! EST # OF LINES IN IDS FOR GDIR-BDIR
	K1=(1.1*NBO*NLO)/BUFLEN ! ESTIMATE FOR DISK SIZE

C--OPEN IDS  (Note that since this is an intermediate data set that is written
C             and read and then deleted by the same program, we can assume that
C             it will be read on the same machine that writes it.  Thus it is
C             not a problem that lgeom mixes bytes and halfwords in this file.

      CALL XVOPEN(IDS,IND,'OP','WRITE','U_NS',BUFLEN,'U_NL',K1,
     2    'U_FORMAT','BYTE','O_FORMAT','BYTE',' ')

      IMAX=IUSE+(ILC-1)*IDEL
      FBI(1)=IMAX+IDEL+1
C--INDEX TO LBUF FOR BYTE 1 OF FIRST OF NB BUFFERS
      DO I = 2,NB
        FBI(I) = FBI(I-1)+BUFLEN
      END DO

      DO I = 1,NB
	BBC(I)=0
	CNT(I)=0
      END DO

      MAXBBC=BUFLEN-2
      MAXCSD=BUFLEN-12
120   FSSI=SSI   ! FLOATING TEST VALUES
      FLNT=SLI+NLO
      FSMT=SSI+NSO
 
	totmem = MAXBUF+48*MAXREC+6*K+MAXREC+K
  
	print *,'MAXBUF,48*MAXREC,6*K,MAXREC,K = ',MAXBUF,48*MAXREC,6*K,MAXREC,K
	print *,'TOTAL MEMORY = ',totmem
        i8maxbuf = int8(maxbuf)                         !byte buffer xvread LBUF(*)
         i8maxrec = int8(48*MAXREC)                     !real*4 incrmnts(12,maxrec)
        i8k = int8(6*K)                                 !int*2 GDIR(*)
C	CALL STACKA(7,LGM,3,MAXBUF,48*MAXREC,6*K,MAXREC,K)
C USE STACKA TO ALLOCATE THE 3 BIG BUFFERS AND CALL LGM
        CALL STACKA_BIG(7,LGM,3,i8maxbuf,i8maxrec,i8k,MAXREC,K)
	RETURN
	END
C=================================================================================
	SUBROUTINE LGM(LBUF,L1,INCRMNTS,L2,GDIR,L3,MAXREC,K)
        integer*4 K,MAXREC
        integer*8 L1,L2,L3
	BYTE LBUF(L1)
	REAL*4 INCRMNTS(12,MAXREC)
	INTEGER*2 GDIR(*)
C
	print *,'Subroutine LGM'
        CALL CHECK_VMS_PAGE_FILE_QUOTA  ! DUMMY SUBROUTINE IF NOT VMS.

	CALL LGEM(LBUF,LBUF,LBUF,INCRMNTS,GDIR,GDIR,MAXREC,K)

C CALL LGEM TO DO THE WORK, PASSING LBUF 3 TIMES FOR THE EQUIVALENT
C BYTE, INTEGER*2 AND INTEGER*4 ARRAY, AND PASSING GDIR 2 TIMES.
C BDIR WILL BE PLACED RIGHT AFTER THE END OF THE GDIR THAT WAS
C ACTUALLY USED.  K IS THE ESTIMATED # OF BLOCKS ON IDS.  IF THE
C PAGE FILE QUOTA IS EXCEEDED, STACKA DOES NOT RETURN AN ERROR AND
C NO ERROR WILL OCCUR UNTIL THE UNAVAILABLE MEMORY IS ACTUALLY USED.
C THAT IS THE REASON FOR ONLY USING THE GDIR-BDIR SPACE THAT IS
C ACTUALLY REQUIRED AND POSSIBLY REQUESTING MORE THAN IS AVAILABLE.

	RETURN
	END
C==================================================================================
	SUBROUTINE LGEM(LBUF,I2BUF,BIGBUF,INCRMNTS,GDIR,BDIR,MAXREC,K)
	IMPLICIT NONE
	INTEGER MAXREC,K
	BYTE LBUF(*)
	INTEGER*2 I2BUF(*)
	INTEGER*4 BIGBUF(*)
	REAL*4 INCRMNTS(12,MAXREC)
	INTEGER*2 GDIR(*)
	INTEGER*4 BDIR(*),BO

C	INPUT,IDS,AND OUT ARE THE UNIT NUMBERS FOR VICAR2
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
	INTEGER*4 INPUT,IDS,OUT
C
	INTEGER*4 MAXNBPAR
	PARAMETER (MAXNBPAR=1200) ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4 LOC(MAXNBPAR)

      INTEGER*4 IUSE,IDEL,LPB,ILC,IMAX
      INTEGER*4 IRMIN,IR,IRMAX,INFORM,IFR,IND,ILB
      INTEGER*4 I,ITEMP,NSO,NRECT
      INTEGER*4 NB,LABREC
      REAL*4 RLT,FSSI,FLNT
C
C--EQUIVALENT AREA
      INTEGER*4 SLI,SSI,NLO,NBO,NLI,NBI
      INTEGER*4 LOCAT(10)
C
      INTEGER*4 CBBC,CCSD,CFBI,ICR,ISAMP,LD,MAXBBC
      INTEGER*4 SC,BLOCK,BUFLEN,MAXCSD
      INTEGER*4 B,FBI(MAXNBPAR)
      INTEGER*4 BBC(MAXNBPAR)
      LOGICAL*4 HALFW,DONE,NEXREC,XVPTST,NOIN,NOIZ, PRINT
      REAL*4 LARGE,SMALL,FLN,FSM,FSMT,RMIN
      REAL*4 RMAX
      CHARACTER*8 FMT
      CHARACTER*80 PBUF
C
C--INCLUDE COMMON BLOCK -- USED TO SEQUENCE BUFFERS

      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT

      COMMON/P1/ HALFW,NOIN,NOIZ,ILC,ILB,IUSE,IDEL,LPB,IMAX,NB,FSSI,FLNT

      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NBO,LOCAT(4)),(NLI,LOCAT(5)),(NBI,LOCAT(6))
C
C
C--Get tiepoints and fill array of INCRMNTS
      print *, 'GET_TIEPOINTS'
      CALL GET_TIEPOINTS(BIGBUF,INCRMNTS,NRECT,PRINT)
      IF(NRECT.GT.MAXREC) CALL MABEND('??E - TOO MANY RECTANGLES')
C
      IF (XVPTST('NOIN')) NOIN = .TRUE. 	! COULD ALREADY BE SET
      IF (XVPTST('NOIZ')) NOIZ = .TRUE.		! COULD ALREADY BE SET
      IF (NOIN) THEN
	  LARGE = 0.5
	  SMALL = 0.5
      ENDIF
C
      IRMIN=1
      IR=1
      IRMAX=ILC
C--BEGIN MAIN IRMIN LOOP
      DO WHILE (IRMIN.NE.NLI)
        IF (IRMAX.GT.NLI) IRMAX = NLI
C--BEGIN IR LOOP
        DO IR = IR,IRMAX
          BIGBUF(IR) = IUSE
C--STORE POINTER FOR NEW LINE
          ITEMP = IUSE+1
          IUSE = IUSE+IDEL
          IF (IUSE.GT.IMAX) IUSE = 4*NLI
	  CALL XVREAD(INPUT,LBUF(ITEMP),INFORM,'LINE',IR,' ')
C--END IR LOOP
        END DO
        IF (PRINT)  THEN
           WRITE (PBUF, 55) IRMIN, IRMAX
55         FORMAT('PROCESSING INPUT LINES',I6,' TO',I6)
           CALL XVMESSAGE( PBUF, ' ')
        END IF
C--DOUBLE CHECK FINAL VALUE OF IR FOR DIFFERENT COMPILERS
        IR = IRMAX+1
        RMIN = IRMIN-SMALL
        RMAX = IRMAX+SMALL
        ICR = 1
C--INDEX TO CURRENT RECTANGLE, ALSO USED TO START RECT SEARCH
        B = 1
        CFBI = FBI(1)
        CBBC = BBC(1)
        LD = 0
        FLN = SLI
        FSM = FSSI
C
C--FLN IS CURRENT LINE AND FSM IS CURRENT SAMPLE
C
C--BEGIN OUTER FLN LOOP
        DONE = .FALSE.
        DO WHILE (.NOT.DONE)
C--DONE INDICATES WHETHER ALL LINES AND ALL RECTANGLES HAVE BEEN DONE,
C--     INDICATED BY THE CONDITION (FLN.GE.FLNT)
C--NEXREC INDICATES WHETHER THERE ARE ANY MORE RECTANGLES ON THE CURRENT
C--       LINE TO BE PROCESSED.
          NEXREC = .TRUE.
C
C--SEARCH FOR RECTANGLE LINE FOR FLN
C--IF LAST LINE OF RECTANGLES, TERMINATE SEARCH
          DO WHILE ((FLN.GT.INCRMNTS(2,ICR)).AND.
     2              (INCRMNTS(2,ICR).NE.INCRMNTS(2,NRECT)))
            ICR = ICR+1
          END DO
C
C--SEARCH FOR RECTANGLE ON THIS LINE FOR FSM
C--IF LAST RECTANGLE ON LINE, TERMINATE SEARCH
          DO WHILE ((FSM.GT.INCRMNTS(4,ICR)).AND. ICR .LT. NRECT .AND.
     2              (INCRMNTS(2,ICR).EQ.INCRMNTS(2,ICR+1)))
            ICR = ICR+1
          END DO
C--SAVE INDEX TO FIRST RECT ON LINE
          IFR = ICR
          RLT = INCRMNTS(2,ICR)
C--STORE LAST LINE THAT USES THIS RECTANGLE
          IF (RLT.EQ.INCRMNTS(2,NRECT)) RLT = FLNT
C
C--BEGIN INNER FLN LOOP
          DO WHILE (NEXREC.AND.(.NOT.DONE))
	   IF(ILB.NE.0) THEN ! IF ONE-PASS
	    IF (HALFW) THEN
		CALL GSUBH1(INCRMNTS,LBUF(ILB),LBUF,I2BUF,BIGBUF)
	      ELSE
		CALL GSUB1(INCRMNTS,LBUF(ILB),LBUF,I2BUF,BIGBUF)
	    ENDIF

	  ELSE
	
            IF (HALFW) THEN
                CALL GSUBH(INCRMNTS,GDIR,LBUF,I2BUF,BIGBUF,K)
              ELSE
                CALL GSUB(INCRMNTS,GDIR,LBUF,I2BUF,BIGBUF,K)
            END IF
            BBC(B) = CBBC

	  ENDIF

            FLN = FLN+1
C--CHECK FOR ALL LINES DONE
            IF (FLN.GE.FLNT) THEN
                DONE = .TRUE.
              ELSE
C--STEP LINE DISPLACEMENT
                LD = LD+1
C--CHECK FOR ALL LINES IN THIS BUFFER DONE
                IF (LD.GE.LPB) THEN
                  LD = 0
                  B = B+1
                  CFBI = FBI(B)
                  CBBC = BBC(B)
                END IF
                FSM = FSSI

                IF (FLN.GT.RLT) THEN
                    NEXREC = .FALSE.
                  ELSE
                    ICR = IFR
                END IF
            END IF
C--END INNER FLN LOOP
          END DO
C--END OUTER FLN LOOP
        END DO
        IRMIN = IRMAX
        IRMAX = IRMIN+ILC-1
        IR = IRMIN+1
C--END MAIN IRMIN LOOP
      END DO

	IF(ILB.NE.0) THEN ! IF ONE PASS
	  CALL XVCLOSE(OUT,IND,' ')
	  RETURN
	ENDIF

      DO B = 1,NB
        IF (BBC(B).NE.0) THEN
          ITEMP  =  (FBI(B)+1)/2
          I2BUF((BBC(B)+1)/2+ITEMP) = 0
	  CALL XVWRIT(IDS,LBUF(FBI(B)),IND,' ')
          BLOCK = BLOCK+1
	  IF(BLOCK.GT.K) CALL MABEND('??E - IDS_NL OVFLO, INCREASE RATIO')
	  CNT(B)=CNT(B)+1
	  GDIR(BLOCK)=B
        END IF
      END DO
C
C--UPDATE NL IN LABEL
      print *, 'UPDATE LABELS'
      CALL XLDEL(IDS,'SYSTEM','NL',IND,' ')
      CALL XLADD(IDS,'SYSTEM','NL',BLOCK,IND,'FORMAT','INT',' ')
C
      CALL XVCLOSE(IDS,IND,' ')
      CALL XVCLOSE(INPUT,IND,' ')
      CALL XVOPEN(IDS,IND,' ')
      CALL XVUNIT(OUT,'OUT',1,IND,' ')
      CALL XVOPEN(OUT,IND,'OP','WRITE','U_NS',NSO,'U_NL',NLO,'O_FORMAT',FMT,
     * 'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C--CONVERT GDIR TABLE TO BDIR TABLE FOR PASS 2
	LOC(1)=0
	DO 500 I=2,NB
500	LOC(I)=LOC(I-1)+CNT(I-1) ! SET UP LOC TABLE
	BO=(BLOCK+1)/2 ! OFFSET FOR BDIR TO PUT IT RIGHT AFTER GDIR
	DO 600 I=1,BLOCK
	B=GDIR(I)
	LOC(B)=LOC(B)+1
600	BDIR(LOC(B)+BO)=I ! STORE BLOCK # IN BDIR
C
C--CALL IN SECOND PASS TO READ INTERMEDIATE DISK AND WRITE OUTPUT
	print *, 'Call 2nd PASS'
      CALL LGP2(LABREC,NB,BUFLEN,NLO,NBO,LPB,CNT,BDIR(BO+1),LBUF,I2BUF,
     .          PRINT)
      CALL XVCLOSE(IDS,IND,'CLOS_ACT','DELETE',' ') 
      CALL XVCLOSE(OUT,IND,' ')
      RETURN
C--END OF MAIN44 (MAIN BODY OF LGEOM)
      END
C=================================================================================
C       GSUB - SUBROUTINE TO PROCESS ONE LINE OF BYTE DATA FOR LGEOM
C
      SUBROUTINE GSUB(INCRMNTS,GDIR,LBUF,I2BUF,BIGBUF,K)
C
C       NOTE:
C               IN THE COMMENTS, VERTICAL REFERS TO LINE POSITION
C               WHEN IT IS A REAL NUMBER, AND HORIZONTAL REFERS TO
C               SAMPLE POSITION WHEN IT IS A REAL NUMBER.
C
C               COORDINATE PAIRS ARE GIVEN WITH THE VERTICAL COORDINATE
C               FIRST.
C
C       METHOD:
C
C               EACH OUTPUT POINT IS USED TO GENERATE A COORDINATE IN THE
C               INPUT FILE FOR A DATA POINT, HERE DENOTED BY (R,P), WHERE
C               R IS THE LINE NUMBER AND P IS THE SAMPLE NUMBER.    IF
C               INTERPOLATION IS ACTIVE, THE POINT IS INTERPOLATED USING
C               ITS FOUR NEAREST NEIGHBORS AS FOLLOWS.
C
C               VAL1    TRY1                    VAL2
C                *       *                       *
C              (IR,IP)                         (IR,IP+1)
C
C
C                      (R,P)
C                        *
C
C
C              (IR+1,IP)                      (IR+1,IP+1)
C                *       *                       *
C               VAL3    TRY2                    VAL4
C
C
C               FIRST VAL1 AND VAL2 ARE INTERPOLATED SO THAT
C                       TRY1 = VAL1+(P-IP)*(VAL2-VAL1)
C
C               TRY2 IS THEN INTERPOLATED SIMILARLY USING VAL3 AND VAL4,
C               AND THEN A VALUE FOR POINT (R,P) IS INTERPOLATED FROM
C               TRY1 AND TRY2.
C
C
C--ALL VARIABLES MUST BE EXPLICITLY TYPED
      IMPLICIT NONE

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

C--PASSED VARIABLES
C
C       INCRMNTS    -- ARRAY CONTAINING INFORMATION OF RECTANGLES
C
        INTEGER*2 I2BUF(*)
        BYTE LBUF(*)
	REAL*4 INCRMNTS(12,*)
	INTEGER*2 GDIR(*)
C
C--LOCAL VARIABLES
C
C       DELP   -- CHANGE IN HORIZONTAL POSITION PER UNIT CHANGE IN VERTICAL
C                 POSITION.
C       DELR   -- CHANGE IN VERTICAL POSITION PER UNIT CHANGE IN HORIZONTAL
C                 POSITION.
C       ICR    -- INDEX TO FIRST DIMENSION OF INCRMNTS ARRAY (Index to Current
C                 Rectangle)
C	IDS    -- UNIT NUMBER OF INTERMEDIATE DATA SET
C       INDEX  -- TEMPORARY VARIABLE FOR ARRAY INDEXING
C       IP     -- INTEGER VALUE OF P
C       IR     -- INTEGER VALUE OF R
C       ITEMP  -- INTEGER TEMPORARY VARIABLE
C       ITEMP2 -- SECOND INTEGER TEMP.
C       NEXSAM -- FLAG TO INDICATE WHETHER THERE ARE MORE SAMPLES TO BE
C                 PROCESSED ON THE CURRENT LINE
C       NSAMP  -- NUMBER OF SAMPLES LEFT TO PROCESS
C       P      -- HORIZONTAL POSITION OF CURRENT POINT IN INPUT FILE.
C       PFRAC  -- FRACTIONAL PART OF P
C       POINT  -- PIXEL VALUE OF INTERPOLATED POINT (R,P) IN INPUT FILE
C       R      -- VERTICAL POSITION OF CURRENT POINT IN INPUT FILE
C       RER    -- FINAL VALUE OF R FOR CURRENT PASS
C       RFRAC  -- FRACTIONAL PART OF R
C       TEMP   -- REAL TEMPORARY VARIABLE
C       VAL1   -- VALUE OF POINT (IR,IP) OF INPUT PICTURE
C       VAL2   -- SAME FOR (IR,IP+1)
C       VAL3   -- SAME FOR (IR+1,IP)
C       VAL4   -- SAME FOR (IR+1,IP+1)
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
      INTEGER*4 INPUT,IDS,OUT
      CHARACTER*8 FMT
      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT


      LOGICAL*4 HALFW,NOIN,NOIZ
      INTEGER*4 DUMMY !DUMMY ARRAY USED TO PREVENT WARNINGS FROM COMPILERS THAT
                    !WANT ALL OCCURRENCES OF A COMMON BLOCK TO HAVE SAME SIXE.
      COMMON /P1/ HALFW,NOIN,NOIZ,DUMMY(9)
      INTEGER*4 NSAMP,IR,IP,ITEMP,ITEMP2,INDEX,LD,K
      INTEGER*4 VAL1,VAL2,VAL3,VAL4,POINT,SAMP,HFBI,HBBC,HCSD
      REAL*4 RER,R,P,DELR,DELP,TEMP,RFRAC,PFRAC,TEMP2,INZ
      LOGICAL*4 NEXSAM, PRINT
C
C--INCLUDE COMMON BLOCK
      INTEGER*4 ICR,CBBC,CCSD,CFBI,B,SC,ISAMP,BLOCK
      INTEGER*4 BUFLEN,MAXBBC,MAXCSD,LOCAT(10),BIGBUF(*)
      REAL*4 FLN,FSM,FSMT,LARGE,SMALL,RMIN,RMAX
      INTEGER*4 MAXNBPAR
      PARAMETER (MAXNBPAR=1200)     ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4   LOC(MAXNBPAR),FBI(MAXNBPAR),BBC(MAXNBPAR)
C
C--VARIABLES EQUIVALENCED INTO COMMON BLOCK
      INTEGER*4 SLI,SSI,NLO,NSO,NLI,NSI
C

      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NSO,LOCAT(4)),(NLI,LOCAT(5)),(NSI,LOCAT(6))
C
C--BEGIN EXECUTION
C
C--INITIALIZE FOR FIRST RUN
      SC = 0
      SAMP = 0
      HFBI = (CFBI+1)/2
      HBBC = CBBC/2
      HCSD = CCSD/2
C
C--BEGIN MAIN RECTANGLE PROCESSING LOOP
      DO WHILE (SAMP.LT.NSO)
C
C--DETERMINE COORDINATE IN INPUT FILE OF CURRENT POINT
        ITEMP = FLN-INCRMNTS(1,ICR)
        DELR = INCRMNTS(12,ICR)+ITEMP*INCRMNTS(11,ICR)
        DELP = INCRMNTS(7,ICR)+ITEMP*INCRMNTS(8,ICR)
        ITEMP2 = FSM-INCRMNTS(3,ICR)
        P = INCRMNTS(5,ICR)+ITEMP*INCRMNTS(9,ICR)+DELP*ITEMP2
        R = INCRMNTS(6,ICR)+ITEMP2*INCRMNTS(12,ICR)+ITEMP*
     2      (INCRMNTS(10,ICR)+ITEMP2*INCRMNTS(11,ICR))
        ITEMP = INCRMNTS(4,ICR)
        IF ((INCRMNTS(2,ICR).NE.INCRMNTS(2,ICR+1)).OR.(ITEMP.GT.FSMT))
     2       ITEMP = FSMT
C
        RER = (ITEMP-FSM)*DELR+R
        NSAMP = ITEMP-FSM
        FSM = ITEMP
        ISAMP = SAMP+NSAMP
C
        NEXSAM = .TRUE.
        IF (NSAMP .LE. 0)  NEXSAM = .FALSE.       ! ADDED FOR MAPTRANS.
C
C--BEGIN SAMPLE PROCESSING LOOP
        DO WHILE (NEXSAM)
C
C--BEGIN MAIN IF BLOCK
          IF (R.LT.RMIN) THEN
              IF (RER.LT.RMIN) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
              IF (SC.NE.0) THEN
                I2BUF(HFBI+HCSD) = SC
                SC = 0
              END IF
            ELSE IF (R.GE.RMAX) THEN	! CHANGED .GT. TO .GE.		HJF
              IF (RER.GE.RMAX) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
              IF (SC.NE.0) THEN
                I2BUF(HFBI+HCSD) = SC
                SC = 0
              END IF
            ELSE
              IR = R
              RFRAC = R-IR
              IP = P
              PFRAC = P-IP
C
C--CHECK FOR POINTS OUTSIDE OF BOUNDARY
              IF ((P.LT.0).OR.
     2           ((IP.EQ.0).AND.(PFRAC.LE.LARGE)).OR.
     3            (IP.GT.NSI).OR.
     4           ((IP.EQ.NSI).AND.(PFRAC.GE.SMALL))) THEN
C
C--CURRENT POINT NO GOOD
                  IF (SC.NE.0) THEN
                    I2BUF(HFBI+HCSD) = SC
                    SC = 0
                  END IF
                ELSE
C
C--POINT IS WITHIN BOUNDARIES
                  IF ((RFRAC.GE.SMALL).AND.(RFRAC.LT.LARGE) .AND.
     .                (RMAX .GE. IR+1)                          )  THEN
                      INDEX = BIGBUF(IR)+IP
                      VAL1 = BYTE2INT( LBUF(INDEX) )
                      IF (IP .EQ. NSI) THEN
                        VAL2 = VAL1
                      ELSE
                        VAL2 = BYTE2INT( LBUF(INDEX+1) )
                      END IF
                      INDEX = IP+BIGBUF(IR+1)
                      VAL3   = BYTE2INT( LBUF(INDEX) )
                      IF (IP .EQ. NSI) THEN
                        VAL4 = VAL3
                      ELSE
                        VAL4 = BYTE2INT( LBUF(INDEX+1) )
                      END IF
		      IF(NOIZ) THEN
			TEMP = INZ(FLOAT(VAL1),FLOAT(VAL2),PFRAC)
			TEMP2 = INZ(FLOAT(VAL3),FLOAT(VAL4),PFRAC)
			POINT = INZ(TEMP,TEMP2,RFRAC)+0.5
		      ELSE
                      	TEMP = FLOAT(VAL1)+PFRAC*(VAL2-VAL1)
                      	TEMP2 = FLOAT(VAL3)+PFRAC*(VAL4-VAL3)
                      	POINT = TEMP+RFRAC*(TEMP2-TEMP)+0.5
		      ENDIF
                    ELSE
                      IF (RFRAC.GE.LARGE) IR = IR+1
                      INDEX = IP+BIGBUF(IR)
                      VAL1 = BYTE2INT( LBUF(INDEX) )
                      POINT = VAL1
                      IF (PFRAC.GE.SMALL) THEN
                        VAL2 = BYTE2INT( LBUF(INDEX+1) )
                        POINT = VAL2
                        IF (PFRAC.LE.LARGE) THEN
			  IF(NOIZ) THEN
			   POINT = INZ(FLOAT(VAL1),FLOAT(VAL2),PFRAC)+.5
			  ELSE
                           POINT = FLOAT(VAL1)+PFRAC*(VAL2-VAL1)+0.5
			  ENDIF
                        END IF
                      END IF
                  END IF
                  IF ((SC.NE.0).AND.(CBBC.GE.MAXBBC)) THEN
                    I2BUF(HFBI+HCSD) = SC
                    SC = 0
                  END IF
                  IF (SC.EQ.0) THEN
C
C--FORCE CBBC TO BE A MULTIPLE OF TWO
                      IF ( MOD(CBBC,2) .EQ. 1) CBBC = CBBC+1
                      HBBC = CBBC/2
                      IF (CBBC.GE.MAXCSD) THEN
C--ZERO LAST STRING COUNT IN BUFFER
                        I2BUF(HFBI+HBBC) = 0
                        CBBC = 0
                        HBBC = 0
			CALL XVWRIT(IDS,LBUF(CFBI),ITEMP,' ')
                        BLOCK = BLOCK+1
		        IF(BLOCK.GT.K) GOTO 900
			CNT(B)=CNT(B)+1 ! ADD TO COUNT FOR THIS GROUP
			GDIR(BLOCK)=B ! SAVE GROUP # FOR THIS BLOCK
                      END IF
                      CCSD = CBBC
                      HCSD = HBBC
                      SC = 1
                      INDEX = HFBI+HBBC
C--STORE LINE DISPLACEMENT
                      I2BUF(INDEX+1) = LD
C--STORE SAMPLE DISPLACEMENT
                      I2BUF(INDEX+2) = SAMP
C--STORE FIRST DATA POINT IN STRING
                      LBUF(CFBI+CBBC+6) = INT2BYTE(POINT)
                      CBBC = CBBC+7
                    ELSE
                      SC = SC+1
                      LBUF(CFBI+CBBC) =  INT2BYTE(POINT)
                      CBBC = CBBC+1
                  END IF
C
C--END BOUNDARY CHECKING IF BLOCK
              END IF
C
C--PREPARE FOR NEXT SAMPLE IN RECTANGLE
              SAMP = SAMP+1
              R = R+DELR
              P = P+DELP
              NSAMP = NSAMP-1
C
C--END MAIN IF BLOCK
          END IF
          IF (NSAMP.EQ.0) NEXSAM = .FALSE.
C
C--END SAMPLE PROCESSING LOOP
        END DO
C
C--NEXT RECTANGLE
        ICR = ICR+1
C
C--END MAIN RECTANGLE PROCESSING LOOP
      END DO
      IF (SC.NE.0) THEN
        I2BUF(HFBI+HCSD) = SC
      END IF
      RETURN
C
900	CALL MABEND('??E - IDS_NL OVFLO, INCREASE RATIO')
C--END OF SUBROUTINE GSUB
      END
C=================================================================================
C       GSUBH  - SUBROUTINE TO PROCESS ONE LINE OF HALFWORD DATA FOR LGEOM
C
C       IDENTICAL TO GSUB EXCEPT FOR INDEXING OF ARRAYS AND LENGTH
C       OF DATA (HALFWORD INSTEAD OF BYTE)
C
      SUBROUTINE GSUBH(INCRMNTS,GDIR,LBUF,I2BUF,BIGBUF,K)
C
C--ALL VARIABLES MUST BE EXPLICITLY TYPED
      IMPLICIT NONE

C
C--PASSED VARIABLES
C
        INTEGER*2 I2BUF(*)
        BYTE LBUF(*)
	REAL*4 INCRMNTS(12,*)
	INTEGER*2 GDIR(*)
C
C--LOCAL VARIABLES
C
C       DELP   -- CHANGE IN HORIZONTAL POSITION PER UNIT CHANGE IN VERTICAL
C                 POSITION.
C       DELR   -- CHANGE IN VERTICAL POSITION PER UNIT CHANGE IN HORIZONTAL
C                 POSITION.
C       ICR    -- INDEX TO FIRST DIMENSION OF INCRMNTS ARRAY (Index to Current
C                 Rectangle)
C	IDS    -- UNIT NUMBER OF INTERMEDIATE DATA SET
C       INDEX  -- TEMPORARY VARIABLE FOR ARRAY INDEXING
C       IP     -- INTEGER VALUE OF P
C       IR     -- INTEGER VALUE OF R
C       ITEMP  -- INTEGER TEMPORARY VARIABLE
C       ITEMP2 -- SECOND INTEGER TEMP.
C       NEXSAM -- FLAG TO INDICATE WHETHER THERE ARE MORE SAMPLES TO BE
C                 PROCESSED ON THE CURRENT LINE
C       NSAMP  -- NUMBER OF SAMPLES LEFT TO PROCESS
C	NWI    -- LOCAL VERSION OF NSI (NUMBER SAMPLES IN).  NOTE THAT NSI IN
C                 GSUBH IS THE SAME AS NBI IN MAIN44 BECAUSE OF THE WAY THAT
C                 THE COMMON BLOCK IS SET UP.
C       P      -- HORIZONTAL POSITION OF CURRENT POINT IN INPUT FILE.
C       PFRAC  -- FRACTIONAL PART OF P
C       POINT  -- PIXEL VALUE OF INTERPOLATED POINT (R,P) IN INPUT FILE
C       R      -- VERTICAL POSITION OF CURRENT POINT IN INPUT FILE
C       RER    -- FINAL VALUE OF R FOR CURRENT PASS
C       RFRAC  -- FRACTIONAL PART OF R
C       TEMP   -- REAL TEMPORARY VARIABLE
C       VAL1   -- VALUE OF POINT (IR,IP) OF INPUT PICTURE
C       VAL2   -- SAME FOR (IR,IP+1)
C       VAL3   -- SAME FOR (IR+1,IP)
C       VAL4   -- SAME FOR (IR+1,IP+1)
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
      INTEGER*4 INPUT,IDS,OUT
      CHARACTER*8 FMT
      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT
      LOGICAL*4 HALFW,NOIN,NOIZ
      INTEGER*4 DUMMY !DUMMY ARRAY USED TO PREVENT WARNINGS FROM COMPILERS THAT
                    !WANT ALL OCCURRENCES OF A COMMON BLOCK TO HAVE SAME SIXE.
      COMMON /P1/ HALFW,NOIN,NOIZ,DUMMY(9)
      INTEGER*4 NSAMP,IR,IP,ITEMP,ITEMP2,INDEX,LD,SAMP,HFBI,CSD,HBBC,NWI
      REAL*4 RER,R,P,DELR,DELP,TEMP,RFRAC,PFRAC,VAL1,VAL2,VAL3
      REAL*4 VAL4,TEMP2,POINT,INZ
      LOGICAL*4 NEXSAM, PRINT
C--INCLUDE COMMON BLOCK
      INTEGER*4 ICR,CBBC,CCSD,CFBI,B,SC,ISAMP,BLOCK,K
      INTEGER*4 BUFLEN,MAXBBC,MAXCSD,LOCAT(10),BIGBUF(*)
      REAL*4 FLN,FSM,FSMT,LARGE,SMALL,RMIN,RMAX
      INTEGER*4 MAXNBPAR
      PARAMETER (MAXNBPAR=1200)     ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4   LOC(MAXNBPAR),FBI(MAXNBPAR),BBC(MAXNBPAR)
C
C--VARIABLES EQUIVALENCED INTO COMMON BLOCK
      INTEGER*4 SLI,SSI,NLO,NSO,NLI,NSI
C
      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NSO,LOCAT(4)),(NLI,LOCAT(5)),(NSI,LOCAT(6))
C
C--BEGIN EXECUTION
C
C--INITIALIZE FOR FIRST RUN
      SC = 0
      SAMP = 0
      NWI = NSI/2
      HFBI = (CFBI+1)/2
      CSD = CCSD/2
      HBBC = CBBC/2
C
C--BEGIN MAIN RECTANGLE PROCESSING LOOP
      DO WHILE (SAMP.LT.NSO)
C
C--DETERMINE COORDINATE IN INPUT FILE OF CURRENT POINT
        ITEMP = FLN-INCRMNTS(1,ICR)
        DELR = INCRMNTS(12,ICR)+ITEMP*INCRMNTS(11,ICR)
        DELP = INCRMNTS(7,ICR)+ITEMP*INCRMNTS(8,ICR)
        ITEMP2 = FSM-INCRMNTS(3,ICR)
        P = INCRMNTS(5,ICR)+ITEMP*INCRMNTS(9,ICR)+DELP*ITEMP2
        R = INCRMNTS(6,ICR)+ITEMP2*INCRMNTS(12,ICR)+ITEMP*
     2      (INCRMNTS(10,ICR)+ITEMP2*INCRMNTS(11,ICR))
        ITEMP = INCRMNTS(4,ICR)
        IF ((INCRMNTS(2,ICR).NE.INCRMNTS(2,ICR+1)).OR.(ITEMP.GT.FSMT)) 
     2       ITEMP = FSMT
C
        RER = (ITEMP-FSM)*DELR+R
        NSAMP = ITEMP-FSM
        FSM = ITEMP
        NSAMP = NSAMP*2
        ISAMP = SAMP+NSAMP
C
        NEXSAM = .TRUE.
        IF (NSAMP .LE. 0)  NEXSAM = .FALSE.       ! ADDED FOR MAPTRANS.
C
C--BEGIN SAMPLE PROCESSING LOOP
        DO WHILE (NEXSAM)
C
C--BEGIN MAIN IF BLOCK
          IF (R.LT.RMIN) THEN
              IF (RER.LT.RMIN) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+2
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-2
              END IF
              IF (SC.NE.0) THEN
                I2BUF(HFBI+CSD) = SC
                SC = 0
              END IF
            ELSE IF (R.GE.RMAX) THEN
              IF (RER.GE.RMAX) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+2
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-2
              END IF
              IF (SC.NE.0) THEN
                I2BUF(HFBI+CSD) = SC
                SC = 0
              END IF
            ELSE
              IR = R
              RFRAC = R-IR
              IP = P
              PFRAC = P-IP
C
C--CHECK FOR POINTS OUTSIDE OF BOUNDARY
              IF ((P.LT.0).OR.
     2           ((IP.EQ.0).AND.(PFRAC.LE.LARGE)).OR.
     3            (IP.GT.NWI).OR.
     4           ((IP.EQ.NWI).AND.(PFRAC.GE.SMALL))) THEN
C
C--CURRENT POINT NO GOOD
                  IF (SC.NE.0) THEN
                    I2BUF(HFBI+CSD) = SC
                    SC = 0
                  END IF
                ELSE
C
C--POINT IS WITHIN BOUNDARIES
                  IF ((RFRAC.GE.SMALL).AND.(RFRAC.LT.LARGE) .AND.
     .                (RMAX .GE. IR+1)                          )  THEN
                      INDEX = (BIGBUF(IR)+1)/2+IP
                      VAL1 = I2BUF(INDEX)
                      IF (IP .EQ. NWI) THEN
                        VAL2 = VAL1
                      ELSE
                        VAL2 = I2BUF(INDEX+1)
                      END IF
                      INDEX = IP+(BIGBUF(IR+1)+1)/2
                      VAL3 = I2BUF(INDEX)
                      IF (IP .EQ. NWI) THEN
                        VAL4 = VAL3
                      ELSE
                        VAL4 = I2BUF(INDEX+1)
                      END IF
		      IF(NOIZ) THEN
			TEMP = INZ(VAL1,VAL2,PFRAC)
			TEMP2 = INZ(VAL3,VAL4,PFRAC)
			POINT = INZ(TEMP,TEMP2,RFRAC)
		      ELSE
                      	TEMP = VAL1+PFRAC*(VAL2-VAL1)
                      	TEMP2 = VAL3+PFRAC*(VAL4-VAL3)
                      	POINT = TEMP+RFRAC*(TEMP2-TEMP)
		      ENDIF
                    ELSE
                      IF (RFRAC.GE.LARGE) IR = IR+1
                      INDEX = IP+(BIGBUF(IR)+1)/2
                      POINT = I2BUF(INDEX)
                      IF (PFRAC.GE.SMALL) THEN
                        VAL1 = POINT
                        POINT = I2BUF(INDEX+1)
                        IF (PFRAC.LE.LARGE) THEN
                          VAL2 = POINT
			  IF(NOIZ) THEN
			    POINT = INZ(VAL1,VAL2,PFRAC)
			  ELSE
                            POINT = VAL1+PFRAC*(VAL2-VAL1)
			  ENDIF
                        END IF
                      END IF
                  END IF
                  IF ((SC.NE.0).AND.(HBBC.GE.MAXBBC/2)) THEN
                    I2BUF(HFBI+CSD) = SC
                    SC = 0
                  END IF
                  IF (SC.EQ.0) THEN
                      IF (HBBC.GE.MAXCSD/2) THEN
C--ZERO LAST STRING COUNT IN BUFFER
                        I2BUF(HFBI+HBBC) = 0
                        HBBC = 0
			CALL XVWRIT(IDS,LBUF(HFBI*2-1),ITEMP,' ')
                        BLOCK = BLOCK+1
		        IF(BLOCK.GT.K) GOTO 900
                        CNT(B)=CNT(B)+1
			GDIR(BLOCK)=B
                      END IF
                      CSD = HBBC
                      SC = 2
                      INDEX = HFBI+HBBC
C--STORE LINE DISPLACEMENT
                      I2BUF(INDEX+1) = LD
C--STORE SAMPLE DISPLACEMENT
                      I2BUF(INDEX+2) = SAMP
C--STORE FIRST DATA POINT IN STRING
                      I2BUF(INDEX+3) = NINT( POINT )
                      HBBC = HBBC+4
                    ELSE
                      SC = SC+2
                      INDEX = HFBI+HBBC
                      I2BUF(INDEX) = NINT( POINT )
                      HBBC = HBBC+1
                  END IF
C
C--END BOUNDARY CHECKING IF BLOCK
              END IF
C
C--PREPARE FOR NEXT SAMPLE IN RECTANGLE
              SAMP = SAMP+2
              R = R+DELR
              P = P+DELP
              NSAMP = NSAMP-2
C
C--END MAIN IF BLOCK
          END IF
          IF (NSAMP.EQ.0) NEXSAM = .FALSE.
C
C--END SAMPLE PROCESSING LOOP
        END DO
C
C--NEXT RECTANGLE
        ICR = ICR+1
C
C--END MAIN RECTANGLE PROCESSING LOOP
      END DO
      IF (SC.NE.0) THEN
        I2BUF(HFBI+CSD) = SC
      END IF
      CFBI = HFBI*2-1
      CBBC = HBBC*2
      CCSD = CSD*2
      RETURN
C
900	CALL MABEND('??E - IDS_NL OVFLO, INCREASE RATIO')
C--END OF SUBROUTINE GSUBH
      END
C=================================================================================
C       GSUB1 - SUBROUTINE TO PROCESS ONE LINE OF BYTE DATA FOR LGEOM (1 PASS)
C
      SUBROUTINE GSUB1(INCRMNTS,LINBUF,LBUF,I2BUF,BIGBUF)
C
C--ALL VARIABLES MUST BE EXPLICITLY TYPED
      IMPLICIT NONE

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

C
C--PASSED VARIABLES
C
C       INCRMNTS    -- ARRAY CONTAINING INFORMATION ON RECTANGLES
C
        INTEGER*2 I2BUF(*)
        BYTE LBUF(*)
	REAL*4 INCRMNTS(12,*)
	BYTE LINBUF(*)
C
C--LOCAL VARIABLES
C
C       DELP   -- CHANGE IN HORIZONTAL POSITION PER UNIT CHANGE IN VERTICAL
C                 POSITION.
C       DELR   -- CHANGE IN VERTICAL POSITION PER UNIT CHANGE IN HORIZONTAL
C                 POSITION.
C       ICR    -- INDEX TO FIRST DIMENSION OF INCRMNTS ARRAY (Index to Current
C                 Rectangle)
C	OUT    -- UNIT NUMBER OF OUTPUT DATA SET
C       INDEX  -- TEMPORARY VARIABLE FOR ARRAY INDEXING
C       IP     -- INTEGER VALUE OF P
C       IR     -- INTEGER VALUE OF R
C       ITEMP  -- INTEGER TEMPORARY VARIABLE
C       ITEMP2 -- SECOND INTEGER TEMP.
C       NEXSAM -- FLAG TO INDICATE WHETHER THERE ARE MORE SAMPLES TO BE
C                 PROCESSED ON THE CURRENT LINE
C       NSAMP  -- NUMBER OF SAMPLES LEFT TO PROCESS
C       P      -- HORIZONTAL POSITION OF CURRENT POINT IN INPUT FILE.
C       PFRAC  -- FRACTIONAL PART OF P
C       POINT  -- PIXEL VALUE OF INTERPOLATED POINT (R,P) IN INPUT FILE
C       R      -- VERTICAL POSITION OF CURRENT POINT IN INPUT FILE
C       RER    -- FINAL VALUE OF R FOR CURRENT PASS
C       RFRAC  -- FRACTIONAL PART OF R
C       TEMP   -- REAL TEMPORARY VARIABLE
C       VAL1   -- VALUE OF POINT (IR,IP) OF INPUT PICTURE
C       VAL2   -- SAME FOR (IR,IP+1)
C       VAL3   -- SAME FOR (IR+1,IP)
C       VAL4   -- SAME FOR (IR+1,IP+1)
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
      INTEGER*4 INPUT,IDS,OUT
      CHARACTER*8 FMT
      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT
      LOGICAL*4 HALFW,NOIN,NOIZ
      INTEGER*4 DUMMY !DUMMY ARRAY USED TO PREVENT WARNINGS FROM COMPILERS THAT
                    !WANT ALL OCCURRENCES OF A COMMON BLOCK TO HAVE SAME SIXE.
      COMMON /P1/ HALFW,NOIN,NOIZ,DUMMY(9)
      INTEGER*4 NSAMP,IR,IP,ITEMP,ITEMP2,INDEX,LD
      INTEGER*4 VAL1,VAL2,VAL3,VAL4,POINT,SAMP
      REAL*4 RER,R,P,DELR,DELP,TEMP,RFRAC,PFRAC,TEMP2,INZ
      LOGICAL*4 NEXSAM, PRINT
C
C--INCLUDE COMMON BLOCK
      INTEGER*4 ICR,CBBC,CCSD,CFBI,B,SC,ISAMP,BLOCK
      INTEGER*4 BUFLEN,MAXBBC,MAXCSD,LOCAT(10),BIGBUF(*)
      REAL*4 FLN,FSM,FSMT,LARGE,SMALL,RMIN,RMAX
      INTEGER*4 MAXNBPAR
      PARAMETER (MAXNBPAR=1200)     ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4   LOC(MAXNBPAR),FBI(MAXNBPAR),BBC(MAXNBPAR)
C
C--VARIABLES EQUIVALENCED INTO COMMON BLOCK
      INTEGER*4 SLI,SSI,NLO,NSO,NLI,NSI
C
      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NSO,LOCAT(4)),(NLI,LOCAT(5)),(NSI,LOCAT(6))
C
      SAMP = 0
      CALL ZBA(LINBUF,NSO) ! ZERO THE OUTPUT BUFFER
C
C--BEGIN MAIN RECTANGLE PROCESSING LOOP
      DO WHILE (SAMP.LT.NSO)
C
C--DETERMINE COORDINATE IN INPUT FILE OF CURRENT POINT
        ITEMP = FLN-INCRMNTS(1,ICR)
        DELR = INCRMNTS(12,ICR)+ITEMP*INCRMNTS(11,ICR)
        DELP = INCRMNTS(7,ICR)+ITEMP*INCRMNTS(8,ICR)
        ITEMP2 = FSM-INCRMNTS(3,ICR)
        P = INCRMNTS(5,ICR)+ITEMP*INCRMNTS(9,ICR)+DELP*ITEMP2
        R = INCRMNTS(6,ICR)+ITEMP2*INCRMNTS(12,ICR)+ITEMP*
     2      (INCRMNTS(10,ICR)+ITEMP2*INCRMNTS(11,ICR))
        ITEMP = INCRMNTS(4,ICR)
        IF ((INCRMNTS(2,ICR).NE.INCRMNTS(2,ICR+1)).OR.(ITEMP.GT.FSMT))
     2       ITEMP = FSMT
C
        RER = (ITEMP-FSM)*DELR+R
        NSAMP = ITEMP-FSM
        FSM = ITEMP
        ISAMP = SAMP+NSAMP
C
        NEXSAM = .TRUE.
        IF (NSAMP .LE. 0)  NEXSAM = .FALSE.       ! ADDED FOR MAPTRANS.
C
C--BEGIN SAMPLE PROCESSING LOOP
        DO WHILE (NEXSAM)
C
C--BEGIN MAIN IF BLOCK
          IF (R.LT.RMIN) THEN
              IF (RER.LT.RMIN) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
            ELSE IF (R.GE.RMAX) THEN
              IF (RER.GE.RMAX) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
            ELSE
              IR = R
              RFRAC = R-IR
              IP = P
              PFRAC = P-IP
C
C--CHECK FOR POINTS OUTSIDE OF BOUNDARY
              IF ((P.LT.0).OR.
     2           ((IP.EQ.0).AND.(PFRAC.LE.LARGE)).OR.
     3            (IP.GT.NSI).OR.
     4           ((IP.EQ.NSI).AND.(PFRAC.GE.SMALL))) GOTO 100
C
C--POINT IS WITHIN BOUNDARIES
                  IF ((RFRAC.GE.SMALL).AND.(RFRAC.LT.LARGE) .AND.
     .                (RMAX .GE. IR+1)                          )  THEN
                      INDEX = BIGBUF(IR)+IP
                      VAL1 = BYTE2INT( LBUF(INDEX) )
                      IF (IP .EQ. NSI) THEN
                        VAL2 = VAL1
                      ELSE
                        VAL2 = BYTE2INT( LBUF(INDEX+1) )
                      END IF
                      INDEX = IP+BIGBUF(IR+1)
                      VAL3 = BYTE2INT( LBUF(INDEX) )
                      IF (IP .EQ. NSI) THEN
                        VAL4 = VAL3
                      ELSE
                        VAL4 = BYTE2INT( LBUF(INDEX+1) )
                      END IF
		      IF(NOIZ) THEN
			TEMP = INZ(FLOAT(VAL1),FLOAT(VAL2),PFRAC)
			TEMP2 = INZ(FLOAT(VAL3),FLOAT(VAL4),PFRAC)
			POINT = INZ(TEMP,TEMP2,RFRAC)+.5
		      ELSE
                      	TEMP = FLOAT(VAL1)+PFRAC*(VAL2-VAL1)
                      	TEMP2 = FLOAT(VAL3)+PFRAC*(VAL4-VAL3)
                      	POINT = TEMP+RFRAC*(TEMP2-TEMP)+0.5
		      ENDIF
                    ELSE
                      IF (RFRAC.GE.LARGE) IR = IR+1
                      INDEX = IP+BIGBUF(IR)
                      VAL1 = BYTE2INT( LBUF(INDEX) )
                      POINT = VAL1
                      IF (PFRAC.GE.SMALL) THEN
                        VAL2 = BYTE2INT( LBUF(INDEX+1) )
                        POINT = VAL2
                        IF (PFRAC.LE.LARGE) THEN
			  IF(NOIZ) THEN
			   POINT = INZ(FLOAT(VAL1),FLOAT(VAL2),PFRAC)+.5
			  ELSE
                           POINT = FLOAT(VAL1)+PFRAC*(VAL2-VAL1)+0.5
			  ENDIF
                        END IF
                      END IF
                  END IF

		LINBUF(SAMP+1)= INT2BYTE(POINT) ! STORE SAMPLE
C
C--PREPARE FOR NEXT SAMPLE IN RECTANGLE
100	      SAMP=SAMP+1
              R = R+DELR
              P = P+DELP
              NSAMP = NSAMP-1
C
C--END MAIN IF BLOCK
          END IF
          IF (NSAMP.EQ.0) NEXSAM = .FALSE.
C
C--END SAMPLE PROCESSING LOOP
        END DO
C
C--NEXT RECTANGLE
        ICR = ICR+1
C
C--END MAIN RECTANGLE PROCESSING LOOP
      END DO

	CALL XVWRIT(OUT,LINBUF,ITEMP,' ') ! WRITE OUTPUT LINE
	RETURN
C
C--END OF SUBROUTINE GSUB1
      END
C=================================================================================
C       GSUBH - SUBROUTINE TO PROCESS ONE LINE OF HALFWORD DATA FOR LGEOM (ONE-PASS)
C
C       SIMILAR TO GSUB1 EXCEPT FOR INDEXING OF ARRAYS AND LENGTH
C       OF DATA (HALFWORD INSTEAD OF BYTE)
C
      SUBROUTINE GSUBH1(INCRMNTS,LINBUF,LBUF,I2BUF,BIGBUF)
C
C--ALL VARIABLES MUST BE EXPLICITLY TYPED
      IMPLICIT NONE


C
C--PASSED VARIABLES
C
        INTEGER*2 I2BUF(*)
        BYTE LBUF(*)
	REAL*4 INCRMNTS(12,*)
	INTEGER*2 LINBUF(*)
C
C--LOCAL VARIABLES
C
C       DELP   -- CHANGE IN HORIZONTAL POSITION PER UNIT CHANGE IN VERTICAL
C                 POSITION.
C       DELR   -- CHANGE IN VERTICAL POSITION PER UNIT CHANGE IN HORIZONTAL
C                 POSITION.
C       ICR    -- INDEX TO FIRST DIMENSION OF INCRMNTS ARRAY (Index to Current
C                 Rectangle)
C	IDS    -- UNIT NUMBER OF INTERMEDIATE DATA SET
C       INDEX  -- TEMPORARY VARIABLE FOR ARRAY INDEXING
C       IP     -- INTEGER VALUE OF P
C       IR     -- INTEGER VALUE OF R
C       ITEMP  -- INTEGER TEMPORARY VARIABLE
C       ITEMP2 -- SECOND INTEGER TEMP.
C       NEXSAM -- FLAG TO INDICATE WHETHER THERE ARE MORE SAMPLES TO BE
C                 PROCESSED ON THE CURRENT LINE
C       NSAMP  -- NUMBER OF SAMPLES LEFT TO PROCESS
C	NWI    -- NUMBER OF WORDS INPUT
C       NWO    -- NUMBER OF WORDS OUTPUT
C       P      -- HORIZONTAL POSITION OF CURRENT POINT IN INPUT FILE.
C       PFRAC  -- FRACTIONAL PART OF P
C       POINT  -- PIXEL VALUE OF INTERPOLATED POINT (R,P) IN INPUT FILE
C       R      -- VERTICAL POSITION OF CURRENT POINT IN INPUT FILE
C       RER    -- FINAL VALUE OF R FOR CURRENT PASS
C       RFRAC  -- FRACTIONAL PART OF R
C       TEMP   -- REAL TEMPORARY VARIABLE
C       VAL1   -- VALUE OF POINT (IR,IP) OF INPUT PICTURE
C       VAL2   -- SAME FOR (IR,IP+1)
C       VAL3   -- SAME FOR (IR+1,IP)
C       VAL4   -- SAME FOR (IR+1,IP+1)
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
      INTEGER*4 INPUT,IDS,OUT
      CHARACTER*8 FMT
      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT
      LOGICAL*4 HALFW,NOIN,NOIZ
      INTEGER*4 DUMMY !DUMMY ARRAY USED TO PREVENT WARNINGS FROM COMPILERS THAT
                    !WANT ALL OCCURRENCES OF A COMMON BLOCK TO HAVE SAME SIXE.
      COMMON /P1/ HALFW,NOIN,NOIZ,DUMMY(9)
      INTEGER*4 NSAMP,IR,IP,ITEMP,ITEMP2,INDEX,LD,SAMP
      REAL*4 RER,R,P,DELR,DELP,TEMP,RFRAC,PFRAC,VAL1,VAL2,VAL3
      REAL*4 VAL4,TEMP2,POINT,INZ
      LOGICAL*4 NEXSAM, PRINT
C--INCLUDE COMMON BLOCK
      INTEGER*4 ICR,CBBC,CCSD,CFBI,B,SC,ISAMP,BLOCK
      INTEGER*4 BUFLEN,MAXBBC,MAXCSD,LOCAT(10),BIGBUF(*)
      REAL*4 FLN,FSM,FSMT,LARGE,SMALL,RMIN,RMAX
      INTEGER*4 MAXNBPAR
      PARAMETER (MAXNBPAR=1200)     ! MAXIMUM NUMBER OF BUFFERS FOR PASS 1.

	INTEGER*2 CNT(MAXNBPAR)
	INTEGER*4   LOC(MAXNBPAR),FBI(MAXNBPAR),BBC(MAXNBPAR)
C
C--VARIABLES EQUIVALENCED INTO COMMON BLOCK
      INTEGER*4 SLI,SSI,NLO,NBO,NLI,NBI,NWO,NWI
C
      COMMON /A1/ ICR,FLN,FSM,FSMT,LARGE,SMALL,
     .             RMIN,RMAX,CBBC,CCSD,CFBI,B,PRINT,
     .             SC,ISAMP,LD,BLOCK,BUFLEN,MAXBBC,MAXCSD,
     .             LOCAT,FBI,BBC,CNT,LOC
C
      EQUIVALENCE (SLI,LOCAT(1)),(SSI,LOCAT(2)),(NLO,LOCAT(3)),
     2            (NBO,LOCAT(4)),(NLI,LOCAT(5)),(NBI,LOCAT(6))
C
C
      SAMP = 0
      NWI = NBI/2
      NWO = NBO/2
      CALL ZBA(LINBUF,NBO) ! ZERO THE OUTPUT LINE
C
C--BEGIN MAIN RECTANGLE PROCESSING LOOP
      DO WHILE (SAMP.LT.NWO)
C
C--DETERMINE COORDINATE IN INPUT FILE OF CURRENT POINT
        ITEMP = FLN-INCRMNTS(1,ICR)
        DELR = INCRMNTS(12,ICR)+ITEMP*INCRMNTS(11,ICR)
        DELP = INCRMNTS(7,ICR)+ITEMP*INCRMNTS(8,ICR)
        ITEMP2 = FSM-INCRMNTS(3,ICR)
        P = INCRMNTS(5,ICR)+ITEMP*INCRMNTS(9,ICR)+DELP*ITEMP2
        R = INCRMNTS(6,ICR)+ITEMP2*INCRMNTS(12,ICR)+ITEMP*
     2      (INCRMNTS(10,ICR)+ITEMP2*INCRMNTS(11,ICR))
        ITEMP = INCRMNTS(4,ICR)
        IF ((INCRMNTS(2,ICR).NE.INCRMNTS(2,ICR+1)).OR.(ITEMP.GT.FSMT)) 
     2       ITEMP = FSMT
C
        RER = (ITEMP-FSM)*DELR+R
        NSAMP = ITEMP-FSM
        FSM = ITEMP
        ISAMP = SAMP+NSAMP
C
        NEXSAM = .TRUE.
        IF (NSAMP .LE. 0)  NEXSAM = .FALSE.       ! ADDED FOR MAPTRANS.
C
C--BEGIN SAMPLE PROCESSING LOOP
        DO WHILE (NEXSAM)
C
C--BEGIN MAIN IF BLOCK
          IF (R.LT.RMIN) THEN
              IF (RER.LT.RMIN) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
            ELSE IF (R.GE.RMAX) THEN
              IF (RER.GE.RMAX) THEN
                  SAMP = ISAMP
                  NEXSAM = .FALSE.
                ELSE 
                  SAMP = SAMP+1
                  R = R+DELR
                  P = P+DELP
                  NSAMP = NSAMP-1
              END IF
            ELSE
              IR = R
              RFRAC = R-IR
              IP = P
              PFRAC = P-IP
C
C--CHECK FOR POINTS OUTSIDE OF BOUNDARY
              IF ((P.LT.0).OR.
     2           ((IP.EQ.0).AND.(PFRAC.LE.LARGE)).OR.
     3            (IP.GT.NWI).OR.
     4           ((IP.EQ.NWI).AND.(PFRAC.GE.SMALL))) GOTO 100
C
C--POINT IS WITHIN BOUNDARIES
                  IF ((RFRAC.GE.SMALL).AND.(RFRAC.LT.LARGE) .AND.
     .                (RMAX .GE. IR+1)                          )  THEN
                      INDEX = (BIGBUF(IR)+1)/2+IP
                      VAL1 = I2BUF(INDEX)
                      IF (IP .EQ. NWI) THEN
                        VAL2 = VAL1
                      ELSE
                        VAL2 = I2BUF(INDEX+1)
                      END IF
                      INDEX = IP+(BIGBUF(IR+1)+1)/2
                      VAL3 = I2BUF(INDEX)
                      IF (IP .EQ. NWI) THEN
                        VAL4 = VAL3
                      ELSE
                        VAL4 = I2BUF(INDEX+1)
                      END IF
		      IF(NOIZ) THEN
			TEMP = INZ(VAL1,VAL2,PFRAC)
			TEMP2 = INZ(VAL3,VAL4,PFRAC)
			POINT = INZ(TEMP,TEMP2,RFRAC)
		      ELSE
                      	TEMP = VAL1+PFRAC*(VAL2-VAL1)
                      	TEMP2 = VAL3+PFRAC*(VAL4-VAL3)
                      	POINT = TEMP+RFRAC*(TEMP2-TEMP)
		      ENDIF
                    ELSE
                      IF (RFRAC.GE.LARGE) IR = IR+1
                      INDEX = IP+(BIGBUF(IR)+1)/2
                      POINT = I2BUF(INDEX)
                      IF (PFRAC.GE.SMALL) THEN
                        VAL1 = POINT
                        POINT = I2BUF(INDEX+1)
                        IF (PFRAC.LE.LARGE) THEN
                          VAL2 = POINT
			  IF(NOIZ) THEN
			    POINT = INZ(VAL1,VAL2,PFRAC)
			  ELSE
                            POINT = VAL1+PFRAC*(VAL2-VAL1)
			  ENDIF
                        END IF
                      END IF
                  END IF

		LINBUF(SAMP+1) = NINT(POINT) ! STORE OUTPUT WORD
C
C--PREPARE FOR NEXT SAMPLE IN RECTANGLE
100           SAMP = SAMP+1
              R = R+DELR
              P = P+DELP
              NSAMP = NSAMP-1
C
C--END MAIN IF BLOCK
          END IF
          IF (NSAMP.EQ.0) NEXSAM = .FALSE.
C
C--END SAMPLE PROCESSING LOOP
        END DO
C
C--NEXT RECTANGLE
        ICR = ICR+1
C
C--END MAIN RECTANGLE PROCESSING LOOP
      END DO

	CALL XVWRIT(OUT,LINBUF,ITEMP,' ')

      RETURN
C
C--END OF SUBROUTINE GSUBH1
      END

C=================================================================================
C	INZ FUNCTION
C
C	INTERPOLATE NON-ZERO DN VALUES
C	USE NEAREST NEIGHBOR IF EITHER DN IS ZERO
C
	REAL*4 FUNCTION INZ(VAL1,VAL2,FRAC)
C
	IF(VAL1.EQ.0.0.OR.VAL2.EQ.0.0) THEN
		IF(FRAC .LT. .5) THEN
			INZ=VAL1
		ELSE
			INZ=VAL2
		ENDIF
	ELSE
		INZ=VAL1+FRAC*(VAL2-VAL1)
	ENDIF
	RETURN
	END
C=================================================================================
C       LGP2 - SECOND PASS FOR LGEOM
C
      SUBROUTINE LGP2(LABREC,NB,BUFLEN,NLO,NSO,LPB,CNT,BDIR,LBUF,I2BUF,
     .                PRINT)
C
      IMPLICIT NONE
C
C       PASSED VARIABLES:
C
	INTEGER*4 NB,BUFLEN,NLO,NSO,LPB,LABREC
	INTEGER*2 CNT(*)
	INTEGER*4 BDIR(*)
	BYTE LBUF(*)
	INTEGER*2 I2BUF(*)
        LOGICAL*4 PRINT
C
C       LOCAL VARIABLES:
C
C	V2UNITS KEEPS TRACK OF THE UNIT NUMBERS OF THE FILES
      INTEGER*4 INPUT,IDS,OUT
      CHARACTER*8 FMT
      COMMON /V2UNITS/ INPUT,IDS,OUT,FMT
      INTEGER*4 IB,G,ITEMP,NL
      INTEGER*4 SC,INDEX,I2IND,I,IND
      CHARACTER*80 PBUF
C
C
C
C--BEGIN EXECUTION
C
C--INITIALIZE ARRAY INDEXES
        print *, '2nd PASS.....'
	IB=1 		! INDEX TO BDIR
	G=1 		! GROUP NUMBER
	NL=NLO 		! SAVE NLO FOR TEST
c	print *,'BEFORE ZBA  LPB = ',LPB,nso
	print *,'LPB*NSO = ', LPB*NSO,' BUFLEN = ',BUFLEN
c        print *,'LBUF(BUFLEN+1) = ',LBUF(BUFLEN+1)
40	continue
  	CALL ZBA(LBUF(BUFLEN+1),LPB*NSO) ! ZERO LPB LINES OF OUTPUT
c	print *,'AFTER ZBA'
        IF (PRINT)  THEN
           WRITE (PBUF, 55) (G-1)*LPB+1,MIN(NL,G*LPB)
55         FORMAT('PROCESSING OUTPUT LINES',I6,' TO',I6)
           CALL XVMESSAGE( PBUF, ' ')
        END IF

80	IF(CNT(G).EQ.0) GOTO 300 ! IF NO MORE DATA FOR THIS GROUP
        print *, 'ib, BDIR(IB) = ',ib, BDIR(IB)
	CALL XVREAD(IDS,LBUF,IND,'LINE',BDIR(IB) ,' ')
	IB=IB+1
	CNT(G)=CNT(G)-1
	INDEX=1
	I2IND=1

150	SC=I2BUF(I2IND) ! STRING COUNT
	IF(SC.EQ.0) GOTO 80 ! IF END OF BLOCK, GOTO 80	

	ITEMP = I2BUF(I2IND+1)*NSO+I2BUF(I2IND+2)+BUFLEN
c        print *, 'BEFORE MVB ITEMP = ',itemp,' string ct = ',sc
c        print *, 'LBUF(INDEX+6) = ',LBUF(INDEX+6)
c        print *, '          '
	CALL MVB(LBUF(INDEX+6),LBUF(ITEMP+1),SC)
c        print *, 'AFTER MVB'
c        print *, 'String count = ',sc
	INDEX=(INDEX+6+SC)                         ! STEP INDEX 
        IF ( MOD(INDEX,2) .EQ. 0) INDEX= INDEX+1   ! (ADD 1 IF NECESSARY)
	I2IND=(INDEX+1)/2
	GOTO 150

300	INDEX=BUFLEN
	print *,'Before XVWRIT - LPB = ',LPB, ' NSO = ',nso
	DO 400 I=1,LPB
c        print *, 'index = ',index
c        print *, LBUF(INDEX+1)
c	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,' ') gives:
c	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'NSAMPS',LPB*NSO,' ') gives:
c	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'LINE',i,' ') gives:
c	[TAE-PRCSTRM] Abnormal process termination; process status code = 11.;
C	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'NSAMPS',nso,' ') gives 296028633888 bytes
C	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'NSAMPS',buflen,' ') gives 396892322592 bytes 
C	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'NSAMPS',1,' ') gives 234248600352 bytes - 5000 lines x 23420176 samples
	CALL XVWRIT(OUT,LBUF(INDEX+1),IND,'NSAMPS',1,' ')
        call chkstat (ind,'??E XVWrit error on output file',1,0,0)
cc        print *,'i = ',i
	NLO = NLO-1
	IF(NLO.EQ.0) then
		print *, ' NLO = 0'
		RETURN
	endif
400	INDEX=INDEX+NSO
	G=G+1 ! STEP TO NEXT GROUP
	GOTO 40
C
C--END OF LGP2
      END
C
C=================================================================================
      SUBROUTINE GET_TIEPOINTS(BUF,INCREMENTS,NRECT,PRINT)
      IMPLICIT NONE
C
C	GET_TIEPOINTS reads in the tiepoints parameter, calculates
C	the offsets needed by the main program, and stores them in
C	INCREMENTS.
C
C	Passed variables:
C
C INCREMENTS -- Array of increments needed by LGEOM to step across
C		the rectangles
C BUF	-- Large working buffer

      REAL*4 INCREMENTS(12,*),BUF(*)
C
C	Local variables:
C
C
C       Output image rectangle:         Input image quadrilateral
C
C                                                      * (A2,B2)
C       (X1,Y1)*      * (X1,Y2)         (A1,B1)*
C
C                                                    * (A4,B4)
C       (X2,Y1)*      * (X2,Y2)         (A3,B3)*
C
C NAH	-- Number of rectangles (areas) horizontally
C NAV	-- Number of rectangles (areas) vertically
C TX1,TY1 etc -- Temporary real variables to fill common block
C DELX,DELY  -- X2-X1,Y2-Y1
C TOP     -- index into buffer of top line of rects
C BOTTOM  -- index into buffer for bottom line of rects
C ROW     -- current rectangle row
C COL     -- current rectangle column
C NVALS   -- number of values needed in TIEPOINTS
C NRECT   -- total number of rectangles
C
      INTEGER*4 NAH,NAV,TOP,BOTTOM,ROW,COL,NVALS,I,N,NRECT
      CHARACTER*80 PBUF
      REAL*4 DELX,DELY
      REAL*4 X1,Y1,A1,B1,TX1,Y2,A2,B2,X2,TY1,A3,B3,TX2,TY2,A4,B4
C
C COUNT -- Variable count returned from xvparm
C DEFAULTED -- Default flag from xvparm
C PDS	-- Logical indicating the presence of a parameter data set.
C PRINT -- Print extra info.
      INTEGER*4 COUNT,DEFAULTED
      LOGICAL*4 PRINT
C
C--Begin execution
C
C--Check for the presence of a parameter data set
      CALL XVPCNT('INP',COUNT)
      IF (COUNT .EQ. 2) THEN
	  CALL LOAD_PDS(BUF,BUF,INCREMENTS,NRECT)
	  RETURN
      ENDIF
C
C--Get the size of the grid
      CALL XVPARM('NAH',NAH,COUNT,DEFAULTED,0)
      IF (COUNT .EQ. 0) THEN
	  CALL XVMESSAGE('??E - NAH not specified',' ')
	  CALL ABEND
      ENDIF
      CALL XVPARM('NAV',NAV,COUNT,DEFAULTED,0)
      IF (COUNT .EQ. 0) THEN
	  CALL XVMESSAGE('??E - NAV not specified',' ')
	  CALL ABEND
      ENDIF
C
C--Get the tiepoints
      CALL XVPARM('TIEPOINT',BUF,COUNT,DEFAULTED,0)
C
C--Check for proper number of tiepoints
      NVALS = 4*(NAH + 1) * (NAV + 1)
      IF (COUNT .NE. NVALS) THEN
	  CALL XVMESSAGE('??E - Improper number of tiepoints specified',' ')
	  CALL ABEND
      ENDIF

C CODE TO PRINT TIEPOINTS

      IF (PRINT) THEN
      CALL XVMESSAGE( '   TIEPOINTS:',' ')
      DO I = 1, NVALS, 4
         WRITE(PBUF,9000) BUF(I), BUF(I+1), BUF(I+2), BUF(I+3)
9000     FORMAT( 4F10.3 )
         CALL XVMESSAGE ( PBUF(1:40), ' ')
      END DO
      END IF
C
C--Verify that the rectangles in the output image are whole numbers
      NVALS = 4*(NAH + 1)
      DO ROW = 1, NAV*NVALS+1, NVALS
	X1 = BUF(ROW)
	I = X1			! TRUNCATE
	IF ((X1 - I) .NE. 0) THEN
	    CALL XVMESSAGE
     2      ('??E - Grid on output not on integral pixel numbers',' ')
	    CALL ABEND
	ENDIF
      ENDDO
      DO COL = 1, 4*NAH+1, 4
	Y1 = BUF(COL)
	I = Y1			! TRUNCATE
	IF ((Y1 - I) .NE. 0) THEN
	    CALL XVMESSAGE
     2      ('??E - Grid on output not on integral pixel numbers',' ')
	    CALL ABEND
	ENDIF
      ENDDO
C
C--Cycle through the rectangles, storing each set of increments
      N = 1
      TOP = 1
      BOTTOM = 4 * NAH + 5
      DO ROW = 1,NAV
	DO COL = 1,NAH
C
C--Move current tiepoint pair into local variables

          X1 = BUF(TOP )
          Y1 = BUF(TOP+1 )
          A1 = BUF(TOP+2 )
          B1 = BUF(TOP+3 )
          TX1 = BUF(TOP+4 )
          Y2 = BUF(TOP+5 )
          A2 = BUF(TOP+6 )
          B2 = BUF(TOP+7 )

          X2 = BUF(BOTTOM )
          TY1 = BUF(BOTTOM+1 )
          A3 = BUF(BOTTOM+2 )
          B3 = BUF(BOTTOM+3 )
          TX2 = BUF(BOTTOM+4 )
          TY2 = BUF(BOTTOM+5 )
          A4 = BUF(BOTTOM+6 )
          B4 = BUF(BOTTOM+7 )
C
C--Verify that tiepoint represents a rectangle
	  IF ((X1 .NE. TX1) .OR. (X2 .NE. TX2) .OR.
     2      (Y1 .NE. TY1) .OR. (Y2 .NE. TY2)) 
     3      CALL MABEND('??E - Tiepoints are not rectangular')
C
C--Now store the increments
	  INCREMENTS(1,N) = X1
	  INCREMENTS(2,N) = X2
	  INCREMENTS(3,N) = Y1
	  INCREMENTS(4,N) = Y2
	  INCREMENTS(5,N) = B1
	  INCREMENTS(6,N) = A1
	  DELX = X2 - X1
	  DELY = Y2 - Y1
	  INCREMENTS(7,N) = (B2 - B1)/DELY
	  INCREMENTS(8,N) = (B4 - B3 - B2 + B1) / (DELX * DELY)
	  INCREMENTS(9,N) = (B3 - B1) / DELX
	  INCREMENTS(10,N) = (A3 - A1) / DELX
	  INCREMENTS(11,N) = (A4 - A3 - A2 + A1) / (DELY * DELX)
	  INCREMENTS(12,N) = (A2 - A1) / DELY
C
C--Next rectangle
	  TOP = TOP + 4
	  BOTTOM = BOTTOM + 4
	  N = N + 1
	ENDDO
C
C--Next line of rectangles
	TOP = TOP + 4
	BOTTOM = BOTTOM + 4
      ENDDO
      NRECT = N - 1
C
C--End of GET_TIEPOINTS
      RETURN
      END
C
C
C=================================================================================
      SUBROUTINE LOAD_PDS(BUF,I_BUF,INCREMENTS,ISAVE)
      IMPLICIT NONE

	INTEGER*4  MAXRPAR,INCRPAR
	PARAMETER (MAXRPAR=400) ! MAX RECTS FROM MAP2
	PARAMETER (INCRPAR=401) ! MAX RECTS FROM MAP2
C
C
C	LOAD_PDS gets the LGEOM parameters from a parameter
C	data set (PDS)  in the old format, and sorts them into
C	the INCREMENTS array.
C
C	Passed variables:
      REAL*4 BUF(*),INCREMENTS(12,INCRPAR)
      INTEGER*4 I_BUF(*)	! Same address as BUF
C
C	Local variables:
      CHARACTER*43 NORECT
      CHARACTER*38 NOSAMP
      CHARACTER*36 NOLINE
      CHARACTER*21 LINES  
      CHARACTER*21 RECTAN 
      CHARACTER*4  CSAVE
C
      INTEGER*4 IND,INPUT,NP,LOCSMP,I,J,K,INDEX,NSAMP,NB
      INTEGER*4 IM,IN,M,N,ISAVE,MLOC,NLOC
      INTEGER*2 ITAB(3,INCRPAR)
      REAL*4 ROW,SAM,VMA,TEMP,TEMPM,TEMP2
      LOGICAL*4 HALFW,NOIN,NOIZ
      INTEGER*4 DUMMY !DUMMY ARRAY USED TO PREVENT WARNINGS FROM COMPILERS THAT
                    !WANT ALL OCCURRENCES OF A COMMON BLOCK TO HAVE SAME SIXE.
      COMMON /P1/ HALFW,NOIN,NOIZ,DUMMY(9)

      DATA NORECT/'THERE WERE NOT ENOUGH PARAMETERS FOR GEOM.'/
      DATA NOSAMP/'GEOM COULD NOT FIND ANY SAMPLE CARDS.'/
      DATA NOLINE/'GEOM COULD NOT FIND ANY LINE CARDS.'/
      DATA LINES/'TOO MANY LINE CARDS.'/
      DATA RECTAN/'TOO MANY RECTANGLES.'/
C==================================================================
C
      CALL XVUNIT(INPUT,'INP',2,IND,' ')
      CALL XVOPEN(INPUT,IND,' ')
C
C--CALL READ READING INTO BUF
      CALL XVREAD(INPUT,BUF,IND,' ')
      NP = I_BUF(1)
      BUF(NP+2) = 0
      CALL XVCLOSE(INPUT,IND,' ')

C
C     THE LINE NUMBERS FROM THE USER INPUT CARDS ARE GOING TO BE STORED
C     IN ITAB AND THEN THEY WILL BE SORTED IN ASCENDING ORDER
C     WHEN THE COMPUTATION IS DONE FOR EACH RECTANGLE,THE CORRESPONDING
C     SAMPLE NUMBERS MUST BE USED ALSO. LOCSMP WILL TELL WHERE IN BUF
C     THE SAMPLE VALUES ARE STORED WHICH CORRESPOND TO A LINE NUMBER.
C     K WILL BE USED TO COUNT THE NUMBER OF LINES IN ITAB
      LOCSMP=0
      K=0
      I = 1
C
C--BEGIN PARAMETER PROCESSING LOOP
      DO WHILE ((I.LT.NP).AND.(K.LT.MAXRPAR))
        I = I+1
        CALL MVLC(I_BUF(I), CSAVE, 4)   ! MOVE TO CHARACTER VARIABLE.
C
C--BEGIN PARAMETER PROCESSING IF BLOCK
        IF (CSAVE.EQ.'SAMP') THEN
            LOCSMP = I+2
          ELSE IF (CSAVE.EQ.'NBUF') THEN
	    NB = I_BUF(I+2)
          ELSE IF (CSAVE.EQ.'NOIN') THEN
C--FORCE NO INTERPOLATION (USE NEAREST POINT)
	    NOIN = .TRUE.	! SET FLAG FOR MAIN44
	  ELSE IF (CSAVE.EQ.'NOIZ') THEN
C--NO INTERPOLATION FOR ZERO DN POINTS
	    NOIZ = .TRUE.
          ELSE IF (CSAVE.EQ.'HALF') THEN
            HALFW = .TRUE.
          ELSE IF (CSAVE.EQ.'LINE') THEN
            K = K+1
            ITAB(1,K)=BUF(I+2)
            ITAB(2,K)=I+2
            ITAB(3,K)=LOCSMP
            IF (K.EQ.INCRPAR) CALL XVMESSAGE(LINES,' ')
C
C--END PARAMETER PROCESSING IF BLOCK
        END IF
C
C--END PARAMETER PROCESSING LOOP
      END DO
C
C
      IF (LOCSMP.EQ.0) THEN
C--NO KEYWORD 'SAMPLE' FOUND
        CALL XVMESSAGE(NOSAMP,' ')
        CALL ABEND
      END IF
C
C--NOW THAT THE LINES ARE ALL IN ITAB - SORT THEM IN ASCENDING ORDER
      DO I = 1,K-1
        M = I+1
        DO J = M,K
          IF (ITAB(1,I).GT.ITAB(1,J)) THEN
            DO N = 1,3
              ISAVE = ITAB(N,I)
              ITAB(N,I) = ITAB(N,J)
              ITAB(N,J) = ISAVE
            END DO
          END IF
        END DO
C--END OF SORT
      END DO
C--WHEN THE LINES ARE SORTED, PROCESS THE RECTANGLES IN THAT ORDER
C
C--ISAVE WILL INDICATE WHERE IN THE BUFFER INCREMENTS THE RESULTS OF THE
C--RECTANGLE CALCULATIONS SHOULD BE STORED
C
C--NSAMP WILL BE USED WITH EACH LINE THAT IS PROCESSED TO TELL HOW
C--MANY SAMPLES WERE SPECIFIED FOR THIS LINE. THE NUMBER OF
C--RECTANGULAR CALCULATIONS DONE WILL BE ONE LESS THAN THE NUMBER
C--OF SAMPLES
      ISAVE=0
C
C--BEGIN PARAMETER STORAGE LOOP
      J = 0
      DO WHILE ((J.LT.K).AND.(ISAVE.LT.MAXRPAR))
        J = J+1
        NSAMP = 1
        INDEX = ITAB(3,J)+1
C
C--COUNT THE NUMBER PER LINE
        CALL MVLC(BUF(INDEX), CSAVE, 4)   ! MOVE TO CHARACTER VARIABLE.
        DO WHILE ((CSAVE.NE.'LINE').AND.(INDEX.LE.NP))
          NSAMP = NSAMP+1
          INDEX = INDEX+1
          CALL MVLC(BUF(INDEX), CSAVE, 4)   ! MOVE TO CHARACTER VARIABLE.
        END DO
C
C--CHECK FOR ERROR (LACK OF KEYWORD "LINE")
        IF (INDEX.GT.NP) THEN
          CALL XVMESSAGE(NOLINE,' ')
          CALL ABEND
        END IF
C
C--IM IS A SUBSCRIPT TO PICK UP THE FIRST LINE IN THE RECTANGLE FROM
C--BUF
        IM=ITAB(2,J)
        CALL MVLC(BUF(IM+1+2*NSAMP), CSAVE, 4)   ! MOVE TO CHARACTER VARIABLE.
        IF (CSAVE .EQ.'LINE') THEN
C--INDEX IS RESET TO POINT TO THE FIRST OF THE SAMPLES
          INDEX = ITAB(3,J)
C--IN WILL POINT TO THE LOCATION OF THE SECOND LINE OF THE RECTANGLE
          IN=IM+3+2*NSAMP
          NSAMP = NSAMP-1
          M=BUF(IM)
          N=BUF(IN)
C--M AND N NOW HAVE THE LINES FOR THE RECTANGLE
          I = 0
C--BEGIN SAMPLE PROCESSING LOOP
          DO WHILE ((I.LT.NSAMP).AND.(ISAVE.LT.MAXRPAR))
            I = I+1
            ISAVE = ISAVE+1
            SAM = I_BUF(INDEX+1)-I_BUF(INDEX)
            ROW = N-M
            MLOC = IM+2*(I-1)+1
C--MLOC IS THE LOCATION IN THE BUFFER BUF OF THE DISPLACEMENTS FOR
C--THE FIRST LINE IN THE CURRENT RECTANGLE
            NLOC = IN+2*(I-1)+1
C--NLOC IS THE SAME FOR THE SECOND LINE
            INCREMENTS(1,ISAVE)=M
            INCREMENTS(2,ISAVE)=N
            INCREMENTS(3,ISAVE)=I_BUF(INDEX)
C--SAVE LINES AND SAMPLES WHICH DEFINE THIS RECTANGLE
            INCREMENTS(4,ISAVE)=I_BUF(INDEX+1)
            INCREMENTS(5,ISAVE)=BUF(MLOC)+I_BUF(INDEX)
C--SAVE THE HORIZONTAL AND VERTICAL DISPLACEMENTS FOR LINE 1
            VMA=BUF(MLOC+1)
            INCREMENTS(6,ISAVE)=VMA+M
            TEMP=BUF(MLOC+2)
            TEMPM=BUF(MLOC)
            INCREMENTS(7,ISAVE)=1.0+((TEMP-TEMPM)/SAM)
            TEMP2=BUF(NLOC)
            TEMP=1.+(BUF(NLOC+2)-TEMP2)/SAM
            INCREMENTS(8,ISAVE)=(TEMP-INCREMENTS(7,ISAVE))/ROW
            INCREMENTS(9,ISAVE)=(TEMP2-TEMPM)/ROW
            TEMP=BUF(NLOC+1)
            INCREMENTS(10,ISAVE)=1.0+(TEMP-VMA)/ROW
            TEMP2=BUF(MLOC+3)
            TEMP=1.+(BUF(NLOC+3)-TEMP2)/ROW
            INCREMENTS(11,ISAVE)=(TEMP-INCREMENTS(10,ISAVE))/SAM
            INCREMENTS(12,ISAVE)=(TEMP2-VMA)/SAM
C--WORD 7 HAS THE DELTA P FOR M (M BEING LINE 1)- STEP SIZE FOR LINE
C--WORD 8 HAS DELTA L -INCREMENT TO STEP SIZE BETWEEN LINES M AND N
C--WORD 9 HAS DELTA M - INCREMENT TO FIRST POINT BETWEEN LINES
C--WORD 10 HAS DELTA L FOR A - STEP SIZE ALONG SAMPLES FOR A
C--WORD 11 HAS DELTA P -INCREMENT TO STEP SIZE BETWEEN SAMPLES A
C--        AND B
C--WORD 12 HAS DELTA A -INCREMENT TO FIRST POINT BETWEEN SAMPLES A
C--        AND B
C
C--INCREMENT TO GET NEXT SAMPLE FOR THIS LINE
            INDEX=INDEX+1
C
C--END SAMPLE PROCESSING LOOP
          END DO
C
C--END "IF (BUF(...).EQ.LINE)" BLOCK
        END IF
C
C--END PARAMETER STORING LOOP
      END DO
C
C--CHECK FOR EXIT CAUSED BY TOO MANY RECTANGLES
      IF (ISAVE.GE.MAXRPAR) THEN
        ISAVE = MAXRPAR
        CALL XVMESSAGE(RECTAN,' ')
      END IF
C--MAKE SURE TEST FOR LAST RECTANGLE ON LINE WORKS
      INCREMENTS(2,ISAVE+1) = 0
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lgeom_unix.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C SUBROUTINE TO GET PHYSICAL MEMORY SIZE
        SUBROUTINE GET_MEM_SIZE(MEMSIZE)
        INTEGER MEMSIZE
        REAL    PMEMORY
	CALL XVPARM('PMEM',PMEMORY,I,K,0) ! GET AMOUNT OF PHYSICAL MEMORY.
        MEMSIZE = PMEMORY*1024*1024       ! CONVERT MEGABYTES TO BYTES
        RETURN
        END

C SUBROUTINE TO EXPLAIN WHY LGEOM CAN'T HANDLE IMAGE.
        SUBROUTINE TOOBIG
	CALL MABEND('** PICTURE IS TOO LARGE FOR AVAILABLE MEMORY')
        RETURN
        END

C SUBROUTINE TO CHECK FOR SMALL PAGE FILE QUOTA UNDER VMS
        SUBROUTINE CHECK_VMS_PAGE_FILE_QUOTA  !the non-VMS VERSION
        RETURN                                !is a dummy.
        END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mvb.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>  /* memcpy, memset */
//#include <stdio.h>

void FTN_NAME(mvb)(char *from_bbuf, char *to_bbuf,int *nb);
void FTN_NAME(zba)(char *bbuf,int *nb);

/************************************************************************
 * MVB (move_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memcpy) 
 ************************************************************************/
void FTN_NAME(mvb)(from_bbuf,to_bbuf,nb)
char *from_bbuf, *to_bbuf;
int *nb;
{
//  printf ("MVB number of bytes = %d\n",*nb);
  memcpy(to_bbuf,from_bbuf,(size_t)*nb);
  return;
}

/************************************************************************
 * ZBA (zero_a_byte_array)
 *     Fortran-Callable Version (no C-version needed -- use memset) 
 ************************************************************************/
void FTN_NAME(zba)(bbuf,nb)
char *bbuf;
int *nb;
{
  memset(bbuf,0,(size_t)*nb);
  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create lgeom.pdf
process help=*
 !
 PARM INP     TYPE=STRING COUNT=(1:2)
 PARM OUT     TYPE=STRING COUNT=1
 PARM SIZE    TYPE=INTEGER COUNT=0:4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NAH     TYPE=INTEGER COUNT=0:1 DEFAULT=--
 PARM NAV     TYPE=INTEGER COUNT=0:1 DEFAULT=--
 PARM TIEPOINT TYPE=REAL COUNT=(16:600)+
               DEFAULT=(1,1,1,1,2,2,2,2,2,1,2,1,2,2,2,2)
 PARM INTERP  TYPE=KEYWORD COUNT=0:1 VALID=NOIN DEFAULT=--
 PARM INTRPZ  TYPE=KEYWORD COUNT=0:1 VALID=NOIZ DEFAULT=--
 PARM PRINT   TYPE=KEYWORD COUNT=0:1 VALID=PRINT DEFAULT=--
 PARM FORMAT  TYPE=KEYWORD COUNT=0:1 VALID=HALF DEFAULT=--
 PARM IDSNAM  TYPE=(STRING) COUNT=1 DEFAULT=LGIDS
 PARM IDSNS   TYPE=INTEGER COUNT=1 DEFAULT=1000
 PARM WSA     TYPE=INTEGER COUNT=1 DEFAULT=150000
 PARM PMEM    TYPE=REAL    COUNT=1 VALID=(0.1:2047) DEFAULT=16.0
 PARM RATIO TYPE=REAL COUNT=1 DEFAULT=2.0
 PARM PARMS TYPE=(STRING) COUNT=0:1 DEFAULT=--
 !
 END-PROC
!
! HELP TEXT FOR LGEOM
.TITLE
LGEOM
.HELP
PURPOSE:
     LGEOM is a VICAR applications program which makes geometric
     changes in pictures.  It can be used for correcting geometric
     distortion, increasing picture size, and reducing picture
     size.  LGEOM should be used instead of MGEOM if any output
     line is rotated more than 85 degrees from its input position.
     It uses an intermediate data set which is created and deleted
     automatically, unless the entire input picture fits in memory.
     In this case, LGEOM operates in a one-pass mode.  Memory is 
     dynamically allocated with STACKA.
.page
EXECUTION:

     lgeom (INPUT,PDS) OUTPUT SIZE QUALIFIERS TIEPOINTS
WHERE
     INPUT          is the input data set.
     PDS            is an optional parameter and tiepoints dataset 
                      created by program MAP.  (Except for in MAP2,
                      LGEOM is almost always run with the PARMS parameter
                      as the means of entering the TIEPOINT, NAH, and NAV
                      parameters.)
     OUTPUT         is the output data set.
     SIZE           is a VICAR size field for the output set
     QUALIFIERS     consist of any of the following parameters
	  IDSNAM	indicates an alternative name for the
			itermediate data set (IDS)
	  IDSNS		indicates an alternative line size for IDS
          NOIN          indicates no interpolation is to be done
	  NOIZ		indicates no interpolation with zero DN values.
     TIEPOINTS      is a collection of parameters as follows:
	  NAH		The number of (tiepoint) rectangles in the horizontal
			direction (Number of areas horizontal)
	  NAV		The number of rectangles vertically.
	  TIEPOINT	A set of points mapping the rectangles in the
			output picture to quadrilaterals in the input
			picture.  For details, see help under the
			TIEPOINT parameter.
           or
	  PARMS		An optional parameter data set (equivalent to
                        PDS, but in the new VAX format).  
                        LGEOM is almost always run with the PARMS parameter
                        as the means of entering the TIEPOINT, NAH, and NAV
                        parameters.

Tiepoint rectangles need not be defined for the entire picture.
If an area of the picture is not within a defined rectangle, it is geomed 
using parameters from the nearest defined rectangle. Thus, in order to have 
a portion of the input picture reproduced exactly in the output, it is 
necessary to define a rectangle with zero displacement around the area.

If the output picture does not fill the area reserved for it by the
SIZE parameter, the pixels in the unfilled area are given the value zero. 
If the size field is defaulted, the output picture will be the same size as 
the input picture.

The default mode for LGEOM is four-point bi-linear interpolation;
i.e. the output pixels will be a function of the DN values of the four
points in the input picture closest to the fractional line and sample
calculated by the specified transformation.

The input image may either be byte or halfword data.  The data format is taken
from the VICAR label of the input file.  The output image has the same data 
format (byte or halfword) as the input image.  

The size field parameters refer to samples, 
even for halfword data.  For example, the following
will enlarge a 100 line by 100 sample (200 bytes) halfword data set A to 216 
lines by 150 samples (300 bytes).
	lgeom A B SIZE=(1,1,216,150) NAH=1 NAV=1 +
	 TIEPOINT=(1,1,1,1, 1,100,1,150, 100,1,216,1, 100,100,216,150)

The keyword NOIN can be used to specify no interpolation. In this
case the DN value of the point closest to the fractional line and sample
is used. This method (sometimes referred to as "nearest neighbor") is
somewhat faster, but is not as accurate as the four point interpolation.

The keyword NOIZ can be used to specify no interpolation with DN values of
zero.  This is used when zero represents missing data and you do not want
to interpolate at the boundary of good data and missing data.

LGEOM may accept parameters from disk as a second input file. 
This is the method used in the MAP2 procedure.  The format
of this disk data set is a single record consisting of NP+1 words, where
NP is the number of words of actual parameters and the first word contains
NP. This is an optional data set.

The common usages of LGEOM are within procedures such as MAP2 and MAPTRANS,
or following such programs as PICREG, RG, TIECONM, or TIEPARM.  TIEPARM, for
example, could be used to convert tiepoints in an IBIS interface file into
a VICAR PARMS file that could be entered as the PARMS parameter in the
LGEOM command line.
.page
EXAMPLE

tieconm OUT=TIEPARMS 'LGEOM NAH=40 NAV=40 +
          MINL=1.,MINS=1.,MAXL=3000.,MAXS=3000.+
          TIEPOIN=(1,1,1,1, 1,3000,1,10, 3000,1,10,1, 3000.29,3000,10,10) 
lgeom LGTEST LGENLARG SIZE=(1,1,3000,3000) PARMS=TIEPARMS

     In this example,  the tiepoints are used to set up a 3000
     x  3000 grid for use by LGEOM.   The tiepoints given to TIECONM
     can  be scattered  over  the  image  in  any  fashion   whereas 
     LGEOM requires a regular grid.
.page
LIMITATIONS:

  A. When a second input dataset is used to enter the rectangles (MAP),
     a maximum of 400 rectangles can be used.  When the TIEPOINT or PARMS
     parameter is used, the number of rectangles is unlimited.  However,
     the TAE maximum count of 32767 limits the number of rectangles to 8192.

  B. The maximum number of samples per line in the output file is 32767.

  C. Since STACKA is used to allocate memory, the maximum picture size
     is unlimited.  When very large pictures are used, the page file quota
     must be large enough to accomodate a buffer used to map the blocks
     in the Intermediate Data Set.  The number of bytes in this buffer is
     6 times the estimated number of blocks in the Intermediate Data Set.
     (The RATIO parameter can be used to change the estimated number of
     blocks.)

  D. The output tiepoint coordinates must be whole numbers.  Users rarely
     need to know of this restriction since the TIEPOINT parameter is
     usually generated by another VICAR program.  Users only need to be
     concerned about this if they are actually entering specific values
     for the TIEPOINT parameter for the LGEOM command line.

.page
RECORD SIZE OF INTERMEDIATE DATA SET FOR LGEOM (IDSNS):

The optimum size of the intermediate data set (IDS) for LGEOM is a
function of the number of rectangles, input and output sizes, and the
available buffer space.

In general, the IDS record length should decrease as the number of pixels
in the output file increases.  The number of lines needed for IDS is 
calculated inside the program.
.PAGE

 TIMING: 
  The following CPU times for LGEOM was obtained on a 
VAX 8650 (MIPL2) in May 1993
			                                                CPU Time
gen LGA1 NL=1000 NS=1000 IVAL=0 
!
! Now do 1.2 times enlargement of a BYTE image IN 1 PASS
lgeom LGA1 LGA2 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    28.53s

For more information, see the file timelgeom.pdf in lgeom.com.
.page
WRITTEN BY: Howard J Frieden		22 December 1970

COGNIZANT PROGRAMMER: Ray Bambery

PORTED TO UNIX: Steve Pohorsky

 REVISION HISTORY

  5-26-1993   SP   Made portable for UNIX.  Gave control over progress messages 
                 to PRINT parameter.  Added PMEM parameter for UNIX.
  26 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5
.LEVEL1
.VARI INP
Input and optional
tiepoint data set names
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR Size Field
.VARI SL
Starting line for output
.VARI SS
Starting sample for output
.VARI NL
Number of lines for output
.VARI NS
Number of samples for output
.VARI IDSNAM
Name of Intermediate Data Set
.VARI IDSNS
Bytes per line in Intermediate
Data Set
.VARI WSA
Working set adjustment 
(bytes).  VMS only
.VARI PMEM
Physical memory available
(megabytes). non-VMS systems
.VARI RATIO
Ratio for estimating number
of blocks in IDS.
.VARI INTERP
no interpolation.
Valid: NOIN
.VARI INTRPZ
no interpolation with 
zero DN values
Valid: NOIZ
.VARI PRINT
Print TIEPOINTS and progress 
messages
Valid: PRINT
.VARI FORMAT
FORMAT is ignored.
.VARI NAH
Number of areas horizontally.
See explanation.
.VARI NAV
Number of areas vertically.
See explanation.
.VARI TIEPOINT
Specifies mapping of control
points between output and
input pictures.
.VARI PARMS
Parameter data set name
.LEVEL2
.VARI INP
Input file name.  This parameter is input as
     INP=in
where
in       is the input file name.

In addition, a second file may be specified which contains the
line and sample (tiepoint) parameters.  If this second file is specified,
the parameter would then be 
     INP=(in,pds)
where
PDS is the name of a program-MAP-type parameter data set.
Note : The second input file (pds) is used only in conjunction with  
program MAP.  

LGEOM is almost always run with the PARMS parameter
as the means of entering the TIEPOINT, NAH, and NAV parameters.
.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 samples.
.VARI SL
Usually defaulted (SL=1).
SL tells the program where in the output coordinate system to start the
output image.  The output coordinate system is the system used for
all (newline,newsample) coordinates in the TIEPOINT parameter.
.VARI SS
Usually defaulted (SS=1).
SS tells the program where in the output coordinate system to start the
output image.  The output coordinate system is the system used for
all (newline,newsample) coordinates in the TIEPOINT parameter.
.VARI NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
.VARI NS
NS can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of samples per line for output.
.VARI IDSNAM
IDSNAM is an optional parameter which can be used to specify the
name of the intermediate data set.  This is often used to change the
location of the data set.  This data set is usually slightly larger than
the output file, but may be up to 2 times as large in some cases.

For example, if the user's current directory is USERDISK:[USER],
and he/she does not have enough disk space for IDS, then he/she could
input IDSNAM=SCX:[USER]LGIDS or some such name to ensure that
the IDS ends up in a valid directory with enough disk space to hold it.

On VMS systems the user may define a DCL logical name of LGIDS to
avoid having to specify the parameter repeatedly.  For example, if
before running the LGEOM job the user inputs

DCL DEFINE LGIDS SCX:[USER]LGIDS

then all subsequent lgeom jobs would point to that Intermediate data
set until the logical name was deassigned (DCL DEASS LGIDS).
.VARI IDSNS
 IDSNS is an optional parameter which may be used to change the number
 of bytes per line in the Intermediate Data Set.  The main purpose
 of this parameter is to allow the user to "tune" LGEOM to obtain the
 optimum speed.  (The value should gradually decrease from its default 
 value of 1000 when NL or NS get large and the working set is small.)  

 The program may change the value of IDSNS if it is necessary for avoiding
 aborting execution due to insufficient memory.
.VARI PMEM
Physical memory (megabytes) available for LGEOM dynamically allocated buffers.
This applies to non-VMS systems only.  The default is 16.0.  (The maximum value
is 2047 (megabytes), to insure that the number of bytes can be stored as a
32-bit integer.)

This parameter can usually be defaulted.  If the amount of memory available
is less than the default, then PMEM should be set accordingly to prevent the
program from thrashing.  If the amount of memory available
is more than the default, then PMEM may be set accordingly to allow larger
images to be handled in one pass.
.VARI RATIO
RATIO is used to estimate the number of blocks in the Intermediate
Data Set and allocate a memory buffer to map the blocks. The default
value of 2.0 should cover all but very unusual cases.  RATIO is the factor
by which the Intermediate Data Set exceeds the Output Data Set.
.VARI WSA
WSA is an optional parameter to reduce the working set size that LGEOM
automatically determines.  The default value is 150,000 bytes to allow
for I/O buffers etc.
.VARI INTERP
NOIN means no interpolation.  The default method for computing the
DN values of the output picture is to use a bi-linear interpolation
on the four nearest neighbors in the input picture.  With NOIN, the
value of the nearest point is simply used.
For example, say a point in the output picture was determined
to have come from point (R,P) in the input picture.  Since R and P
are real values, we must somehow calculate a DN value for that
point.  Take IR and IP as the truncated values.  We then have
          VAL1                                 VAL2
           *                                    *
         (IR,IP)                              (IR,IP+1)
                     POINT
                       *
                     (R,P)
          VAL3                                 VAL4
           *                                    *
         (IR+1,IP)                           (IR+1,IP+1)
Here, POINT is the result of a bilinear interpolation using
VAL1, VAL2, VAL3, and VAL4.
If NOIN is specified, then POINT would be VAL1, the nearest
neighbor.
.VARI INTRPZ
NOIZ means no interpolation with zero DN values.  Whenever an interpolation
is to be done and one of the points is zero, nearest neighbor is used.
This option is used when zero represents missing data and you do not want
to interpolate between good data and missing data.
.VARIABLE PRINT
 Causes a listing of the TIEPOINT parameters to be generated and causes
 periodic progress messages to be printed 
 (Default is that no such listing is printed.)
.VARI FORMAT
The format is obtained from the input image label. 
.VARI NAH
NAH=N where N is an integer value specifing the number of areas horizontally;
which is also the number of columns of tiepoints less 1. NAH must
be specified unless an optional dataset containing the parameters NAH, NAV
and TIEPOINTs is specified. (2nd optional input dataset if the dataset is 
created by program MAP, optional parameter dataset (see PARMS parameter) if the
dataset is created by any other program besides program MAP).
NAH must also be >= 1.
.VARI NAV
NAV=N where N is an integer value specifing the number of areas vertically;
which is also the number of rows of tiepoints less 1. NAV must be specified
unless an optional dataset containing the parameters NAH, NAV, and TIEPOINTs
is specified (2nd optional input dataset if the dataset is created by program
MAP, optional parameter dataset (see PARMS parameter) if the dataset is created
by any other program besides program MAP).  NAV must also be >= 1. 
.VARI TIEPOINT
TIEPOINT=(newline(1),newsamp(1),oldline(1),oldsamp(1),newline(2),...). The
values of TIEPOINT specify the mapping of control points between output
and input pictures.  The numbers which follow the keyword are in multiples
of four, one multiple of four for each tiepoint. The numbers may be either
integer or real type. However since the grid in the output must be 
rectangular, the (newline,newsamp) points must be whole numbers.  The
total number of tiepoint numbers must be
			4*(NAH+1)*(NAV+1)
Within each group of four numbers describing a tiepoint, the first number
specifies the line coordinate of that tiepoint in the output (transformed)
picture, the second number specifies the sample coordinate of that tiepoint
in the output picture, the third number specifies the line coordinate of the
input picture and the fourth specifies the sample coordinate of the input
picture. The order in which the tiepoints are specified is left to right
within a horizontal row of tiepoints. The horizontal rows of tiepoints are
specified in top-to-bottom sequence. Tiepoint specification is further
clarified in the OPERATIONS and TIMING sections.  The TIEPOINT parameter
must be given unless an optional dataset containing the NAH, NAV, and
TIEPOINT parameters is specified.(2nd optional input dataset if the dataset
is generated by program MAP, optional parameter data set (see PARMS parameter) 
if the dataset is generated by any other program besides program MAP ).
.VARI PARMS
PARMS can be used to specify the name of an optional parameter data 
set.  Any combination of the allowable parameters may be given.  If
any of the parameters are given interactively, the interactive value
takes precedence.

LGEOM is almost always run with the PARMS parameter
as the means of entering the TIEPOINT, NAH, and NAV parameters.
This parameter names a VICAR PARMS file, (xvpopen type for you programmers.)

Note: If the data set is generated by program MAP, it has to be entered as
a second input instead of PARMS.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlgeom.pdf
procedure
! Jul 25, 2016- RJB
! TEST SCRIPT FOR LGEOM
! tests BYTE and HALF images
!
! Vicar Programs:
!       gen list
!
! External Programs;
!   tlgeom
!           
! parameters:
!   <none>
!
! Requires no external test data: 
!
refgbl $echo
body
let _onfail="stop"
let $echo="yes"
!   TEST PDF FOR LGEOM
!
! First make sure no left over files
! 
! Now generate BYTE input data set
! 
gen LGTEST 10 10 SINC=40 LINC=40
! 
! Verify existence of input file
list LGTEST
! 
! Perform simple enlargement to 2X size with NOIN and NOIZ
lgeom LGTEST LGENLARG NAH=1 NAV=1 SIZE=(1,1,20,20) +
  TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10) 'NOIN
! 
! Print it out
list LGENLARG
lgeom LGTEST LGENLARG NAH=1 NAV=1 SIZE=(1,1,20,20) +
  TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10) 'NOIZ
! 
! Print it out
list LGENLARG
! 
! Perform 90 degree rotation clockwise with two times enlargement
lgeom LGTEST LGROTAT SIZE=(1,1,20,20) NAH=1 NAV=1 +
 TIEPOIN=(1,1,10,1, 1,20,1,1, 20,1,10,10, 20,20,1,10)
!
! Print it out
list LGROTAT
! 
! Now do simple tests for halfword data
! 
! First generate input data set. (Try some negative DNs.)
gen LGTESTH 10 10 IVAL=-10000 LINC=3000 SINC=3000 'HALF
! 
! Verify its existance
list LGTESTH
! 
! Now do 2 times enlargement getting format from label
lgeom LGTESTH LGENLARGH SIZE=(1,1,20,20) NAH=1 NAV=1 +
 TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10) 
! 
! Print it out
list LGENLARGH
! 
! Now rotate 90 degrees
lgeom LGTESTH LGROTATH SIZE=(1,1,10,10) NAH=2 NAV=2 +
  TIEPOIN=(1,1,10,1,   1,6,5,1,   1,10,1,1, +
           6,1,10,6,   6,6,5,6,   6,10,1,6, +
	   10,1,10,10, 10,6,5,10, 10,10,1,10) 
! 
! Print it out
list LGROTATH
! 
! try again with SS not 1
lgeom LGTESTH LGROTATH SIZE=(1,3,10,8) NAH=2 NAV=2 +
  TIEPOIN=(1,1,10,1,   1,6,5,1,   1,10,1,1, +
           6,1,10,6,   6,6,5,6,   6,10,1,6, +
	   10,1,10,10, 10,6,5,10, 10,10,1,10) 
! 
! Print it out
list LGROTATH
! 
! Create a parameter data set 
tlgeom PDS
!
! Do halfword enlargement with parameter data set
lgeom (LGTESTH,PDS) LGENLARGH (1,1,20,20) 'PRINT
!
! Print it out
list LGENLARGH
! 
! now for a more complicated transformation, test 'PRINT
gen lgtest 512 512 sinc=0 linc=20
lgeom lgtest lgtout.img size=(1,1,512,512) nah=2 nav=2  'PRINT +
 tiepoin=(1,1,20,20, 1,256,100,200, 1,512,50,450, +
          256,1,180,120, 256,256,256,256, 256,512,150,350, +
	  512,1,380,50,  512,256,300,200, 512,512,512,512)
write "Display lgtout.img to demonstrate 4 twisted quadrants "
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstlgeom.log
tstlgeom
gen LGTEST 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
list LGTEST
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:41 2016
     Samp     1       3       5       7       9
   Line
      1       0  40  80 120 160 200 240  24  64 104
      2      40  80 120 160 200 240  24  64 104 144
      3      80 120 160 200 240  24  64 104 144 184
      4     120 160 200 240  24  64 104 144 184 224
      5     160 200 240  24  64 104 144 184 224   8
      6     200 240  24  64 104 144 184 224   8  48
      7     240  24  64 104 144 184 224   8  48  88
      8      24  64 104 144 184 224   8  48  88 128
      9      64 104 144 184 224   8  48  88 128 168
     10     104 144 184 224   8  48  88 128 168 208
lgeom LGTEST LGENLARG NAH=1 NAV=1 SIZE=(1,1,20,20)  +
  TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10) 'NOIN
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGENLARG
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:41 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0  40  40  80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104
      2       0   0  40  40  80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104
      3      40  40  80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144
      4      40  40  80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144
      5      80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184
      6      80  80 120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184
      7     120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224
      8     120 120 160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224
      9     160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8
     10     160 160 200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8
     11     200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48
     12     200 200 240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48
     13     240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88
     14     240 240  24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88
     15      24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128
     16      24  24  64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128
     17      64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128 168 168
     18      64  64 104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128 168 168
     19     104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128 168 168 208 208
     20     104 104 144 144 184 184 224 224   8   8  48  48  88  88 128 128 168 168 208 208
lgeom LGTEST LGENLARG NAH=1 NAV=1 SIZE=(1,1,20,20)  +
  TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10) 'NOIZ
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGENLARG
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:41 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0  40  57  76  95 114 133 152 171 189 208 227 206 104  28  47  66  85 104
      2       0   0  58  76  95 114 133 152 171 189 208 202 163 123  78  47  66  85 104 123
      3      40  59  76  95 114 133 152 171 189 208 227 195  99  40  52  66  85 104 123 142
      4      57  76  95 114 133 152 171 189 208 199 167 126  75  47  66  85 104 123 142 161
      5      76  95 114 133 152 171 189 208 227 186  96  49  56  66  85 104 123 142 161 180
      6      95 114 133 152 171 189 208 198 172 128  71  47  66  85 104 123 142 161 180 199
      7     114 133 152 171 189 208 227 178  95  58  58  66  85 104 123 142 161 180 199 218
      8     133 152 171 189 208 198 178 129  65  47  66  85 104 123 142 161 180 194 175 156
      9     152 171 189 208 227 172  95  65  59  66  85 104 123 142 161 180 199 207 130  53
     10     171 189 208 199 186 128  58  47  66  85 104 123 142 161 180 192 179 159  89  19
     11     189 208 227 167  96  71  58  66  85 104 123 142 161 180 199 198 127  63  50  37
     12     208 202 195 126  49  47  66  85 104 123 142 161 180 190 184 161  85  19  37  56
     13     227 163  99  75  56  66  85 104 123 142 161 180 199 190 126  72  53  37  56  75
     14     206 123  40  47  66  85 104 123 142 161 180 190 190 162  79  19  37  56  75  94
     15     104  78  52  66  85 104 123 142 161 180 199 184 126  79  53  37  56  75  94 113
     16      28  47  66  85 104 123 142 161 180 192 198 161  72  19  37  56  75  94 113 132
     17      47  66  85 104 123 142 161 180 199 179 127  85  53  37  56  75  94 113 132 151
     18      66  85 104 123 142 161 180 194 207 159  63  19  37  56  75  94 113 132 151 170
     19      85 104 123 142 161 180 199 175 130  89  50  37  56  75  94 113 132 151 170 189
     20     104 123 142 161 180 199 218 156  53  19  37  56  75  94 113 132 151 170 189 208
lgeom LGTEST LGROTAT SIZE=(1,1,20,20) NAH=1 NAV=1  +
 TIEPOIN=(1,1,10,1, 1,20,1,1, 20,1,10,10, 20,20,1,10)
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGROTAT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:41 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     104  85  66  47  28 104 206 227 208 189 171 152 133 114  95  76  57  38  19   0
      2     123 104  85  66  47  78 123 163 202 208 189 171 152 133 114  95  76  57  38  19
      3     142 123 104  85  66  52  40  99 195 227 208 189 171 152 133 114  95  76  57  38
      4     161 142 123 104  85  66  47  75 126 167 199 208 189 171 152 133 114  95  76  57
      5     180 161 142 123 104  85  66  56  49  96 186 227 208 189 171 152 133 114  95  76
      6     199 180 161 142 123 104  85  66  47  71 128 172 198 208 189 171 152 133 114  95
      7     218 199 180 161 142 123 104  85  66  58  58  95 178 227 208 189 171 152 133 114
      8     156 175 194 180 161 142 123 104  85  66  47  65 129 178 198 208 189 171 152 133
      9      53 130 207 199 180 161 142 123 104  85  66  59  65  95 172 227 208 189 171 152
     10      19  89 159 179 192 180 161 142 123 104  85  66  47  58 128 186 199 208 189 171
     11      37  50  63 127 198 199 180 161 142 123 104  85  66  58  71  96 167 227 208 189
     12      56  37  19  85 161 184 190 180 161 142 123 104  85  66  47  49 126 195 202 208
     13      75  56  37  53  72 126 190 199 180 161 142 123 104  85  66  56  75  99 163 227
     14      94  75  56  37  19  79 162 190 190 180 161 142 123 104  85  66  47  40 123 206
     15     113  94  75  56  37  53  79 126 184 199 180 161 142 123 104  85  66  52  78 104
     16     132 113  94  75  56  37  19  72 161 198 192 180 161 142 123 104  85  66  47  28
     17     151 132 113  94  75  56  37  53  85 127 179 199 180 161 142 123 104  85  66  47
     18     170 151 132 113  94  75  56  37  19  63 159 207 194 180 161 142 123 104  85  66
     19     189 170 151 132 113  94  75  56  37  50  89 130 175 199 180 161 142 123 104  85
     20     208 189 170 151 132 113  94  75  56  37  19  53 156 218 199 180 161 142 123 104
gen LGTESTH 10 10 IVAL=-10000 LINC=3000 SINC=3000 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list LGTESTH
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -10000 -7000 -4000 -1000  2000  5000  8000 11000 14000 17000
      2     -7000 -4000 -1000  2000  5000  8000 11000 14000 17000 20000
      3     -4000 -1000  2000  5000  8000 11000 14000 17000 20000 23000
      4     -1000  2000  5000  8000 11000 14000 17000 20000 23000 26000
      5      2000  5000  8000 11000 14000 17000 20000 23000 26000 29000
      6      5000  8000 11000 14000 17000 20000 23000 26000 29000 32000
      7      8000 11000 14000 17000 20000 23000 26000 29000 32000-30536
      8     11000 14000 17000 20000 23000 26000 29000 32000-30536-27536
      9     14000 17000 20000 23000 26000 29000 32000-30536-27536-24536
     10     17000 20000 23000 26000 29000 32000-30536-27536-24536-21536
lgeom LGTESTH LGENLARGH SIZE=(1,1,20,20) NAH=1 NAV=1  +
 TIEPOIN=(1,1,1,1, 1,20,1,10, 20,1,10,1, 20,20,10,10)
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGENLARGH
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1    -10000 -8579 -7158 -5737 -4316 -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895
      2     -8579 -7158 -5737 -4316 -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316
      3     -7158 -5737 -4316 -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316 12737
      4     -5737 -4316 -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316 12737 14158
      5     -4316 -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316 12737 14158 15579
      6     -2895 -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316 12737 14158 15579 17000
      7     -1474   -53  1368  2789  4211  5632  7053  8474  9895 11316 12737 14158 15579 17000 18421
      8       -53  1368  2789  4211  5632  7053  8474  9895 11316 12737 14158 15579 17000 18421 19842
      9      1368  2789  4211  5632  7053  8474  9895 11316 12737 14158 15579 17000 18421 19842 21263
     10      2789  4211  5632  7053  8474  9895 11316 12737 14158 15579 17000 18421 19842 21263 22684
     11      4211  5632  7053  8474  9895 11316 12737 14158 15579 17000 18421 19842 21263 22684 24105
     12      5632  7053  8474  9895 11316 12737 14158 15579 17000 18421 19842 21263 22684 24105 25526
     13      7053  8474  9895 11316 12737 14158 15579 17000 18421 19842 21263 22684 24105 25526 26947
     14      8474  9895 11316 12737 14158 15579 17000 18421 19842 21263 22684 24105 25526 26947 28368
     15      9895 11316 12737 14158 15579 17000 18421 19842 21263 22684 24105 25526 26947 28368 29789
     16     11316 12737 14158 15579 17000 18421 19842 21263 22684 24105 25526 26947 28368 28700 26854
     17     12737 14158 15579 17000 18421 19842 21263 22684 24105 25526 26947 28368 29789 25220  8668
     18     14158 15579 17000 18421 19842 21263 22684 24105 25526 26947 28368 29063 28851 19379 -8609
     19     15579 17000 18421 19842 21263 22684 24105 25526 26947 28368 29789 23949  9031 -5342-18625
     20     17000 18421 19842 21263 22684 24105 25526 26947 28368 29789 31211 18834-10788-30062-28641

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp      16    17    18    19    20
   Line
      1     11316 12737 14158 15579 17000
      2     12737 14158 15579 17000 18421
      3     14158 15579 17000 18421 19842
      4     15579 17000 18421 19842 21263
      5     17000 18421 19842 21263 22684
      6     18421 19842 21263 22684 24105
      7     19842 21263 22684 24105 25526
      8     21263 22684 24105 25526 26947
      9     22684 24105 25526 26947 28368
     10     24105 25526 26947 28368 29789
     11     25526 26947 28368 29789 31211
     12     26947 28368 29063 23949 18835
     13     28368 29789 28850  9031-10788
     14     28700 25220 19379 -5342-30062
     15     26854  8668 -8609-18625-28641
     16     19561 -6794-30062-28641-27220
     17     -6794-18444-28641-27220-25799
     18    -30062-28641-27220-25799-24378
     19    -28641-27220-25799-24378-22957
     20    -27220-25799-24378-22957-21536
lgeom LGTESTH LGROTATH SIZE=(1,1,10,10) NAH=2 NAV=2  +
  TIEPOIN=(1,1,10,1,   1,6,5,1,   1,10,1,1,  +
           6,1,10,6,   6,6,5,6,   6,10,1,6,  +
	   10,1,10,10, 10,6,5,10, 10,10,1,10)
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGROTATH
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     17000 14000 11000  8000  5000  2000 -1000 -4000 -7000-10000
      2     20000 17000 14000 11000  8000  5000  2000 -1000 -4000 -7000
      3     23000 20000 17000 14000 11000  8000  5000  2000 -1000 -4000
      4     26000 23000 20000 17000 14000 11000  8000  5000  2000 -1000
      5     29000 26000 23000 20000 17000 14000 11000  8000  5000  2000
      6     32000 29000 26000 23000 20000 17000 14000 11000  8000  5000
      7    -30536 32000 29000 26000 23000 20000 17000 14000 11000  8000
      8    -27536-30536 32000 29000 26000 23000 20000 17000 14000 11000
      9    -24536-27536-30536 32000 29000 26000 23000 20000 17000 14000
     10    -21536-24536-27536-30536 32000 29000 26000 23000 20000 17000
lgeom LGTESTH LGROTATH SIZE=(1,3,10,8) NAH=2 NAV=2  +
  TIEPOIN=(1,1,10,1,   1,6,5,1,   1,10,1,1,  +
           6,1,10,6,   6,6,5,6,   6,10,1,6,  +
	   10,1,10,10, 10,6,5,10, 10,10,1,10)
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
list LGROTATH
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp       1     2     3     4     5     6     7     8
   Line
      1     11000  8000  5000  2000 -1000 -4000 -7000-10000
      2     14000 11000  8000  5000  2000 -1000 -4000 -7000
      3     17000 14000 11000  8000  5000  2000 -1000 -4000
      4     20000 17000 14000 11000  8000  5000  2000 -1000
      5     23000 20000 17000 14000 11000  8000  5000  2000
      6     26000 23000 20000 17000 14000 11000  8000  5000
      7     29000 26000 23000 20000 17000 14000 11000  8000
      8     32000 29000 26000 23000 20000 17000 14000 11000
      9    -30536 32000 29000 26000 23000 20000 17000 14000
     10    -27536-30536 32000 29000 26000 23000 20000 17000
tlgeom PDS
Beginning VICAR task tlgeom
lgeom (LGTESTH,PDS) LGENLARGH (1,1,20,20) 'PRINT
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
PROCESSING INPUT LINES     1 TO    10
list LGENLARGH
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1    -10000-10000 -7000 -7000 -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000
      2    -10000-10000 -7000 -7000 -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000
      3     -7000 -7000 -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000
      4     -7000 -7000 -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000
      5     -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000
      6     -4000 -4000 -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000
      7     -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000
      8     -1000 -1000  2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000
      9      2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000
     10      2000  2000  5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000
     11      5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000
     12      5000  5000  8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000
     13      8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000
     14      8000  8000 11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000
     15     11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000
     16     11000 11000 14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000
     17     14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000 32000-30536
     18     14000 14000 17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000 32000-30536
     19     17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000 32000-30536-30536-27536
     20     17000 17000 20000 20000 23000 23000 26000 26000 29000 29000 32000 32000-30536-30536-27536

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
 Task:LGEOM     User:rjb       Date_Time:Wed Jul 27 00:42:42 2016
     Samp      16    17    18    19    20
   Line
      1     11000 14000 14000 17000 17000
      2     11000 14000 14000 17000 17000
      3     14000 17000 17000 20000 20000
      4     14000 17000 17000 20000 20000
      5     17000 20000 20000 23000 23000
      6     17000 20000 20000 23000 23000
      7     20000 23000 23000 26000 26000
      8     20000 23000 23000 26000 26000
      9     23000 26000 26000 29000 29000
     10     23000 26000 26000 29000 29000
     11     26000 29000 29000 32000 32000
     12     26000 29000 29000 32000 32000
     13     29000 32000 32000-30536-30536
     14     29000 32000 32000-30536-30536
     15     32000-30536-30536-27536-27536
     16     32000-30536-30536-27536-27536
     17    -30536-27536-27536-24536-24536
     18    -30536-27536-27536-24536-24536
     19    -27536-24536-24536-21536-21536
     20    -27536-24536-24536-21536-21536
gen lgtest 512 512 sinc=0 linc=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
lgeom lgtest lgtout.img size=(1,1,512,512) nah=2 nav=2  'PRINT  +
 tiepoin=(1,1,20,20, 1,256,100,200, 1,512,50,450,  +
          256,1,180,120, 256,256,256,256, 256,512,150,350,  +
	  512,1,380,50,  512,256,300,200, 512,512,512,512)
Beginning VICAR task lgeom
LGEOM - 26-JUL-2016 (64-bit) - rjb
   TIEPOINTS:
     1.000     1.000    20.000    20.000
     1.000   256.000   100.000   200.000
     1.000   512.000    50.000   450.000
   256.000     1.000   180.000   120.000
   256.000   256.000   256.000   256.000
   256.000   512.000   150.000   350.000
   512.000     1.000   380.000    50.000
   512.000   256.000   300.000   200.000
   512.000   512.000   512.000   512.000
PROCESSING INPUT LINES     1 TO   512
write "Display lgtout.img to demonstrate 4 twisted quadrants "
Display lgtout.img to demonstrate 4 twisted quadrants 
let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tlgeom.f
      PROGRAM TLGEOM
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
      IMPLICIT NONE
      INTEGER UNIT,STAT
      INTEGER IBUF(250)
      REAL RBUF(250)
      EQUIVALENCE (IBUF,RBUF)
      IBUF(1) = 21
      CALL MVCL('SAMPLE  ',IBUF(2), 8)
      IBUF(4) = 1
      IBUF(5) = 20
      CALL MVCL('LINE    ',IBUF(6), 8)
      RBUF(8) = 1.0
      RBUF(9) = 0
      RBUF(10) = 0
      RBUF(11) = -10
      RBUF(12) = 0
      CALL MVCL('LINE    ',IBUF(13), 8)
      RBUF(15) = 20
      RBUF(16) = 0
      RBUF(17) = -10
      RBUF(18) = -10
      RBUF(19) = -10
      CALL MVCL('NOIN    ',IBUF(20), 8)
      CALL XVUNIT(UNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(UNIT,STAT,'OP','WRITE','U_NL',1,'U_NS',4*21,' ')
      CALL XVWRIT(UNIT,IBUF,STAT,' ')
      CALL XVCLOSE(UNIT,STAT,' ')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tlgeom.pdf
PROCESS
PARM OUT TYPE=STRING 
END-PROC
$!-----------------------------------------------------------------------------
$ create tlgeom.imake
/* Imake file for test program for VICAR program lgeom */

#define PROGRAM tlgeom

#define MODULE_LIST tlgeom.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create timelgeom.pdf
procedure        !  PROCEDURE FOR MEASURING lgeom PERFORMANCE ON VAX
refgbl $echo     !  R2LIB:lgeom WAS THE UNPORTED VERSION; MODIFY ACCORDINGLY
body             !  IF RERUNNING.
let _onfail="continue"
let $echo="yes"
gen LGA1 NL=1000 NS=1000 IVAL=0 
!
! Now do 1.2 times enlargement of a BYTE image IN 1 PASS
lgeom LGA1 LGA2 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    28.53s
!
r2lib:lgeom LGA1 LGA3 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    26.70s
DIFPIC (LGA2, LGA3)
!
! Now do 1.2 times enlargement of a BYTE image. FORCE 2 PASSES USING WSA.
lgeom LGA1 LGA2 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  WSA=1000000 +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    32.26s
!
r2lib:lgeom LGA1 LGA3 SIZE=(1,1,1200,1200) NAH=1 NAV=1 WSA=1000000 +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    31.17s
DIFPIC (LGA2, LGA3)
!
gen LGA1 NL=800 NS=800 IVAL=0 'HALF
!
! Now do 1.5 times enlargement of a HALFWORD image IN 1 PASS.
lgeom LGA1 LGA2 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    23.74s
!
r2lib:lgeom LGA1 LGA3 SIZE=(1,1,1200,1200) NAH=1 NAV=1  +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    22.81s
DIFPIC (LGA2, LGA3)
!
! Now do 1.2 times enlargement of a HALFWORD image.   FORCE 2 PASSES USING WSA.
lgeom LGA1 LGA2 SIZE=(1,1,1200,1200) NAH=1 NAV=1 'PRINT  WSA=1000000 +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    27.41s
!
r2lib:lgeom LGA1 LGA3 SIZE=(1,1,1200,1200) NAH=1 NAV=1  WSA=1000000 +
 TIEPOIN=(1,1,1,1, 1,1200,1,1000, 1200,1,1000,1, 1200,1200,1000,1000) 
!
! 				PERFORMANCE RECORD FOR ABOVE RUN
!
!  				5-93  SP  CPU TIME ON MIPL2 (VAX8650)    27.26s
DIFPIC (LGA2, LGA3)
!
!
end-proc
$ Return
$!#############################################################################
$Imake_File:
$ create lgeom.imake
#define PROGRAM lgeom

#if VMS_OS
#define MODULE_LIST lgeom.f lgeom_vms.f mvb.mar
#define CLEAN_OTHER_LIST lgeom_unix.f mvb.c
#else
#define MODULE_LIST lgeom.f lgeom_unix.f mvb.c
#define CLEAN_OTHER_LIST lgeom_vms.f mvb.mar
#endif

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#if VMS_OS
#define USES_MACRO
#else
#define USES_C
#endif

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
