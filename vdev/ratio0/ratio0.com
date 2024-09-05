$!****************************************************************************
$!
$! Build proc for MIPL module ratio0
$! VPACK Version 1.9, Tuesday, August 16, 2016, 20:00:57
$!
$! Execute by entering:		$ @ratio0
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
$ write sys$output "*** module ratio0 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ratio0.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("ratio0.imake") .nes. ""
$   then
$      vimake ratio0
$      purge ratio0.bld
$   else
$      if F$SEARCH("ratio0.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ratio0
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ratio0.bld "STD"
$   else
$      @ratio0.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ratio0.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ratio0.com -mixed -
	-s ratio0.f -
	-i ratio0.imake -
	-p ratio0.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ratio0.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

C     VAX CONVERSION BY ASM, JAN 1984
C     REVISION TO HANDLE F2 BEING CONVERTED TO VICAR2
C     ADD 'NOCENTER OPTION    -- REA 4JUN86
C     TWEAKED TO PREVENT DIVIDE BY 0 FOR FLAT IMAGES.  UNCOMMENTED HISTOGRAM
C     PRINTING.               -- SXP 17SEP90
C     MSTP S/W Conversion (VICAR Porting)      CRI    10-JUL-95
C
C**********************************************************************
C
      SUBROUTINE MAIN44
        implicit none
      INCLUDE 'pgminc'        ! TAE CONSTANTS & PARAMETERS
      EXTERNAL RAT,DIFF,LNRAT,LNDIFF

      INTEGER*4 IPAR(4),IPOP(500),JPOP(500),PARB(xprdim)
      integer*4 I,II,JJ,M,N,ICUT,IHI,INC,IND,INP1,INP2,ISL,ISS
      integer*4 ISTATUS,ISUM,IWT,LINC,LOW,MAXMUM,MEAN,NL,NS
      integer*4 NLIN,NSAT,NSIN,NUM,INL,INS
      integer*8 II8
      REAL*4    RPAR(2)
      real*4    ATM1,ATM2,BOTTOM,DELTA,SAT,SCALE,THRESH,SUBSAMP
      real*4    TOP
      REAL*8    VAL0,VAL255,OFF,GAIN,VALUE

      LOGICAL*4 XVPTST, QLOG,QDIFF,QDISP,QFILT,QCENTER
      character*50 unconfmt
      character*50 func, FUNCSTR

      EQUIVALENCE (FUNC,FUNCSTR)

      COMMON ISL,ISS,INL,INS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP

      VALUE(I) = FLOAT(I-1)/SCALE+BOTTOM       ! intrinsic function
C
      CALL IFMESSAGE('** RATIO version 19-JUL-2016 - (64bit) - RJB')
C
       
      CALL XVEACTION('SA',' ')
C
C	OPEN INPUT DATASETS
C
      CALL XVUNIT(INP1,'INP',1,ISTATUS,' ')
      write(func,'(i4)') istatus  
      CALL CHKSTAT(ISTATUS,'??E - XVUNIT failed fpr first input dataset',-1)
      CALL XVOPEN(INP1,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVUNIT(INP2,'INP',2,ISTATUS,' ')
      CALL CHKSTAT(ISTATUS,'??E - XVUNIT failed for second input dataset',-1)
      CALL XVOPEN(INP2,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVSIZE(ISL,ISS,INL,INS,NLIN,NSIN)
        nl=inl
        ns=ins
C
C     GET AND PROCESS THE PARAMETERS
C
      QLOG = XVPTST('LOG')
      QDISP = .NOT.XVPTST('NODISPLA') 
      QFILT = .NOT.XVPTST('NOFILTER')
      QDIFF = XVPTST('DIFFEREN')
      QCENTER = XVPTST('CENTER')
C							INCLUDE
      CALL XVPARM('INCLUDE',RPAR,NUM,IND,2)
      IF (QDIFF.AND.IND.NE.0) THEN
	  BOTTOM = -249.5
	  TOP = 250.5
      ELSE
	  BOTTOM = MIN(RPAR(1),RPAR(2))
	  TOP = MAX(RPAR(1),RPAR(2))
      END IF
C							ATMOSPHERIC CORRECTIONS
      CALL XVPARM('ATM1',ATM1,NUM,IND,1)
      CALL XVPARM('ATM2',ATM2,NUM,IND,1)
C							PERCENT SATURATION
      CALL XVPARM('PERCENT',SAT,NUM,IND,1)
      SAT = 0.01*SAT
C							THRESHOLD
      CALL XVPARM('THRESHOL',THRESH,NUM,IND,1)
      THRESH = 0.01*THRESH
C	
      subsamp = 100.0					!	SUBSAMPLING PARAMS
      CALL XVPARM('SAMPLE',SUBSAMP,NUM,IND,1)
      INC = int(1.0/(0.01*SUBSAMP))
      CALL XVPARM('LINC',LINC,NUM,IND,1)
      IF (IND.EQ.0) INC=LINC
      IF (INC.LE.0) INC=1
C							AREA
      CALL XVPARM('AREA',IPAR,NUM,IND,4)
      IF (IND.EQ.0) THEN
	  ISL = IPAR(1)
	  ISS = IPAR(2)
	  NL = IPAR(3)
	  NS = IPAR(4)
           ! if dataset lt 20x20, do full sampling
          if (nl .le. 20 .or. ns .le. 20) then
                INC = 1
                subsamp = 100.0
          endif
      END IF
C
C	IF GIVEN PARAMETERS ARE UNREASONABLE, ADJUST THEM
C
      IF(QLOG.AND.TOP.LT.0.0) TOP=250.5
      IF(QLOG.AND.BOTTOM.LT.-255.) BOTTOM=-255.0
      IF(QLOG.AND..NOT.QDIFF.AND.BOTTOM.LE.0.0) BOTTOM=0.2
      IF(QLOG.AND.QDIFF) ATM1=ATM1+256.0
C
      II = 4*INS   !allocate data set ins even if your windowing ns < ins
                   !else stacka_big crashes with 
                   ! [TAE-PRCSTRM] Abnormal process termination; process status code = 6.
      JJ = II
      CALL ZIA(IPOP,500)
      IF(QLOG) THEN
	  IF(.NOT.QDIFF) THEN
C                                                     LOG RAT
	      BOTTOM = ALOG(BOTTOM)
	      SCALE = 500.0/(ALOG(TOP)-BOTTOM)
c              call xvmessage('CALL STACKA_BIG(4,LNRAT,2,II8,II8)',' ')
              ii8 = int8(II)
	      CALL STACKA_BIG(4,LNRAT,2,II8,II8)
c              call xvmessage('Return from LNRAT',' ')
	  ELSE
C                                                     LOG DIFF
	      BOTTOM = ALOG(BOTTOM+256.0)
	      SCALE = 500.0/(ALOG(TOP+256.0)-BOTTOM)
c              call xvmessage('CALL STACKA_BIG(4,LNDIFF,2,II8,II8)',' ')
              ii8 = int8(II)
              CALL STACKA_BIG(4,LNDIFF,2,II8,II8)
c              call xvmessage('Return from LNDIFF',' ')
	  END IF
C                                                     RAT & DIFF
      ELSE
          SCALE = 500.0/(TOP-BOTTOM)
          ii8 = int8(II)
          IF (QDIFF)  then
c                call xvmessage('CALL STACKA_BIG(4,DIFF,2,II8,II8',' ')
                CALL STACKA_BIG(4,DIFF,2,II8,II8)
c                call xvmessage('Return from DIFF',' ')
          ELSE
c                call xvmessage('CALL STACKA_BIG(4,RAT,2,II8,II8)',' ')
                CALL STACKA_BIG(4,RAT,2,II8,II8)
c                call xvmessage('Return from RAT',' ') 
         ENDIF
      END IF
C
C     FIND MAXIMUM POPULATED BIN; FILTER HISTOGRAM IF REQUESTED
C
      IF(.NOT.QFILT) THEN
          CALL MVE(4,500,IPOP,JPOP,1,1)
          MAXMUM = 0
          DO I=1,500
              MAXMUM = MAX(MAXMUM,IPOP(I))
	  END DO
      ELSE
          JPOP(3) = IPOP(1)+IPOP(2)+IPOP(3)+IPOP(4)+IPOP(5)
          JPOP(2) = JPOP(3)-IPOP(5)+IPOP(1)
          JPOP(1) = JPOP(2)-IPOP(4)+IPOP(2)
          MAXMUM = MAX(JPOP(1),JPOP(2),JPOP(3))
          DO  I=4,498
              JPOP(I) = JPOP(I-1)-IPOP(I-3)+IPOP(I+2)
              MAXMUM = MAX(MAXMUM,JPOP(I))
	  END DO
          JPOP(499) = JPOP(498)-IPOP(496)+IPOP(500)
          JPOP(500) = JPOP(499)-IPOP(497)+IPOP(499)
          MAXMUM = MAX(MAXMUM,JPOP(499),JPOP(500))
      END IF
C
C     ZERO OUT ALL BINS POPULATED BELOW THRESHOLD LEVEL;
C     COMPUTE THE MEAN
C
      ICUT = FLOAT(MAXMUM)*THRESH+0.5
      ISUM = 0
      IWT = 0
      DO I=1,500
          IF(JPOP(I).LT.ICUT) JPOP(I)=0
          ISUM = ISUM+JPOP(I)
          IWT = IWT+JPOP(I)*I
      END DO
      MEAN = FLOAT(IWT)/FLOAT(ISUM)+0.5
C
C     COMPUTE THE HIGH AND LOW BINS THAT CORRESPOND TO THE REQUESTED
C     SATURATION LEVEL
C
      N = 0
      NSAT = MAX(ISUM*SAT,1.0)
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(I)
          I=I+1
      END DO
      IF (N.LT.NSAT) CALL MABEND('??E - Saturation greater than 100 percent')
      LOW = I-1
      N = 0
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(501-I)
          I=I+1
      END DO
      IF (N.LT.NSAT) CALL MABEND('??E - Saturation greater than 100 percent')
      IHI = 502-I
C
C     IF NEEDED, FORCE THE MEAN TO BE CENTERED (OUTPUT DN=128)
C     BY DECREASING THE SATURATION AT ONE END.
C
      IF (QCENTER) THEN
          M = MEAN-LOW
          N = IHI-MEAN
          IF(N.GT.M) LOW=MEAN-N
          IF(M.GT.N) IHI=MEAN+M
      END IF
C
C     COMPUTE THE GAIN AND OFFSET;  IF REQUESTED, CALL DISPLY TO
C     PRINT OUT THE HISTOGRAM
C
      VAL0 = VALUE(LOW)
      VAL255 = VALUE(IHI)

C...TWEAK FOR FLAT IMAGES TO PREVENT DIVIDE BY 0.

      IF ( VAL0 .EQ. VAL255)  THEN
         DELTA = VAL0 * .01
         IF (DELTA .EQ. 0)  DELTA=.01
         VAL0 = VAL0 - DELTA
         VAL255 = VAL255+DELTA
      END IF

      GAIN = 255.0/(VAL255-VAL0)
      OFF = -GAIN*VAL0
      IF(QDISP) CALL DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,
     +    QDIFF,QLOG)
C
C     GENERATE THE STRING THAT CONTAINS THE FUNCTION TO BE PASSED TO F2.
C
      func=' '
      M = 7-DLOG10(GAIN)
      FUNC(1:1) = '"'
      write(unconfmt,'(''(f10.'',i4.4,'')'')') m
      write(func(2:11),unconfmt) gain
      func(12:12) = '*'
      N = 13
      IF (QLOG) THEN
	  func(N:N+3) = 'ALOG'
	  N = N+4
      END IF
      FUNC(N:N) = '('
      N = N+1
      IF (ATM1.EQ.0.0) THEN
	      FUNC(N:N+2) = 'IN1'
	      N = N+3
	  ELSE
	      FUNC(N:N+3) = '(IN1'
	      write(FUNC(N+9-5:N+9),'(f6.1)') ATM1
	      IF (ATM1.GT.0.0) FUNC(N+4:N+4) = '+'
	      FUNC(N+10:N+10) = ')'
	      N = N+11
      END IF
      IF (QDIFF) THEN
	      FUNC(N:N) = '-'
	  ELSE
	      FUNC(N:N) = '/'
      END IF
      N = N+1
      IF (ATM2.EQ.0.0) THEN
	      FUNC(N:N+2) = 'IN2'
	      N = N+3
	  ELSE
	      FUNC(N:N+3) = '(IN2'
	      write(FUNC(N+9-5:N+9),'(f6.1)') ATM2
	      IF (ATM2.GT.0.0) FUNC(N+4:N+4) = '+'
	      FUNC(N+10:N+10) = ')'
	      N = N+11
      END IF
      FUNC(N:N) = ')'
      M = DLOG10(ABS(OFF)+1)+4
      write(unconfmt,'(''(f'',i4.4,''.1)'')') m
      write(FUNC(N+M-(M)+1:N+M),unconfmt) OFF
      IF (OFF.GE.0.0) FUNC(N+1:n+1)='+'
      FUNC(N+M+1:N+M+1) = '"'
      CALL XVMESSAGE('The ratio function is: ',' ' ) 
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(FUNC,' ')
      CALL XVMESSAGE(' ',' ')
C
C	SEND THE FUNCTION BACK TO THE PARAMETER 'FUNC' FOR F2
C
      CALL XQINI(PARB,xprdim,xabort)
      CALL XQSTR(PARB,'FUNC',1,FUNCSTR,xadd,ISTATUS)
      CALL XVQOUT(PARB,ISTATUS)
C
      RETURN
      END
C
C
C**********************************************************************
C
      SUBROUTINE RAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING RATIO HISTOGRAMS
C
        implicit none
      integer*4 I,J,N,INC,INP1,INP2,ISL,ISS
      integer*4 ISTATUS,NL,NS,INL,INS
      integer*8 II,JJ
      real*4    ATM1,ATM2,BOTTOM,SCALE,X,Y
      REAL*4 XARR(NS),YARR(NS)
      INTEGER*4 IPOP(500)
      character*60 msg

      COMMON ISL,ISS,INL,INS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP

      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (Y.NE.0.0) THEN
	          N = int(SCALE*(X/Y-BOTTOM)+1.0)
                  if (N .gt. 500) then 
                        write(msg,10100) i,j
10100 format ("pixel rejected at ",i6,x,i6" > 500" )
                        call xvmessage(msg,' ')
                  endif
	          IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	      END IF
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING DIFFERENCE HISTOGRAMS
C
         implicit none
      integer*4 I,J,N,INC,INP1,INP2,ISL,ISS
      integer*4 ISTATUS,NL,NS,INL,INS
      integer*8 II,JJ
      real*4    ATM1,ATM2,BOTTOM,SCALE,OFFSET
      REAL*4 XARR(NS),YARR(NS)
      INTEGER*4 IPOP(500)

      COMMON ISL,ISS,INL,INS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP

      IF(JJ.NE.II) CALL MABEND('INSUFFICIENT ROOM FOR STACKA BUFFERS')
      OFFSET = ATM1-ATM2-BOTTOM
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,' ')
          DO J=1,NS
	      N = SCALE*(XARR(J)-YARR(J)+OFFSET)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE LNRAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG RATIO HISTOGRAMS
C
        implicit none
      integer*4 I,J,N,INC,INP1,INP2,ISL,ISS
      integer*4 ISTATUS,NL,NS,INL,INS
      integer*8 II,JJ
      real*4    ATM1,ATM2,BOTTOM,SCALE,X,Y
      REAL*4 XARR(NS),YARR(NS)
      INTEGER*4 IPOP(500)

      COMMON ISL,ISS,INL,INS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP

      IF(JJ.NE.II) CALL MABEND('0INSUFFICIENT ROOM FOR STACKA BUFFERS')
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (X.GT.0.0.AND.Y.GT.0.0) THEN
	          N = SCALE*(ALOG(X/Y)-BOTTOM)+1.0
	          IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	      END IF
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE LNDIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG DIFFERENCE HISTOGRAMS
C
        implicit none
      integer*4 I,J,N,INC,INP1,INP2,ISL,ISS
      integer*4 ISTATUS,NL,NS,INL,INS
      integer*8 II,JJ
      real*4    ATM1,ATM2,BOTTOM,SCALE,OFFSET
      REAL*4 XARR(NS),YARR(NS)
      INTEGER*4 IPOP(500)

      COMMON ISL,ISS,INL,INS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP

      IF(JJ.NE.II) CALL MABEND('INSUFFICIENT ROOM FOR STACKA BUFFERS')
      OFFSET = ATM1-ATM2
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,' ')
          DO J=1,NS
	      N = SCALE*(ALOG(XARR(J)-YARR(J)+OFFSET)-BOTTOM)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,QDIFF,
     +                     QLOG)
C
C     ROUTINE FOR PRINTING HISTOGRAMS
C     IF FILTERING WAS PERFORMED, THIS IS THE FILTERED HISTOGRAM.
C
        implicit none
      integer*4 I,II,N,IC
      integer*4 LOW,MEAN,IHI,MAXMUM
      real*4    BOTTOM,SCALE,X
      INTEGER*4 JPOP(500)
      character*132 buf, GRID
      LOGICAL*4 QDIFF,QLOG                    !GRID(101)

      GRID=' '
      DO II=1,101,10
          GRID(II:II)='+'
      END DO
C
      BOTTOM = BOTTOM+0.5/SCALE
C
C     PRINT THE PROPER HEADING
C
      CALL XVMESSAGE(' ',' ')
      IF(.NOT.QDIFF.AND..NOT.QLOG) CALL XVMESSAGE('R A T I O',' ')
      IF(.NOT.QDIFF.AND.QLOG) CALL XVMESSAGE('L O G   R A T I O',' ')
      IF(QDIFF.AND..NOT.QLOG) CALL XVMESSAGE('D I F F E R E N C E',' ')
      IF(QDIFF.AND.QLOG) CALL XVMESSAGE('L O G   D I F F E R E N C E',
     +                                   ' ')
      buf(1:122) = ' '
      DO ii=0,100,10
          write(buf(22+ii-2:22+ii),'(i3)') ii
      END DO
c      BUF(1:1)='0'
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(BUF(1:122),' ')
      BUF(1:22) = ' '
C      CALL MVLC(GRID,BUF(23:123),101)
      BUF(22:122)=GRID(1:101)
C
C     PRINT A LINE FOR EACH POPULATED BIN, NORMALIZED TO THE MAXIMUM
C     POPULATED BIN
C
      DO I=1,500
          IF(JPOP(I).NE.0) THEN
	      X = FLOAT(I-1)/SCALE+BOTTOM
              write(buf(1:12),'(f12.3)') X
C
C     LABEL THE MEAN (OUTPUT=128DN) AND SATURATION LEVEL BINS.  IN SOME
C     CASES, ONE OF '0DN' AND '255DN' WILL BE OUTSIDE THE POPULATED RANG
C     OF THE HISTOGRAM, AND WILL NOT APPEAR.
C
              IF(I.EQ.LOW) buf(17:19) = '0DN'
      	      IF(I.EQ.MEAN) buf(16:19) = 'MEAN'
	      IF(I.EQ.IHI) buf(15:19) = '255DN'
	      BUF(22:122)=GRID(1:101)
	      N = (100*JPOP(I))/MAXMUM+1
	      do ic=22,22+N-1
		buf(ic:ic)='*'
	      enddo
              CALL XVMESSAGE(BUF(1:122),' ')
              buf(15:19) = '     '
	    ELSE IF (I.NE.1 .AND. JPOP(I-1).NE.0) THEN
	      CALL XVMESSAGE(' ',' ')
	  END IF
      END DO
      CALL XVMESSAGE(' ',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ratio0.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ratio

   To Create the build file give the command:

		% vimake ratio0			(Unix)


************************************************************************/


#define PROGRAM	ratio0
#define R2LIB

#define MODULE_LIST ratio0.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ratio0.pdf
PROCESS HELP=*
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
PARM FUNC TYPE=NAME
END-PROC
.TITLE
Vicar Program ratio0
.HELP
PURPOSE:
ratio0 operates as part of the procedure ratio. It uses a NAME type parameter,
and therefore should not be run outside of its procedure. For details of its
operation see ratio.


History

WRITTEN BY:  A.R. Gillespie, 4 January 1974
COGNIZANT PROGRAMMER:  Ray Bambery
REVISION:  4        4 June 1986
Made Portable for UNIX      CRI      10-JUL-95

Revision 6 19-Jul-2016 - R.J. Bambery
            Added SILENT parameter to silence the commands in ratio
            script
            Removed 2 GB maximum image size.
            by changing stacka call with stacka_big call
            Centos-7.2 & gcc-4.8.5

$ Return
$!#############################################################################
