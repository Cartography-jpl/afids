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
