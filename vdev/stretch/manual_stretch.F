CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute look-up table for non-histogram stretches
C
	SUBROUTINE MANUAL_STRETCH(format,lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 format	!'BYTE' or 'HALF'
      INTEGER*2 LUT(-32768:32767)!Computed stretch table		!64-bit
      INTEGER*4 IMODE		!Stretch mode
      INTEGER*4 NSTRETCH	!Number of stretch options specified
      INTEGER*4 NCHAR		!Number of characters in PRT buffer
      CHARACTER*80 PRT		!String to be inserted in image label

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      CALL SLINEAR(lut,imode,nstretch,nchar,prt)
      CALL SCOMP(lut,imode,nstretch,nchar,prt)
      CALL SCLIP(format,lut,imode,nstretch,nchar,prt)
      CALL SCONTOUR(lut,imode,nstretch,nchar,prt)
      CALL SALARM(lut,imode,nstretch,nchar,prt)
      CALL SITABLE(lut,imode,nstretch,nchar,prt)
      CALL STABLE(lut,imode,nstretch,nchar,prt)
      CALL SPERIODIC(lut,imode,nstretch,nchar,prt)
      CALL SLOG(lut,imode,nstretch,nchar,prt)
      CALL SFUNC(lut,imode,nstretch,nchar,prt)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCOMP(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)				!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ITRUNC
      LOGICAL*4 XVPTST

      IF (.NOT.XVPTST('COMP')) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** COMPLEMENT MODE ***',' ')
      IMODE=1

      DO IDN=INMIN,INMAX
         LUT(IDN) = ITRUNC(DNMIN+DNMAX-IDN)
      ENDDO

      WRITE (PRT,115) DNMIN,DNMAX,DNMAX,DNMIN
  115 FORMAT ('Complement Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR=58
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SLINEAR(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN
      REAL*4 RPARM(2),RMIN,RMAX,A,B,RTRUNC

      CALL XVPARM('LINEAR',RPARM,ICOUNT,IDEF,2)
      IF (ICOUNT.LT.2) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** LINEAR CONTRAST STRETCH MODE ***',' ')
      IMODE=2
      RMIN = RPARM(1)
      RMAX = RPARM(2)
      IF (RMIN.EQ.RMAX) RMAX=RMAX+1

      A = FLOAT(DNMAX-DNMIN)/(RMAX-RMIN)	!Scale
      B = -A*RMIN+DNMIN				!Offset
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

      WRITE (PRT,125) NINT(RMIN),DNMIN,NINT(RMAX),DNMAX
  125 FORMAT ('Linear Stretch:',I7,' to',I7,' and',I7,' to',I7)
      IF (NINT(RMIN) .NE. RMIN)  CALL REALCON(RMIN,PRT(16:),7)
      IF (NINT(RMAX) .NE. RMAX)  CALL REALCON(RMAX,PRT(37:),7)
      NCHAR=54
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCLIP(FORMAT,lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT
      include  'fortport'       !Defines INT2BYTE

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,NBITS,IDN,ODN,ITRUNC
      INTEGER*2 HVALUE
c      LOGICAL*1 LVALUE
	byte LVALUE				! use byte rather than logical*1 for 64-bit

  133 FORMAT ('Bit Clip Stretch:',I3,
     1 ' Most  Significant Bits Clipped')
  136 FORMAT ('Bit Clip Stretch:',I3,
     1 ' Least Significant Bits Clipped')

      CALL XVPARM('CLIP',nbits,icount,idef,1)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** BIT CLIPPING MODE ***',' ')
      IMODE = 3

      DO IDN=INMIN,INMAX
         IF (FORMAT.EQ.'BYTE') THEN
            LVALUE = INT2BYTE(IDN)
            CALL SHFV(1,1,NBITS,lvalue,1)
            ODN = BYTE2INT(LVALUE)
         ELSE
            HVALUE = IDN
            CALL SHFV(2,1,NBITS,hvalue,1)
            ODN = HVALUE
         END IF
         LUT(IDN) = ITRUNC(ODN)
      ENDDO

      IF (NBITS.GT.0) THEN
         WRITE (PRT,133) NBITS
      ELSE
         WRITE (PRT,136) -NBITS
      ENDIF
      NCHAR=52
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCONTOUR(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,INC,DNVAL,IDN,ITRUNC

      CALL XVPARM('CONTOUR',inc,icount,idef,1)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** CONTOUR MODE ***',' ')
      IMODE = 4
      CALL INIT_LUT(LUT)	!Initialize the LUT

C     ....Get DNVALUE of contours
      CALL XVPARM('DNVALUE',dnval,icount,idef,1)
      IF (ICOUNT.EQ.0) DNVAL=DNMAX
      DNVAL = ITRUNC(DNVAL)

C     ....Put contours into LUT
      DO IDN=DNMIN,DNMAX,INC
         LUT(IDN) = DNVAL
      ENDDO

      WRITE (PRT,145) INC,DNVAL
  145 FORMAT ('Contour Stretch: Interval =',I5,'  DNvalue =',I7)
      NCHAR=51
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SALARM(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/CW/ALRBUF				!!!!!!!!!!!!
      INTEGER*4 ALRBUF(512)		! change to 512 to match - 64-bit

      INTEGER*4 I,ICOUNT,IDEF,DNVAL,IDN,ITRUNC,NALARM
  155 FORMAT ('Alarm Stretch: DNvalue =',I7)

      CALL XVPARM('ALARM',alrbuf,nalarm,idef,100)
      IF (NALARM.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** ALARM MODE ***',' ')
      IMODE = 5
      CALL INIT_LUT(LUT)	!Initialize the LUT

      CALL XVPARM('DNVALUE',dnval,icount,idef,1)	!Alarm DN value
      IF (ICOUNT.EQ.0) DNVAL=DNMAX
      DNVAL = ITRUNC(DNVAL)

      DO I=1,NALARM		!Set the alarms
         IDN = ALRBUF(I)
         LUT(IDN) = DNVAL
      END DO
      WRITE (PRT,155) DNVAL
      NCHAR=32
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SITABLE(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/CW/IPARM				!!!!!!!!!!!!
      INTEGER*4 IPARM(512)

      INTEGER*4 I,ICOUNT,IDEF,IDN,NPAIRS,ITRUNC

      CALL XVPARM('ITABLE',iparm,icount,idef,512)
      IF (ICOUNT.LT.2) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** INDIVIDUAL DN TABLE STRETCH MODE ***',' ')
      IMODE = 7
      NPAIRS = ICOUNT/2
      IF (2*NPAIRS.NE.ICOUNT) THEN
         CALL XVMESSAGE('??E - Invalid number of table entries',' ')
         CALL ABEND
      END IF

      CALL INIT_LUT(LUT)	!Initialize the lookup table
      ICOUNT = 2*NPAIRS		!Set the individual table elements
      DO I=1,ICOUNT,2
         IDN = IPARM(I)
         LUT(IDN) = ITRUNC(IPARM(I+1))
      END DO
      PRT = 'Individual Table Stretch'
      NCHAR=25
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE STABLE(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

c	integer*2 lutx, luty
      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/CW2/TABBUF
      REAL*4 TABBUF(512)		!Input table (in1,out1,int2,out2...)

      INTEGER*4 I,N,ICOUNT,IDEF,IDN,NPAIRS
      INTEGER*4 INDN1,OUTDN1,INDN2,OUTDN2
      REAL*4 A,B,RTRUNC

      CALL XVPARM('TABLE',tabbuf,icount,idef,512)
      IF (ICOUNT.LT.4) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** TABLE STRETCH MODE ***',' ')
      IMODE = 6
      NPAIRS = ICOUNT/2
      IF (2*NPAIRS.NE.ICOUNT) THEN
         CALL XVMESSAGE('??E - Invalid # of TABLE entries',' ')
         CALL ABEND
      END IF
      N = 2*(NPAIRS-1)		!2*(number of intervals)
      CALL INIT_LUT(lut)	!Initialize the look-up table
c    step through table (indn1,outdn1,indn2,outdn2,...)
      DO I=1,N,2
         INDN1  = NINT(TABBUF(I))		!INDN1 = NINT(TABBUF(I))
         OUTDN1 = TABBUF(I+1)			!OUTDN1 = TABBUF(I+1)
         INDN2  = NINT(TABBUF(I+2))		!INDN2  = NINT(TABBUF(I+2))
         OUTDN2 = TABBUF(I+3)			!OUTDN2 = TABBUF(I+3)
         IF (INDN2.LE.INDN1)
     &		 CALL MABEND('??E - Table stretch parameter error')
c	 print *, 'indn1 = ',indn1,' outdn1 = ',outdn1,' indn2 = ',indn2,' outdn2 = ',outdn2
         A=(OUTDN2-OUTDN1)/(INDN2-INDN1)	!Scale
         B=OUTDN1-(A*INDN1)			!Offset
c	 lutx = RTRUNC(A*indn1+B) 
c	 luty = RTRUNC(A*indn2+B)
c	 print *, 'A = ',a,' B = ', b,' lutx = ',lutx,' luty = ',luty
         DO IDN=INDN1,INDN2
            LUT(IDN) = RTRUNC(A*IDN+B)
         ENDDO
      ENDDO

      PRT = 'Table Stretch'
      NCHAR=14
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SPERIODIC(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN
      REAL*4 FREQ,AMPL,PHI,DC,W,RTRUNC
      LOGICAL*4 XVPTST
  185 FORMAT ('Periodic Stretch: FREQ=',F6.2,'  DC=',F8.1,
     +        '  AMPL=',F8.1,'  PHI=',F6.2)

      IF (.NOT.XVPTST('PSTRETCH')) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** PERIODIC STRETCH MODE ***',' ')
      IMODE = 8
      CALL XVPARM('FREQ',freq,icount,idef,1)
      CALL XVPARM('DC',dc,icount,idef,1)
      IF (ICOUNT.EQ.0) DC=(DNMAX+DNMIN)/2.0
      CALL XVPARM('AMPL',ampl,icount,idef,1)
      IF (ICOUNT.EQ.0) AMPL=DNMAX-DNMIN
      CALL XVPARM('PHI',phi,icount,idef,1)
      W = 2.0*3.14159*FREQ/(DNMAX-DNMIN)
      AMPL = AMPL/2.0
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(AMPL*SIN(W*IDN+PHI)+DC)
      END DO
      WRITE (PRT,185) FREQ,DC,2.0*AMPL,PHI
      NCHAR=70
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SFUNC(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX
C
c  PROBLEMS CW is defined as INTEGER*4 (512)
        INTEGER*4 IPARM(512)
        COMMON /CW/ IPARM

        INTEGER*4 HISTW(65536,2)
      COMMON/CW1/HISTW                          !Commented out 02/01/2010 - RJB

      REAL*4 TABBUF(512)
        COMMON/CW2/TABBUF


      COMMON/CW3/RPARM,FNCBUF			!Commented out 02/01/2010 - RJB
      REAL*4 RPARM(100)
      CHARACTER*1024 FNCBUF

      INTEGER*4 I,J,ICOUNT,IDEF,IER,NCFUNC,IDN
      REAL*4 DN,RTRUNC

      CALL XVPARM('FUNCTION',fncbuf,icount,idef,1)
      CALL XVSPTR(FNCBUF,1,i,ncfunc)
      IF (ICOUNT.EQ.0 .OR. NCFUNC.LE.1) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** USER SPECIFIED FUNCTION MODE ***',' ')
      IMODE = 9
      J = MIN(NCFUNC,50)
      NCHAR = 30 + J
      PRT = 'Function Stretch: FUNCTION = ' // FNCBUF(1:J)
      CALL XVMESSAGE(PRT,' ')
      CALL KNUTH(FNCBUF,RPARM,IER)
      IF (IER.NE.0) THEN
         CALL XVMESSAGE('??E - Error in parsing function with KNUTH',' ')
         CALL XVMESSAGE(FNCBUF(1:NCFUNC),' ')
         CALL ABEND
      END IF
      DO IDN=INMIN,INMAX
         RPARM(1) = IDN
         CALL XKNUTH(RPARM,DN)
         LUT(IDN) = RTRUNC(DN)
      END DO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SLOG(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)			!64-bit
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN,ODN
      REAL*4 LOW,HIGH,CURVE,RPARM(2),R,RTRUNC
      REAL*8 AA,BB,CC
  205 FORMAT ('LOGARITHMIC Stretch:',F7.1,' to',I7,' and',F7.1,
     .        ' to',I7,' with CURVE=',G10.2)

      CALL XVPARM('LOG',rparm,icount,idef,2)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** LOGARITHMIC STRETCH MODE ***',' ')
      IMODE = 10
      IF (ICOUNT .EQ. 2) THEN
         LOW  = RPARM(1)
         HIGH = RPARM(2)
      ELSE
         LOW  = DNMIN
         HIGH = DNMAX
      END IF
      IF ( HIGH .EQ. LOW)    CALL MABEND(
     .     '??E - HIGH and LOW parameters may not be equal.')

      CALL XVPARM('CURVE',curve,icount,idef,0)
      IF (LOW + CURVE .LE. 0.)
     .     CALL MABEND('??E - LOW + CURVE must be greater than zero.')
      IF (HIGH + CURVE .LE. 0.)
     .     CALL MABEND('??E - HIGH + CURVE must be greater than zero.')

      CC = CURVE
      AA = (DNMAX-DNMIN)/DLOG((HIGH+CC)/(LOW+CC))
      BB = DNMIN - AA*DLOG(LOW+CC)

      DO IDN=INMIN,INMAX
         IF (IDN+CC .LE. 0.) THEN
            IF (LOW .LT. HIGH) THEN
               ODN = DNMIN
            ELSE
               ODN = DNMAX
            END IF
         ELSE
            R = AA*DLOG(IDN+CC)+BB
            LUT(IDN) = RTRUNC(R)
         END IF
      ENDDO
      WRITE (PRT,205) LOW,DNMIN,HIGH,DNMAX,CURVE
      NCHAR=80
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Truncate output DN to fit in DNMIN to DNMAX range.
C
      INTEGER FUNCTION ITRUNC(ODN)
	IMPLICIT NONE
      INTEGER*4 ODN

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      ITRUNC = ODN
      IF (ODN.LT.DNMIN) ITRUNC=DNMIN
      IF (ODN.GT.DNMAX) ITRUNC=DNMAX
      RETURN
      END      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Round-off DN to an integer and fit in DNMIN to DNMAX range.
C 3/07/2010 - adjust so that negative values are not 
c		rounded by adding 0.5
c
      REAL FUNCTION RTRUNC(DN)
	IMPLICIT NONE
      REAL*4 DN

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 IDN

	if (dn.ge.0) then
      	   IDN = DN + 0.5
	else
	   IDN = DN
	endif	
      IF (IDN.GT.DNMAX) IDN=DNMAX
      IF (IDN.LT.DNMIN) IDN=DNMIN
      RTRUNC = IDN
      RETURN
      END      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initiale the look-up table.
c
      SUBROUTINE INIT_LUT(LUT)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)		!64-bit

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER IDN,ITRUNC,BCKGND,ICOUNT,IDEF

      CALL XVPARM('BACKGND',bckgnd,icount,idef,1)
      IF (ICOUNT.EQ.1) THEN
         BCKGND = ITRUNC(BCKGND)
         DO IDN=INMIN,INMAX	!Initialize LUT to background if specified
            LUT(IDN) = BCKGND
         ENDDO
      ELSE			!Else, initialize LUT to identify transform
         DO IDN=INMIN,INMAX
            LUT(IDN) = ITRUNC(IDN)
         ENDDO
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check that only one stretch was specified
C
c	this is called within each routine to verify that only one
c	stretch imode was called
c
      SUBROUTINE CHK_NUM_STRETCH(NSTRETCH)

	IMPLICIT NONE
      INTEGER*4 NSTRETCH

      NSTRETCH = NSTRETCH + 1
      IF (NSTRETCH.LE.1) RETURN
      CALL XVMESSAGE('??E - More than one stretch specified',' ')
      CALL XVMESSAGE('***STRETCH task cancelled',' ')
      CALL ABEND
      END
