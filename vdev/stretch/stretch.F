c      PROGRAM STRETCH
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT NONE
c  Feb 10, 2010 - RJB - All input is processed internally as HALF
C
c ALL commons shown here in all stretch modules
c	astretch 		/C3/
c	bimodal			/C3/, /CW/
c	exclude_histogram	/C3/
c	get_histogram		/C1/, /C3/
c	hist			/C1/, /C2/, /C3/, /CPAR/, /PAUSE/, 
c	histogram_stretch	/C2/, /C3/
c	hpeak			/C3/
c	manual_stretch		/C2/, /CW/
c	post_stretch		/C3/
c	print_histogram		/C3/
c	stati			/C3/
c	stretch (main)		/C1/, /C2/, /C3/
c
	INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

	REAL*4 GSIGMA,GMEAN,POWER
      COMMON/C2/GSIGMA,GMEAN,POWER

	INTEGER*4 INMIN,INMAX             !Input DN range
	INTEGER*4 DNMIN,DNMAX             !Output DN range
      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX

	INTEGER*4 IPARM(512)
	COMMON /CW/ IPARM

	INTEGER*4 HISTW(65536,2)
      COMMON/CW1/HISTW                          !Commented out 02/01/2010 - RJB

      REAL*4 TABBUF(512)
	COMMON/CW2/TABBUF

	REAL*4 RPARM(100)
	CHARACTER*1024 FNCBUF
      COMMON/CW3/RPARM,FNCBUF

C
c confusing size and type assignments for COMMON /CW/
c
c  (1)	other CW defs		manual_stretch (subroutine sfunc) COMMENTED OUT (2/1/2010)
c	REAL*4 RPARM(100)
c	CHARACTER*1024 FNCBUF
c	COMMON/CW/RPARM,FNCBUF                   !!!!!!!!!!!!
c  (2)				manual_stretch (subroutine salarm)
c      INTEGER*4 ALRBUF(256)             ! change to 512 to match - 64-bit
c     COMMON/CW/ALRBUF                          !!!!!!!!!!!!
c
c  (3)				manual_stretch (subroutine sitable)
c	INTEGER*4 IPARM(512) 
c      COMMON/CW/IPARM                           !!!!!!!!!!!!
c
c  (4)				manual_stretch (subroutine stable)
C	REAL*4 TABBUF(512)
c	INTEGER*4 ITABBUF(512)                    !Changed to ITABBUF(512)
c      COMMON/CW/ITABBUF
c
c  (5)				bimodal	(subroutine twopk)	COMMENTED OUT (2/1/2010)
c      INTEGER*4 HISTW(65536,2)
c      COMMON/CW/HISTW
c
c
c	IMODE:
c	0 - INITIAL STATE
c	1 - COMPLEMENT MODE      		(MANUAL)
c	2 - LINEAR CONTRAST STRETCH MODE   	(MANUAL)
c	3 - BIT CLIPPING MODE			(MANUAL)
c	4 - CONTOUR MODE			(MANUAL)
c	5 - ALARM MODE				(MANUAL)
c	6 - TABLE STRETCH MODE			(MANUAL)
c	7 - INDIVIDUAL DN TABLE STRETCH MODE	(MANUAL)
c	8 - PERIODIC STRETCH MODE		(MANUAL)
c	9 - USER SPECIFIED FUNCTION MODE	(MANUAL)
c	10 - LOGARITHMIC STRETCH MODE		(MANUAL)
c	11 - SMOOTH 				(HISTOGRAM)
c	12 - GAUSS				(HISTOGRAM)
c	13 - ELLIPSE				(HISTOGRAM)
c	14 - POWER				(HISTOGRAM)
c	15 - BIMODAL				(HISTOGRAM)
c	16 - PEAK				(HISTOGRAM)
c	17 - MEAN				(HISTOGRAM)
c	18 - ASTRETCH				(HISTOGRAM)
c 

C     ....Note: HIST is made an extra 512 words longer to prevent overflow
C     ....when reading it in from an IBIS file.
      INTEGER*4 HIST(-32768:32767)	!Input histogram   (before 02/02/2010 was (-32768:33280)
      INTEGER*4 BHIST(0:255)		!Copy of input histogram (byte data)
      INTEGER*2 BUF(65536)		!Input line buffer
      INTEGER*2 LUT(-32768:32767)	!Stretch look-up table	!64-bit

      INTEGER*4 STAT,IMODE,NSTRETCH
      INTEGER*4 HFLAG,IHIST,OHIST,ICDF,OCDF,NPTS
      CHARACTER*8 FORMAT

      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NCHAR,NCHAR2
      CHARACTER*80 PRT,PRT2

      call xvmessage('STRETCH - Jun 06, 2012 (64-bit) RJB **',' ')

C     ....Open input image and check format and size field
      CALL OPENINP(format,sl,ss,sb,nlo,nso,nbo,nli,nsi,nbi,iunit,inmin,
     .             inmax)

C     ....Open output image and check output DN range
      CALL OPENOUT(FORMAT,NLO,NSO,NBO,ounit,dnmin,dnmax)

      IMODE=0			!Stretch mode
      NSTRETCH = 0              !Count of number of stretches specified
C     ....Compute look-up table for non-histogram stretches
      CALL MANUAL_STRETCH(FORMAT,lut,imode,nstretch,nchar,prt)
C     ....Get most histogram-related user parameters
      CALL HIST_PARAMS(FORMAT,imode,hflag,ihist,ohist,
     &          icdf,ocdf,power,nstretch)
c	HFLAG = 1 if histogram is needed (set in hist_params)
      IF (HFLAG.EQ.0) GOTO 50
C     ....Compute or read the input image histogram
      CALL GET_HISTOGRAM(hist,buf,tmean,tsigma,npts)
C     ....Print input histogram and CDF, if specified
      CALL PRINT_HISTOGRAM(HIST,IHIST,ICDF,INMAX,TMEAN,TSIGMA,NPTS)
C     ....Exclude DN ranges from histogram, if specified
      CALL MVE(4,256,HIST(0),BHIST,1,1)		!Save it
      CALL EXCLUDE_HISTOGRAM(HIST,npts)
C     ....Compute look-up table for histogram stretches
      CALL HISTOGRAM_STRETCH(HIST,NPTS,imode,lut,nchar,prt)

   50 CALL POST_STRETCH(lut,nchar2,prt2)
C     ....Stretch the image and write to output
      CALL STRETCH_IMAGE(FORMAT,PRT,PRT2,NCHAR,NCHAR2,LUT,buf)
C     ....Print output histogram and CDF
	IF (OHIST+OCDF.GT.0) THEN
             CALL  PRINT_OHISTOGRAM(BHIST,LUT,OHIST,OCDF,NPTS)
	ENDIF
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input image and check format and size field
C
      SUBROUTINE OPENINP(format,sl,ss,sb,nlo,nso,nbo,nli,nsi,nbi,
     +		iunit,inmin,inmax)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,IUNIT,INMIN,INMAX,SB,NBO,NBI

      INTEGER*4 STAT
	character*80 MSG

      CALL XVUNIT(iunit,'INP',1,stat, ' ')
      CALL XVOPEN(IUNIT,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,STAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  '??E - BIP files not supported, use program TRAN to convert to BSQ')


      CALL XVSIZE(sl,ss,nlo,nso,nli,nsi)
      CALL XVBANDS(sb,nbo,nbi)

      IF ( sb .GT. nbi ) CALL MABEND(
     +  '??E - SB is greater than the total number of bands')
      
      IF (SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE('??W - Number of lines truncated',' ')
         NLO = NLI - SL + 1
      ENDIF
      IF (SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE('??W - Number of samples truncated', ' ')
         NSO = NSI - SS + 1
      ENDIF
      IF (SB+NBO-1 .GT. NBI) THEN
         CALL XVMESSAGE('??W - Number of bands truncated', ' ')
         NBO = NBI - SB + 1
      ENDIF

      CALL XVGET(IUNIT,stat,'FORMAT',format,' ')
      IF (FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
	 WRITE (MSG,100) FORMAT
100   FORMAT ('??E - Input image is ',A5,' format')
         CALL XVMESSAGE(MSG,' ')
         CALL XVMESSAGE('***Image must be byte or halfword',' ')
         CALL MABEND('***STRETCH task cancelled')
      ENDIF
c	2-10-2010 - added reopen to ensure all input is HALF
	CALL XVCLOSE(IUNIT,stat,' ')
        CALL XVOPEN(IUNIT,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',FORMAT,'U_FORMAT','HALF',' ')

C     ....Set input DN range
      IF (FORMAT.EQ.'BYTE') THEN
         INMIN = 0
         INMAX = 255
      ELSE
         INMIN = -32768
         INMAX = 32767
      ENDIF
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open output image and check output DN range
C
      SUBROUTINE OPENOUT(FORMAT,NLO,NSO,NBO,ounit,dnmin,dnmax)
      IMPLICIT NONE
      INTEGER*4 OUNIT,NLO,NSO,DNMIN,DNMAX,NBO
      CHARACTER*8 FORMAT

      INTEGER*4 STAT,ICOUNT,IDEF,IDEF2,NI

      CALL XVUNIT(ounit,'OUT',1,stat,' ')
c	2-10-2010 - pass out internal HALF format to output format
      CALL XVOPEN(OUNIT,stat,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     &            'U_NL',NLO,'U_NS',NSO, 'U_NB' ,NBO,'O_FORMAT',FORMAT,
     & 'U_FORMAT','HALF',' ')

C     ....Get output DN range DNMIN,DNMAX
      CALL XVPARM('DNMIN',dnmin,icount,idef,1)
      CALL XVPARM('DNMAX',dnmax,icount,idef2,1)
      IF (FORMAT.EQ.'HALF') THEN      
         IF (IDEF.EQ.1) DNMIN=-32768
         IF (IDEF2.EQ.1) DNMAX=32767
      ELSE		!Here for byte data
         IF (DNMIN.LT.0) THEN
            CALL XVMESSAGE('??W - DNMIN RESET TO BYTE MIN OF 0', ' ')
            DNMIN=0
         ENDIF
         IF (DNMAX.GT.255) THEN
            CALL XVMESSAGE('??W -  DNMAX RESET TO BYTE MAX OF 255', ' ')
            DNMAX=255
         ENDIF
      ENDIF

C     ....Check restrictions imposed by HISTGEN input
      CALL XVPCNT('INP',NI)	!Number of input files.
      IF (DNMIN.NE.0 .AND. NI.EQ.2) DNMIN=0
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Stretch the image using LUT and write to output
C
      SUBROUTINE STRETCH_IMAGE(FORMAT,PRT,PRT2,NCHAR,NCHAR2,LUT,buf)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 NCHAR,NCHAR2	!Number of chars in labels
      CHARACTER*80 PRT,PRT2	!Labels for stretch and post-stretch
      INTEGER*2 LUT(-32768:32767)			!64-bit
	integer*2 BUF(*)

      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,L,EL,IDN,STAT
!      LOGICAL*1 LUTB(256)		!Byte look-up table
	byte LUTB(256)              !Byte look-up table 		64-bit
	byte bbuf(65536)
      INTEGER*4 BAND
      INTEGER*4 BANDOUT,LINEOUT

C     ....If byte data, compress LUT to byte table
      IF (FORMAT.EQ.'BYTE') CALL MVE(-3,256,LUT(0),LUTB,1,1)

      EL = NLO+SL-1
      BANDOUT = 0
      DO BAND=SB,SB+NBO-1
       BANDOUT = BANDOUT + 1
       LINEOUT = 0
c READ
       DO 10 L=SL,EL
        LINEOUT = LINEOUT + 1  
        CALL XVREAD(IUNIT,buf,stat,'LINE',L,'SAMP',SS,'NSAMPS',NSO,
     +             'BAND',BAND,' ')
c DO CONVERSION
c	if (l.lt.15) then
c		print *, 'in = ',buf(1),buf(2),buf(3),buf(4),buf(5),buf(6),buf(7),buf(8),buf(9),buf(10),buf(11),buf(12)
c	endif
        IF (FORMAT.EQ.'BYTE') THEN
	 DO I=1,NSO
	     BBUF(I) = BUF(I)
	 ENDDO
         CALL TBL(bbuf,LUTB,NSO)			!p2sub
         DO I=1,NSO
             BUF(I) = BBUF(I)
         ENDDO

        ELSE
         DO I=1,NSO
            IDN = BUF(I)
            BUF(I) = LUT(IDN)
         ENDDO
       ENDIF
c        if (l.lt.15) then
c                print *, 'out = ',buf(1),buf(2),buf(3),buf(4),buf(5),buf(6),buf(7),buf(8),buf(9),buf(10),buf(11),buf(12)
c        endif

C WRITE
   10  CALL XVWRIT(OUNIT,BUF,stat,'LINE',LINEOUT,'BAND',BANDOUT,' ')
      END DO

C     ....Add description of stretch and post-stretch to label
      CALL XLADD(OUNIT,'HISTORY','PARMS',PRT,STAT,
     +  	 'FORMAT','STRING','ULEN',NCHAR,' ')
      IF (NCHAR2.GT.0) CALL XLADD(OUNIT,'HISTORY','PARMS2',
     +		PRT2,STAT,'FORMAT','STRING','ULEN',NCHAR2,' ')
      RETURN
      END
