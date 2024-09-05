      INCLUDE 'VICMAIN_FOR'
C Vicar program HIST - Computes and plots or prints histogram
C	HIST  INP=A  user-parameters...
C Note: PPLOT has been disabled (see AVIRIS version).  To enable PPLOT,
C enable all statements commented out (containing ###).
C

      subroutine main44

	implicit none
      external work
      integer*4 IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI
      COMMON/C1/IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI

	integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
	integer*4 IBINS,ILIM
	real*8 BOUNDL,BOUNDU,BINWID
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,ILIM,BOUNDL,BOUNDU,BINWID

	integer*4 MODE,ISPIKE
	logical*4 NOCUM,QEXCLUDE,QPPLOT
      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT

	logical*4 qpause
      COMMON/PAUSE/qpause

      integer*4 batch,istat,ibp,icnt,idef,ibpu,inc,itemp
	integer*8 m,n
	integer*4 xvpixsizeu
      real*4 bounds(2)
      logical*4 xvptst
      character*8 fmt
      character*3 orgin 

C==================================================================
	ibpu = 0
      call xvmessage( '*** HIST version 2019-03-05 ***',' ')
C     ....Open input image
      call xvunit(iunit,'INP',1,ISTAT,' ')
      call xvopen(iunit,istat,'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      call xvget(iunit,istat,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') call mabend(
     +  '??E - BIP files not supported, use program TRAN to convert to BSQ')

      call xvsize(sl,ss,nl,ns,nli,nsi)
      call xvbands(sb,nb,nbi)

      IF ( sb .GT. nbi ) call mabend(
     +  '??E - SB is greater than the total number of bands')
                 
      IF ( sb + nb - 1 .GT. nbi)  THEN
         call xvmessage('??W - Number of bands truncated', ' ')
         nb = nbi + 1 - sb
      ENDIF

C     ....Determine input image data format:  user specification can 
C     ....override label, but check that NS is correct
      call xvget( iunit, istat, 'FORMAT', FMT, ' ') 
      istat = xvpixsizeu( ibp, fmt, iunit)  !PIXEL SIZE
      call xvparm('FORMAT',fmt,icnt,idef,0)
      IF (FMT.EQ.'BYTE') THEN
	IFORM=1
	IBPU = 1
      ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	IFORM=2
	IBPU = 2
      ELSEIF (FMT.EQ.'FULL') THEN
	IFORM=4
	IBPU = 4
      ELSEIF (FMT.EQ.'REAL') THEN
	IFORM=7
	IBPU = 4
      ELSEIF (FMT.EQ.'DOUB') THEN
	IFORM=8
	IBPU = 8
      ELSE
	call xvmessage('??E - FORMAT '//FMT//' NOT SUPPORTED **',' ')
	call abend
      ENDIF
      IF (NS*IBPU .GT. NSI*IBP) THEN
	call xvmessage(
     1  '??E -  LINE LENGTH EXCEEDED, SAMPLES TRUNCATED ***',' ')
	NS = (NSI*IBP)/IBPU
      ENDIF

      call xvparm('LINC',linc,icnt,idef,0)
      call xvparm('SINC',sinc,icnt,idef,0)
      call xvparm('INC',inc,icnt,idef,0)
      IF (ICNT .GT. 0) THEN
         LINC = INC
         SINC = INC
      ENDIF
C     ....Determine plot mode
      MODE = 0				!NORMAL
      IF (xvptst('NOHIST')) MODE=-1
      IF (xvptst('WIDE'))   MODE=1
      IF (xvptst('SCREEN')) MODE=2
C###      IF (xvptst('SPLOT'))  MODE=3		!removed 6-22-2016
C###      QPPLOT = (XVPTST('PPLOT'))
C###      IF (QPPLOT) THEN	
C###	CALL XVPARM('MODE',FMT,ICNT,IDEF)
C###	IF (IDEF.EQ.1) MODE=4
C###      ENDIF

      IF (MODE.EQ.1) nocum=xvptst('NOCUM')
      IF (MODE.EQ.2 .AND. IFORM.NE.1) call mabend(
     +	'??E - Screen oriented histogram not supported for non-byte data')
c###      IF (MODE.EQ.3 .AND. BATCH().EQ.1) MODE=-1		!No SPLOT's in batch - Removed 6-22-2016
C
      call xvparm('SPIKES',ispike,icnt,idef,0)
      call xvparm('NLINES',ibins,icnt,idef,0)

      call xvparm('LIMITS',bounds,icnt,idef,0)
      IF (ICNT .GT. 0) THEN
         ILIM = 1
         BOUNDL = dble(BOUNDS(1))
         BOUNDU = dble(BOUNDS(2))
         BINWID = (BOUNDU-BOUNDL)/dble((IBINS-1))
         IF (IFORM.LT.7 .AND. BINWID.LT.1.0d0) BINWID=1.0d0
         ITEMP = int(BOUNDU-BOUNDL+1)  ! NUMBER OF VALUES IN THE LIMITS.
         IF (FMT .EQ. 'BYTE' .AND. IBINS .GT. ITEMP) IBINS = ITEMP
      ELSE
         ILIM = 0
         IF (FMT .EQ. 'BYTE') THEN
            BOUNDL = 0.0d0     ! DEFAULTS FOR BYTE.
            BOUNDU = 255.0d0
            IBINS = MIN(256, IBINS)
            IF (IBINS .EQ. 256)  THEN
               BINWID = 1.0d0
            ELSE
               BINWID = (BOUNDU-BOUNDL)/dble(IBINS-1)
            END IF
         END IF
      END IF
         
      IBINS = IBINS + 2  !HIST(1), HIST(IBINS) ARE FOR OUT OF LIMITS PIXELS.

      qexclude = xvptst('EXCLUDE')		!Exclude zeroes?
      qpause = xvptst('PAUSE').AND.BATCH().NE.1 !Interactive pauses? 		[SHOULD BE REMOVED]
C reserve up to 8-BYTES for DOUB format
      M = 8*MAX(NSI,IBINS)		!M is bufsiz in BYTES
      N = 8*MAX(65538,IBINS)		!N is hstsiz in BYTES
	print *,"stacka_big"
      call stacka_big(4,work,2,m,n)   ! ALLOCATE 2 BUFFERS & CALL WORK    CHANGED 11-26-2012
	print *, "after"
      RETURN
      END
C HIST main driver
C
c=========================================================================
      subroutine work(buf,bufsiz,hist,hstsiz)
        implicit none

      include 'pgminc'            ! FOR XQINI...

	integer*8 HSTSIZ,BUFSIZ
        integer*8 HIST(4*HSTSIZ)
	byte BUF(BUFSIZ)
	integer*8 IHIST(65536)
C	real*4 rhist(65536)

	integer*4 IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI
      COMMON/C1/IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI

        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
        integer*4 IBINS,ILIM
        real*8 BOUNDL,BOUNDU,BINWID
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,ILIM,BOUNDL,BOUNDU,BINWID

        integer*4 MODE,ISPIKE
        LOGICAL*4 NOCUM,QEXCLUDE,QPPLOT
      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT

        logical*4 QPAUSE
      COMMON/PAUSE/QPAUSE
	
	integer*4 i,idef,icnt,nareas,lines
        integer*8 npixels,nzeroes
	integer*8 inl8,linc8,ins8,sinc8,inb8
	integer*4 istat
      integer*4 IAREA(600),MM(2),PARB(xprdim)
      real*8 sum,sum2,rmean,sdev
      real*4 RMM(2)

      EQUIVALENCE (MM,RMM)
C     ....Get area specifications
	print *, "work"
      call xvparm('AREA',iarea,icnt,idef,0)
      IF (icnt .eq. 0) THEN
         IAREA(1) = 1
         IAREA(2) = 1
         IAREA(3) = NL
         IAREA(4) = NS
         icnt     = 4
      ENDIF
      if ( mod(icnt,4) .ne. 0)  
     .   call mabend('??E - Invalid count for AREA parameter')

      NAREAS = ICNT/4		!Number of areas to be plotted
      LINES = 0

      ISB = SB  !starting band
      INB = NB  !number of bands
      DO 100 I=1,NAREAS			!Loop through each area
         ISL = IAREA(1+(I-1)*4) + SL - 1 !Starting line
         ISS = IAREA(2+(I-1)*4) + SS - 1 !Starting sample
         INL = IAREA(3+(I-1)*4) !Number of lines
         INS = IAREA(4+(I-1)*4) !Number of samples
         IF (ISL+INL-1.GT.NLI) THEN
            call xvmessage(
     * '??E - Specified area crosses image boundary, lines truncated',' ')
         INL = NLI - ISL + 1
         LINES = LINES + 1
      ENDIF
      IF (ISS+INS-1.GT.NSI) THEN
         call xvmessage(
     * '??E- Specified area crosses image boundary, samples truncated',' ')
         INS = NSI - ISS + 1
         LINES = LINES + 1
      ENDIF
C     ....Compute the compressed histogram (HIST).  If the DN limits are
C     ....not user-specified (ILIM=0), determine BOUNDL,BOUNDU,BINWID.
      IF (IFORM.EQ.1) THEN
         call tab1(iunit,buf,ihist,hist,mm,sum,sum2,nzeroes)
      ELSE IF (IFORM.EQ.2) THEN
         call tab2(iunit,ilim,buf,ihist,hist,mm,sum,sum2,nzeroes,
     &		ibins,boundl,boundu,binwid)
      ELSE IF (IFORM.EQ.4) THEN
        call tab4(iunit,ilim,ibins,buf,hist,mm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ELSE IF (IFORM.EQ.7) THEN
        call tab7(iunit,ilim,ibins,buf,hist,rmm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ELSE
        call tab8(iunit,ilim,ibins,buf,hist,rmm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ENDIF
C     ....Compute mean and standard deviation of area
	inl8=inl
	linc8=linc
	ins8=ins
	sinc8=sinc
	inb8=inb 
      NPIXELS = (1+(INL8-1)/LINC8)*(1+(INS8-1)/SINC8)*(INB8)
      IF (QEXCLUDE) NPIXELS=NPIXELS-NZEROES
      if (npixels.ne.0) then
         rmean = sum/dble(npixels)
         sdev = dble(npixels)*sum2-sum*sum
         if (sdev.gt.0.0d0) sdev=dsqrt(sdev)/dble(npixels)
         if (sdev.le.0.0d0) sdev= 0.0d0
      else
         rmean = 0.0d0
         sdev = 0.0d0
      endif
C     ....Print the histogram
      call xvmessage(' ',' ')
      IF (MODE.LE.1) THEN				! normal, wide,

         call phist(hist,npixels,mm,rmm,lines,rmean,sdev) ! or nohist
         call xvmessage(' ',' ')
      ELSE IF (MODE.EQ.2) THEN				! screen
         call shist(ihist,mm,npixels,ispike,rmean,sdev,
     &			boundl,boundu)			!remove binwid - 7/2/2012
      ENDIF
c### remove?
C###      IF (MODE.EQ.3 .OR. QPPLOT) THEN			! splot and/or
C###         IF (IFORM.LT.7) THEN				! pplot
C###         RMM(1) = MM(1)
C###            RMM(2) = MM(2)
C###         ENDIF
C###	 do j=1,ibins
C###	    rhist(j)=real(ihist(j))	 
C###	 enddo
C###         call plotxy(hist,rhist,buf,rmean,sdev,npixels,rmm)
C###      ENDIF
c---
  100 CONTINUE
C     ....Output MEAN and SIGMA parameters
	call xqini(parb, xprdim, xabort)
	call xqreal(parb,'MEAN',1,sngl(rmean),xadd,istat)      
	call xqreal(parb,'SIGMA',1,sngl(sdev),xadd,istat)  
	call xqintg(parb,'COUNT',1,npixels,xadd,istat)
	call xqreal(parb,'SUM',1,sngl(sum),xadd,istat)	    
	call xqout(parb,istat)
	return
	end
C Compute histogram of input image (IHIST) and compress it (HIST).
C
c==============================================================================
      subroutine tab1(iunit,buf,ihist,hist,mm,sum,sum2,nzeroes)
c
c   for histograms of BYTE images
c
	implicit none
	byte BUF(*)
	integer*8 IHIST(0:255)
        integer*4 MM(2)
	integer*8 HIST(258)

        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
        integer*4 IBINS,dummy
        real*8 BOUNDL,BOUNDU,BINWID
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,dummy,BOUNDL,BOUNDU,BINWID

	integer*4 iunit,band,iel,nx,line,istat
	integer*4 k,maxdn,mindn,idn
        integer*8 ifreq,nzeroes
	real*8 sum,sum2
	real*8 RDN
C
      NZEROES = 0
c      call zia(ihist,256)
	do idn=0,255
           ihist(idn)=0
        enddo
      IEL = ISL + INL - 1	!ending line
      
      NX = INS
      IF (SINC.GT.1) NX = 1+(INS-1)/SINC
C     ....Compute 256 grey-level histogram
      DO BAND= ISB, ISB+INB-1
       DO LINE=ISL,IEL,LINC
	   call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
	   IF (SINC.GT.1) call mve(1,nx,buf(iss),buf(iss),sinc,1)
	      call h8sub(1,nx,buf(iss),ihist)
        ENDDO
      ENDDO

      MINDN = 255
      MAXDN = 0
      sum = 0.0d0
      sum2 = 0.0d0
c      call zia(hist,ibins)
	do idn=1,ibins
           hist(idn) = 0
	enddo

C     ....Compress the histogram into IBINS grey-levels
      DO IDN=0,255
         RDN = IDN
         IFREQ = IHIST(IDN)
         SUM = SUM + IFREQ*RDN
         SUM2 = SUM2 + IFREQ*RDN**2
         IF (IFREQ.GT.0) THEN
            MAXDN = IDN			!Maximum DN value
            MINDN = MIN0(MINDN,IDN)	!Minimum DN value
         ENDIF
	 if (binwid.eq.0) then
	    k = 1
	 else
            K = MAX((IDN-BOUNDL)/BINWID+2.5,1.5)
	 endif
         K = MIN(K,IBINS)
         HIST(K) = HIST(K) + IFREQ
      ENDDO

      MM(1) = MINDN
      MM(2) = MAXDN
      NZEROES = IHIST(0)
      RETURN
      END
C Compute histogram for halfword data.
C
c====================================================================
      subroutine tab2(iunit,ilim,buf,ihist,hist,mm,sum,sum2,nzeroes,
     &		ibins,boundl,boundu,binwid)
c
	implicit none
      integer*2 BUF(*)
      integer*8 IHIST(-32768:32767)
      integer*4 MM(2)
	integer*8 HIST(*)
        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC


      integer*4 iunit,band,iel,ies,line,istat
        integer*4 i,k,maxdn,mindn,idn,ifreq,ilim
	integer*4 ibins,ibinwid
	integer*8 nzeroes
	real*4 dnmin,realdn,rbinwid
	real*8 boundl,boundu,binwid
      real*8 sum,sum2
      real*8 RDN

c      call zia(ihist,65536)
	do i=-32768,32767
	  ihist(i) = 0
        enddo
      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
C     ....Compute 64K grey-level histogram
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
	    call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF

         DO I=ISS,IES,SINC
            IDN = BUF(I)      ! FIND MIN AND MAX
            IF (IDN.LT.MINDN) THEN
               MINDN = IDN
            ELSE IF (IDN.GT.MAXDN) THEN
               MAXDN = IDN
            ENDIF
            IHIST(IDN) = IHIST(IDN) + 1
         ENDDO
       ENDDO
      ENDDO
C     ....Determine optimum compression parameters (if not user-specified)
      IF (ILIM.EQ.0) THEN
         call pinc(ihist,ibins,mindn,maxdn,ibinwid)
         BOUNDL = dble(MINDN)
         BOUNDU = dble(MAXDN)
         IF (MINDN.EQ.MAXDN) BOUNDU=BOUNDU+1.
         BINWID = IBINWID
         IBINS = (MAXDN-MINDN+IBINWID-1)/IBINWID + 3 
      ENDIF  ! ALLOW SPACE, SINCE HIST(1), HIST(IBINS) ARE FOR OUT OF LIMITS
             ! PIXELS

c      call zia(hist,ibins)
      do idn=1,ibins
	 hist(idn) = 0
      enddo
      NZEROES = IHIST(0)
      SUM = 0.0d0
      SUM2 = 0.0d0
C     ....Compress the histogram
      DO IDN=-32768,32767
         RDN = dble(IDN)
         IFREQ = IHIST(IDN)
         SUM = SUM + IFREQ*RDN
         SUM2 = SUM2 + IFREQ*RDN**2
c         K = MAX1((IDN-BOUNDL)/BINWID+2.5,1.5)
	dnmin = sngl(BOUNDL)
	realdn = sngl(RDN)
	rbinwid = sngl(BINWID)
	K = MAX1((realdn-dnmin)/rbinwid+2.5,1.5)
         K = MIN(K,IBINS)
         HIST(K) = HIST(K) + IFREQ
      ENDDO

      MM(1) = MINDN
      MM(2) = MAXDN
      RETURN
      END
C Compute histogram for FULL data.
C
c==================================================================
      subroutine tab4(iunit,ilim,ibins,buf,hist,mm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
c
	implicit none
      integer*4 BUF(*)
      integer*4   MM(2)
	integer*8 HIST(*)
        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC

      integer*4 iunit,band,iel,ies,line,istat
        integer*4 i,k,maxdn,mindn,idn,ilim
        integer*4 ibins
	integer*8 nzeroes
        real*8 boundl,boundu,binwid
      real*8 sum,sum2

C==================================================================

        mindn = 0
        maxdn = 0

      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits were user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            IDN = BUF(I)      ! FIND MIN AND MAX
            IF (IDN.LT.MINDN) THEN
               MINDN = IDN
            ELSE IF (IDN.GT.MAXDN) THEN
               MAXDN = IDN
            ENDIF
         ENDDO
       ENDDO
      ENDDO

      BOUNDL = MINDN
      BOUNDU = MAXDN
      BINWID = (BOUNDU-BOUNDL)/(IBINS-3)
      IF(MINDN .EQ. MAXDN) BINWID = 1.0 ! AVOID DIVISION BY ZERO.

   50 continue
c	call zia(hist,ibins)
       do i=1,ibins
	 hist(i)=0
       enddo
	
      SUM = 0.0d0
      SUM2 = 0.0d0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            IDN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (IDN.LT.MINDN) THEN
                  MINDN = IDN
               ELSE IF (IDN.GT.MAXDN) THEN
                  MAXDN = IDN
               ENDIF
            ENDIF
            IF (IDN.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + DBLE(IDN)
            SUM2 = SUM2 + DBLE(IDN)**2
            K = MAX((IDN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      ENDDO
      MM(1) = MINDN
      MM(2) = MAXDN
      RETURN
      END
C Compute histogram for REAL*4 data
C
c====================================================================
      subroutine tab7(iunit,ilim,ibins,buf,hist,rmm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
c
	implicit none
      real*4 RMM(2),BUF(*)
      integer*8 HIST(*)

        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC

      integer*4 iunit,band,iel,ies,line,istat
        integer*4 i,k,ilim
        integer*4 ibins
        integer*8 nzeroes
	real*4 dn,dnmax,dnmin
        real*8 boundl,boundu,binwid
      real*8 sum,sum2

        dnmin = 0.0
        dnmax = 0.0

      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits are user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (DN.LT.DNMIN) THEN
               DNMIN = DN
            ELSE IF (DN.GT.DNMAX) THEN
               DNMAX = DN
            ENDIF
         ENDDO
       ENDDO
      ENDDO

      BOUNDL = DNMIN
      BOUNDU = DNMAX
      BINWID = (BOUNDU-BOUNDL)/(IBINS-3)

   50 continue
c	call zia(hist,ibins)
	do i=1,ibins
	    hist(i)=0
        enddo
      SUM = 0.0d0
      SUM2 = 0.0d0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (DN.LT.DNMIN) THEN
                  DNMIN = DN
               ELSE IF (DN.GT.DNMAX) THEN
                  DNMAX = DN
               ENDIF
            ENDIF
            IF (DN.EQ.0.0e0) NZEROES=NZEROES+1
            SUM = SUM + DN
            SUM2 = SUM2 + DN*DN    ! IF THIS OVERFLOWS OR UNDERFLOWS
                     ! IT CAN BE REPLACED WITH METHOD IN MATH77 SSTAT1
            K = MAX((DN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      ENDDO
      RMM(1) = real(DNMIN)
      RMM(2) = real(DNMAX)
      RETURN
      END
C Compute histogram for REAL*8 data
C
c=====================================================================
      subroutine tab8(iunit,ilim,ibins,buf,hist,rmm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
c
	implicit none
      real*4 RMM(2)
      real*8 BUF(*)
      integer*8 HIST(*)

        integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC

      integer*4 iunit,band,iel,ies,line,istat
        integer*4 i,k,ilim
        integer*4 ibins
	integer*8 nzeroes
        real*8 boundl,boundu,binwid
	real*8 dn,dnmin,dnmax
      real*8 sum,sum2

	dnmin = 0.0d0
	dnmax = 0.0d0
      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits are user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (DN.LT.DNMIN) THEN
               DNMIN = DN
            ELSE IF (DN.GT.DNMAX) THEN
               DNMAX = DN
            ENDIF
         ENDDO
       ENDDO
      enddo

      BOUNDL = DNMIN
      BOUNDU = DNMAX
      BINWID = (BOUNDU-BOUNDL)/dble(IBINS-3)

   50 continue
c	call zia(hist,ibins)
	do i=1,ibins 
           hist(i) = 0
        enddo
      sum = 0.0d0
      sum2 = 0.0d0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         call xvread(iunit,buf,istat,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (DN.LT.DNMIN) THEN
                  DNMIN = DN
               ELSE IF (DN.GT.DNMAX) THEN
                  DNMAX = DN
               ENDIF
            ENDIF
            IF (DN.EQ.0.0d0) NZEROES=NZEROES+1
            SUM = SUM + DN
            SUM2 = SUM2 + DN*DN
            K = MAX((DN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      enddo
      RMM(1) = DNMIN
      RMM(2) = DNMAX
      RETURN
      END

C Find a suitable DN increment for printing out a halfword histogram.
C  (FIND MINDN, MAXDN, AND INC)
c=====================================================================
      subroutine pinc(hist,nlin,mindn,maxdn,inc)
c
	implicit none
      integer*8 hist(-32768:32767)

	integer*4 n1,n2,n4,n8,n16,n32,n64,n128,n256
	integer*4 i,i2,i4,i8,i16,i32,i64,i128,i256,i512
	integer*4 m1,m2,m4,m8,m16,m32,m64,m128
	integer*4 nlin,mindn,maxdn,inc

      N1 = 0
      N2 = 0
      N4 = 0
      N8 = 0
      N16 = 0
      N32 = 0
      N64 = 0
      N128 = 0
      N256 = 0
      MINDN = 32767
      MAXDN = -32768
      I = -32769
C
      DO I512=1,65536,256
	M128 = N128
	DO I256=1,2
          M64 = N64
          DO I128=1,2  
            M32 = N32
            DO I64=1,2   
              M16 = N16
              DO I32=1,2  
                M8 = N8
                DO I16=1,2
                  M4 = N4
                  DO I8=1,2 
                    M2 = N2
                    DO I4=1,2  
                      M1 = N1
                      DO I2=1,2 
                        I = I + 1
                        IF (HIST(I).GT.0) THEN
                           MAXDN = I
                           MINDN = MIN0(MINDN,I)
                           N1=N1+1    
                        ENDIF
                      ENDDO                          
                      IF (M1.LT.N1) N2=N2+1
                    ENDDO                            
                    IF (M2.LT.N2) N4=N4+1
                  ENDDO                              
                  IF (M4.LT.N4) N8=N8+1
                ENDDO                                
                IF (M8.LT.N8) N16=N16+1
              ENDDO                                   
              IF (M16.LT.N16) N32=N32+1
            ENDDO     
            IF (M32.LT.N32) N64=N64+1
          ENDDO         
          IF (M64.LT.N64) N128=N128+1
        ENDDO 
        IF (M128.LT.N128) N256=N256+1
      ENDDO

      INC = 1
      IF (N1.LE.NLIN.AND.N1.GT.N2) RETURN
      INC = 2
      IF (N2.LE.NLIN.AND.N2.GT.N4) RETURN
      INC = 4
      IF (N4.LE.NLIN.AND.N4.GT.N8) RETURN
      INC = 8
      IF (N8.LE.NLIN.AND.N8.GT.N16) RETURN
      INC = 16
      IF (N16.LE.NLIN.AND.N16.GT.N32) RETURN
      INC = 32
      IF (N32.LE.NLIN.AND.N32.GT.N64) RETURN
      INC = 64
      IF (N64.LE.NLIN.AND.N64.GT.N128) RETURN
      INC = 128
      IF (N128.LE.NLIN.AND.N128.GT.N256)RETURN
      INC = 256
      IF (N256.GT.NLIN) INC=512
      RETURN
      END
C Output histogram in normal or wide format
C (MODE=0 or 1).
C
c=======================================================================
      subroutine phist(hist,npixels,mm,rmm,lines,rmean,sdev)
c
	implicit none
      integer*8 HIST(*)
	integer*4 MM(2)
      real*4 RMM(2)

	integer*4 IUNIT,IFORM,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9
      COMMON/C1/IUNIT,IFORM,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9

	integer*4 ISB,INB,ISL,ISS,INL,INS,LINC,SINC
	integer*4 IBINS,dummy
	real*8 BOUNDL,BOUNDU,BINWID
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,dummy,BOUNDL,BOUNDU,BINWID

        integer*4 MODE,ISPIKE
        logical*4 NOCUM,QEXCLUDE,QPPLOT
      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT

        integer*8 max,spikes(9),npixels
	integer*4 i,j,k,jsav,imax,nleft,ndec,lines
	integer*4 iuncon
c      integer*4 SPIKES(9)
	real*4 pt,rbinwid,dnmin,dnmax
      real*8 GRAYLEVEL
	real*8 rmean,sdev
      character*80  RUNTIME

      character*132 PBUF
      DATA PBUF/' '/
      character*132 WIDELI
      DATA WIDELI/' '/
      character*132 MSG

      character*132 MSG2
      DATA MSG2/'MIN. DN=              MAX. DN=             '/
      character*6 COL3,PCTILE,CDF
      DATA        PCTILE/'PCTILE'/,CDF/'   CDF'/

	character*132 MSG3

        imax = 0
        jsav = 0
        max = 0

      IF (MODE.LT.0) GOTO 1000
C     ....Find spike locations
c      call zia(spikes,9)
	do j=1,9
           spikes=0
        enddo
      DO J=1,ISPIKE
         JSAV = J
         MAX=0
         DO I=1,IBINS
            IF (HIST(I).GT.MAX) IMAX=I
            IF (HIST(I).GT.MAX) MAX=HIST(I)
         ENDDO
         IF (MAX.EQ.0) GOTO 6
         SPIKES(J) = HIST(IMAX)
         HIST(IMAX) = -J		!Flag spikes as negative
      ENDDO
    6 continue
	IF (MAX.EQ.0 .AND. JSAV.NE.1) MAX=SPIKES(JSAV-1)
	if (binwid .gt. 1.0e8 .or. binwid .lt. 1.0e-2) then
	   write (MSG3,10050) binwid
10050 FORMAT ('Bin Width = ',e20.10)
	else
	   write (MSG3,10060) binwid
10060 FORMAT ('Bin Width = ',f10.1)
	endif
	call xvmessage(msg3,' ')
      GRAYLEVEL = BOUNDL - BINWID	!Compute DN of HIST(1)
C     ....Compute number of decimal places needed to represent the gray level
C     ....value (NDEC). If fixed point representation wont work, NDEC is
C     ....negative.
	dnmin = sngl(boundl)
	dnmax = sngl(boundu)	
      NLEFT = ALOG10(AMAX1(ABS(dnmin),ABS(dnmax))) + 1.0
      IF (IFORM .GE. 7)  THEN   ! floating point data could be out of INT range.
	  rbinwid = sngl(binwid)
	  NDEC = 1.5-ALOG10(rbinwid)

      ELSE IF (GRAYLEVEL.EQ.INT(GRAYLEVEL) .AND. 
     .         BINWID.EQ.INT(BINWID)) THEN
	  NDEC = 0
      ELSE
	  rbinwid = sngl(binwid)
	  NDEC = 1.5-ALOG10(rbinwid)
      ENDIF
      IF (NLEFT .GT. 7 .AND. IFORM .GE. 7) THEN  !FOR REALS, USE EXP. NOTATION
          NDEC=-1                              !IF MORE THAN 7 DIGITS.
      ELSE IF (NDEC.LT.0 .AND. NLEFT.LE.11) THEN
	  NDEC=0
      ELSE IF (NDEC.GT. 10 .OR. NLEFT.GT.10) THEN
	  NDEC=-1
      ELSE IF (NDEC+NLEFT.GT.10) THEN
	  NDEC=-1
      ENDIF
C     ....Create/print format headers
      PT = 0.0
c  do following if WIDE MODE (=1)
      IF (MODE.EQ.1) THEN	!bar-graph line with + marking tens columns
         DO I=30,130,10
            WIDELI(I:I) = '+'
         ENDDO
C			     	!print header lines
         WRITE (MSG,100) ISL,ISS,INL,INS,LINC,SINC
  100 FORMAT('FREQUENCY DISTRIBUTION     SL=',I5,'     SS=',I5,
     1   '     NL=',I5,'     NS=',I5,'     LINC=',I3,
     2   '    SINC=',I3)
         call qprnt(msg(1:100),lines)
         call qprnt(' ',lines)
C
         IF (NOCUM) THEN
            COL3 = PCTILE
         ELSE
            COL3 = CDF
         ENDIF

         WRITE (MSG,200) COL3,10,20,30,40,50,60,70,80,90,100
  200    FORMAT('       GRAY    FREQ  ',A6,'   ',10I10,' ')
         call qprnt(msg,lines)
         call qprnt(wideli,lines)
c  print out each bin - pbuf is the bin line
         DO I=1,IBINS
            IF (HIST(I).EQ.0) THEN   ! ZERO ENTRIES ARE SKIPPED.
               IF (I.NE.1.AND.HIST(I-1).NE.0) call qprnt(' ',lines)
            ELSE
               PBUF = WIDELI
               IF (I.EQ.1) THEN
                  PBUF(1:11) = '< LOW LIMIT'
               ELSE IF (I.EQ.IBINS) THEN
                  PBUF(1:11) = '>HIGH LIMIT'
               ELSE
                  IF ( NDEC .EQ. 0) THEN
                     WRITE (PBUF(1:11),'(I11)') NINT(GRAYLEVEL)
                  ELSE IF (NDEC .LT. 0) THEN
                     if (graylevel .lt. 0.D0) then
                        WRITE (PBUF(1:11),'(1PE11.4)') GRAYLEVEL
                     else
                        WRITE (PBUF(1:11),'(1PE11.5)') GRAYLEVEL
                     endif
                  ELSE IF (NDEC .GT. 0) THEN
                     NDEC = MIN(10,NDEC)
                     WRITE (RUNTIME,130) NDEC
130                     FORMAT( '(F11.', I2.2, ')' )
                     WRITE (PBUF(1:11), RUNTIME) GRAYLEVEL
                  ENDIF
               ENDIF
C              ....if one of the n=spike largest, label on graph
               IF (HIST(I) .LT. 0) THEN
                  J = -HIST(I)
                  HIST(I) = SPIKES(J)
                  WRITE (PBUF(131:131),'(I1)') J
               ENDIF
C	       ....update number of pixels seen - increase to I9 3/07/2010
               WRITE (PBUF(12:20),'(I9)') HIST(I)
               IF (NOCUM) THEN
                  WRITE (PBUF(20:27),
     +             '(F8.3)') 100.0*HIST(I)/FLOAT(NPIXELS)
               ELSE
                  PT = PT+HIST(I)
                  WRITE (PBUF(20:27),'(F8.3)') 100.0*PT/FLOAT(NPIXELS)
               ENDIF
C		       draw bar of chart
               J = (MIN(HIST(I),MAX)*100)/MAX
               IF (J.NE.0) THEN
                  DO K=1,J
                     PBUF(30+K-1:30+K-1) = '*'
                  END DO
               END IF
               call qprnt(pbuf,lines)
            ENDIF
            GRAYLEVEL = GRAYLEVEL + BINWID
         ENDDO
      ELSE		!IF (MODE.NE.1) THEN
         DO I=1,IBINS
            IF (HIST(I).NE.0) THEN
               PBUF(1:76) = ' '
               IF (I.EQ.1) THEN
                  PBUF(1:11) = '< LOW LIMIT'
               ELSE IF (I.EQ.IBINS) THEN
                  PBUF(1:11) = '>HIGH LIMIT'
               ELSE
                  IF ( NDEC .EQ. 0) THEN
                     WRITE (PBUF(1:11),'(I11)') NINT(GRAYLEVEL)
                  ELSE IF (NDEC .LT. 0) THEN
                     if (graylevel .lt. 0.D0) then
                        WRITE (PBUF(1:11),'(1PE11.4)') GRAYLEVEL
                     else
                        WRITE (PBUF(1:11),'(1PE11.5)') GRAYLEVEL
                     endif
                  ELSE IF (NDEC .GT. 0) THEN
                     NDEC = MIN(10,NDEC)
                     WRITE (RUNTIME,130) NDEC
                     WRITE (PBUF(1:11), RUNTIME) GRAYLEVEL
                  ENDIF
               ENDIF
C              ....    if one of the n=spike largest, label on graph
               IF (HIST(I) .LT. 0) THEN
                  J = -HIST(I)
                  HIST(I) = SPIKES(J)
                  WRITE (PBUF(76:76),'(I1)') J
               ENDIF
C	       ....update number of pixels seen - I9 on 3/07/2010
               WRITE (PBUF(12:22),'(I11)') HIST(I)
               IF (I.GT.2.AND.HIST(I-1).EQ.0) PBUF(12:12) = '*'		!WHY?
C	       ....draw bar of chart
               J = (MIN(HIST(I),MAX)*50)/MAX
               IF (J.NE.0) THEN
                   DO IUNCON = 24,24+J-1
                       PBUF(IUNCON:IUNCON) = '*'
                   ENDDO
               ENDIF
               call qprnt(pbuf(1:76),lines)
            ENDIF
            GRAYLEVEL = GRAYLEVEL + BINWID
         ENDDO
      ENDIF
C     ....print statistics for graph data and return
c	expanded to I10 - Jun 24, 2011
1000  MSG(1:44) = 'AVERAGE GRAY LEVEL=XXXXXXXX       STANDARD D'
      MSG(45:89) = 'EVIATION=XXXXXXXX       NUMBER ELEMENTS=XXXXX'
      MSG(90:95) = 'XXXXX '
      call realcon( sngl(rmean), msg(20:), 8)
      call realcon( sngl(sdev), msg(54:), 8)
      WRITE (MSG(85:94),'(I10)') NPIXELS !Number of pixels used
      call qprnt(' ',lines)
      IF (QEXCLUDE) call xvmessage('NOTE - EXCLUDING PIXELS OF DN=0',
     & ' ')
      IF (MODE.EQ.0) THEN
         call qprnt(msg(1:34),lines)
         call qprnt(msg(35:68),lines)
         call qprnt(msg(69:94),lines)
      ELSE
         call qprnt(msg(1:94),lines)
      ENDIF
C     ....Report minimum and maximum DN
      IF (IFORM.LT.7) THEN	
         WRITE (MSG2(9:18),'(I10)') MM(1)
         WRITE (MSG2(31:40),'(I10)') MM(2)
      ELSE
         call realcon( rmm(1), msg2(9:),8)
         call realcon( rmm(2), msg2(31:),8)
      ENDIF
      IF (MODE.EQ.1) THEN
         call qprnt(msg2(1:42),lines)
      ELSE
         call qprnt(msg2(1:20),lines)
         call qprnt(msg2(23:42),lines)
      ENDIF
      RETURN
      END
C Plot histogram onto 23x80 window screen with horizontal
C DN-axis.
C
c=======================================================================
      subroutine shist(ihist,mm,npixels,ispike,rmean,sdev,
     &		boundl,boundu)
c
	implicit none
      integer*8 IHIST(256)
      integer*4 MM(2)
	integer*4 ispike,low,ihi,ibinwidth,jsav,nbins
	integer*4 i,j,k,n,imax,num
	real*4 divisor
      real*8 rmean,sdev
	real*8 boundl,boundu

      integer*8 SPIKES(9),HIST(80),max,npixels
      character*132 MSG
      character*80 screen(22)  ! THE TOP 22 LINES OF SCREEN
C==================================================================

	imax = 0
	jsav = 0
	max = 0
      IF (BOUNDL.EQ.0.0 .AND. BOUNDU.EQ.255.0) THEN
         LOW = MM(1)
         IHI = MM(2)
      ELSE
         LOW = BOUNDL
         IHI = BOUNDU
      ENDIF
      IBINWIDTH = 1 + (IHI-LOW)/80
      NBINS = 1 + (IHI-LOW)/IBINWIDTH
c      call zia(hist,80)
	do i=1,80
	   hist(i)=0
        enddo
      N = LOW
C     ....Compress the histogram to 80 grey-levels
      DO I=1,NBINS
         DO J=1,IBINWIDTH
            N = N+1
            HIST(I) = HIST(I)+IHIST(N)
         ENDDO
      ENDDO
C     ....Find spike locations
c      call zia(spikes,9)
	do j=1,9
	  spikes(j)=0
        enddo
      DO J=1,ISPIKE
         JSAV = J
         MAX = 0
         DO I=1,NBINS
            IF (HIST(I) .GT. MAX) THEN
               IMAX = I
               MAX = HIST(I)
            ENDIF
         ENDDO
         IF(MAX.EQ.0) GOTO 6
         SPIKES(J) = HIST(IMAX)
         HIST(IMAX) = -J
        ENDDO
    6 IF (MAX.EQ.0 .AND. JSAV.NE.1) MAX=SPIKES(JSAV-1)
      DIVISOR = MAX/19.0
      NUM = LOW+IBINWIDTH/2
      DO I = 1,22
         SCREEN(I) = ' '  ! BLANK EACH LINE.  FORTRAN PADS WITH BLANKS.
      END DO
C
C     ....Plot the histogram on the screen
      DO I=1,NBINS
         IF (MOD(I,5).EQ.1) THEN
            WRITE (MSG(1:3),'(I3)') NUM
            DO J=1,3      ! LABEL AXIS EVERY FIFTH BIN IN LINES 20-22.
               SCREEN(J+19)(I:I) = MSG(J:J)
            ENDDO
            NUM = NUM+5*IBINWIDTH
         ENDIF
         IF (HIST(I).NE.0) THEN
            IF (HIST(I) .LT. 0) THEN		!Flag spike
               WRITE (SCREEN(1)(I:I), '(I1)') -HIST(I)
               N = 2
            ELSE
               N = 20.5-HIST(I)/DIVISOR
            ENDIF
            DO K=N,19
               SCREEN(K)(I:I) = '*'
            ENDDO
         ENDIF
      ENDDO
C     ....Print the screen
      DO I=1,22
         call xvmessage(screen(i),' ')
      ENDDO
C     ....Print statistics for graph data and return mean
      WRITE (MSG,500) NPIXELS,MM(1),MM(2),rmean,sdev
  500 FORMAT(I10,'PIXELS   RANGE',I4,'-',I3,'     MEAN',F8.3,
     1 '     STD DEV',F8.3)
      call xvmessage(msg(1:70),' ')
      RETURN
      END
C Remove all blanks from array BUF, and replace '_' with blank.
C A zero byte is placed at the new end of the string, and the length,
C N, is updated.
C
c===================================================================
	subroutine squeeze(buf,n)
c
	implicit none

	CHARACTER*(*) BUF
	integer*4 i,j,n
C
	I = 0
	DO J=1,N
           IF (BUF(J:J).NE.' ') THEN
              I=I+1
              IF (BUF(J:J).NE.'_') THEN
                 BUF(I:I) = BUF(J:J)
              ELSE
                 BUF(I:I) = ' '
              ENDIF
           ENDIF
	ENDDO

	BUF(I+1:I+1) = CHAR(0)
	N = I
	RETURN
	END
C Print message, increase line count, and check for full screen
C
c==================================================================
      subroutine qprnt(buf,lines)
c
	implicit none
      COMMON/PAUSE/QPAUSE
      logical*4 QPAUSE
      character*(*) BUF

	integer*4 lines
C
      call xvmessage(buf,' ')
      LINES = LINES + 1
      IF (LINES.GE.23 .AND. QPAUSE) THEN
	  call xvintract('IPARAM','PRESS RETURN')
	  LINES = 0
      ENDIF
      RETURN
      END
C*********************************************************************
      SUBROUTINE H8SUB(DCODE ,NS ,BUF ,HIST)
C
C  2012-11-26 R.J. Bambery - extracted from hsub.f from p2 sub lib 
C		     Made HIST integer*8 to work with large earth
C		     satellite images such as WorldView 2 which
C		     can have billions of pixels
C
C  92-9-30 ...SP.... Changed to use fortport.fin as improved portability
C                    method.  Went back from MACRO to FORTRAN CMS generation 1.
C                    Changed ILOW and IHIGH to required parameters.
C                    (Defaults were 0 & 255 for byte, 0 & 32767 for half.)
C
C  83-6-1  ...LWK... support byte data, make ILOW/HIGH optional
C  83-5-2  ...LWK... found in LIBSOR: (UCL routine?)
C
C     H8SUB PRODUCES CUMULATIVE HISTOGRAM OF INPUT ARRAY( BUF)
C
C     DCODE      :- DATA FORMAT FOR INPUT ARRAY BUF
C        (1=byte, 2=halfwd, 3=full)
C     NS         :- NUMBER OF SAMPLES IN ARRAY  BUF
C     BUF        :- INPUT ARRAY CONTAINING INPUT SAMPLES
C     HIST       :- ARRAY HOLDING SAMPLE FREQUENCIES
C
      INTEGER*8   HIST(*) 
      INTEGER*4 DCODE,NS
      BYTE BUF(*) 
C==================================================================
      IF (DCODE.EQ.1) CALL H8SUBB(NS,BUF,HIST)
      RETURN
      END
C**********************************************************
      SUBROUTINE H8SUBB(NS,BUF,HIST)

      IMPLICIT INTEGER (A-Z)
      include  'fortport'  ! defines BYTE2INT.

      INTEGER*8 HIST(*)
      BYTE BUF(*)
C==================================================================

      DO 10 I = 1,NS
	K = BYTE2INT(BUF(I))+1
	HIST(K) = HIST(K) + 1
   10 CONTINUE
C
      RETURN
      END
