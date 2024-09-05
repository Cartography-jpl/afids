$!****************************************************************************
$!
$! Build proc for MIPL module hist
$! VPACK Version 1.9, Tuesday, August 16, 2016, 17:31:34
$!
$! Execute by entering:		$ @hist
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
$ write sys$output "*** module hist ***"
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
$ write sys$output "Invalid argument given to hist.com file -- ", primary
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
$   if F$SEARCH("hist.imake") .nes. ""
$   then
$      vimake hist
$      purge hist.bld
$   else
$      if F$SEARCH("hist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hist.bld "STD"
$   else
$      @hist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hist.com -mixed -
	-s hist.f -
	-i hist.imake -
	-p hist.pdf -
	-t tsthist.pdf tsthist.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
      call xvmessage( '*** HIST Jul 02, 2016 (64-bit) -  RJB ***',' ')
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
	do idn=1,256
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
1000  continue

c	MSG(1:44) = 'AVERAGE GRAY LEVEL=XXXXXXXX       STANDARD D'
c	MSG(45:89) = 'EVIATION=XXXXXXXX       NUMBER ELEMENTS=XXXXX'
c	MSG(90:95) = 'XXXXX '
c	call realcon( sngl(rmean), msg(20:), 8)
c	call realcon( sngl(sdev), msg(54:), 8)
c	print *,"npixels = ",npixels
	
c      WRITE (MSG(85:94),'(I10)') npixels !number of pixels used
c	call xvmessage (msg,' ')
	write (msg,10200) rmean
10200 format ('AVERAGE GRAY LEVEL=',e10.2)
	call xvmessage(msg,' ')
	write (msg,10205) sdev
10205 format ('STANDARD DEVIATION=',e10.3)
	call xvmessage(msg,' ')
	write (msg,10210) npixels
10210 format ('NUMBER ELEMENTS=',i10)
	call xvmessage(msg,' ')

c      call qprnt(' ',lines)
c     ...report excluding zero, if requested
      IF (QEXCLUDE) call xvmessage('NOTE - EXCLUDING PIXELS OF DN=0',' ')
c      IF (MODE.EQ.0) THEN			!NORMAL
c         call qprnt(msg(1:34),lines)
c         call qprnt(msg(35:68),lines)
c         call qprnt(msg(69:93),lines)
c      ELSE
c         call qprnt(msg(1:93),lines)
c      ENDIF
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hist.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM hist

   To Create the build file give the command:

		$ vimake hist			(VMS)
   or
		% vimake hist			(Unix)


************************************************************************/


#define PROGRAM	hist
#define R2LIB

#define MODULE_LIST hist.f

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
$ create hist.pdf
process help=*
!###SUBCMD-DEFAULT NORMAL           - Removed 6-22-2016
LOCAL LMEAN   TYPE=REAL INITIAL=0.0
LOCAL LSIGMA  TYPE=REAL INITIAL=0.0
LOCAL SUMX    TYPE=REAL INITIAL=0.0
LOCAL ICNT    TYPE=INT  INITIAL=0
PARM INP      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=4 VALID=(0:999999)	DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SB       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM NB       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM INC      TYPE=INTEGER COUNT=0:1 VALID=(1:999999)	DEFAULT=--
PARM LINC     TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SINC     TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM FORMAT   TYPE=KEYWORD COUNT=0:1 VALID=(FULL,BYTE,REAL,HALF,DOUB) DEFAULT=--
PARM SPIKES   TYPE=INTEGER COUNT=1 VALID=(1:9)	DEFAULT=2
PARM NLINES   TYPE=INTEGER COUNT=1 VALID=(8:20000)	DEFAULT=256
PARM LIMITS   TYPE=REAL    COUNT=(0,2)			DEFAULT=--
PARM EXCLUDE  TYPE=KEYWORD COUNT=(0:1) VALID=EXCLUDE	DEFAULT=--
PARM MODE  TYPE=KEYWORD VALID=(WIDE,NORMAL,SCREEN,NOHIST) DEFAULT=NORMAL
!###PARM PPLOT KEYWORD COUNT=0:1 VALID=PPLOT		DEFAULT=--
!###PARM TITLE TYPE=STRING DEFAULT=""
PARM NOCUM    TYPE=KEYWORD COUNT=0:1 VALID=NOCUM 	DEFAULT=--
PARM AREA     TYPE=INTEGER COUNT=0:100 VALID=(1:999999)	DEFAULT=--
PARM PAUSE    TYPE=KEYWORD VALID=(PAUSE,NOPAUSE)	DEFAULT=NOPAUSE
PARM MEAN     TYPE=NAME					DEFAULT=LMEAN
PARM SIGMA    TYPE=NAME					DEFAULT=LSIGMA
PARM COUNT    TYPE=NAME                 DEFAULT=ICNT
PARM SUM      TYPE=name                 default=sumx
!END-SUBCMD
!SUBCMD IPARAM
!PARM HPPLOT   TYPE=KEYWORD COUNT=(0:1) VALID=YES	DEFAULT=--
!PARM NEWPAGE  TYPE=STRING				DEFAULT="NEWPAGE"
!END-SUBCMD
END-PROC
.TITLE
VICAR Program HIST
.HELP
PURPOSE:
HIST prints DN-frequency histograms of one or more areas of an image.

EXECUTION:
	HIST  INP=PIC  user-parameters...
where
    PIC is a VICAR image of arbitrary size.  PIC may be in byte, halfword
    (16-bit integer), fullword (32-bit integer), floating-point (REAL*4),
    double precision (REAL*8) data format.  See FORMAT keyword.


.page
OPERATION:

The format of the plotted histogram is specified via the MODE keyword.  Valid
values are NORMAL, WIDE, SCREEN, and NOHIST.  The DN axis may be plotted
vertically using 80 columns (NORMAL) or 132 columns (WIDE); or plotted
horizontally and formatted to fit on a 23x80 terminal screen or window (SCREEN).


The FORMAT keyword may be used to specify the data format of the input image.
If defaulted, the data format is determined from the input picture label.
Valid values are BYTE, HALF, FULL, REAL, and DOUB.

The DN-range displayed in the histogram may be specified using the LIMITS
parameter.  If defaulted, these limits are set to the minimum and maximum
DN values in the specified image area.

The number of lines (DN-levels) used to plot the output histogram is specified
by the NLINES parameter (default=256).

The 'PAUSE keyword introduces pauses every 23 lines to prevent the histogram
from scrolling off the terminal screen.  [THIS SHOULD BE REMOVED]

The computed mean and standard deviation, the number of pixels and the sum
of the values of all pixels in the input image may be output as
parameters which may be subsequently passed on to another program:
	LOCAL SD  TYPE=REAL		!Declare SD and AVG
	LOCAL AVG TYPE=REAL		!as local TAE variables
    LOCAL PIX TYPE=INTEGER  ! number of pixels
    LOCAL SUM TYPE=REAL     !sum of values of pixels
	HIST  PIC  SIGMA=SD  MEAN=AVG, PIX=COUNT, SUM=SUM	!Compute SD and AVG
	PROG  PIC  STATS=(SD,AVG)	!Pass SD and AVG to program PROG

.page
DESCRIPTION OF THE PRINTED HISTOGRAM

The following is an example of a histogram printed using the 132-column (WIDE)
format:

        GRAY    FREQ     CDF           10        20        30        40 . . .
                              +         +         +         +         + . . .
 < LOW LIMIT     123   0.023  *         +         +         +         + . . .
        -129    1275   0.199  **        +         +         +         + . . .
         128    5395   1.042  **********+         +         +         + . . .
         385    9440   2.517  ******************  +         +         + . . .
         642   13536   4.632  ***************************   +         + . . .
          .       .       .      .      .      .     .    .     .     .
          .       .       .      .      .      .     .    .     .     .
          .       .       .      .      .      .     .    .     .     .
        6296    1238  95.234  **        +         +         +         + . . .
 >HIGH LIMIT     935 100.000  *         +         +         +         + . . .

The histogram consists of a bar graph, where the length of the bar at each DN
level is proportional to the number of pixels at that level.  The graph is
annotated on the left with a table of the frequency and cummulative distri-
bution function (CDF) at each DN-level.  The CDF is the percent count of all
pixels up to and including the given DN-level.  If the keyword 'NOCUM is
specified, this column changes to the percent count at that DN-level.
The CDF is not reported in the NORMAL screen format.

If there are pixels outside the DN-range specified by the LIMITS parameter,
these are indicated by the <LOW LIMIT and >HIGH LIMIT entries.

If the input data format is other than BYTE, the histogram is usually
compressed before display.  This is because the DN-range is normally larger
than can be displayed in the specified number of lines (NLINES parameter).
In these cases, the frequency displayed at a given DN-level represents the
number of pixels between that DN-level and the next higher DN-level in the
display.

For WIDE mode, histograms are preceded by an information line, such as:
FREQUENCY DISTRIBUTION    SL=1  SS=1  NL=15  NS=15   LINC=  1    SINC=  1
If the SIZE field is specified, then the SL and SS values in this message
are computed relative to the subimage defined by the SIZE field.

Any zero entries in a wide or normal mode histogram are skipped in the
printout.  For wide mode, one or more consecutive zero entries is represented
by a single blank line in the histogram.  For normal mode, one or more 
consecutive zero entries is indicated by an * following the DN value
in the next line of the displayed histogram.
.page
Examples

The following command prints out an 80-column-format histogram of input
image A:
	HIST  A 

The following command will print out a histogram of the 15x15 pixel area in
the upper-left corner of image A.  The histogram will be plotted using a wide
(132 column) format:
	HIST  A  AREA=(1,1,15,15)  'WIDE

The following command will print two graphs in the narrow format using a
maximum of three hundred lines to represent the data:
	HIST  A  AREA=(1,1,15,15,100,100,10,40)  NLINES=300
Two histograms are plotted.  The first for the area (1,1,15,15) and the second
for the area (100,100,10,40).

The following command produces a histogram of image A for which only every
third sample of every second line will be read (for speed).
	HIST  A  SPIKES=3  LINC=2  SINC=3
SPIKES specifies that the 3 largest frequencies will be saturated.

.page
PROGRAM HISTORY

WRITTEN BY:  Alan Mazer, September 1983
COGNIZANT PROGRAMMER:  Ray Bambery
REVISIONS:
     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 23 SEPT 1983
     REVISION 1 BY ASM, FEBRUARY 7 1984 - SPEED ENHANCEMENTS
         1) REPLACED CONVERSION OF ALL INPUT DATA TO REAL-TYPE WITH
	     SEPARATE TABULATION ROUTINES FOR EACH TYPE
         2) ADDED LOOK-UP TABLE FOR BYTE-IMAGE PROCESSING
  1984-10-09  LWK  converted to Vicar2, check for rounding error in sdev.
  1984-10-11  LWK  for byte data, compute stats from histogram.
  1984-12-13  LWK  revised treatment of BINS, LIMITS.
  1985-04-17  REA  fixed bug in LINC & AREA parameters
  1985-04-17  LWK  revised processing of REAL*4 data
  1986-11-11  REA  modify formatting, hist collection routines, add
		       output parameters MEAN, SIGMA
  1987-01-12  REA  add EXCLUDE, SCREEN parameters
  1987-02-04  REA  add SPLOT, PPLOT and TITLE parameters
  1987-03-02  REA   disable PPLOT (AVIRIS version)
  1989-10-12  GMY   Massive changes to source code and help file.
	  	            Replaced BINS parameter with NLINES parameter.
		            Changed algorithms for collecting and compressing
		            histograms.
  1990-11-07  LWK   Fixed bug in treatment of SINC with byte data
  1991-02-18  GMY   Fixed case where halfword image has only one DN.
  1992-10-19  SP    Made portable for UNIX.  modified to support DOUB format.
                    Corrected bug where HIST did not find MIN and MAX correctly
                    when LIMITS specified for halfword image.
  1998-03-98  RRP   Fixed TAB4 to prevent division by zero when both lower and
                    upper bounds are the same in which case bandwidth equals 0.
  1999-01-19  LWK   enabled for 3-D images
  2005-11-15  RGD   Fixed bug with overflow of RDN on certain images
  2008-08-22  RJB - Recoded to modern stds, realigned subroutine parameters for
                    64-bit Linux - to get rid of the following error message
                    which happened at random times: 
                    [TAE-IMSGERR] Error sending initiation message to process; host code = 32.;
  2010-01-30  RJB - Made compatible with 64-bit afids Build 793      
                    Linux, MacOSX (both Intel/PowerPC)    
  2010-03-07  RJB - Increased field for number of pixels in a histogram line from I8 to I9
  2010-12-02  RJB - Add sum and count output parameters
  2011-05-05  RJB - Fix continuation on xvmessage
  2011-06-20  RJB - Removed warning messages with gcc 4.4.4 on linux 5.6
  2011-06-24  RJB - Fixed pixel count maximum format statement I8 to to 99 Gpixels (I10)
  2012-06-30  RJB - Removed <tab> in front of continuation
                    lines to make backward compatible with
                    32-bit Linux gfortran 4.2.1, otherwise
                    compatible 64-bit Linux gfortran 4.6.3
  2012-07-02  RJB - Removed extraneous binwid parameter in shist call
                    Note: get warnings in gfortran 4.6.3 that can be ignored, e.g., 
                    Warning: Type mismatch in argument 'buf' at (1); passed INTEGER(1) to INTEGER(2)
                    stacka call defines buf in bytes (times num of bytes) since dynamic 
                    allocation is in bytes.
  2012-11-26  RJB - HIST fails on large worldview-2 images (specific example of a
                    byte image of 54000x72000 = 3888000000 bytes). Got 
                    [TAE-PRCSTRM] Abnormal process termination; process status code = 7.
                    Had to change internal histogram arrays to integer*8 as well as
                    a number of other variables. Remove call to HSUB library call for byte images
                    and replaced it with an internal H8SUB subroutine. Replaced 
                    stacka call with stacka_big. This requires other integer*8 variables.
                    Removed zia routines in spots. Still many IBM BYTE routine calls remain.
  2016-06-22  RJB - For years hist has erroneously truncated the number of elements, i.e,
                    For 10,000 ELEMENTS it reported 1,000, etc. This was fixed. Removed
                    SPLOT option from MODE (Regis graphics was vax-based). Removed subcommnad
                    IPARAM since it was for HPPLOT which was removed in 1987.
                    Fixed bug in bin width which printed 0.0 for any value < 0.1 or ******
                    for any value above 1.0e08. Added LWK's bug fix of 20120707 for case
                    where  binwid=0 when an image is all zeroes in hist.F. Already had incorporated
                    in this version, WLB's xvqout calls (2015-08-10) in mipl's version of hist. 
                    Removed routines plotxy and splot.c from build.

  2016-07-02  RJB   linked against new stacka_big routine
                    Centos-7.2 & gcc-4.8.5
  
.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE FORMAT
KEYWORD - Input data format 
(BYTE, HALF, FULL, REAL, DOUB)
.VARIABLE SPIKES
Bar to be normalized 
.VARIABLE SINC
Sample increment
.VARIABLE LINC
Line increment
.VARIABLE INC
Line/sample increment
.VARIABLE AREA
Area(s) to be graphed
.VARIABLE MODE
KEYWORD - Format options
Valid: NORMAL, WIDE, SCREEN,
       NOHIST
.VARI NOCUM
KEYWORD - Non-cumulative
percentages
Valid: NOCUM
.VARIABLE NLINES
Max number of graph bars
.VARIABLE LIMITS
Range of pixel values
.VARIABLE EXCLUDE
Exclude 0 DN pixels from
mean and std dev calculations
.vari NOHIST
KEYWORD: suppress histogram
Valid: NOHIST
.vari PAUSE
KEYWORD: adds interactive
pausing when screen full.
Valid: NOPAUSE, PAUSE
.vari MEAN
Output parameter
.vari SIGMA
Output parameter
.vari COUNT
Output parameter
.vari SUM
Output parameter
.LEVEL2
.vari INP
	EX:  INP=BPIC.DAT
specifies the input image file name.
.vari SIZE
SIZE specifies the portion of the input image which is to be processed by 
the program.  It consists of 4 integers:
	SIZE=(SL,SS,NL,NS)
where
	SL = starting line
	SS = starting sample
	NL = number of lines
	NS = number of samples
These can also be specified separately by the parameters SL, SS, NL, and NS.
See also AREA parameter.
.VARIABLE FORMAT
FORMAT specifies the input image data format.  Valid data formats are
BYTE, HALF, FULL, REAL, or DOUB.  If FORMAT is not specified, the data format is
determined from the image label.
.VARIABLE SPIKES
SPIKES=N specifies that the printed histogram will be normalized in frequency 
such that the N-th most frequent value is printed as the full width on 
the page.  All values which occur more frequently will also be represented 
by a full width bar, ranked by number on the right side. 

SPIKES may have any value from 1 to 9  (default=2 for NORMAL width, and 1 for
WIDE width).
.VARIABLE SINC
SINC=N specifies that every N-th sample in the specified area is to be used
in computing the histogram.  Default SINC=1.
Note that if INC is specified, this parameter is ignored.
.VARIABLE LINC
LINC=N specifies that every N-th line in the specified area is to be used
in computing the histogram.  Default LINC=1.
Note that if INC is specified, this parameter is ignored.
.VARIABLE INC
INC=N specifies that every N-th line and every N-th sample in the specified area
is to be used in computing the histogram.  Default INC=1.
This parameter overrides the SINC and LINC parameters.
.VARIABLE AREA
AREA specifies the area(s) of the input image which is/are for which histograms
are to be plotted.  If AREA is not specified, a histogram of the entire image
is computed.  The areas are specified as multiples of four integers:
	AREA=(SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...)
where
	SL1 = starting line of the first image area
        SS1 = starting line of the first image area
        NL1 = number of lines in the first image area
        NS1 = number of samples in the first image area
	ETC...
Up to 150 image areas may be specified.
If the SIZE field is entered as well, the AREA coordinates must be relative
to the subimage described by the SIZE field.
.VARIABLE MODE
MODE specifies the histogram plotting mode.  Valid values are:

    NORMAL - An 80 column wide histogram is printed with a vertical DN axis.
	     NORMAL is the default.
    WIDE   - A 132 column wide histogram is printed with a vertical DN axis.
    SCREEN - A 22 row by (up to) 80 column histogram is printed with a
 	     horizontally DN axis. This plot is designed to fit on a 
	     80x22 terminal screen. BYTE is the only permissible data format
             for this option.
    NOHIST - No histogram is printed.  Only the statistical data for the
             image is reported: mean, standard deviation, minimum and maximum
	     DN values, and total number of pixels.
.VARIABLE TITLE
Specifies a title to be placed beneath pen plots.  TITLE is ignored if PPLOT
is not specified. In addition, two other caption lines are always generated
for pen plots. One line contains the histogram statistics, while the other
lists the file name, area, linc, and sinc.
.vari NOCUM
NOCUM specifies that the percentages printed for each DN-level are the
percentage of the pixels at that DN-level.  The default is to display
the cummulative distribution function, which is the percentage of all pixels
at DN levels up to and including the current one.  Note that this
percentage is only reported in the WIDE format.
.VARIABLE NLINES
Specifies the maximum number of lines to be used in plotting the histogram.
The default is 256.
.VARIABLE LIMITS
LIMITS specifies the lower and upper bounds of the histogram.
If defaulted, these limits are set to the minimum and maximum
DN values in the specified image area, except for byte data,
for which the defaults are 0 and 255.
.VARIABLE EXCLUDE
Exclude 0 DN pixels from the mean and standard deviation values reported
at the end of the histogram. This does not affect the histogram itself.
.VARI PAUSE
PAUSE introduces pauses whenever the plot fills the terminal screen.
This parameter is ignored in batch mode. 
.VARI MEAN
An output parameter. When HIST has completed running, this parameter
contains the mean value of the last histogram. This value may be used by
subsequent programs within a procedure by declaring MEAN as a local real
variable in the procedure.
.VARI SIGMA
An output parameter. When HIST has completed running, this parameter
contains the value of the standard deviation for the last histogram. This 
value may be used by subsequent programs within a procedure by declaring 
SIGMA as a local real variable in the procedure.
.vari COUNT
An output parameter. When HIST has completed running, this parameter
contains the number of pixels in the last plotted histogram. This
value may be used by subsequent programs within a procedure by declaring
COUNT as a local integer variable in the procedure.
.vari SUM
An output parameter. When HIST has completed running, this parameter
contains the sum of all of the values of the pixels in the last plotted
histogram. This value may be used by subsequent programs within a 
procedure by declaring SUM as a local real variable in the procedure
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsthist.pdf
procedure
refgbl $echo
refgbl $autousage
local sd  type=real		!Declare SD and AVG
local avg type=real		!as local TAE variables
local sum type=real     !sum of all pixel values
local pix type=integer  !number of pixels
! Revised Jun 22, 2016 - RJB
! TEST SCRIPT FOR HIST   
! tests BYTE, HALF, FULL, REAL, DOUB images
!
! Vicar Programs:
!       gen                  
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
body
let _onfail="stop"
let $echo="yes"
let $autousage="none"

! TEST SCRIPT FOR HIST
!
! BYTE IMAGE
gen g1515 15 15
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
hist g1515 NLINES=10 LIMITS=(5,24)
hist g1515 NLINES=10 LIMITS=(5,23)
hist g1515           LIMITS=(5,23) 'wide
!
! TEST SPIKES
hist g1515 SPIKE=5
hist g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix	!Compute SD and AVG
let $echo="no"
write  "     Print average, stdev, number of pixels and sum of pixels variables" 
write  "     Average    = &avg"
write  "     Std Dev    = &sd"
write  "     Num of PIX = &pix"
write  "     Sum of DNs = &sum" 
let $echo="yes"
!!  Throw in some extra tests for good coverage.
!
! test SCREEN keyword
hist g1515 'screen spike=1
!
!  make an all zero file.  Hist should not blow up on 'exclude.
gen g 10 10 linc=0 sinc=0
hist g 'exclude
gen g 1024 1000 sinc=0 
let $echo="no"
write "     Should get mean =128.0 because of exclude DN=0"
let $echo="yes"
hist g nlines=10 'exclude
!
gen g 10 10 sinc=0
hist g inc=3
let $echo="no"
write "     Mean should be 4.5."
let $echo="yes"
hist g inc=3 'exclude
let $echo="no"
write "     Mean should be 6.0.  because of excluded DN=0"
let $echo="yes"
!
! HALF
gen g1515 15 15 linc=-1000 sinc=-1000 'half
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
hist g1515 NLINES=10 LIMITS=(-24000, -5000)
hist g1515 NLINES=10 LIMITS=(-23000, -5000)
hist g1515           LIMITS=(-23000, -5000) 'wide
!
! TEST SPIKES
hist g1515 SPIKE=5
hist g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist sigma=sd  mean=avg sum=sum count=pix	!Compute SD and AVG
let $echo="no"
write  "     Print average, stdev, number of pixels and sum of pixels variables"
write  "     Average    = &avg"
write  "     Std Dev    = &sd"
write  "     Num of PIX = &pix"
write  "     Sum of DNs = &sum"
let $echo="yes"
!!
! try a case that HIST used to get wrong.  Should not be any
! > HIGH LIMIT entry.
gen g 10 10 linc=1000 'half
hist g nlines=30 spikes=9
!
! FULL
gen f1515 15 15 linc=-100000 sinc=-100000 'full
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist f1515 area=(1,1,3,3,10,10,2,2)
!
hist f1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist f1515 linc=2 sinc=3
!
! test NLINES & LIMITS
hist f1515 NLINES=10 LIMITS=(-2400000, -500000)
hist f1515 NLINES=10 LIMITS=(-2300000, -500000)
hist f1515           LIMITS=(-2300000, -500000) 'wide
!
! TEST SPIKES
hist f1515 SPIKE=5
hist f1515 SPIKE=1
!
! test NOHIST keyword
hist f1515 'nohist
!
!  test output to TCL variables.
hist f1515 'nohist sigma=sd  mean=avg sum=sum count=pix	!Compute SD and AVG
let $echo="no"
write  "     Print average, stdev, number of pixels and sum of pixels variables"
write  "     Average    = &avg"
write  "     Std Dev    = &sd"
write  "     Num of PIX = &pix"
write  "     Sum of DNs = &sum"
let $echo="yes"
!!
!    ! test REAL*4 data
gen r1515 15 15 linc=1.e8 sinc=1.e8 'real4
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist r1515 area=(1,1,3,3,10,10,2,2)
!
hist r1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist r1515 linc=2 sinc=3
!
! test NLINES & LIMITS
hist r1515 NLINES=10 LIMITS=(5.e+8, 24.E+8) 
hist r1515 NLINES=10 LIMITS=(5.e+8, 23.E+8)
hist r1515 'nocum          LIMITS=(5.e+8, 23.E+8) 'wide
!
! TEST SPIKES
hist r1515 SPIKE=5
hist r1515 SPIKE=1
!
! test NOHIST keyword
hist r1515 'nohist
!
!  test output to TCL variables.
hist r1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix	!Compute SD and AVG
let $echo="no"
write  "     Print average, stdev, number of pixels and sum of pixels variables"
write  "     Average    = &avg"
write  "     Std Dev    = &sd"
write  "     Num of PIX = &pix"
write  "     Sum of DNs = &sum"
let $echo="yes"
!!
gen r1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
let $echo="no"
write "      Try some wild numbers.  3000 bins.  225 are non-empty."
write "      Should skip all empty bins and put a * after DN to indicate skip."
let $echo="yes"
hist r1515 nlines=3000
!    ! test REAL*8 data
! DOUB
gen d1515 15 15 linc=-1.e-9 sinc=-1.e-9 'real8
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist d1515 area=(1,1,3,3,10,10,2,2)
!
hist d1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist d1515 linc=2 sinc=3
!
! test NLINES & LIMITS
hist d1515 NLINES=10 LIMITS= (-24.e-9, -5.E-9)
hist d1515 NLINES=10 LIMITS=(-23.e-9, -5.E-9)
hist d1515 'nocum          LIMITS=(-23.e-9, -5.E-9) 'wide
!
! TEST SPIKES
hist d1515 SPIKE=5
hist d1515 SPIKE=1
!
! test NOHIST keyword
hist d1515 'nohist
!
!  test output to TCL variables.
hist d1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix	!Compute SD and AVG
let $echo="no"
write  "     Print average, stdev, number of pixels and sum of pixels variables"
write  "     Average    = &avg"
write  "     Std Dev    = &sd"
write  "     Num of PIX = &pix"
write  "     Sum of DNs = &sum"
let $echo="yes"
!
!  test of bands capability
gen x1515 NS=10 NL=10 NB=10
hist x1515
hist x1515 NB=4
!!
! test AR 112483.  Std dev should be exactly 0.  Depends on image size.
gen x1515 337 364 ival=200 linc=0 sinc=0
hist x1515
let $echo="no"
write "     Std Dev should be 0.0"
write " "
!
end-proc
$!-----------------------------------------------------------------------------
$ create tsthist.log
tsthist
let $autousage="none"
gen g1515 15 15
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1 *************************
          1          2 **************************************************  2
          2          3 **************************************************  1
          3          2 **************************************************
          4          1 *************************
AVERAGE GRAY LEVEL=  0.20E+01
STANDARD DEVIATION= 0.115E+01
NUMBER ELEMENTS=         9
MIN. DN=         0
MAX. DN=         4


Bin Width =        1.0
          0          1 **************************************************  2
         18*         1 **************************************************
         19          2 **************************************************  1
         20          1 **************************************************
AVERAGE GRAY LEVEL=  0.19E+02
STANDARD DEVIATION= 0.707E+00
NUMBER ELEMENTS=         4
MIN. DN=         0
MAX. DN=        20

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          2*         1 *************************
          3          2 **************************************************  2
          4          3 **************************************************  1
          5          2 **************************************************
          6          1 *************************
AVERAGE GRAY LEVEL=  0.40E+01
STANDARD DEVIATION= 0.115E+01
NUMBER ELEMENTS=         9
MIN. DN=         2
MAX. DN=         6


Bin Width =        1.0
         20*         1 **************************************************  2
         21          2 **************************************************  1
         22          1 **************************************************
AVERAGE GRAY LEVEL=  0.21E+02
STANDARD DEVIATION= 0.707E+00
NUMBER ELEMENTS=         4
MIN. DN=        20
MAX. DN=        22

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1 ****************
          2*         1 ****************
          3          1 ****************
          4          1 ****************
          5          1 ****************
          6          2 *********************************
          7          1 ****************
          8          2 *********************************
          9          2 *********************************
         10          2 *********************************
         11          2 *********************************
         12          3 **************************************************  1
         13          2 *********************************
         14          3 **************************************************  2
         15          2 *********************************
         16          2 *********************************
         17          2 *********************************
         18          2 *********************************
         19          1 ****************
         20          2 *********************************
         21          1 ****************
         22          1 ****************
         23          1 ****************
         24          1 ****************
         26*         1 ****************
AVERAGE GRAY LEVEL=  0.13E+02
STANDARD DEVIATION= 0.624E+01
NUMBER ELEMENTS=        40
MIN. DN=         0
MAX. DN=        26

hist g1515 NLINES=10 LIMITS=(5,24)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        2.1
< LOW LIMIT         10 ******************
        5.0         18 *********************************
        7.1         17 *******************************
        9.2         21 **************************************
       11.3         25 **********************************************
       13.4         29 **************************************************  1
       15.6         27 **************************************************  2
       17.7         23 ******************************************
       19.8         19 ***********************************
       21.9         15 ***************************
       24.0         15 ***************************
>HIGH LIMIT          6 ***********
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

hist g1515 NLINES=10 LIMITS=(5,23)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        2.0
< LOW LIMIT         10 ******************
          5         11 ********************
          7         15 ***************************
          9         19 ***********************************
         11         23 ******************************************
         13         27 **************************************************  2
         15         29 **************************************************  1
         17         25 **********************************************
         19         21 **************************************
         21         17 *******************************
         23         13 ************************
>HIGH LIMIT         15 ***************************
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

hist g1515           LIMITS=(5,23) 'wide
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT       1   6.667  ****************************************************************************************************+1
          5           9.333  ****************************************+         +         +         +         +         +         +
          6          12.444  **********************************************    +         +         +         +         +         +
          7          16.000  *****************************************************       +         +         +         +         +
          8          20.000  ************************************************************+         +         +         +         +
          9       1  24.444  ******************************************************************    +         +         +         +
         10       1  29.333  *************************************************************************       +         +         +
         11       1  34.667  ********************************************************************************+         +         +
         12       1  40.444  **************************************************************************************    +         +
         13       1  46.667  *********************************************************************************************       +
         14       1  53.333  ****************************************************************************************************+2
         15       1  59.556  *********************************************************************************************       +
         16       1  65.333  **************************************************************************************    +         +
         17       1  70.667  ********************************************************************************+         +         +
         18       1  75.556  *************************************************************************       +         +         +
         19       1  80.000  ******************************************************************    +         +         +         +
         20          84.000  ************************************************************+         +         +         +         +
         21          87.556  *****************************************************       +         +         +         +         +
         22          90.667  **********************************************    +         +         +         +         +         +
         23          93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT       1 100.000  ****************************************************************************************************+
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0    MAX. DN=        28

hist g1515 SPIKE=5
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1 ***
          1          2 *******
          2          3 ***********
          3          4 ***************
          4          5 *******************
          5          6 ***********************
          6          7 **************************
          7          8 ******************************
          8          9 **********************************
          9         10 **************************************
         10         11 ******************************************
         11         12 **********************************************
         12         13 **************************************************  4
         13         14 **************************************************  2
         14         15 **************************************************  1
         15         14 **************************************************  3
         16         13 **************************************************  5
         17         12 **********************************************
         18         11 ******************************************
         19         10 **************************************
         20          9 **********************************
         21          8 ******************************
         22          7 **************************
         23          6 ***********************
         24          5 *******************
         25          4 ***************
         26          3 ***********
         27          2 *******
         28          1 ***
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

hist g1515 SPIKE=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1 ***
          1          2 ******
          2          3 **********
          3          4 *************
          4          5 ****************
          5          6 ********************
          6          7 ***********************
          7          8 **************************
          8          9 ******************************
          9         10 *********************************
         10         11 ************************************
         11         12 ****************************************
         12         13 *******************************************
         13         14 **********************************************
         14         15 **************************************************  1
         15         14 **********************************************
         16         13 *******************************************
         17         12 ****************************************
         18         11 ************************************
         19         10 *********************************
         20          9 ******************************
         21          8 **************************
         22          7 ***********************
         23          6 ********************
         24          5 ****************
         25          4 *************
         26          3 **********
         27          2 ******
         28          1 ***
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.611E+01
NUMBER ELEMENTS=       225
MIN. DN=         0
MAX. DN=        28

let $echo="no"
     Print average, stdev, number of pixels and sum of pixels variables
     Average    = 1.400000000000e+01
     Std Dev    = 6.110100746155e+00
     Num of PIX = 225
     Sum of DNs = 3.150000000000e+03
hist g1515 'screen spike=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

              1
             ***
             ***
            *****
           *******
          *********
         ***********
         ***********
        *************
       ***************
      *****************
     *******************
     *******************
    *********************
   ***********************
  *************************
 ***************************
 ***************************
*****************************

          1    1    2    2
0    5    0    5    0    5
       225PIXELS   RANGE   0- 28     MEAN  14.000     STD DEV   6.110
gen g 10 10 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g 'exclude
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0        100 **************************************************  1
AVERAGE GRAY LEVEL=  0.00E+00
STANDARD DEVIATION= 0.000E+00
NUMBER ELEMENTS=         0
NOTE - EXCLUDING PIXELS OF DN=0
MIN. DN=         0
MAX. DN=         0

gen g 1024 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
     Should get mean =128.0 because of exclude DN=0
hist g nlines=10 'exclude
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =       28.3
          0      60000 *************************
         28     112000 ************************************************
         57     112000 ************************************************
         85     116000 **************************************************  1
        113     112000 ************************************************
        142     112000 ************************************************
        170     116000 **************************************************  2
        198     112000 ************************************************
        227     112000 ************************************************
        255      60000 *************************
AVERAGE GRAY LEVEL=  0.13E+03
STANDARD DEVIATION= 0.736E+02
NUMBER ELEMENTS=   1020000
NOTE - EXCLUDING PIXELS OF DN=0
MIN. DN=         0
MAX. DN=       255

gen g 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g inc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          4 **************************************************  1
          3*         4 **************************************************  2
          6*         4 **************************************************
          9*         4 **************************************************
AVERAGE GRAY LEVEL=  0.45E+01
STANDARD DEVIATION= 0.335E+01
NUMBER ELEMENTS=        16
MIN. DN=         0
MAX. DN=         9

let $echo="no"
     Mean should be 4.5.
hist g inc=3 'exclude
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          4 **************************************************  1
          3*         4 **************************************************  2
          6*         4 **************************************************
          9*         4 **************************************************
AVERAGE GRAY LEVEL=  0.60E+01
STANDARD DEVIATION= 0.245E+01
NUMBER ELEMENTS=        12
NOTE - EXCLUDING PIXELS OF DN=0
MIN. DN=         0
MAX. DN=         9

let $echo="no"
     Mean should be 6.0.  because of excluded DN=0
gen g1515 15 15 linc=-1000 sinc=-1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =      256.0
      -4000          1 *************************
      -2976*         2 **************************************************  2
      -1952*         3 **************************************************  1
       -928*         2 **************************************************
         96*         1 *************************
AVERAGE GRAY LEVEL= -0.20E+04
STANDARD DEVIATION= 0.115E+04
NUMBER ELEMENTS=         9
MIN. DN=     -4000
MAX. DN=         0


Bin Width =      256.0
     -20000          1 **************************************************  2
     -18976*         2 **************************************************  1
     -17952*         1 **************************************************
AVERAGE GRAY LEVEL= -0.19E+05
STANDARD DEVIATION= 0.707E+03
NUMBER ELEMENTS=         4
MIN. DN=    -20000
MAX. DN=    -18000

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =      256.0
      -6000          1 *************************
      -4976*         2 **************************************************  2
      -3952*         3 **************************************************  1
      -2928*         2 **************************************************
      -1904*         1 *************************
AVERAGE GRAY LEVEL= -0.40E+04
STANDARD DEVIATION= 0.115E+04
NUMBER ELEMENTS=         9
MIN. DN=     -6000
MAX. DN=     -2000


Bin Width =      256.0
     -22000          1 **************************************************  2
     -20976*         2 **************************************************  1
     -19952*         1 **************************************************
AVERAGE GRAY LEVEL= -0.21E+05
STANDARD DEVIATION= 0.707E+03
NUMBER ELEMENTS=         4
MIN. DN=    -22000
MAX. DN=    -20000

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =      256.0
     -26000          1 ****************
     -23952*         1 ****************
     -22928*         1 ****************
     -21904*         1 ****************
     -20880*         1 ****************
     -20112*         2 *********************************
     -19088*         1 ****************
     -18064*         2 *********************************
     -17040*         2 *********************************
     -16016*         2 *********************************
     -14992*         2 *********************************
     -13968*         3 **************************************************  1
     -12944*         2 *********************************
     -11920*         3 **************************************************  2
     -10896*         2 *********************************
      -9872*         2 *********************************
      -9104*         2 *********************************
      -8080*         2 *********************************
      -7056*         1 ****************
      -6032*         2 *********************************
      -5008*         1 ****************
      -3984*         1 ****************
      -2960*         1 ****************
      -1936*         1 ****************
        112*         1 ****************
AVERAGE GRAY LEVEL= -0.13E+05
STANDARD DEVIATION= 0.624E+04
NUMBER ELEMENTS=        40
MIN. DN=    -26000
MAX. DN=         0

hist g1515 NLINES=10 LIMITS=(-24000, -5000)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     2111.1
< LOW LIMIT          6 ***********
     -24000         15 ***************************
     -21889         15 ***************************
     -19778         19 ***********************************
     -17667         23 ******************************************
     -15556         27 **************************************************  2
     -13444         29 **************************************************  1
     -11333         25 **********************************************
      -9222         21 **************************************
      -7111         17 *******************************
      -5000         18 *********************************
>HIGH LIMIT         10 ******************
AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 NLINES=10 LIMITS=(-23000, -5000)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     2000.0
< LOW LIMIT         10 ******************
     -23000         11 ********************
     -21000         15 ***************************
     -19000         19 ***********************************
     -17000         23 ******************************************
     -15000         27 **************************************************  2
     -13000         29 **************************************************  1
     -11000         25 **********************************************
      -9000         21 **************************************
      -7000         17 *******************************
      -5000         13 ************************
>HIGH LIMIT         15 ***************************
AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

hist g1515           LIMITS=(-23000, -5000) 'wide
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =       70.6
FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT       1   6.667  ****************************************************************************************************+1
     -23000           9.333  ****************************************+         +         +         +         +         +         +

     -22012          12.444  **********************************************    +         +         +         +         +         +

     -21024          16.000  *****************************************************       +         +         +         +         +

     -19965          20.000  ************************************************************+         +         +         +         +

     -18976       1  24.444  ******************************************************************    +         +         +         +

     -17988       1  29.333  *************************************************************************       +         +         +

     -17000       1  34.667  ********************************************************************************+         +         +

     -16012       1  40.444  **************************************************************************************    +         +

     -15024       1  46.667  *********************************************************************************************       +

     -13965       1  53.333  ****************************************************************************************************+2

     -12976       1  59.556  *********************************************************************************************       +

     -11988       1  65.333  **************************************************************************************    +         +

     -11000       1  70.667  ********************************************************************************+         +         +

     -10012       1  75.556  *************************************************************************       +         +         +

      -9024       1  80.000  ******************************************************************    +         +         +         +

      -7965          84.000  ************************************************************+         +         +         +         +

      -6976          87.556  *****************************************************       +         +         +         +         +

      -5988          90.667  **********************************************    +         +         +         +         +         +

      -5000          93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT       1 100.000  ****************************************************************************************************+
AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000    MAX. DN=         0

hist g1515 SPIKE=5
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =      256.0
     -28000          1 ***
     -26976*         2 *******
     -25952*         3 ***********
     -24928*         4 ***************
     -23904*         5 *******************
     -22880*         6 ***********************
     -22112*         7 **************************
     -21088*         8 ******************************
     -20064*         9 **********************************
     -19040*        10 **************************************
     -18016*        11 ******************************************
     -16992*        12 **********************************************
     -15968*        13 **************************************************  4
     -14944*        14 **************************************************  2
     -13920*        15 **************************************************  1
     -12896*        14 **************************************************  3
     -11872*        13 **************************************************  5
     -11104*        12 **********************************************
     -10080*        11 ******************************************
      -9056*        10 **************************************
      -8032*         9 **********************************
      -7008*         8 ******************************
      -5984*         7 **************************
      -4960*         6 ***********************
      -3936*         5 *******************
      -2912*         4 ***************
      -1888*         3 ***********
      -1120*         2 *******
        -96*         1 ***
AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 SPIKE=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =      256.0
     -28000          1 ***
     -26976*         2 ******
     -25952*         3 **********
     -24928*         4 *************
     -23904*         5 ****************
     -22880*         6 ********************
     -22112*         7 ***********************
     -21088*         8 **************************
     -20064*         9 ******************************
     -19040*        10 *********************************
     -18016*        11 ************************************
     -16992*        12 ****************************************
     -15968*        13 *******************************************
     -14944*        14 **********************************************
     -13920*        15 **************************************************  1
     -12896*        14 **********************************************
     -11872*        13 *******************************************
     -11104*        12 ****************************************
     -10080*        11 ************************************
      -9056*        10 *********************************
      -8032*         9 ******************************
      -7008*         8 **************************
      -5984*         7 ***********************
      -4960*         6 ********************
      -3936*         5 ****************
      -2912*         4 *************
      -1888*         3 **********
      -1120*         2 ******
        -96*         1 ***
AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist sigma=sd  mean=avg sum=sum count=pix
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E+05
STANDARD DEVIATION= 0.611E+04
NUMBER ELEMENTS=       225
MIN. DN=    -28000
MAX. DN=         0

let $echo="no"
     Print average, stdev, number of pixels and sum of pixels variables
     Average    = -1.400000000000e+04
     Std Dev    = 6.110101074219e+03
     Num of PIX = 225
     Sum of DNs = -3.150000000000e+06
gen g 10 10 linc=1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g nlines=30 spikes=9
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        4.0
          0          2 *************************
          4          4 **************************************************  1
          8          4 **************************************************  2
       1000*         2 *************************
       1004          4 **************************************************  3
       1008          4 **************************************************  4
       2000*         2 *************************
       2004          4 **************************************************  5
       2008          4 **************************************************  6
       3000*         2 *************************
       3004          4 **************************************************  7
       3008          4 **************************************************  8
       4000*         2 *************************
       4004          4 **************************************************  9
       4008          4 **************************************************
       5000*         2 *************************
       5004          4 **************************************************
       5008          4 **************************************************
       6000*         2 *************************
       6004          4 **************************************************
       6008          4 **************************************************
       7000*         2 *************************
       7004          4 **************************************************
       7008          4 **************************************************
       8000*         2 *************************
       8004          4 **************************************************
       8008          4 **************************************************
       9000*         2 *************************
       9004          4 **************************************************
       9008          4 **************************************************
AVERAGE GRAY LEVEL=  0.45E+04
STANDARD DEVIATION= 0.287E+04
NUMBER ELEMENTS=       100
MIN. DN=         0
MAX. DN=      9009

gen f1515 15 15 linc=-100000 sinc=-100000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist f1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     1568.6
    -400000          1 *************************
    -299608*         2 **************************************************  2
    -199216*         3 **************************************************  1
    -100392*         2 **************************************************
          0*         1 *************************
AVERAGE GRAY LEVEL= -0.20E+06
STANDARD DEVIATION= 0.115E+06
NUMBER ELEMENTS=         9
MIN. DN=   -400000
MAX. DN=         0


Bin Width =      784.3
   -2000000          1 **************************************************  2
   -1899608*         2 **************************************************  1
   -1800000*         1 **************************************************
AVERAGE GRAY LEVEL= -0.19E+07
STANDARD DEVIATION= 0.707E+05
NUMBER ELEMENTS=         4
MIN. DN=  -2000000
MAX. DN=  -1800000

hist f1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     1568.6
    -600000          1 *************************
    -499608*         2 **************************************************  2
    -399216*         3 **************************************************  1
    -300392*         2 **************************************************
    -200000*         1 *************************
AVERAGE GRAY LEVEL= -0.40E+06
STANDARD DEVIATION= 0.115E+06
NUMBER ELEMENTS=         9
MIN. DN=   -600000
MAX. DN=   -200000


Bin Width =      784.3
   -2200000          1 **************************************************  2
   -2099608*         2 **************************************************  1
   -2000000*         1 **************************************************
AVERAGE GRAY LEVEL= -0.21E+07
STANDARD DEVIATION= 0.707E+05
NUMBER ELEMENTS=         4
MIN. DN=  -2200000
MAX. DN=  -2000000

hist f1515 linc=2 sinc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =    10196.1
   -2600000          1 ****************
   -2396078*         1 ****************
   -2304314*         1 ****************
   -2202353*         1 ****************
   -2100392*         1 ****************
   -1998431*         2 *********************************
   -1896471*         1 ****************
   -1804706*         2 *********************************
   -1702745*         2 *********************************
   -1600784*         2 *********************************
   -1498824*         2 *********************************
   -1396863*         3 **************************************************  1
   -1294902*         2 *********************************
   -1203137*         3 **************************************************  2
   -1101176*         2 *********************************
    -999216*         2 *********************************
    -897255*         2 *********************************
    -795294*         2 *********************************
    -703529*         1 ****************
    -601569*         2 *********************************
    -499608*         1 ****************
    -397647*         1 ****************
    -295686*         1 ****************
    -203922*         1 ****************
          0*         1 ****************
AVERAGE GRAY LEVEL= -0.13E+07
STANDARD DEVIATION= 0.624E+06
NUMBER ELEMENTS=        40
MIN. DN=  -2600000
MAX. DN=         0

hist f1515 NLINES=10 LIMITS=(-2400000, -500000)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =   211111.1
< LOW LIMIT          6 ***********
   -2400000         15 ***************************
   -2188889         15 ***************************
   -1977778         19 ***********************************
   -1766667         23 ******************************************
   -1555556         27 **************************************************  2
   -1344444         29 **************************************************  1
   -1133333         25 **********************************************
    -922222         21 **************************************
    -711111         17 *******************************
    -500000         18 *********************************
>HIGH LIMIT         10 ******************
AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

hist f1515 NLINES=10 LIMITS=(-2300000, -500000)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =   200000.0
< LOW LIMIT         10 ******************
   -2300000         11 ********************
   -2100000         15 ***************************
   -1900000         19 ***********************************
   -1700000         23 ******************************************
   -1500000         27 **************************************************  2
   -1300000         29 **************************************************  1
   -1100000         25 **********************************************
    -900000         21 **************************************
    -700000         17 *******************************
    -500000         13 ************************
>HIGH LIMIT         15 ***************************
AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

hist f1515           LIMITS=(-2300000, -500000) 'wide
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     7058.8
FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT       1   6.667  ****************************************************************************************************+1
   -2300000           9.333  ****************************************+         +         +         +         +         +         +

   -2201176          12.444  **********************************************    +         +         +         +         +         +

   -2102353          16.000  *****************************************************       +         +         +         +         +

   -1996471          20.000  ************************************************************+         +         +         +         +

   -1897647       1  24.444  ******************************************************************    +         +         +         +

   -1798824       1  29.333  *************************************************************************       +         +         +

   -1700000       1  34.667  ********************************************************************************+         +         +

   -1601176       1  40.444  **************************************************************************************    +         +

   -1502353       1  46.667  *********************************************************************************************       +

   -1396471       1  53.333  ****************************************************************************************************+2

   -1297647       1  59.556  *********************************************************************************************       +

   -1198824       1  65.333  **************************************************************************************    +         +

   -1100000       1  70.667  ********************************************************************************+         +         +

   -1001176       1  75.556  *************************************************************************       +         +         +

    -902353       1  80.000  ******************************************************************    +         +         +         +

    -796471          84.000  ************************************************************+         +         +         +         +

    -697647          87.556  *****************************************************       +         +         +         +         +

    -598824          90.667  **********************************************    +         +         +         +         +         +

    -500000          93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT       1 100.000  ****************************************************************************************************+
AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000    MAX. DN=         0

hist f1515 SPIKE=5
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =    10980.4
   -2800000          1 ***
   -2701176*         2 *******
   -2602353*         3 ***********
   -2503529*         4 ***************
   -2404706*         5 *******************
   -2294902*         6 ***********************
   -2196078*         7 **************************
   -2097255*         8 ******************************
   -1998431*         9 **********************************
   -1899608*        10 **************************************
   -1800784*        11 ******************************************
   -1701961*        12 **********************************************
   -1603137*        13 **************************************************  4
   -1504314*        14 **************************************************  2
   -1394510*        15 **************************************************  1
   -1295686*        14 **************************************************  3
   -1196863*        13 **************************************************  5
   -1098039*        12 **********************************************
    -999216*        11 ******************************************
    -900392*        10 **************************************
    -801569*         9 **********************************
    -702745*         8 ******************************
    -603922*         7 **************************
    -505098*         6 ***********************
    -395294*         5 *******************
    -296471*         4 ***************
    -197647*         3 ***********
     -98824*         2 *******
          0*         1 ***
AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

hist f1515 SPIKE=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =    10980.4
   -2800000          1 ***
   -2701176*         2 ******
   -2602353*         3 **********
   -2503529*         4 *************
   -2404706*         5 ****************
   -2294902*         6 ********************
   -2196078*         7 ***********************
   -2097255*         8 **************************
   -1998431*         9 ******************************
   -1899608*        10 *********************************
   -1800784*        11 ************************************
   -1701961*        12 ****************************************
   -1603137*        13 *******************************************
   -1504314*        14 **********************************************
   -1394510*        15 **************************************************  1
   -1295686*        14 **********************************************
   -1196863*        13 *******************************************
   -1098039*        12 ****************************************
    -999216*        11 ************************************
    -900392*        10 *********************************
    -801569*         9 ******************************
    -702745*         8 **************************
    -603922*         7 ***********************
    -505098*         6 ********************
    -395294*         5 ****************
    -296471*         4 *************
    -197647*         3 **********
     -98824*         2 ******
          0*         1 ***
AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

hist f1515 'nohist
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

hist f1515 'nohist sigma=sd  mean=avg sum=sum count=pix
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E+07
STANDARD DEVIATION= 0.611E+06
NUMBER ELEMENTS=       225
MIN. DN=  -2800000
MAX. DN=         0

let $echo="no"
     Print average, stdev, number of pixels and sum of pixels variables
     Average    = -1.400000000000e+06
     Std Dev    = 6.110100625000e+05
     Num of PIX = 225
     Sum of DNs = -3.150000000000e+08
gen r1515 15 15 linc=1.e8 sinc=1.e8 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist r1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =  1568627.5
0.00000E+00          1 *************************
1.00392E+08*         2 **************************************************  2
2.00784E+08*         3 **************************************************  1
2.99608E+08*         2 **************************************************
4.00000E+08*         1 *************************
AVERAGE GRAY LEVEL=  0.20E+09
STANDARD DEVIATION= 0.115E+09
NUMBER ELEMENTS=         9
MIN. DN=0.000000
MAX. DN=4.000E+8


Bin Width =   784313.7
1.80000E+09          1 **************************************************  2
1.90039E+09*         2 **************************************************  1
2.00000E+09*         1 **************************************************
AVERAGE GRAY LEVEL=  0.19E+10
STANDARD DEVIATION= 0.707E+08
NUMBER ELEMENTS=         4
MIN. DN=1.800E+9
MAX. DN=2.000E+9

hist r1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =  1568627.5
2.00000E+08          1 *************************
3.00392E+08*         2 **************************************************  2
4.00784E+08*         3 **************************************************  1
4.99608E+08*         2 **************************************************
6.00000E+08*         1 *************************
AVERAGE GRAY LEVEL=  0.40E+09
STANDARD DEVIATION= 0.115E+09
NUMBER ELEMENTS=         9
MIN. DN=2.000E+8
MAX. DN=6.000E+8


Bin Width =   784313.7
2.00000E+09          1 **************************************************  2
2.10039E+09*         2 **************************************************  1
2.20000E+09*         1 **************************************************
AVERAGE GRAY LEVEL=  0.21E+10
STANDARD DEVIATION= 0.707E+08
NUMBER ELEMENTS=         4
MIN. DN=2.000E+9
MAX. DN=2.200E+9

hist r1515 linc=2 sinc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width = 10196078.4
0.00000E+00          1 ****************
2.03922E+08*         1 ****************
2.95686E+08*         1 ****************
3.97647E+08*         1 ****************
4.99608E+08*         1 ****************
6.01569E+08*         2 *********************************
7.03529E+08*         1 ****************
7.95294E+08*         2 *********************************
8.97255E+08*         2 *********************************
9.99216E+08*         2 *********************************
1.10118E+09*         2 *********************************
1.20314E+09*         3 **************************************************  1
1.30510E+09*         2 *********************************
1.39686E+09*         3 **************************************************  2
1.49882E+09*         2 *********************************
1.60078E+09*         2 *********************************
1.70275E+09*         2 *********************************
1.80471E+09*         2 *********************************
1.89647E+09*         1 ****************
1.99843E+09*         2 *********************************
2.10039E+09*         1 ****************
2.20235E+09*         1 ****************
2.30431E+09*         1 ****************
2.39608E+09*         1 ****************
2.60000E+09*         1 ****************
AVERAGE GRAY LEVEL=  0.13E+10
STANDARD DEVIATION= 0.624E+09
NUMBER ELEMENTS=        40
MIN. DN=0.000000
MAX. DN=2.600E+9

hist r1515 NLINES=10 LIMITS=(5.e+8, 24.E+8)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.2111111111E+09
< LOW LIMIT         10 ******************
5.00000E+08         18 *********************************
7.11111E+08         17 *******************************
9.22222E+08         21 **************************************
1.13333E+09         25 **********************************************
1.34444E+09         29 **************************************************  1
1.55556E+09         27 **************************************************  2
1.76667E+09         23 ******************************************
1.97778E+09         19 ***********************************
2.18889E+09         15 ***************************
2.40000E+09         15 ***************************
>HIGH LIMIT          6 ***********
AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist r1515 NLINES=10 LIMITS=(5.e+8, 23.E+8)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.2000000000E+09
< LOW LIMIT         10 ******************
5.00000E+08         11 ********************
7.00000E+08         15 ***************************
9.00000E+08         19 ***********************************
1.10000E+09         23 ******************************************
1.30000E+09         27 **************************************************  2
1.50000E+09         29 **************************************************  1
1.70000E+09         25 **********************************************
1.90000E+09         21 **************************************
2.10000E+09         17 *******************************
2.30000E+09         13 ************************
>HIGH LIMIT         15 ***************************
AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist r1515 'nocum          LIMITS=(5.e+8, 23.E+8) 'wide
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =  7058823.5
FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT       1   6.667  ****************************************************************************************************+1
5.00000E+08           2.667  ****************************************+         +         +         +         +         +         +

5.98824E+08           3.111  **********************************************    +         +         +         +         +         +

6.97647E+08           3.556  *****************************************************       +         +         +         +         +

8.03529E+08           4.000  ************************************************************+         +         +         +         +

9.02353E+08       1   4.444  ******************************************************************    +         +         +         +

1.00118E+09       1   4.889  *************************************************************************       +         +         +

1.10000E+09       1   5.333  ********************************************************************************+         +         +

1.19882E+09       1   5.778  **************************************************************************************    +         +

1.29765E+09       1   6.222  *********************************************************************************************       +

1.40353E+09       1   6.667  ****************************************************************************************************+2

1.50235E+09       1   6.222  *********************************************************************************************       +

1.60118E+09       1   5.778  **************************************************************************************    +         +

1.70000E+09       1   5.333  ********************************************************************************+         +         +

1.79882E+09       1   4.889  *************************************************************************       +         +         +

1.89765E+09       1   4.444  ******************************************************************    +         +         +         +

2.00353E+09           4.000  ************************************************************+         +         +         +         +

2.10235E+09           3.556  *****************************************************       +         +         +         +         +

2.20118E+09           3.111  **********************************************    +         +         +         +         +         +

2.30000E+09           2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT       1   6.667  ****************************************************************************************************+
AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000      MAX. DN=2.800E+9

hist r1515 SPIKE=5
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width = 10980392.2
0.00000E+00          1 ***
9.88235E+07*         2 *******
1.97647E+08*         3 ***********
2.96471E+08*         4 ***************
3.95294E+08*         5 *******************
5.05098E+08*         6 ***********************
6.03922E+08*         7 **************************
7.02745E+08*         8 ******************************
8.01569E+08*         9 **********************************
9.00392E+08*        10 **************************************
9.99216E+08*        11 ******************************************
1.09804E+09*        12 **********************************************
1.19686E+09*        13 **************************************************  4
1.29569E+09*        14 **************************************************  2
1.40549E+09*        15 **************************************************  1
1.50431E+09*        14 **************************************************  3
1.60314E+09*        13 **************************************************  5
1.70196E+09*        12 **********************************************
1.80078E+09*        11 ******************************************
1.89961E+09*        10 **************************************
1.99843E+09*         9 **********************************
2.09725E+09*         8 ******************************
2.19608E+09*         7 **************************
2.29490E+09*         6 ***********************
2.40471E+09*         5 *******************
2.50353E+09*         4 ***************
2.60235E+09*         3 ***********
2.70118E+09*         2 *******
2.80000E+09*         1 ***
AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist r1515 SPIKE=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width = 10980392.2
0.00000E+00          1 ***
9.88235E+07*         2 ******
1.97647E+08*         3 **********
2.96471E+08*         4 *************
3.95294E+08*         5 ****************
5.05098E+08*         6 ********************
6.03922E+08*         7 ***********************
7.02745E+08*         8 **************************
8.01569E+08*         9 ******************************
9.00392E+08*        10 *********************************
9.99216E+08*        11 ************************************
1.09804E+09*        12 ****************************************
1.19686E+09*        13 *******************************************
1.29569E+09*        14 **********************************************
1.40549E+09*        15 **************************************************  1
1.50431E+09*        14 **********************************************
1.60314E+09*        13 *******************************************
1.70196E+09*        12 ****************************************
1.80078E+09*        11 ************************************
1.89961E+09*        10 *********************************
1.99843E+09*         9 ******************************
2.09725E+09*         8 **************************
2.19608E+09*         7 ***********************
2.29490E+09*         6 ********************
2.40471E+09*         5 ****************
2.50353E+09*         4 *************
2.60235E+09*         3 **********
2.70118E+09*         2 ******
2.80000E+09*         1 ***
AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist r1515 'nohist
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist r1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL=  0.14E+10
STANDARD DEVIATION= 0.611E+09
NUMBER ELEMENTS=       225
MIN. DN=0.000000
MAX. DN=2.800E+9

let $echo="no"
     Print average, stdev, number of pixels and sum of pixels variables
     Average    = 1.400000000000e+09
     Std Dev    = 6.110101120000e+08
     Num of PIX = 225
     Sum of DNs = 3.149999964160e+11
gen r1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
      Try some wild numbers.  3000 bins.  225 are non-empty.
      Should skip all empty bins and put a * after DN to indicate skip.
hist r1515 nlines=3000
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.6339561688E+11
-1.9012E+14          1 *************************
-1.8892E+14*         1 *************************
-1.8765E+14*         1 *************************
-1.8645E+14*         1 *************************
-1.8518E+14*         1 *************************
-1.8397E+14*         1 *************************
-1.8271E+14*         1 *************************
-1.8150E+14*         1 *************************
-1.8023E+14*         1 *************************
-1.7903E+14*         1 *************************
-1.7776E+14*         2 **************************************************  1
-1.7656E+14*         2 **************************************************  2
-1.7529E+14*         2 **************************************************
-1.7408E+14*         2 **************************************************
-1.7282E+14*         2 **************************************************
-1.7161E+14*         1 *************************
-1.7034E+14*         1 *************************
-1.6914E+14*         1 *************************
-1.6787E+14*         1 *************************
-1.6667E+14*         1 *************************
-1.6546E+14*         2 **************************************************
-1.6419E+14*         2 **************************************************
-1.6299E+14*         2 **************************************************
-1.6172E+14*         2 **************************************************
-1.6052E+14*         2 **************************************************
-1.5925E+14*         1 *************************
-1.5805E+14*         1 *************************
-1.5678E+14*         1 *************************
-1.5557E+14*         1 *************************
-1.5430E+14*         1 *************************
-1.5310E+14*         2 **************************************************
-1.5183E+14*         2 **************************************************
-1.5063E+14*         2 **************************************************
-1.4936E+14*         2 **************************************************
-1.4816E+14*         2 **************************************************
-1.4689E+14*         1 *************************
-1.4568E+14*         1 *************************
-1.4442E+14*         1 *************************
-1.4321E+14*         1 *************************
-1.4201E+14*         1 *************************
-1.4074E+14*         2 **************************************************
-1.3953E+14*         2 **************************************************
-1.3827E+14*         2 **************************************************
-1.3706E+14*         2 **************************************************
-1.3579E+14*         2 **************************************************
-1.3459E+14*         1 *************************
-1.3332E+14*         1 *************************
-1.3212E+14*         1 *************************
-1.3085E+14*         1 *************************
-1.2964E+14*         1 *************************
-1.2838E+14*         2 **************************************************
-1.2717E+14*         2 **************************************************
-1.2590E+14*         2 **************************************************
-1.2470E+14*         2 **************************************************
-1.2343E+14*         2 **************************************************
-1.2223E+14*         1 *************************
-1.2096E+14*         1 *************************
-1.1975E+14*         1 *************************
-1.1855E+14*         1 *************************
-1.1728E+14*         1 *************************
-1.1608E+14*         2 **************************************************
-1.1481E+14*         2 **************************************************
-1.1360E+14*         2 **************************************************
-1.1234E+14*         2 **************************************************
-1.1113E+14*         2 **************************************************
-1.0986E+14*         1 *************************
-1.0866E+14*         1 *************************
-1.0739E+14*         1 *************************
-1.0619E+14*         1 *************************
-1.0492E+14*         1 *************************
-1.0372E+14*         2 **************************************************
-1.0245E+14*         2 **************************************************
-1.0124E+14*         2 **************************************************
-9.9975E+13*         2 **************************************************
-9.8770E+13*         2 **************************************************
-9.7502E+13*         1 *************************
-9.6298E+13*         1 *************************
-9.5030E+13*         1 *************************
-9.3826E+13*         1 *************************
-9.2621E+13*         1 *************************
-9.1353E+13*         2 **************************************************
-9.0149E+13*         2 **************************************************
-8.8881E+13*         2 **************************************************
-8.7676E+13*         2 **************************************************
-8.6408E+13*         2 **************************************************
-8.5204E+13*         1 *************************
-8.3936E+13*         1 *************************
-8.2731E+13*         1 *************************
-8.1463E+13*         1 *************************
-8.0259E+13*         1 *************************
-7.8991E+13*         2 **************************************************
-7.7786E+13*         2 **************************************************
-7.6519E+13*         2 **************************************************
-7.5314E+13*         2 **************************************************
-7.4046E+13*         2 **************************************************
-7.2842E+13*         1 *************************
-7.1574E+13*         1 *************************
-7.0369E+13*         1 *************************
-6.9165E+13*         1 *************************
-6.7897E+13*         1 *************************
-6.6692E+13*         2 **************************************************
-6.5424E+13*         2 **************************************************
-6.4220E+13*         2 **************************************************
-6.2952E+13*         2 **************************************************
-6.1747E+13*         2 **************************************************
-6.0479E+13*         1 *************************
-5.9275E+13*         1 *************************
-5.8007E+13*         1 *************************
-5.6802E+13*         1 *************************
-5.5535E+13*         1 *************************
-5.4330E+13*         2 **************************************************
-5.3062E+13*         2 **************************************************
-5.1858E+13*         2 **************************************************
-5.0590E+13*         2 **************************************************
-4.9385E+13*         2 **************************************************
-4.8117E+13*         1 *************************
-4.6913E+13*         1 *************************
-4.5708E+13*         1 *************************
-4.4440E+13*         1 *************************
-4.3236E+13*         1 *************************
-4.1968E+13*         2 **************************************************
-4.0763E+13*         2 **************************************************
-3.9495E+13*         2 **************************************************
-3.8291E+13*         2 **************************************************
-3.7023E+13*         2 **************************************************
-3.5819E+13*         1 *************************
-3.4551E+13*         1 *************************
-3.3346E+13*         1 *************************
-3.2078E+13*         1 *************************
-3.0874E+13*         1 *************************
-2.9606E+13*         2 **************************************************
-2.8401E+13*         2 **************************************************
-2.7133E+13*         2 **************************************************
-2.5929E+13*         2 **************************************************
-2.4661E+13*         2 **************************************************
-2.3456E+13*         1 *************************
-2.2252E+13*         1 *************************
-2.0984E+13*         1 *************************
-1.9779E+13*         1 *************************
-1.8512E+13*         1 *************************
-1.7307E+13*         2 **************************************************
-1.6039E+13*         2 **************************************************
-1.4835E+13*         2 **************************************************
-1.3567E+13*         2 **************************************************
-1.2362E+13*         2 **************************************************
-1.1094E+13*         1 *************************
-9.8897E+12*         1 *************************
-8.6218E+12*         1 *************************
-7.4173E+12*         1 *************************
-6.1494E+12*         1 *************************
-4.9449E+12*         1 *************************
-3.6769E+12*         1 *************************
-2.4724E+12*         1 *************************
-1.2045E+12*         1 *************************
2.06030E+00*         1 *************************
AVERAGE GRAY LEVEL= -0.95E+14
STANDARD DEVIATION= 0.536E+14
NUMBER ELEMENTS=       225
MIN. DN=-1.9E+14
MAX. DN=0.000000

gen d1515 15 15 linc=-1.e-9 sinc=-1.e-9 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist d1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.1568627451E-10
-4.0000E-09          1 *************************
-2.9961E-09*         2 **************************************************  2
-1.9922E-09*         3 **************************************************  1
-1.0039E-09*         2 **************************************************
2.29220E-23*         1 *************************
AVERAGE GRAY LEVEL= -0.20E-08
STANDARD DEVIATION= 0.115E-08
NUMBER ELEMENTS=         9
MIN. DN=-4.00E-9
MAX. DN=0.000000


Bin Width =     0.7843137255E-11
-2.0000E-08          1 **************************************************  2
-1.8996E-08*         2 **************************************************  1
-1.8000E-08*         1 **************************************************
AVERAGE GRAY LEVEL= -0.19E-07
STANDARD DEVIATION= 0.707E-09
NUMBER ELEMENTS=         4
MIN. DN=-2.00E-8
MAX. DN=-1.80E-8

hist d1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.1568627451E-10
-6.0000E-09          1 *************************
-4.9961E-09*         2 **************************************************  2
-3.9922E-09*         3 **************************************************  1
-3.0039E-09*         2 **************************************************
-2.0000E-09*         1 *************************
AVERAGE GRAY LEVEL= -0.40E-08
STANDARD DEVIATION= 0.115E-08
NUMBER ELEMENTS=         9
MIN. DN=-6.00E-9
MAX. DN=-2.00E-9


Bin Width =     0.7843137255E-11
-2.2000E-08          1 **************************************************  2
-2.0996E-08*         2 **************************************************  1
-2.0000E-08*         1 **************************************************
AVERAGE GRAY LEVEL= -0.21E-07
STANDARD DEVIATION= 0.707E-09
NUMBER ELEMENTS=         4
MIN. DN=-2.20E-8
MAX. DN=-2.00E-8

hist d1515 linc=2 sinc=3
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.1019607843E-09
-2.6000E-08          1 ****************
-2.3961E-08*         1 ****************
-2.3043E-08*         1 ****************
-2.2024E-08*         1 ****************
-2.1004E-08*         1 ****************
-1.9984E-08*         2 *********************************
-1.8965E-08*         1 ****************
-1.8047E-08*         2 *********************************
-1.7027E-08*         2 *********************************
-1.6008E-08*         2 *********************************
-1.4988E-08*         2 *********************************
-1.3969E-08*         3 **************************************************  1
-1.3051E-08*         2 *********************************
-1.2031E-08*         3 **************************************************  2
-1.1012E-08*         2 *********************************
-9.9922E-09*         2 *********************************
-8.9725E-09*         2 *********************************
-7.9529E-09*         2 *********************************
-7.0353E-09*         1 ****************
-6.0157E-09*         2 *********************************
-4.9961E-09*         1 ****************
-3.9765E-09*         1 ****************
-2.9569E-09*         1 ****************
-2.0392E-09*         1 ****************
-1.5055E-22*         1 ****************
AVERAGE GRAY LEVEL= -0.13E-07
STANDARD DEVIATION= 0.624E-08
NUMBER ELEMENTS=        40
MIN. DN=-2.60E-8
MAX. DN=0.000000

hist d1515 NLINES=10 LIMITS= (-24.e-9, -5.E-9)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.2111111138E-08
< LOW LIMIT          6 ***********
***********         15 ***************************
***********         15 ***************************
***********         19 ***********************************
***********         23 ******************************************
***********         27 **************************************************  2
***********         29 **************************************************  1
***********         25 **********************************************
***********         21 **************************************
***********         17 *******************************
***********         18 *********************************
>HIGH LIMIT         10 ******************
AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist d1515 NLINES=10 LIMITS=(-23.e-9, -5.E-9)
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.2000000017E-08
< LOW LIMIT         10 *****************
***********         11 ******************
***********         15 *************************
***********         19 ********************************
***********         23 ***************************************
***********         27 **********************************************
***********         29 **************************************************  2
***********         25 *******************************************
***********         30 **************************************************  1
***********         15 *************************
***********         11 ******************
>HIGH LIMIT         10 *****************
AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist d1515 'nocum          LIMITS=(-23.e-9, -5.E-9) 'wide
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.7058823591E-10
FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT       1   6.667  ****************************************************************************************************+1
-2.3000E-08           2.667  ****************************************+         +         +         +         +         +         +

-2.2012E-08           3.111  **********************************************    +         +         +         +         +         +

-2.1024E-08           3.556  *****************************************************       +         +         +         +         +

-1.9965E-08           4.000  ************************************************************+         +         +         +         +

-1.8976E-08       1   4.444  ******************************************************************    +         +         +         +

-1.7988E-08       1   4.889  *************************************************************************       +         +         +

-1.7000E-08       1   5.333  ********************************************************************************+         +         +

-1.6012E-08       1   5.778  **************************************************************************************    +         +

-1.5024E-08       1   6.222  *********************************************************************************************       +

-1.3965E-08       1   6.667  ****************************************************************************************************+2

-1.2976E-08       1   6.222  *********************************************************************************************       +

-1.1988E-08       1   5.778  **************************************************************************************    +         +

-1.1000E-08       1   5.333  ********************************************************************************+         +         +

-1.0012E-08       1   4.889  *************************************************************************       +         +         +

-9.0235E-09       1   4.444  ******************************************************************    +         +         +         +

-8.0353E-09           4.000  ************************************************************+         +         +         +         +

-6.9765E-09           3.556  *****************************************************       +         +         +         +         +

-5.9882E-09           3.111  **********************************************    +         +         +         +         +         +

-5.0000E-09           2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT       1   6.667  ****************************************************************************************************+
AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8      MAX. DN=0.000000

hist d1515 SPIKE=5
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.1098039216E-09
-2.8000E-08          1 ***
-2.7012E-08*         2 *******
-2.6024E-08*         3 ***********
-2.5035E-08*         4 ***************
-2.4047E-08*         5 *******************
-2.2949E-08*         6 ***********************
-2.1961E-08*         7 **************************
-2.0973E-08*         8 ******************************
-1.9984E-08*         9 **********************************
-1.8996E-08*        10 **************************************
-1.8008E-08*        11 ******************************************
-1.7020E-08*        12 **********************************************
-1.6031E-08*        13 **************************************************  4
-1.5043E-08*        14 **************************************************  2
-1.4055E-08*        15 **************************************************  1
-1.2957E-08*        14 **************************************************  3
-1.1969E-08*        13 **************************************************  5
-1.0980E-08*        12 **********************************************
-9.9922E-09*        11 ******************************************
-9.0039E-09*        10 **************************************
-8.0157E-09*         9 **********************************
-7.0275E-09*         8 ******************************
-6.0392E-09*         7 **************************
-5.0510E-09*         6 ***********************
-3.9529E-09*         5 *******************
-2.9647E-09*         4 ***************
-1.9765E-09*         3 ***********
-9.8824E-10*         2 *******
-1.0929E-22*         1 ***
AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist d1515 SPIKE=1
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =     0.1098039216E-09
-2.8000E-08          1 ***
-2.7012E-08*         2 ******
-2.6024E-08*         3 **********
-2.5035E-08*         4 *************
-2.4047E-08*         5 ****************
-2.2949E-08*         6 ********************
-2.1961E-08*         7 ***********************
-2.0973E-08*         8 **************************
-1.9984E-08*         9 ******************************
-1.8996E-08*        10 *********************************
-1.8008E-08*        11 ************************************
-1.7020E-08*        12 ****************************************
-1.6031E-08*        13 *******************************************
-1.5043E-08*        14 **********************************************
-1.4055E-08*        15 **************************************************  1
-1.2957E-08*        14 **********************************************
-1.1969E-08*        13 *******************************************
-1.0980E-08*        12 ****************************************
-9.9922E-09*        11 ************************************
-9.0039E-09*        10 *********************************
-8.0157E-09*         9 ******************************
-7.0275E-09*         8 **************************
-6.0392E-09*         7 ***********************
-5.0510E-09*         6 ********************
-3.9529E-09*         5 ****************
-2.9647E-09*         4 *************
-1.9765E-09*         3 **********
-9.8824E-10*         2 ******
-1.0929E-22*         1 ***
AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist d1515 'nohist
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist d1515 'nohist SIGMA=SD  MEAN=AVG sum=sum count=pix
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

AVERAGE GRAY LEVEL= -0.14E-07
STANDARD DEVIATION= 0.611E-08
NUMBER ELEMENTS=       225
MIN. DN=-2.80E-8
MAX. DN=0.000000

let $echo="no"
     Print average, stdev, number of pixels and sum of pixels variables
     Average    = -1.400000027019e-08
     Std Dev    = 6.110100869705e-09
     Num of PIX = 225
     Sum of DNs = -3.150000111418e-06
gen x1515 NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1
          1          3 **
          2          6 ****
          3         10 ******
          4         15 **********
          5         21 **************
          6         28 ******************
          7         36 ************************
          8         45 ******************************
          9         55 ************************************
         10         63 ******************************************
         11         69 **********************************************
         12         73 ************************************************
         13         75 **************************************************  1
         14         75 **************************************************  2
         15         73 ************************************************
         16         69 **********************************************
         17         63 ******************************************
         18         55 ************************************
         19         45 ******************************
         20         36 ************************
         21         28 ******************
         22         21 **************
         23         15 **********
         24         10 ******
         25          6 ****
         26          3 **
         27          1
AVERAGE GRAY LEVEL=  0.14E+02
STANDARD DEVIATION= 0.497E+01
NUMBER ELEMENTS=      1000
MIN. DN=         0
MAX. DN=        27

hist x1515 NB=4
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
          0          1 *
          1          3 ****
          2          6 ********
          3         10 *************
          4         14 *******************
          5         18 *************************
          6         22 ******************************
          7         26 ************************************
          8         30 *****************************************
          9         34 ***********************************************
         10         36 **************************************************  1
         11         36 **************************************************  2
         12         34 ***********************************************
         13         30 *****************************************
         14         26 ************************************
         15         22 ******************************
         16         18 *************************
         17         14 *******************
         18         10 *************
         19          6 ********
         20          3 ****
         21          1 *
AVERAGE GRAY LEVEL=  0.10E+02
STANDARD DEVIATION= 0.421E+01
NUMBER ELEMENTS=       400
MIN. DN=         0
MAX. DN=        21

gen x1515 337 364 ival=200 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** HIST Jul 02, 2016 (64-bit) -  RJB ***

Bin Width =        1.0
        200*    122668 **************************************************  1
AVERAGE GRAY LEVEL=  0.20E+03
STANDARD DEVIATION= 0.000E+00
NUMBER ELEMENTS=    122668
MIN. DN=       200
MAX. DN=       200

let $echo="no"
     Std Dev should be 0.0
 
exit
slogoff
$ Return
$!#############################################################################
