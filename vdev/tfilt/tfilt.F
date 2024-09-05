      Include   'VICMAIN_FOR'
      Subroutine  Main44
        implicit none
      External    FiltB

      integer*4 k,key,L1,L2,L3,L4,L5,L6,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3 
      integer*8 tot8a,tot8b,tot8c
      real*4  boost,dclvl,dctran,scale,offset

      Common/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
c
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
      call xvmessage(' *** TFILT version 16-Jul-2016 - (64bit) - RJB',' ')

      call FILTA
      L1 = Totlen * 4
      L2 = Totlen * 4
      L3 = Totlen * 2
      L4 = Totlen * 2
      L5 = Totlen * 2
      L6 = NPIX * 2
c      LTOT = L1 + L2 + L3 + L4 + L5 + L6

      tot8a = int8(L1)
      tot8b = int8(L3)
      tot8c = int8(L6)  
      call STACKA(8,FiltB,6,tot8a,tot8a,tot8b,tot8b,tot8b,tot8c)
c
      Return
      End
c
c********************************************************************
c
      Subroutine FiltB(SUMDN,L1,COUNT,L2,IN,L3,LAST,L4,NEXT,L5,OUT,L6)
c
        implicit none

      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 i,j,ib,il,irec2,line,nend,status
      integer*8 L1,L2,L3,L4,L5,L6
      real*4  boost,dclvl,dctran,scale,offset

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL

      INTEGER*4    SUMDN(totlen), COUNT(totlen)
      INTEGER*2  IN(totlen), LAST(totlen), NEXT(totlen), OUT(NPIX)

c      If (L1+L2+L3+L4+L5+L6 .NE. LTOT)  then
c        call xvmessage('*** STACKA Error ***', ' ')
c	call Abend
c      Endif
c
c   Copy RUNIT to wunit1 and wunit2.  Rearranging such that Line I of wunit1
c   Contins the Next (Highest) Line in the Filter Window for Line I of RUNIT,
c   and Line I of wunit2 the Last (Lowest) Line in the Window for Line I-1

c  band loop, over entire subroutine (not reflected in indentation!):
      do ib=1,nb

c  for multi-band case, re-initialize wunit2 and wunit1:
      if (ib.gt.1) then
        call xvclose( wunit2, status, ' ')
        call xvopen( wunit2, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
        call xvclose( wunit1, status, ' ')
        call xvopen( wunit1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      Do  LINE=2,NLW2
          IREC2 = NLW2+SLIN+1-LINE
          call xvread( RUNIT, IN, status, 'LINE',IREC2, 'SAMP',SSAM,
     &	   'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit2, IN, status, ' ')
      enddo

      NEND = NLO-NLW1
      call xvread( RUNIT, IN , status, 'LINE',SLIN, 'SAMP',SSAM,
     &	'NSAMPS',NSO, 'band', ib, ' ')
      LINE=1
      IF (LINE.LE.NEND) call xvwrit( wunit2, IN, status,' ')  ! USU. DONE
      IF (LINE.GT.NLW2) call xvwrit( wunit1,  IN, status,' ')  ! USU. SKIPPED
c
      Do  LINE=2,NLO
	 call xvread( RUNIT, IN, status, 'SAMP',SSAM, 'NSAMPS',NSO,
     &    'line', slin+line-1, 'band', ib, ' ')
	 If (LINE.LE.NEND) call xvwrit( wunit2, IN, status, ' ')
	 If (LINE.GT.NLW2) call xvwrit( wunit1, IN, status, ' ')
      enddo

      Do  I=1,NLW2-1
          LINE=NLO-1+SLIN-I
          call xvread( RUNIT, IN, status, 'LINE',LINE, 'SAMP',SSAM,
     &	  'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit1, IN, status, ' ')
      enddo
c
C  Close  Scratch DISK & Re-Open for INPUT:
c
      call xvclose( wunit2, status, ' ')
      call xvopen( wunit2, status, 'OP','READ','U_FORMAT','HALF',
     *   'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
c
C  Close  OUTPUT File  & Re-Open  for UPDATE
c
      call xvclose( wunit1, status, ' ')
      call xvopen( wunit1, status, 'OP','UPDATE', 'U_FORMAT','HALF',
     * 'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     * ' ')
c
c   Compute the Initial SUMDN and Count buffers from the 
c   first NLW  lines 
c   SUMDN = SUM of DN'S  GT  THRESH in each column 
c   COUNT = SUM of PIXELS GT THRESH in each column 

      call  ZIA(SUMDN, L1/4)
      call  ZIA(COUNT, L2/4)
      Do  LINE=1,NLW
	  call xvread( wunit2, IN(NSW2), status, 'LINE',LINE,
     &	 'NSAMPS',NSO, ' ')
	call  RFLCT(IN)
	Do  J=1,TOTLEN
	  If (IN(J) .GT. THRESH)  Then
            SUMDN(J) = SUMDN(J) + IN(J)
            COUNT(J) = COUNT(J) + 1
	  Endif
	enddo
      enddo
c
c    Initialize the xvreads:
c
      call xvread( wunit1,NEXT(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call  RFLCT(NEXT)
      call xvread( wunit2,LAST(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call  RFLCT(LAST)
      call xvread( RUNIT,IN(NSW2),status,'LINE',SLIN,'NSAMPS',NSO,
     &      'SAMP',SSAM, 'band', ib, ' ')
      call  RFLCT(IN)
c
C   MAIN  LINE Loop for  OUTPUT Picture
c
      DO  LINE = 1,NLO
          DNSUM = 0
          NSUM  = 0
c
c    Add up the First NSW DN:
c 
          DO J = 1,NSW
            DNSUM = DNSUM + SUMDN(J)
            NSUM  = NSUM  + COUNT(J)
 	  enddo
c
c    Apply the appropriate Filter to Current LINE 
c
         If (MODE .EQ. 1) call  HPFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 2) call  SDFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 3) call  LPFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 4) call  DFILT (IN,OUT,SUMDN,COUNT)
	 call xvwrit( wunit1, OUT, status, ' ')
         If (LINE .EQ. NLO) Go To 2000
c
c   Update SUMDN & COUNT buffers by deleting LAST line & Adding NEXT:
c
	 DO J = 1,TOTLEN
	   If (LAST(J) .GT. THRESH)  Then
	     SUMDN(J) = SUMDN(J) - LAST(J)
	     COUNT(J) = COUNT(J) - 1
	   EndIf
	   If (NEXT(J) .GT. THRESH)  Then
	     SUMDN(J) = SUMDN(J) + NEXT(J)
	     COUNT(J) = COUNT(J) + 1
	   EndIf
	 enddo

	 If (SSAM .EQ. 1)  Then	   ! READ NEXT LINE
	   call xvread( RUNIT, IN(NSW2), status,'NSAMPS',NSO,
     &      'line', slin+line, 'band', ib,  ' ')
	 Else
	   call xvread( RUNIT, IN(NSW2), status, 'SAMP',SSAM,
     &	    'NSAMPS',NSO, 'line', slin+line, 'band', ib, ' ')
	 EndIf
c
         call RFLCT(IN)
c
c    LAST   Line in Window  
c
         call xvread( wunit2,LAST(NSW2),status, 'NSAMPS',NSO, ' ')
         call RFLCT(LAST)
c
c    NEXT  Line in Window  
c
         call xvread( wunit1,NEXT(NSW2),status,'NSAMPS',NSO, ' ')
         call RFLCT(NEXT)
c
      enddo
c
C   Close Files and Exit
c
2000  Continue

c  multi-band case:  copy wunit1 to band IB of wunit3
      if (nb.gt.1) then
        do il=1,nlo
          call xvread( wunit1, in, status, 'line', il, 'nsamps',nso,
     &     ' ')
          call xvwrit( wunit3, in, status, 'line', il, 'nsamps',nso,
     &     'band', ib, ' ')
        enddo
      endif

      enddo   ! END OF BAND LOOP

      call xvclose( RUNIT, status, ' ')
      call xvclose( wunit2, status, ' ')
      call xvclose( wunit1, status, ' ')
c
      Return 
      End
c
c*************************************************************
c
      Subroutine RFLCT(BUF)
        implicit none

      INTEGER*2 BUF(*)
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3

      real*4  boost,dclvl,dctran,scale,offset      

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
     
      call  MVE(2,NSW1,BUF(NSW3),BUF(NSW1),1,-1)
      call  MVE(2,NSW1,BUF(TOTL2),BUF(TOTL1),-1,1)
c
      Return
      End
c
c******************************************************************
c
      Subroutine FILTA
        implicit none
c
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 i,j,n,idum,status,nnsw
      integer*4 boosdf,dcledf,dctrdf,nboos,ndcle,ndctr,nlwdf,nnlw
      integer*4 noffs,nscal,nswdf,nthre,ofsdf,sb,scaldf,thredf
      real*4  boost,dclvl,dctran,scale,offset

      LOGICAL*4 xvptst,HIGH,LOW,SCENE,DIVIDE

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL

      call xvunit( RUNIT,'INP',1,status, ' ')
      call xvopen( RUNIT, status,'U_FORMAT','HALF','OPEN_ACT','SA',
     * 'IO_ACT','SA', ' ')
      call xvsize(SLIN, SSAM,NLO,NSO,NX,NS, ' ')
      call xvbands(sb,nb,idum)
      call xvget( RUNIT,status,'PIX_SIZE',N, ' ')
      if (n.gt.2) call mabend(' Input must be byte or halfword!')

      call xvpcnt('OUT',i,j)
      if (nb.gt.1 .and.	i.lt.3) call mabend(
     *	 ' 3 output files required when input is multi-band')

      if (nb.eq.1) then
	call xvunit( WUNIT1,'OUT',1,status,' ')
	call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      else
	call xvunit( WUNIT3, 'OUT', 1, status, ' ')
	call xvopen( WUNIT3, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
        call xvunit( WUNIT1, 'OUT', 3, status, ' ')
        call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      call xvunit( WUNIT2, 'OUT', 2, status, ' ')
      call xvopen( WUNIT2, status,'OP','WRITE','U_FORMAT','HALF',
     * 'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     * 'SA','IO_ACT','SA', ' ')

      KEY = 2-N
      call Prnt(4,1,N,' #BYTES/PXL=.')
      MODE = 4
      NLW  = NLO/10
      NSW  = NLW
      call xvparm('NLW', NLW, NNLW, NLWDF, 0)
      call xvparm('NSW', NSW, NNSW, NSWDF, 0)
      call xvparm('THRESH', THRESH, NTHRE, THREDF, 0)
      call xvparm('SCALE' ,SCALE, NSCAL, SCALDF, 0)
      call xvparm('DCTRAN', DCTRAN, NDCTR, DCTRDF, 0)
      call xvparm('BOOST', BOOST, NBOOS, BOOSDF, 0)
      call xvparm('DCLEVEL', DCLVL, NDCLE, DCLEDF, 0)
      call xvparm('OFFSET', OFFSET, NOFFS, OFSDF, 0)
      HIGH  =  xvptst('HIGH')
      LOW   =  xvptst('LOW')
      SCENE =  xvptst('SCENE')
      DIVIDE=  xvptst('DIVIDE')

      If (HIGH)   MODE = 1
      If (LOW)    MODE = 3
      If (SCENE)  MODE = 2
      If (DIVIDE) MODE = 4

      If ((NLW/2)*2 .EQ. NLW) NLW = NLW + 1
      If ((NSW/2)*2 .EQ. NSW) NSW = NSW + 1

      If (KEY .EQ. 0)  Then
	NPIX   =    NSO
	LOWER  = -32768
	UPPER  =  32767
      Else
	NPIX   =   NSO
	LOWER  =     0
	UPPER  =   255
      EndIf

      NLW2 = (NLW+1) / 2
      NLW1 = NLW2 - 1
      NSW2 = (NSW+1) / 2
      NSW1 = NSW2 - 1
      TOTLEN = NPIX +  2*NSW1
      NSW3 = NSW2 + 1
      TOTL1 = TOTLEN - NSW1 + 1
      TOTL2 = TOTLEN - NSW2
      call  PRNT(4,1,NLW,' NLW=.')
      call  PRNT(4,1,NSW,' NSW=.')
c
c     Get  Number  Pixels/Line
c   
      Return
      End
c
c******************************************************************
c
      Subroutine HPFILT(IN,OUT,SUMDN,COUNT)

        implicit none

      INTEGER*2 IN(*),OUT(*)
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 j,jj,jk1,jk2,lowlim
      INTEGER*4 SUMDN(*),COUNT(*),UPLIM

      real*4  boost,dclvl,dctran,scale,offset,x

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
      If (IN(NSW2) .LE. THRESH) Go To 90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = (IN(NSW2) - X) * BOOST + DCTRAN*IN(NSW2) + DCLVL
      If (X .GT. UPPER)  Then
	OUT(1) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(1) = LOWER
      Else
	OUT(1) = X
      EndIf
      Go  To 100
90    OUT(1)=0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
c
      Do 200  J = LOWLIM,UPLIM
         JJ  = JJ + 1
         JK1 = J - NSW2
         JK2 = J + NSW1
         DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
         NSUM  = NSUM - COUNT(JK1) + COUNT(JK2)
         If (IN(J) .LE. THRESH) Go To 160
         If (NSUM .NE. 0) X = Float(DNSUM)/Float(NSUM)
         If (NSUM .EQ. 0) X = Float(DNSUM)
         X = (IN(J) - X) * BOOST + DCTRAN * IN(J) + DCLVL
         If (X .GT. UPPER)   Then
	   OUT(JJ)=UPPER
         ElseIf (X .LE. LOWER)  Then
	   OUT(JJ)=LOWER
         Else
	   OUT(JJ) = X
         EndIf
c
         Go To  200
160      OUT(JJ)=0
200   Continue
c
      Return
      End
c
c******************************************************************
c
      Subroutine DFILT(IN,OUT,SUMDN,COUNT)
       implicit none

      INTEGER*2 IN(*),OUT(*)
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 j,jj,jk1,jk2,lowlim
      INTEGER*4 SUMDN(*),COUNT(*),UPLIM

      real*4  boost,dclvl,dctran,scale,offset,x

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c

      IF(IN(NSW2).LE.THRESH)GO TO 90
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/FLOAT(DNSUM)
      IF(NSUM.EQ.0)X=FLOAT(NSUM)
      X = IN(NSW2)*(SCALE*X+DCTRAN)+OFFSET
      IF (X.GT.UPPER) THEN
	OUT(1)=UPPER
      ELSEIF (X.LE.LOWER) THEN
	OUT(1)=LOWER
      ELSE
	OUT(1) = X
      ENDIF
      GO TO 100
 90   OUT(1)=0
 100  CONTINUE
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      DO 200 J=LOWLIM,UPLIM
      JJ=JJ+1
      JK1=J-NSW2
      JK2=J+NSW1
      DNSUM=DNSUM-SUMDN(JK1)+SUMDN(JK2)
      NSUM=NSUM-COUNT(JK1)+COUNT(JK2)
      IF(IN(J).LE.THRESH)GO TO 160
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/FLOAT(DNSUM)
      IF(DNSUM.EQ.0)X=FLOAT(NSUM)
      X=IN(J)*(SCALE*X+DCTRAN)+OFFSET
      IF (X.GT.UPPER) THEN
	OUT(JJ)=UPPER
      ELSEIF (X.LE.LOWER) THEN
	OUT(JJ)=LOWER
      ELSE
	OUT(JJ) = X
      ENDIF
      GO TO 150
 160  OUT(JJ)=0
 150  CONTINUE
 200  CONTINUE
      RETURN
      END
c
c******************************************************************
c
      Subroutine SDFILT(IN,OUT,SUMDN,COUNT)

      INTEGER*2 IN(*),OUT(*)
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 j,jj,jk1,jk2,lowlim
      INTEGER*4 SUMDN(*),COUNT(*),UPLIM

      real*4  boost,dclvl,dctran,scale,offset,x

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
C
C
c
      call MVE(2,NPIX,IFIX(DCLVL),OUT,0,1)
      If (IN(NSW2) .LE. THRESH) Go To  90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      If (X .NE. 0) X = (IN(NSW2)-X)*SCALE/X + DCTRAN*IN(NSW2) + DCLVL
      If (X .GT. UPPER) Then
	OUT(1) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(1)=LOWER
      Else
	OUT(1) = X
      EndIf
      Go  To  100
90    OUT(1) = 0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      If (IN(J) .LE. THRESH) Go To  160
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      If (X .NE. 0) X = (IN(J)-X)*SCALE / X + DCTRAN*IN(J) + DCLVL
      If (X .GT. UPPER) Then
	OUT(JJ)=UPPER
      ElseIf (X .LE. LOWER) Then
	OUT(JJ)=LOWER
      Else
	OUT(JJ) = X
      EndIf
      Go  To  150
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      RETURN
      END
c
c******************************************************************
c
      Subroutine  LPFILT(IN,OUT,SUMDN,COUNT)

        implicit none

      INTEGER*2 IN(*),OUT(*)
      integer*4 k,key,lower,mode,nlo,nlw,nlw1
      integer*4 nlw2,npix,ns,nso,nsum,nsw,dnsum,slin,totlen,ssam
      integer*4 thresh,totl1,totl2,nx,nb,upper,nsw1,nsw2,nsw3
      integer*4 runit,wunit1,wunit2,wunit3
      integer*4 j,jj,jk1,jk2,lowlim
      INTEGER*4 SUMDN(*),COUNT(*),UPLIM

      real*4  boost,dclvl,dctran,scale,offset,x

      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2

      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
c
      If (IN(NSW2) .LE. THRESH) Go To 90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = BOOST*X + OFFSET
      If (X .GT. UPPER)  Then
	OUT(1) = UPPER
      ElseIf  (X .LE. LOWER)  Then
	OUT(1) = LOWER
      Else
	OUT(1) = X
      EndIf
      Go To  100
90    OUT(1) = 0
100   Continue
c
c
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      If (IN(J) .LE. THRESH) Go To 160
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = BOOST*X + OFFSET
      If (X .GT. UPPER)  Then
	OUT(JJ) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(JJ) = LOWER
      Else
	OUT(JJ) = X
      EndIf
      Go to 150 
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      Return
      End
