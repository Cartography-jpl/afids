C
C    REVISION HISTORY
C       1-85  SP   EXPANDED BUFFER SIZES TO 60000 AND REMOVED MESSAGE ABOUT
C                  PROCESSING FIRST 10000 BYTES IF IMAGE LINE TOO BIG. NOW
C                  USES 2 BUFFERS INSTEAD OF 3.
C       1-85  SP   MOVED COUNTING OF DIFFERENCES BEFORE COMPARING AGAINST
C                  MINDN BECAUSE MINDN IS 0 FOR BYTE DATA.
C       1-85  SP   ADDED MOD PARAMETER FOR BYTE DATA.
C       1-85  SP   ADDED WCHECK CALL AFTER WRITES.
C       1-85  SP   ADDED CODE TO AVOID INTEGER OVERFLOW ON EXTREME HALFWORD
C                  VALUES.
C       1-85  SP   CONVERTED TO VICAR2 SUBROUTINE CALLS.  ( U_FORMAT and 
C                  optional parameters in XVREAD and XVWRIT avoided because
C                  of apparent speed problems.)
C       1-85  SP   CHANGED MESSAGE 'NUMBER OF NONZERO PIXELS' TO 'NUMBER OF
C                  DIFFERENT PIXELS'.
C       1-85  SP   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 USES
C                  ONLY THE FORMAT IN LABEL.
C       1-85  SP   MADE OUTPUT FILE OPTIONAL TO ALLOW GREATER SPEED.
C      12-91  SP   REPLACED PRNT CALLS WITH CALLS TO PRNINT AND PRNTREAL
C                  FOR SIMPLICITY.
C      12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
C       9-92  SP   Made buffer size 200000 bytes. Modified to handle 
C                  all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
C                  CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
C       3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
C                  Added ability to handle 3d files if SIZE field defaulted
C                  and no output file specified.
C       7-94  SP   Allowed format="WORD" as alternative to HALF.
C       8-03  lwk  removed restrictions on NB, added SB parameter;  use of
C		   optionals in XVREAD/WRIT is no longer a speed issue.
C      12-03  lwk  added checks on size/format of input files
C       6-04  lwk  allow for deviant Format types WORD & COMPLEX; removed 
c		mabend at BIP check because SIT objected to it(!)  (I don't
c		agree, and have retained all the other mabend calls, but
c		there's no need to make an issue of it)
c	2-15  rjb  Made 64-bit clean to use large images using stacka_big
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

	implicit none
        external difpicd,difpicc                        !stacka 

        integer*4 stat,icode1,icode2,nods
        integer*4 ounit,iunit1,iunit2,nl1,ns1,nb1,nl2,ns2,nb2
	integer*4 sl,ss,nlo,nso,nli,nsi,sb,nbo,nbi,el,es,eb,nb

        integer*8  ll
      	character*8 orgin1, orgin2
        character*8 fmt(6)/'BYTE','HALF','FULL','REAL','DOUB','COMP'/
        character*8 format1,format2

C============================================================================
C
      	call xvmessage('DIFPIC version 12-Jul-2016 - rjb (64-bit)', ' ')

C
C  OPEN INPUTS & OUTPUT FOR SEQUENTIAL I/O
C
      	call xvunit(iunit1,'INP',1,stat,' ')
        call xvopen(iunit1,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',' ')
      	call xvget(iunit1,stat,'FORMAT',FORMAT1,'ORG',orgin1,' ')
        call xvget(iunit1,stat,'NL',nl1,'NS',ns1,'NB',nb1,' ')
        icode1 = 0
        if (format1.eq.'BYTE') icode1=1
        if (format1.eq.'HALF'.or.format1.eq.'WORD') icode1=2
	if (format1.eq.'WORD') format1 = 'HALF'
        if (format1.eq.'FULL') icode1=3
        if (format1.eq.'REAL') icode1=4
        if (format1.eq.'DOUB') icode1=5
	if (format1(1:4).eq.'COMP') icode1=6
        if (icode1.eq.0) then
                call xvmessage('??E - Unknown data format for 1st input image',' ')
                call abend
        endif
        if (orgin1.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert 1st input to BSQ')

        call xvclose(iunit1,stat,' ')
c	reopen 1st input
        if (format1(1:4).eq.'COMP') then
            call xvopen(iunit1,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',
     +          'I_FORMAT',fmt(icode1),'U_FORMAT',fmt(6),' ')
        else
            call xvopen(iunit1,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',
     +            'I_FORMAT',fmt(icode1),'U_FORMAT',fmt(5),' ')
        endif

c	check 2nd input
      	call xvunit(iunit2,'INP',2,stat,' ')
        call xvopen(iunit2,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',' ')
      	call xvget(iunit2,stat,'FORMAT',FORMAT2,'ORG',orgin2,' ')
        call xvget(iunit2,stat,'NL',nl2,'NS',ns2,'NB',nb2,' ')
        icode2 = 0
        if (format2.eq.'BYTE') icode2=1
        if (format2.eq.'HALF'.or.format2.eq.'WORD') icode2=2
	if (format2.eq.'WORD') format2 = 'HALF'
        if (format2.eq.'FULL') icode2=3
        if (format2.eq.'REAL') icode2=4
        if (format2.eq.'DOUB') icode2=5
        if (format2(1:4).eq.'COMP') icode2=6
        if (icode2.eq.0) then
                call xvmessage('??E - Unknown data format for 2nd input image',' ')
                call abend
        endif
        if (orgin2.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert 2nd input to BSQ')

        call xvclose(iunit2,stat,' ')
c	reopen 2nd input
	if (format2(1:4).eq.'COMP') then
	    call xvopen(iunit2,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',
     +		'I_FORMAT',fmt(icode2),'U_FORMAT',fmt(6),' ')
	else
      	    call xvopen(iunit2,stat,'OPEN_ACT','SA','IO_ACT','SA', 'OP', 'READ',
     +            'I_FORMAT',fmt(icode2),'U_FORMAT',fmt(5),' ')
	endif

      ! just look at 4 bytes to make COMP same as COMPLEX
      	if (format1(1:4).ne.format2(1:4)) call mabend(
     1   '??E - Input files must have same format')

c	setup for multiband images
      	call xvsize( sl, ss, nlo, nso, nli, nsi )   ! GET SIZE PARAMETER.
      	call xvbands( sb, nbo, nbi)
      ! nbi is from 1st input label
      ! nbo is from param NB or BANDS, whichever is non-zero;  else zero
      	if ((sb+nbo-1).gt.nbi) then
            call xvmessage(' NB too large, reduced to fit input',' ')
            nbo = nbi-sb+1
        endif
        if (nbo.le.0) nb = nbi-sb+1

       if ( (nl1.ne.nl2.and.sl.eq.1.and.nlo.eq.nli) .or.
     +     (ns1.ne.ns2.and.ss.eq.1.and.nso.eq.nsi) .or.
     +     (nb1.ne.nb2.and.sb.eq.1.and.nbo.eq.nbi) ) call mabend(
     +'??E - Files have different dimensions, specify SIZE/BANDS parameter!')

       el=sl+nlo-1
       es=ss+nso-1
       eb=sb+nbo-1

C	open output if requested
        call xvpcnt('OUT',nods)         !get number of output data sets

	if (nods .gt. 0) then
	    call xvunit(ounit,'OUT',1,stat,' ')
	    if (format1(1:4).eq.'COMP') then
            	call xvopen(ounit,stat,'OPEN_ACT','SA','IO_ACT','SA','OP', 'WRITE',
     +          'I_FORMAT',fmt(icode1),'U_FORMAT',fmt(6),'U_NL', nlo, 'U_NS', nso, 'U_NB', nbo,' ')
            else
            	call xvopen(ounit,stat,'OPEN_ACT','SA','IO_ACT','SA','OP', 'WRITE',
     +            'I_FORMAT',fmt(icode1),'U_FORMAT',fmt(5),'U_NL', nlo, 'U_NS', nso, 'U_NB', nbo,' ')
            endif
	endif

        ll = ns1*8    !real*8 size in bytes

c       print *, "nl   ns  = ll,kk,jj = ",nl,ns, ll,kk,jj       
        if (format1(1:4).eq.'COMP') then
             call stacka_big (14,difpicc,2,ll,ll,ns1,sl,ss,el,es,sb,eb,iunit1,iunit2,nods,ounit)
c               call horzavg (10,horzavg,3,ll,kk,kk,i8window,nl,ns,iunit,ounit)
c		  subroutine horzavg (iimage,ll,avg,jj,adj,kk,i8window,nl,ns,iunit,ounit)

        else
             call stacka_big (15,difpicd,2,ll,ll,ns1,sl,ss,el,es,sb,eb,iunit1,iunit2,nods,ounit,format1)
c               call vertavg (iunit,ounit,nl,ns,window)
        endif

        call xvclose(iunit1,stat,' ')
	call xvclose(iunit2,stat,' ')

        if (nods.gt.0 ) call xvclose(ounit,stat,' ')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICD(BUF1,ll,BUF2,mm,ns1,sl,ss,el,es,sb,eb,
     .iunit1,iunit2,nods,ounit,format1)
c
c	FIND differences in pixel values line by line in both input images
c	put differences in output file if requested
c
	implicit none
	integer*8 ll,mm
      	real*8     BUF1(ns1),BUF2(ns1)
	integer*4  sl,ss,el,es,sb,eb,iunit1,iunit2,nods,ounit
      	integer*4  npix, npos, nneg, irec, il, ib, ilo, ibo, ii, j
 	integer*4  ns1,nl, ns, nb, stat
     	real*8     r, iptot, intot
	integer*4    ar
	character*8 format1
      	logical*4 modflag
      	logical*4 XVPTST

C==================================================================
      MODFLAG = XVPTST( 'MOD' )     ! MOD ONLY FOR BYTE DATA.

      iptot=0.
      intot=0.
      npix=0
      npos=0
      nneg=0
      irec=0
c	print *, "SB, EB = ",SB,EB
      if (nods .eq. 0)   then
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            call xvread(iunit1, BUF1, stat, 'LINE', IL, 'BAND', IB,' ' )
            call xvread(iunit2, BUF2, stat, 'LINE', IL, 'BAND', IB,' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
	call retval (npix)
        RETURN
      endif

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          call xvread( iunit1, BUF1, stat, 'LINE', IL, 'BAND', IB, ' ' )
          call xvread( iunit2, BUF2, stat, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)
c	    print *,'BUF1(J)  BUF2(J)  R = ',BUF1(J), BUF2(J), R
            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF
            ENDIF
c	put in these tests because if r<0 then r = 1 for byte
c	integer*4  2,147,483,647.0
c	double precision floating point 4,503,599,627,370,496
	    if (format1.eq.'FULL') then
c		print *, "in R = ",r
c	   the following diddling is due to the fact that
c		r > 2147483647.0 test always gives r=2147483648.0
		if (r.gt.2147483647.0) then
			r = 2147483647.0
			ar = r
			r = ar
c		print *,"   R = ",r,ar
		endif
		if (r.lt.-2147483648.0) r = -2147483648.0
	    elseif (format1.eq.'HALF') then
		if (r.gt.32767.0) r = 32767.0
		if (r.lt.-32768.0) r = -32768.0
	    elseif (format1.eq.'BYTE') then
		if (r.gt.255.0) r = 255.0
		if (modflag) then
		    if (r .lt. 0.0) r = 256 + r
		else 
		    if (r .lt. 0.0) r = 0
		endif
	    endif
c	    print *,"   R = ",r
            BUF1(II) = R
c		print *,"   R = ",r
            II = II + 1
          END DO
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          call xvwrit( ounit, BUF1, stat, 'LINE', ILO, 'BAND', IBO,' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO
C
      IF (NPOS.NE.0)
     .  CALL PRNTREAL(SNGL(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(SNGL(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(SNGL(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')
      npix = npos+nneg      
      call retval (npix)
      RETURN
      END
C##################################################################
      SUBROUTINE DIFPICC(BUF1,ll,BUF2,mm,ns1,sl,ss,el,es,sb,eb,
     . iunit1,iunit2,nods,ounit)
c
c       FIND differences in pixel values line by line in both input images
c       put differences in output file if requested
c	(For complex data sets)
c
	implicit none
	integer*8  ll,mm
      	complex*8   BUF1(*),BUF2(*)
       	integer*4  sl,ss,el,es,sb,eb,iunit1,iunit2,nods,ounit
       	integer*4  npix, npos, nneg, irec, il, ib, ilo, ibo, ii, j
       	integer*4  ns1,nl, ns, nb, stat, dumm
       	complex*8  r, iptot, intot
C==================================================================
	dumm=ll		! to prevent compiler warnings
	dumm=mm
      IPTOT=(0., 0.)
      INTOT=(0., 0.)
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      if (nods .eq. 0)   then
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD(iunit1, BUF1, stat, 'LINE', IL, 'BAND', IB,' ')
            CALL XVREAD(iunit2, BUF2, stat, 'LINE', IL, 'BAND', IB,' ')
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
	call retval (npix)
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE
      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD(iunit1, BUF1, stat, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD(iunit2, BUF2, stat, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(BUF1(J) .NE. BUF2(J) )THEN
              IF(REAL(R).GE.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(REAL(R).LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( ounit, BUF1, stat, 'LINE', ILO, 'BAND', IBO,' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)CALL PRNT(10,1,IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)CALL PRNT(10,1,INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNT(10,1,(IPTOT+INTOT)/(NB*NL*NS),' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')
      npix = npos+nneg
      call retval (npix)

      RETURN
      END
C##################################################################

      SUBROUTINE PRNTINT( IVAL, TITLE )
C
C     PURPOSE: PRNTINT prints the INTEGER value IVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
	implicit none
      INTEGER*4     IVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER*4     L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

C  IF 4 DIGITS ARE ENOUGH USE 4 DIGITS; ELSE USE 11.

      IF (-999 .LE. IVAL .AND. IVAL .LE. 9999)  THEN
         WRITE( BUF(L+1:L+4), 9040) IVAL
         N = L+4
      ELSE
         WRITE( BUF(L+1:L+11), 9110) IVAL
         N = L+11
      END IF

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9040  FORMAT( I4 )
9110  FORMAT( I11)              ! MAXIMUM LENGTH FOR 32-BIT INTEGER
      END
C###################################################################
      SUBROUTINE PRNTREAL( RVAL, TITLE )
C
C     PURPOSE: PRNTREAL prints the REAL value RVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
	implicit none
      REAL*4        RVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER*4     L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

      WRITE( BUF(L+1:L+13), 9130) RVAL
      N = L+13

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9130  FORMAT( G13.6)              ! 6 SIGNIFICANT DIGITS IN FIXED POINT
                                  ! FOR MAGNITUDES FROM .1 TO 999999.
                                  ! OTHERWISE IN EXPONENTIAL FORMAT.
      END
C###################################################################
	subroutine retval(npix)
c
        implicit none
        include 'pgminc'
        integer*4  vblock(xprdim),stat,npix

	call xqini( vblock, xprdim, xabort)
	call xqintg (vblock,'diffpix',1,npix,xadd, stat)
	Call xvqout( vblock, stat)

        return
        end

