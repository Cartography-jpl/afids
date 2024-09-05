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
