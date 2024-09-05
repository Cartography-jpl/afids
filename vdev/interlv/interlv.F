      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      CHARACTER*8 ORG                                      
      CHARACTER*5 DIRCTION
      CHARACTER*8 INTERP,FORMAT
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      INTEGER*4 SL,SS,SB,NL,NS,NB,INPUT,OUTPUT,STAT,CNT
cc      COMMON /INSZ/ NL,NS,NB
      INTEGER*4 SAMP,I,J,nlo,nso,nli,nsi
c      INTEGER*4 BUF(16384) 
      INTEGER*4 FIRSTL, LASTL, FIRSTS, NSAMPS, LSAMPS
      INTEGER*4 PIXOFF,icode     
      REAL*4    BUFF1(1024,1024),BUFF2(1024,1024) 

	do i = 1,1024
	  do j=1,1024
	     buff1(i,j) = 0
	     buff2(i,j) = 0
          enddo
        enddo
	call xvmessage ('**INTERLV - Sep 15, 2013 (64-bit) rjb',' ')
      CALL XVUNIT(INPUT,'INP',1,STAT,' ')
      CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA',' ' )
	call xvget (INPUT,stat,'FORMAT',FORMAT,' ')
	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
c      CALL XVBANDS(SB,NB)
	sb=1
	nb=1
      CALL XVGET(INPUT,STAT,'ORG',ORG,' ')
	call xvclose(input,stat,' ')
	call xvopen(input,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')
C        GET SIZE INFORMATION AND CHECK
      call xvsize(sl,ss,nlo,nso,nli,nsi)
      if(sl+nlo-1 .gt. nli) then
         call xvmessage('??E - Number of lines requested exceeds input size',' ')
         call abend
      endif
      if(ss+nso-1 .gt. nsi) then
         call xvmessage('??E - Number of samples requested exceeds input size',' ')
         call abend
      endif
C
C        OPEN OUTPUT DATA SET

      call xvunit(output,'OUT',1,stat,' ')
        call xvopen(output,stat,'OP','WRITE','U_NL',nlo,'U_NS',nso,
     & 'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),'U_NB',NB,'U_ORG',ORG,' ')		!,FMT(OUTCODE),' ')
C--- GET PIXEL OFFSET VALUE AND CHECK IF VALID
      CALL XVP('PIXOFF',PIXOFF,CNT)
      IF (PIXOFF .LT. 0) THEN
         CALL xvmessage ('??E - Parameter error-pixel offset cannot be NEGATIVE',' ') 
         CALL xvmessage (' **INTERLV TASK CANCELLED',' ')
         CALL ABEND
      END IF
      IF (PIXOFF .GT. NSI-1) THEN
         CALL xvmessage ('??E - Parameter error-pixel offset must be greater than ns',' ') 
         CALL xvmessage (' **INTERLV TASK CANCELLED',' ')
         CALL ABEND
      END IF

C--- GET DIRECTION AND INTERP VALUES 
      CALL XVP('DIRCTION',DIRCTION,CNT)
      CALL XVP('INTERP',INTERP,CNT)
                                 
	NS=NSI
	NL=NLI
C--- BEGINNING OF INTERLV PROGRAM
C--- INITIALIZE LOOP LIMITING VARIABLES
c--- First copy input into output

	do i=sl,nl
	    CALL XVREAD(INPUT,BUFF1(1,i),STAT,'LINE',i,
     +                     'NSAMPS',NS,'BAND',SB,' ')
	    do j=1,ns
	       buff2(j,i) = buff1(j,i)
            enddo
	enddo
      FIRSTL = SL + 3
      LASTL = SL + NL - 1   
      NSAMPS = NS - PIXOFF
      LSAMPS = NS + PIXOFF
      IF (NSAMPS .GT. 0) THEN      

C--- THERE IS AN OFFSET DISCREPANCY, SO CONTINUE
         IF (PIXOFF .GT. 0) THEN

C--- INSERT IF LESS THAN 3 LINES CONDITIONS HERE

c            IF (NL .GE. 3) THEN                    
c
C--- READ FIRST THREE LINES INTO BUFF1,2,3 RESPECTIVELY
c               CALL XVREAD(INPUT,BUFF1,STAT,'LINE',SL,'SAMP',SS,
c     +                     'NSAMPS',NS,'BAND',SB,' ') 
c               CALL XVREAD(INPUT,BUFF2,STAT,'LINE',SL+1,'SAMP',SS,
c     +                     'NSAMPS',NS,'BAND',SB,' ') 
c               CALL XVREAD(INPUT,BUFF3,STAT,'LINE',SL+2,'SAMP',SS,
c     +                     'NSAMPS',NS,'BAND',SB,' ') 
C            END IF 

C--- OTHERWISE, INVALID NL    
C           ELSE GO TO ERROR
C--- WRITE OUT THE FIRST LINE SINCE NO MODIFICATIONS ARE REQUIRED HERE
c      CALL XVWRIT(OUTPUT,BUFF1,STAT,' ')

C--- DETERMINE WHETHER TO INTERPOLATE THE 0 DN PIXELS WITH SURROUNDING PIXELS
C INSERT    IF (INTERP .EQ. 'INTERP') ...    

cc            IF (INTERP .NE. 'INTERP') THEN   
C--- PAD OFFSET WITH ZEROS SINCE NO INTERPOLATION
               IF (DIRCTION .EQ. 'LEFT') THEN
                  FIRSTS = NSAMPS + 1   

C--- WRITE AS MUCH LINE INTO BUFF2 AS CAN
c		print *,'pixoff = ',pixoff
		do i=2,nl,2
c		 print *,'i = ',i
                  DO SAMP = 1, NSAMPS        			!ns-pixoff   
                     BUFF2(SAMP,i) = BUFF1((SAMP+PIXOFF),i)
c	             if (i.lt.6) print *,'i,buff1,buff2 = ',i,BUFF1(samp,i),buff2(samp,i)
                  END DO                

C--- PAD THE END OF LINE WITH ZEROS        
                  DO SAMP = FIRSTS, NS 
                     BUFF2(SAMP,nl)=0
                  END DO                            
                enddo


C--- WRITE BUFF2 TO OUTPUT
	        do i=1,nl
                   CALL XVWRIT(OUTPUT,BUFF2(1,i),STAT,' ')
		enddo


C---  ADD REMAINING LINES TO THE FILE
               ELSE IF (DIRCTION .NE. 'LEFT') THEN                

	        FIRSTS = PIXOFF + 1 
                do i=2,nl,2
                  DO SAMP = 1, PIXOFF
                     BUFF2(SAMP,i) = 0
		  enddo
		  do samp = firsts,ns
		      buff2(samp,i) = BUFF1(SAMP-PIXOFF,i)
c                    BUFF2(SAMP) = BUFFX(SAMP)
c                        print *,'samp,Buff2(samp),buff1(samp) = ',samp,Buff2(samp,nl),buffx(samp,nl)    
                  enddo

                enddo
C--- WRITE BUFF2 AND BUFF3 TO OUTPUT
                do i=1,nl
                   CALL XVWRIT(OUTPUT,BUFF2(1,i),STAT,' ')
                enddo


C (INVALID DIRECTION)   DIRECTION WAS NEITHER LEFT NOR RIGHT 
               ELSE
		 GO TO 10 
               END IF                     
C (INVALID INTERP)   INTERP WAS NEITHER INTERP NOR NOINTERP

ccc            END IF                     

C  (INVALID PIXOFF)            
         ELSE 
            GO TO 10 
         END IF     
C  (NSAMPS TOO SMALL)
      ELSE 
         GO TO 10 
      END IF

10    CONTINUE
C      WRITE APPROPRIATE ERROR MESSAGES
      CALL XVCLOSE(INPUT,stat,' ')
      CALL XVCLOSE(OUTPUT,stat,' ')
      RETURN
      END             

