       PROGRAM  FASTMOS
C#######################################################################
C  NAME OF ROUTINE
C      FASTMOS ( FAST MOSaic )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program FASTMOS takes input images and mosaics them together to form
C      an output image.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    NOVEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL FASTMOS PROGRAM BY
C      W. D. BENTON
C
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    11-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    11-83  SP   ADDED LABEL PROCESSING OF INPUT FILE BY USING LABELC
C                TO FIND THE DATA FORMAT AND USING FORMAT IN LABEL AS DEFAULT.
C    11-83  SP   ADDED BYTE PARAMETER SO USER CAN OVERRIDE LABEL WHEN LABEL 
C                SAYS HALF.
C    11-83  SP   REMOVED CODE FOR UNDOCUMENTED 'NOABORT' KEYWORD.
C    11-83  SP   MODIFIED SO THAT RDCHEK AND WRCHEK USED FOR I/O ERROR MESSAGES
C    11-83  SP   ADDED LABEL PROCESSING OF OUTPUT FILE TO ALWAYS INCLUDE 
C                CORRECT DATA FORMAT IN LABEL. 
C    11-83  SP   MODIFIED PROGRAM SO THAT SECOND AND FOURTH OFFSET VALUES REFER
C                TO PIXEL NUMBERS INSTEAD OF SAMPLE NUMBERS FOR HALFWORD DATA.
C    11-83  SP   CORRECTED PROGRAM TO NOT PASS NEGATIVE LENGTHS TO READ WHEN
C                INPUT IMAGE DOES NOT INTERSECT WITH OUTPUT IMAGE.
C    11-83  SP   CHANGED PROGRAM TO NOT ASSUME AT LEAST ONE PIXEL WILL BE
C                NIBBLED EACH TIME WHEN EDGING USED.
C     1-84  SP   MADE ISIZE IN STACKA CALL AT LEAST 360*NRECIN+5000.
C     5-84  SP   INCREASED ISIZE SINCE NRECIN IS NOW 0.
C    10-84  SP   CONVERTED TO VICAR2 CALLS.  MISCELLANEOUS RELATED CHANGES.
C                (USE STATCH INSTEAD OF CHECKIT.  MADE OFFPARS IN-LINE.)
C    10-84  SP   DELETED FORMAT PARAMETER TO AVOID CONFUSION.  FORMAT NOW 
C                ALWAYS TAKEN FROM VICAR LABEL.
C    10-84  SP   CHANGED TO ALLOW UP TO 48 INPUT IMAGES.
C    10-84  SP   CHANGED TO NOT USE U_FORMAT HALF AND A NUMBER OF RELATED 
C                CHANGES. (IPIXSIZE NOW PASSED TO MOS AND EDGE).  THIS CHANGE
C                IS FOR SPEED.
C    10-84  SP   CHANGED INBUFS FROM A 2- TO A 1-DIMENSIONAL ARRAY FOR SPEED.
C    10-84  SP   MODIFIED CODE FOR OVERLAY MODE TO SKIP DO LOOP IF DN FOR FIRST
C                IMAGE >= THRESH PARAMETER.
C    10-84  SP   ADDED CHECK TO VERIFY THAT INPUT FILE IS EITHER BYTE OR HALF.
C     6-87  SP   MODIFIED TO ALLOW FOR THRESH VALUES < 1 AS REQUESTED BY
C                GLENN GARNEAU.  NOW BUFFERS ARE ZEROED WITH A LOGICAL
C                ZERO VALUE (L0) WHICH IS MIN(0,THRESH-1).
C     9-91  SP   CORRECTED PROBLEM CAUSED BY MODIFICATION OF 6-87. FOR CASE
C                OF BYTE DATA AND THRESH=0, L0 WAS 255.  CHANGED L0 TO 0.
C     4-93  SP   Made portable for UNIX.  Changed code to use BYTE2INT AND
C                INT2BYTE for converting between BYTE and INTEGER.  ADDED
C                SUBROUTINE FASTFILL in place of some non-portable MVE calls.
C                Added PROGRESS parameter to print percentage DONE during 
C                execution.
C     8-03  NTT  Enabled for 3D images.
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      fastmos INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      fastmos INP=(a...) OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      fastmos (a...) b (sl,ss,nl,ns) optional parameters
C      fastmos (a...) b optional parameters
C
C       Here '(a...)' represents the input image file names,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      MMODE  - Mosaicking mode -- OVERLAY, AVERAGE, MOD, MAX, or MIN.
C      THRESH - Threshold used for mosaicking.
C      EDGE   - for edging.
C      NTHRESH- Threshold for edging on both left and right.
C      LTHRESH- Threshold for edging on left.
C      RTHRESH- Threshold for edging on right.
C      NSEQ   - Number of sequential pixels which must satisfying edging 
C               threshold criteria at edge of scene data.
C      NIBBLE - Number of pixels to remove beyond edge of scene data for
C               edging on both left and right.
C      LNIBBLE- Number of pixels to remove beyond edge of scene data for
C               edging on left.
C      RNIBBLE- Number of pixels to remove beyond edge of scene data for
C               edging on right.
C      NINCR  - If NINCR=n, then scanning for edge of scene data will check
C               every nth pixel.
C      OFF1   - Offset values for first input image.
C      OFF2,OFF3,... - Offset values for other input images.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input and output images must be byte or halfword data.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by user and calls MOSAICIT via
C      STACKA to perform mosaicking.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     NOVEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM FASTMOS.
C      
C  CALLED BY
C      FASTMOS
C  SUBROUTINES CALLED
C      The library routines  ABEND, XVMESSAGE, STACKA, XVCLOSE, XVGET, XVPARM,
C      XVPCNT, XVOPEN, XVSIZE, XVUNIT.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   13 NOV 78    ...WDB...    INITIAL RELEASE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  THIS PROGRAM FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C  VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

        implicit none
      EXTERNAL MOSAICIT
 
      COMMON /C1/ PAR, HALF, ITHRESH, IB, MTYP, NSEQ, LNIB, RNIB,
     &            LTHR, RTHR, NIBINC, OFF, IPIXSIZE, ISIZE
      LOGICAL*4 ABORTFLAG, HALF
      LOGICAL*4 XVPTST
      INTEGER*4 ISB,NB,IB,IDEF,IDSRN,IDUM1,IDUM2,IND,INFILE
      INTEGER*4 IPIXSIZE,ISL,ISSAMP,ITERMCODE
      INTEGER*4 ITHRESH,IVALS,MTYP,NBI,NI,NIBINC,NL,NPIXEL
      INTEGER*4 NSEQ,NTHR
      INTEGER*4   PAR(100), LNIB, RNIB, LTHR, RTHR, OFF(4,48)
      INTEGER*4 XVPIXSIZEU
      INTEGER*8 ISIZE

      CHARACTER*8 FMT
      CHARACTER*5 OFFC(48) 
      CHARACTER*3  ORGIN
      DATA OFFC/ 
     .           'OFF1',  'OFF2', 'OFF3', 'OFF4', 'OFF5', 
     .           'OFF6',  'OFF7', 'OFF8', 'OFF9', 'OFF10', 
     .           'OFF11', 'OFF12', 'OFF13', 'OFF14', 'OFF15', 
     .           'OFF16', 'OFF17', 'OFF18', 'OFF19', 'OFF20', 
     .           'OFF21', 'OFF22', 'OFF23', 'OFF24', 'OFF25', 
     .           'OFF26', 'OFF27', 'OFF28', 'OFF29', 'OFF30', 
     .           'OFF31', 'OFF32', 'OFF33', 'OFF34', 'OFF35', 
     .           'OFF36', 'OFF37', 'OFF38', 'OFF39', 'OFF40', 
     .           'OFF41', 'OFF42', 'OFF43', 'OFF44', 'OFF45', 
     .           'OFF46', 'OFF47', 'OFF48'                   /

     

C
C=================START OF EXECUTABLE CODE===============================

C   INITIALIZE VARIABLES

        CALL XVMESSAGE ('** fastmos - 13-Jul-2016 - (64-bit) - rjb',' ')
      IB=0
      MTYP=0
      ITHRESH=1
      NSEQ=8
      LNIB=4
      RNIB=LNIB
      NIBINC=1
      LTHR=32769
      RTHR=32769
      NTHR=32769
      CALL MVE(4,4*48,0,OFF,0,1)      ! USE MVE FOR ZEROING OFF ARRAYS.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND ,' ')
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFILE,IND,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  '??E- BIP files not supported, use TRAN to convert to BSQ')     

c      ISL: start line, ISSAMP: start sample, NL: no. lines, NPIXEL: no. samples
      CALL XVSIZE( ISL, ISSAMP, NL, NPIXEL,idum1,idum2 )  ! SIZE PARAMETERS.
      CALL XVBANDS( ISB, NB, NBI)

      IF ( ISB .GT. NBI ) CALL MABEND(
     +  '??E- SB is greater than the total number of bands')
                 
      IF ( ISB + NB - 1 .GT. NBI) THEN
         CALL XVMESSAGE('??W-  Number of bands truncated', ' ') 
         NB = NBI + 1 - ISB
      ENDIF

      CALL XVGET( INFILE, IND, 'FORMAT', FMT, ' ') 
      IND = XVPIXSIZEU( IPIXSIZE, FMT, INFILE)  !PIXEL SIZE IN BYTES

      IF (IPIXSIZE .EQ. 2)   THEN
          HALF = .TRUE.
      ELSE IF (IPIXSIZE .EQ. 1)  THEN
          HALF = .FALSE.
      ELSE
          CALL XVMESSAGE( '??E- INPUT FILE NOT BYTE OR HALFWORD.',' ')
          CALL ABEND
      END IF
C    get number of input files
      CALL XVPCNT( 'INP', NI )
      CALL XVCLOSE( INFILE, IND ,' ')
 
C  NOW LOOK AT PARAMETERS ENTERED BY USER.

      IF ( XVPTST('EDGE') )   IB = 1

      CALL XVPARM( 'THRESH', ITHRESH, IVALS, IDEF,1 )
      IF ( ITHRESH .LT. 0  .AND. .NOT. HALF)  
     .     CALL MABEND( '??E- NEGATIVE THRESH NOT VALID FOR BYTE DATA')

      CALL XVPARM( 'NSEQ', NSEQ, IVALS, IDEF ,1)

      CALL XVPARM( 'NIBBLE', PAR, IVALS, IDEF,1 )
          LNIB = PAR(1)
          RNIB = LNIB

      CALL XVPARM( 'LNIBBLE', PAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          LNIB = PAR(1)
      END IF

      CALL XVPARM( 'RNIBBLE', PAR, IVALS, IDEF ,1)
      IF (IVALS .GT. 0)   THEN
          RNIB = PAR(1)
      END IF

      CALL XVPARM( 'NTHRESH', PAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          NTHR = PAR(1)
      END IF

      CALL XVPARM( 'LTHRESH', PAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          LTHR = PAR(1)
      END IF

      CALL XVPARM( 'RTHRESH', PAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          RTHR = PAR(1)
      END IF

      IF ( XVPTST('OVERLAY') )     MTYP = 0
      IF ( XVPTST('AVERAGE') )     MTYP = 1
      IF ( XVPTST('MOD' ) )        MTYP = 2
      IF ( XVPTST('MAX' ) )        MTYP = 3
      IF ( XVPTST('MIN' ) )        MTYP = 4

      CALL XVPARM( 'NINCR', NIBINC, IVALS, IDEF,1 )

c  offset parameters must have 2 or 4 values
      DO IDSRN = 1, NI                  ! GET OFFSET PARAMETERS.
         CALL XVPARM( OFFC(IDSRN), PAR, IVALS, IDEF,4 )
             IF ( IVALS .NE. 2  .AND.  IVALS .NE. 4 ) THEN 
                CALL XVMESSAGE('??E- WRONG NUMBER OF VALUES IN OFFSET PARAM',' ') 
                CALL ABEND
             ENDIF
             OFF(1,IDSRN) = 1 - PAR(1)
             OFF(2,IDSRN) = 1 - PAR(2)

             IF ( IVALS .EQ. 4 )  THEN
                 OFF(3,IDSRN) = PAR(3)
                 OFF(4,IDSRN) = PAR(4)
             END IF
      END DO


      IF(NTHR .EQ. 32769) NTHR=ITHRESH
      IF(LTHR .EQ. 32769) LTHR=NTHR
      IF(RTHR .EQ. 32769) RTHR=NTHR

C  CALL SPECIAL LIBRARY SUBROUTINE STACKA TO ALLOCATE THE NECESSARY BUFFER
C  AND TO CALL SUBROUTINE MOSAICIT. (WE ALLOCATE NI BUFFERS TOGETHER IN A
C  TWO-DIMENSIONAL ARRAY.)
C     ipixsize = 1 (byte), 2(half) 
c     npixel = num samples
c     ni = number of data sets
c
      ISIZE = IPIXSIZE*NPIXEL*NI
        print *,"IPIXSIZE = ",IPIXSIZE,"  NPIXEL = ",NPIXEL," NI = ",NI
        print *,"ISIZE = ",ISIZE 
      CALL STACKA_big( 9, MOSAICIT, 1, ISIZE, NL, NPIXEL, NB, NI,     
     &             ABORTFLAG, ITERMCODE)

                           ! NORMAL RETURN POINT FROM STACKA AFTER
                           ! EXECUTING SUBROUTINE MOSAICIT.

      IF (.NOT. ABORTFLAG)  GOTO 8000    ! IF MOSAICIT SUCCESSFUL, ALL DONE.

      IF (ITERMCODE .EQ. 1) THEN 
         CALL XVMESSAGE('??E- INSUFFICIENT MEMORY OBTAINED.',' ')
         CALL ABEND                  
      ENDIF
C

8000  RETURN          ! NORMAL END.
      END
C
      SUBROUTINE MOSAICIT(INBUFS,INL,NL,NPIXEL,NB,NI,
     &                    ABORTFLAG,ITERMCODE)
C#######################################################################
C  NAME OF ROUTINE
C     MOSAICIT ( MOSAIC IT )
C
C  PURPOSE
C      MOSAICIT TAKES INPUT IMAGES AND MOSAICS THEM TOGETHER TO FORM AN OUTPUT
C      IMAGE.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     NOVEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    11-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C  CALLING SEQUENCE
C      CALLED VIA LIBRARY SUBROUTINE STACKA.
C  INPUT PARAMETERS ( all parameters are INTEGER   except as otherwise noted )
C      INBUFS(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THE DATA FORMAT IS THE SAME AS IN THE IMAGE FILE,
C                          BYTE OR HALF.  THE LINE BUFFERS ARE PACKED END TO 
C                          END IN THIS ARRAY.  THE INDEX OF THE START OF THE
C                          LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL*IPIXSIZE. (INBUFS IS BYTE.)
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      INL               - NUMBER OF BYTES ALLOCATED BY STACKA FOR INBUFS.
C      NPIXEL            - NUMBER OF PIXELS IN A LINE OF THE OUTPUT IMAGE.
C      NI                - NUMBER OF INPUT IMAGES.
C  OUTPUT PARAMETERS
C      ABORTFLAG - .TRUE. IF MOSAICIT FAILED. .FALSE. IF SUCCESSFUL. ABORTFLAG
C                  IS A LOGICAL   VARIABLE.
C      ITERMCODE - ERROR MESSAGE CODE IF MOSAICIT FAILS.
C  CALLED BY
C      STACKA
C  SUBROUTINES CALLED 
C      EDGE, MOS, FASTFILL plus the library routines XVCLOSE, XVGET,
C      XVOPEN, XVPTST, XVREAD, XVUNIT, XVWRIT.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c    ISIZE = IPIXSIZE*NPIXEL*NI
        implicit none
      COMMON /C1/ PAR, HALF, ITHRESH, IB, MTYP, NSEQ, LNIB, RNIB,
     &            LTHR, RTHR, NIBINC, OFF, IPIXSIZE, ISIZE
      INTEGER*8 ISIZE
      BYTE INBUFS(IPIXSIZE*NPIXEL*NI)
      LOGICAL*4 ABORTFLAG, HALF
      INTEGER*4 NL,NPIXEL,IB,NB,BAND,INL,NI,ITERMCODE
      INTEGER*4 I,II,IDSRN,IDSRNX,IND,INPIX,INPOS,IPIXSIZE
      INTEGER*4 IREC,ISSAMP,ITHRESH,L0,MTYP,NIBINC,NLTENTH
      INTEGER*4 NSEQ,NTENTHS, LINEOUT,BANDOUT
      INTEGER*4 PAR(100), LNIB, RNIB, LTHR, RTHR, OFF(4,48)
      INTEGER*4 OFFBACK(4,48)
      INTEGER*4 IPOS(48), IPIX(48), ISKIP(48), OUTFILE, INP(48), IS(48)
      LOGICAL*4 XVPTST
      LOGICAL*4 PROGRESS_MSG
      CHARACTER*40 PMESSAGE
      CHARACTER*3 ORGIN
C
C=================START OF EXECUTABLE CODE===============================     

      ABORTFLAG = .FALSE.               ! INITIALIZE TO SUCCESSFUL-SO-FAR.

C....CHECK IF STACKA GOT ENOUGH MEMORY. 

      IF ( INL .LT. ISIZE )  THEN
         ABORTFLAG = .TRUE.             ! INDICATE SUBROUTINE FAILURE
         ITERMCODE  = 1                 ! AND REASON FOR FAILURE.
         GOTO 8000                      ! EXIT SUBROUTINE.
      END IF

C   OPEN INPUT FILES AND GET SIZE VALUES IF NOT ENTERED BY USER.

      DO  IDSRN = 1, NI

        CALL XVUNIT( INP(IDSRN), 'INP', IDSRN, IND ,' ')
        CALL XVOPEN( INP(IDSRN), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')
c       Check organization of image, prohibit BIP
        CALL XVGET(INP(IDSRN),IND,'ORG',ORGIN, ' ')
        IF (ORGIN.EQ.'BIP') CALL MABEND(
     +    '??E- BIP files not supported, use program TRAN to convert to BSQ')

        IF ( OFF(3,IDSRN) .EQ. 0 )  THEN
            CALL XVGET(  INP(IDSRN), IND, 'NL', OFF(3,IDSRN),
     .                   'NS',  OFF(4,IDSRN) ,' ')
        END IF

      END DO

C  OPEN OUTPUT FILE.

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND ,' ')
      CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'U_NL', NL,'U_NS', 
     .             NPIXEL,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',' ')

C   SET UP FOR MOSAICING LOOP.

      L0 = MIN ( 0, ITHRESH-1 )              ! SET UP LOGICAL ZERO VALUE THAT
      L0 = MAX (L0, -32768)                  ! MAKES SENSE IF THRESH < 1.
      if  (ITHRESH .EQ. 0 .AND. .NOT. HALF)  L0 = 0

      CALL FASTFILL(IPIXSIZE, NPIXEL*NI,L0,INBUFS,INBUFS) !ZERO ALL INPUT BUFFS
      DO IDSRN = 1, NI
         OFF(3,IDSRN) = OFF(3,IDSRN) - MAX( 0, OFF(1,IDSRN) )

         ISKIP(IDSRN)  = MAX( 0, OFF(2,IDSRN) )
         IPIX(IDSRN)   = MIN( OFF(4,IDSRN)-MAX( OFF(2,IDSRN), 0 ),
     &                            NPIXEL+MIN( OFF(2,IDSRN), 0 )    )
         IF ( IPIX(IDSRN) .LE. 0 )   IPIX(IDSRN) = 0
         IS(IDSRN) = 1 + (IDSRN-1)*NPIXEL*IPIXSIZE   !START OF LINE BUFFER.
         IPOS(IDSRN) = IS(IDSRN) - IPIXSIZE*MIN(0,OFF(2,IDSRN))

      END DO

          NLTENTH = NINT( NL/10.)      ! SET UP FOR PROGRESS PARAMETER
          II = 0
          PROGRESS_MSG = XVPTST('PROGRESS')

C  COPY OFF() to OFFBACK() for multiband reuse

      DO IDSRN = 1,NI
         OFFBACK(1,IDSRN) = OFF(1,IDSRN)
         OFFBACK(2,IDSRN) = OFF(2,IDSRN)
         OFFBACK(3,IDSRN) = OFF(3,IDSRN)
         OFFBACK(4,IDSRN) = OFF(4,IDSRN)
      END DO


C  MOSAICING LOOP:  MOSAIC LINE BY LINE.
      BANDOUT=0
      DO 3000 BAND=1,NB
         BANDOUT = BANDOUT + 1
         LINEOUT = 0

c      RESET OFF() from BACK UP OFFBACK()
       DO IDSRNX = 1,NI
         OFF(1,IDSRNX) = OFFBACK(1,IDSRNX)
         OFF(2,IDSRNX) = OFFBACK(2,IDSRNX)
         OFF(3,IDSRNX) = OFFBACK(3,IDSRNX)
         OFF(4,IDSRNX) = OFFBACK(4,IDSRNX)
       END DO

      DO 1000 I=1,NL
         LINEOUT = LINEOUT + 1
         DO IDSRN = 1, NI

           IF( OFF(3,IDSRN) .EQ. 0  .OR. IDSRN .EQ. 1 )  THEN
               CALL FASTFILL(IPIXSIZE, NPIXEL, L0, INBUFS(IS(IDSRN)), 
     &                       INBUFS(IS(IDSRN)))
           END IF

           IF( OFF(1,IDSRN) .GE. 0 .AND. 
     &         OFF(3,IDSRN) .GT. 0 .AND. 
     &         IPIX(IDSRN) .GT. 0        )          THEN

               IREC   = 1 + OFF(1,IDSRN)
               ISSAMP = ISKIP(IDSRN) + 1
               INPIX  = IPIX(IDSRN)
               INPOS  = IPOS(IDSRN)
               CALL XVREAD( INP(IDSRN), INBUFS(INPOS), IND,
     &                'LINE',IREC,'SAMP',ISSAMP,'NSAMPS',INPIX,
     &                'BAND', BAND, ' ')
           END IF

           IF ( OFF(1,IDSRN) .GE. 0 )  OFF(3,IDSRN)=OFF(3,IDSRN)-1
           OFF(1,IDSRN) = OFF(1,IDSRN)+1
         END DO

         IF(IB .NE. 0)  CALL EDGE(INBUFS,IPIXSIZE,NPIXEL,NI,IPIX,IPOS,
     &                            NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC,L0)

         CALL MOS( INBUFS, INBUFS, IPIXSIZE, NPIXEL, NI, ITHRESH, MTYP)
         CALL XVWRIT(OUTFILE, INBUFS(1), IND,
     +        'LINE',LINEOUT,'BAND',BANDOUT, ' ')

            II = II + 1
            IF (PROGRESS_MSG .AND. II .GE. NLTENTH) THEN
               II = 0                     !PRINT PROGRESS IF DESIRED.
               NTENTHS = NINT(I * 10. / NL)
               IF (NTENTHS .GT. 0 .AND. NTENTHS .LE. 9) THEN
                  WRITE (PMESSAGE, 9100) NTENTHS
                  CALL XVMESSAGE( PMESSAGE, ' ')
               END IF
            END IF
1000  CONTINUE            ! END OF MOSAICING LOOP.
3000  CONTINUE
C  CLOSE ALL FILES AND RETURN TO MAIN44 VIA STACKA.


      DO IDSRN = 1, NI
         CALL XVCLOSE(INP(IDSRN),IND,' ')
      END DO
         CALL XVCLOSE(OUTFILE, IND,' ')
8000  RETURN

9100  FORMAT( '   fastmos ',I1,'0% done')
      END
C
      SUBROUTINE EDGE(INBUFS, IPIXSIZE, NPIXEL,NI,IPIX,IPOS,
     &                NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC, L0)
C#######################################################################
C  NAME OF ROUTINE
C     EDGE ( nibble off EDGEs of lines )
C
C  PURPOSE
C      EDGE removes the edges of lines from the input files.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     NOVEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    11-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    10-84  SP   IPIXSIZE ADDED TO CALLING SEQUENCES.
C     6-87  SP   L0 ADDED TO CALLING SEQUENCES.
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETERS  
C      INBUFS(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THE DATA FORMAT IS THE SAME AS IN THE IMAGE FILE,
C                          BYTE OR HALF.  THE LINE BUFFERS ARE PACKED END TO 
C                          END IN THIS ARRAY.  THE INDEX OF THE START OF THE
C                          LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL*IPIXSIZE. (INBUFS IS BYTE.)
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      IPIXSIZE          - BYTES PER PIXEL: 1 FOR BYTE DATA, 2 FOR HALF.
C      NPIXEL            - NUMBER OF PIXELS IN THE LINE.
C      NI                - NUMBER OF INPUT IMAGES.
C      IPIX              - ARRAY CONTAINING NUMBER OF PIXELS READ FOR LINE 
C                          FROM EACH INPUT FILE.
C      IPOS              - DATA READ FOR LINES STARTS AT 
C                          INBUFS( IPOS(IDSRN) ) FOR EACH IDSRN.
C      NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC - PARAMETERS PASSED TO NIBLR FOR EDGING
C      L0                - LOGICAL ZERO VALUE TO ZERO BUFFERS WITH.
C  OUTPUT PARAMETERS
C      PIXELS REMOVED FROM LINES HAVE THEIR DATA NUMBERS ZEROED IN INBUFS.
C  CALLED BY
C      MOSAICIT
C  SUBROUTINES CALLED 
C      NIBLR and FASTFILL.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      implicit none
      BYTE INBUFS(*)
      INTEGER*4   IPOS(48), IPIX(48)
      INTEGER*4   LTHR,RTHR,LNIB,RNIB, L0, IPIXSIZE,NPIXEL,NI
      INTEGER*4   NSEQ,NIBINC,IANSW,IANSWR,IDSRN,NIBLGTH,NIBSTRT
C
C=================START OF EXECUTABLE CODE===============================     

      DO  IDSRN = 1, NI                     ! FOR EACH FILE, EDGE THE LINE
        IF ( IPIX(IDSRN) .GT. 0 )  THEN       ! DATA IF PRESENT.

         CALL NIBLR(IPIX(IDSRN),INBUFS(IPOS(IDSRN)),INBUFS(IPOS(IDSRN)), ! EDGE
     &              IPIXSIZE, LTHR, NSEQ, 0, IANSW, NIBINC )         ! ON LEFT.

C             IF EDGE FOUND, THEN NIBBLE OFF LNIB PIXELS PAST THE EDGE.
C             IF EDGE NOT FOUND, THIS MEANS NO DATA VALUES WERE FOUND THAT
C             WERE NOT BELOW THE THRESHOLD; SO ZERO THE WHOLE LINE.
C             SIMILARLY FOR NIBBLING FROM THE RIGHT. FASTFILL USED FOR ZEROING.
 
           IF ( IANSW .GT. 0 )  THEN                         
              NIBLGTH = MIN(IANSW+LNIB-1,IPIX(IDSRN))        ! NUMBER OF PIXELS
              IF ( NIBLGTH .GE. 1 )   CALL FASTFILL(          ! TO NIBBLE. 
     &          IPIXSIZE,NIBLGTH,L0,INBUFS(IPOS(IDSRN)),
     &          INBUFS(IPOS(IDSRN)) )

              CALL NIBLR( IPIX(IDSRN), INBUFS(IPOS(IDSRN)),
     &        INBUFS(IPOS(IDSRN)),IPIXSIZE,RTHR, NSEQ, 1, IANSWR,NIBINC)

              IF ( IANSWR .GT. 0 )  THEN
               NIBLGTH = MIN( IPIX(IDSRN)-IANSWR+RNIB, IPIX(IDSRN) )
               NIBSTRT = IPOS(IDSRN) + (IANSWR - RNIB)*IPIXSIZE
               IF ( NIBLGTH .GE. 1 )  CALL FASTFILL(IPIXSIZE, NIBLGTH,
     &              L0, INBUFS(NIBSTRT), INBUFS(NIBSTRT) )

              ELSE
               CALL FASTFILL(IPIXSIZE,IPIX(IDSRN),L0,INBUFS(IPOS(IDSRN))
     &                      ,INBUFS(IPOS(IDSRN)) )
              END IF

           ELSE
             CALL FASTFILL(IPIXSIZE,IPIX(IDSRN),L0,INBUFS(IPOS(IDSRN)),
     &                       INBUFS(IPOS(IDSRN)) )

           END IF
        END IF
      END DO
      RETURN
      END
C
      SUBROUTINE MOS(INBUFS,INBUFH,IPIXSIZE,NPIXEL,NI, ITHRESH, MTYPE)
C#######################################################################
C  NAME OF ROUTINE
C      MOS ( MOSaic )
C
C  PURPOSE
C      MOS PRODUCES A LINE OF THE OUTPUT IMAGE FROM THE CORRESPONDING
C      LINES OF THE INPUT IMAGES.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    NOVEMBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  DERIVED FROM ORIGINAL MOS SUBROUTINE BY
C      W. D. BENTON
C
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    11-83  SP   CONVERTED TO FORTRAN FROM IBM ASSEMBLER.  CONVERTED TO HANDLE
C                ONLY INTEGER*2 DATA SINCE SIMPLER AND MORE EFFICIENT TO HAVE
C                CALLING ROUTINE CONVERT TO INTEGER*2.
C    10-84  SP   CHANGED MY MIND ABOUT WHAT IS MORE EFFICIENT.  MODIFIED TO
C                ALLOW BOTH HALFWORD AND BYTE.
C    6-87   SP   CHANGED NAME OF VARIABLE NTHRESH TO ITHRESH FOR CONSISTENCY
C                WITH THE REST OF THE PROGRAM.
C  CALLING SEQUENCE 
C     CALL MOS( INBUFS, INBUFS, IPIXSIZE, NPIXEL, NI, ITHRESH, MTYPE )
C  INPUT PARAMETERS ( all parameters are INTEGER   except as otherwise noted )
C      INBUFS(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THIS SUBROUTINE ARGUMENT IS USED WHEN THE
C                          THE DATA FORMAT IS BYTE. THE LINE BUFFERS ARE PACKED
C                          END TO END IN THIS ARRAY.  THE INDEX OF THE START 
C                          OF THE LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL.  (INBUFS IS BYTE.)
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      INBUFH(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THIS SUBROUTINE ARGUMENT IS USED WHEN THE
C                          THE DATA FORMAT IS HALF. THE LINE BUFFERS ARE PACKED
C                          END TO END IN THIS ARRAY.  THE INDEX OF THE START 
C                          OF THE LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL.  (INBUFH IS INTEGER*2.)
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      IPIXSIZE          - BYTES PER PIXEL: 1 FOR BYTE DATA, 2 FOR HALF.
C      NPIXEL            - NUMBER OF PIXELS IN THE LINE.
C      NI                - NUMBER OF INPUT IMAGES.
C      ITHRESH           - DATA NUMBER THRESHOLD. DATA NUMBERS BELOW THE
C                          THRESHOLD ARE IGNORED FOR THE MOST PART.
C      MTYPE             - INDICATES THE THE MOSAICING METHOD BY WHICH THE 
C                          OUTPUT DATA NUMBERS ARE DETERMINED.
C                           = 0 FOR OVERLAY MODE
C                           = 1 FOR AVERAGE MODE
C                           = 2 FOR MOD MODE
C                           = 3 FOR MAXIMUM MODE
C                           = 4 FOR MINIMUM MODE
C  OUTPUT PARAMETERS
C      THE OUTPUT IMAGE LINE IS RETURNED IN  INBUFS(I,1) FOR I = 1 TO NPIXEL.
C      IF MTYPE IS INVALID, THEN THIS IS THE SAME AS IT WAS IN INPUT.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        implicit none
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.


C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      BYTE  INBUFS(*)
      INTEGER*2  INSAVE(48), INBUFH(*)
      INTEGER*4  ITEMP, NCLOSE, NMIN, NMAX, NTOTAL, IPIXSIZE
      INTEGER*4  I,J,NPIXEL,NI,ITHRESH,MTYPE,IDSRN,IN,JDIFF
      INTEGER*4  MINDIFF,NAVE,NCOUNT
      INTEGER*4 IS(48)
C
C=================START OF EXECUTABLE CODE===============================     
C
      IF (MTYPE .NE. 0)  THEN
        IS(1) = 1
        DO I = 2, NI
           IS(I) = IS(I-1) + NPIXEL
        END DO
      END IF

      IF ( IPIXSIZE .EQ. 2 )      THEN    ! SECTION FOR HALFWORD DATA !

      IF      (MTYPE .EQ. 0)  THEN            ! OVERLAY MODE.

         IS(1) = 0
         DO I = 2, NI
            IS(I) = IS(I-1) + NPIXEL
         END DO

         DO 1000 I = 1, NPIXEL

           IF ( INBUFH(I) .GE. ITHRESH )  GOTO 1000
           DO IDSRN = 2, NI      ! FIND FIRST ELEMENT >= ITHRESH.

             IF ( INBUFH(I+IS(IDSRN)) .GE. ITHRESH )  THEN
                INBUFH(I) = INBUFH(I+IS(IDSRN))
                GOTO 1000 
             END IF

           END DO
1000     CONTINUE


      ELSE IF (MTYPE .EQ. 1)  THEN            ! AVERAGE MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= ITHRESH.

             IF ( INBUFH(IS(IDSRN)) .GE. ITHRESH )  THEN
                NTOTAL = NTOTAL + INBUFH(IS(IDSRN))
                NCOUNT = NCOUNT + 1
             END IF
             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUFH(I) = NTOTAL

           ELSE IF (NCOUNT .GT. 1) THEN
                    INBUFH(I) = NTOTAL/NCOUNT

           END IF

         END DO

      ELSE IF (MTYPE .EQ. 2)  THEN            ! MOD MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= ITHRESH.

             IF ( INBUFH(IS(IDSRN)) .GE. ITHRESH )  THEN
                NTOTAL = NTOTAL + INBUFH(IS(IDSRN))
                NCOUNT = NCOUNT + 1
                INSAVE(NCOUNT) = INBUFH(IS(IDSRN))
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUFH(I) = NTOTAL

           ELSE IF (NCOUNT .EQ. 2) THEN
                    INBUFH(I) = NTOTAL/2      ! USE AVERAGE IF 2 VALUES.

           ELSE IF (NCOUNT .GT. 2) THEN         ! IF >2 VALUES, USE CLOSEST
                    NAVE = NTOTAL/NCOUNT        ! VALUE TO AVERAGE.
                    NCLOSE = INSAVE(1)
                    MINDIFF  = IABS( NAVE-INSAVE(1) )

                    DO J = 2, NCOUNT
                       JDIFF = IABS( NAVE-INSAVE(J) )
                       IF (JDIFF .LT. MINDIFF)  THEN
                           MINDIFF = JDIFF
                           NCLOSE  = INSAVE(J)
                       END IF
                    END DO

                    INBUFH(I) = NCLOSE 

           END IF

         END DO

      ELSE IF (MTYPE .EQ. 3)  THEN            ! MAX MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MAX VALUE >= ITHRESH.

             IF ( INBUFH(IS(IDSRN)) .GE. ITHRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMAX = INBUFH(IS(IDSRN))
                  NCOUNT = 1
                ELSE 
                  NMAX = MAX(  NMAX, INBUFH(IS(IDSRN))  )
                END IF
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  INBUFH(I) = NMAX

         END DO

      ELSE IF (MTYPE .EQ. 4)  THEN            ! MIN MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MIN VALUE >= ITHRESH.

             IF ( INBUFH(IS(IDSRN)) .GE. ITHRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMIN = INBUFH(IS(IDSRN))
                  NCOUNT = 1
                ELSE 
                  NMIN = MIN(  NMIN, INBUFH(IS(IDSRN))  )
                END IF
             END IF

             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  INBUFH(I) = NMIN

         END DO

      END IF

      ELSE            ! SECTION FOR BYTE DATA !


      IF      (MTYPE .EQ. 0)  THEN            ! OVERLAY MODE.

         IS(1) = 0
         DO I = 2, NI
            IS(I) = IS(I-1) + NPIXEL
         END DO

         DO 2000 I = 1, NPIXEL

           IF ( BYTE2INT(INBUFS(I)) .GE. ITHRESH )  GOTO 2000
           DO IDSRN = 2, NI      ! FIND FIRST ELEMENT >= ITHRESH.

             IF ( BYTE2INT(INBUFS(I+IS(IDSRN))) .GE. ITHRESH )  THEN
                INBUFS(I) = INBUFS(I+IS(IDSRN))
                GOTO 2000 
             END IF

           END DO
2000     CONTINUE


      ELSE IF (MTYPE .EQ. 1)  THEN            ! AVERAGE MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= ITHRESH.
             IN = BYTE2INT(INBUFS(IS(IDSRN)) )   ! CONVERT TO INTEGER  
             IF ( IN .GE. ITHRESH )  THEN
                NTOTAL = NTOTAL + IN
                NCOUNT = NCOUNT + 1
             END IF
             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUFS(I) = INT2BYTE(NTOTAL)

           ELSE IF (NCOUNT .GT. 1) THEN
                    ITEMP     = NTOTAL/NCOUNT
                    INBUFS(I) = INT2BYTE(ITEMP)

           END IF

         END DO

      ELSE IF (MTYPE .EQ. 2)  THEN            ! MOD MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= ITHRESH.
             IN = BYTE2INT(INBUFS(IS(IDSRN)) )   ! CONVERT TO INTEGER  

             IF ( IN .GE. ITHRESH )  THEN
                NTOTAL = NTOTAL + IN
                NCOUNT = NCOUNT + 1
                INSAVE(NCOUNT) = IN
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUFS(I) = INT2BYTE(NTOTAL)

           ELSE IF (NCOUNT .EQ. 2) THEN
                    ITEMP     = NTOTAL/2      ! USE AVERAGE IF 2 VALUES.
                    INBUFS(I) = INT2BYTE(ITEMP)
           ELSE IF (NCOUNT .GT. 2) THEN         ! IF >2 VALUES, USE CLOSEST
                    NAVE = NTOTAL/NCOUNT        ! VALUE TO AVERAGE.
                    NCLOSE = INSAVE(1)
                    MINDIFF  = IABS( NAVE-INSAVE(1) )

                    DO J = 2, NCOUNT
                       JDIFF = IABS( NAVE-INSAVE(J) )
                       IF (JDIFF .LT. MINDIFF)  THEN
                           MINDIFF = JDIFF
                           NCLOSE  = INSAVE(J)
                       END IF
                    END DO

                    INBUFS(I) = INT2BYTE(NCLOSE)
           END IF

         END DO

      ELSE IF (MTYPE .EQ. 3)  THEN            ! MAX MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MAX VALUE >= ITHRESH.

             IN = BYTE2INT(INBUFS(IS(IDSRN)) )   ! CONVERT TO INTEGER  
             IF ( IN .GE. ITHRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMAX = IN
                  NCOUNT = 1
                ELSE 
                  NMAX = MAX(  NMAX, IN )
                END IF
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  INBUFS(I) = INT2BYTE(NMAX)

         END DO

      ELSE IF (MTYPE .EQ. 4)  THEN            ! MIN MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MIN VALUE >= ITHRESH.

             IN = BYTE2INT(INBUFS(IS(IDSRN)) )   ! CONVERT TO INTEGER  
             IF ( IN .GE. ITHRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMIN = IN
                  NCOUNT = 1
                ELSE 
                  NMIN = MIN(  NMIN, IN )
                END IF
             END IF

             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  INBUFS(I) = INT2BYTE(NMIN)

         END DO

      END IF

      END IF
      RETURN          
      END
C
      SUBROUTINE NIBLR(NS,BUF,BUFH,IPIXSIZE,ITHRESH,NSEQ,MODE,
     .                 IANSWER,INC)
C#######################################################################
C  NAME OF ROUTINE
C      NIBLR ( NIBLeR )
C
C  PURPOSE
C      NIBLR SCANS THROUGH THE PIXELS OF A LINE TO FIND THE 'IMAGE EDGE'.
C      THE SCAN STARTS AT THE BEGINNING OR THE END OF THE LINE ACCORDING
C      TO THE MODE PARAMETER.  IF THE INC PARAMETER IS NOT 1, THEN THE SCAN
C      DOES NOT GO PIXEL BY PIXEL BUT CHECKS ONLY EVERY INCth PIXEL.  NIBLR
C      SCANS UNTIL IT FINDS A GROUP OF NSEQ CONSECUTIVE (IN TERMS OF INC)
C      PIXELS ALL OF WHICH HAVE A DATA NUMBER GREATER THAN OR EQUAL TO THE
C      ITHRESH PARAMETER VALUE.  THE EDGE IS DEFINED AS THE FIRST PIXEL 
C      (ACCORDING TO THE DIRECTION OF THE SCAN) OF THAT GROUP.  
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCTOBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  DERIVED FROM ORIGINAL NIBLR SUBROUTINE BY
C      W. D. BENTON
C
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    11-83  SP   CONVERTED TO FORTRAN FROM IBM ASSEMBLER.  CONVERTED TO HANDLE
C                ONLY INTEGER*2 DATA SINCE SIMPLER AND MORE EFFICIENT TO HAVE
C                CALLING ROUTINE CONVERT TO INTEGER*2.
C    10-84  SP   CHANGED MY MIND ABOUT WHAT IS MORE EFFICIENT.  MODIFIED TO
C                ALLOW BOTH HALFWORD AND BYTE.
C  CALLING SEQUENCE 
C     CALL NIBLR(NS,BUF,BUFH,IPIXSIZE,ITHRESH,NSEQ,MODE, IANSWER ,INC)
C             
C  INPUT PARAMETERS ( all parameters are integers.)
C      NS      - NUMBER OF PIXELS IN THE LINE.
C      BUF     - DATA NUMBERS FOR THE LINE. ( BUF(I) FOR I = 1 TO NS. )
C                THIS PARAMETER IS USED FOR BYTE DATA.  BUF IS BYTE.
C      BUFH    - DATA NUMBERS FOR THE LINE. ( BUFH(I) FOR I = 1 TO NS. )
C                THIS PARAMETER IS USED FOR HALFWORD DATA.  BUFH IS INTEGER*2.
C      IPIXSIZE          - BYTES PER PIXEL: 1 FOR BYTE DATA, 2 FOR HALF.
C      ITHRESH - DATA NUMBER THRESHOLD FOR DEFINING EDGE.
C      NSEQ    - NUMBER OF CONSECUTIVE PIXELS REQUIRED TO BE AT OR ABOVE THE
C                ITHRESH DATA NUMBER FOR DETERMINING THE EDGE.
C      MODE    - 0 FOR SCAN FROM BEGINNING OF LINE ( NIBBLE FROM LEFT ),
C                1 FOR SCAN FROM END OF LINE ( NIBBLE FROM RIGHT ).
C      INC     - SCAN WILL CHECK EVERY INCth PIXEL. 
C  OUTPUT PARAMETERS
C      IANSWER - PIXEL NUMBER WHERE EDGE IS.  IF NO EDGE IS FOUND, IANSWER=0.
C  FUNCTIONS CALLED
C      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        implicit none
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      INTEGER*4 I,NS,IPIXSIZE,ITHRESH,NSEQ,MODE,IANSWER,INC
      INTEGER*4 IBEG,ICNT,IEND,INCPAR,ISTEP,ITEMPA
      INTEGER*2 BUFH(NS)
      LOGICAL*4   LTOR
      BYTE BUF(NS)
C
C=================START OF EXECUTABLE CODE===============================     
C
      LTOR = .TRUE.                ! SCAN FROM LEFT TO RIGHT.
      INCPAR = INC
      IF (  MODE    .NE. 0 )       LTOR   = .FALSE. ! RIGHT TO LEFT.

      IF ( LTOR )  THEN            ! SET SCANNING ACCORDING TO DIRECTION.
           IBEG = 1
           IEND = NS
           ISTEP = INCPAR
      ELSE 
           IBEG = NS
           IEND = 1
           ISTEP = -INCPAR
      END IF

      ICNT = 0                               ! CONSECUTIVE PIXEL COUNTER.

      IF (IPIXSIZE .EQ. 2)   THEN   ! SECTION FOR HALFWORD DATA !
      DO  I = IBEG, IEND, ISTEP

          IF ( BUFH(I) .GE. ITHRESH ) THEN         ! IF THRESHOLD MET,
               IF ( ICNT .EQ. 0 )  ITEMPA = I     ! SAVE LOCATION OF FIRST
               ICNT = ICNT + 1                    ! PIXEL, & CHECK FOR
               IF ( ICNT .GE. NSEQ )  GOTO 5000   ! NSEQ CONSECUTIVE
          ELSE                                    ! PIXELS.
               ICNT = 0                           ! RESET COUNTER IF 
          END IF                                  ! THRESHOLD NOT MET.

      END DO

      ELSE                        ! SECTION FOR BYTE DATA !
      DO  I = IBEG, IEND, ISTEP

          IF ( BYTE2INT(BUF(I)) .GE. ITHRESH ) THEN   ! IF THRESHOLD MET,
               IF ( ICNT .EQ. 0 )  ITEMPA = I     ! SAVE LOCATION OF FIRST
               ICNT = ICNT + 1                    ! PIXEL, & CHECK FOR
               IF ( ICNT .GE. NSEQ )  GOTO 5000   ! NSEQ CONSECUTIVE
          ELSE                                    ! PIXELS.
               ICNT = 0                           ! RESET COUNTER IF 
          END IF                                  ! THRESHOLD NOT MET.

      END DO
      END IF
      ITEMPA = 0                                  ! NO EDGE FOUND.

5000  CONTINUE
      IANSWER = ITEMPA

      RETURN          
      END
C
       SUBROUTINE FASTFILL( IPIXSIZE,NPIXEL,L0,BBUF,HBUF )
C#######################################################################
C  NAME OF ROUTINE
C      FASTFILL( FASTmos FILL)
C
C  PURPOSE
C      Subroutine FASTFILL fills a BYTE or Halfword buffer with the value L0.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY        JPL                   4-93
C
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  CALLING SEQUENCE 
C     CALL FASTFILL( IPIXSIZE,NPIXEL,L0,BUF,BUF )
C  INPUT PARAMETERS ( all parameters are INTEGER except for BUF.)
C      IPIXSIZE - data type of BUF, 1 for BYTE, 2 for halfword.
C      NPIXEL   - dimension of BUF array.
C      L0       - value to fill BUF with.
C      BUF      - buffer to fill.(This arg. passed twice for Fortran mechanics.)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        implicit none
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      INTEGER*4 I,IPIXSIZE, NPIXEL, L0
      BYTE      BBUF(*), LB
      INTEGER*2 HBUF(*), LH
C
C=================START OF EXECUTABLE CODE===============================     

      IF ( IPIXSIZE .EQ. 1 ) THEN
         LB = INT2BYTE( L0 )
         DO I = 1, NPIXEL
            BBUF(I) = LB
         END DO
      ELSE IF ( IPIXSIZE .EQ. 2 ) THEN
         LH = L0
         DO I = 1, NPIXEL
            HBUF(I) = LH
         END DO
      END IF
      RETURN
      END
