$!****************************************************************************
$!
$! Build proc for MIPL module fftmagic
$! VPACK Version 1.9, Tuesday, August 16, 2016, 16:49:53
$!
$! Execute by entering:		$ @fftmagic
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
$ write sys$output "*** module fftmagic ***"
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
$ write sys$output "Invalid argument given to fftmagic.com file -- ", primary
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
$   if F$SEARCH("fftmagic.imake") .nes. ""
$   then
$      vimake fftmagic
$      purge fftmagic.bld
$   else
$      if F$SEARCH("fftmagic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftmagic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftmagic.bld "STD"
$   else
$      @fftmagic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftmagic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftmagic.com -mixed -
	-s fftmagic.f -
	-i fftmagic.imake -
	-p fftmagic.pdf -
	-t tstfftmagic.pdf tstfftmagic.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftmagic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ASM, OCT 1983
C**********************************************************************
C
      SUBROUTINE MAIN44
C
        implicit none
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET 
      INTEGER*4 INUNIT1,INUNIT2,BUFSIZ
      INTEGER*8 I8BUFSIZ
      REAL*4  BETA
      EXTERNAL WORK

      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &   BETA,NPSF,LOOP,NRESET
C        
      CALL IFMESSAGE('** FFTMAGIC version 25-JUL-2016 - (64bit) - RJB')
C
      CALL PARAMP(INUNIT1,INUNIT2)
      BUFSIZ = 4*NL2*NPIX
      I8BUFSIZ = int8(BUFSIZ)
      CALL STACKA_BIG(6,WORK,2,I8BUFSIZ,I8BUFSIZ,INUNIT1,INUNIT2)
      RETURN
C100   CALL MABEND('NOT ENOUGH CORE FOR STACKA')
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE WORK(PIX,LPIX,REF,LREF,INUNIT1,INUNIT2)
        implicit none
      INTEGER*8 LPIX,LREF 
      COMMON /MBUF/ MBUF
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER*4 PSF
      REAL*4 PIX(NPIX,NL2),REF(NPIX,NL2)
      COMMON /PSF/ PSF(7,100)

      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 INUNIT1,INUNIT2,STATUS
      REAL*4  BETA
      CHARACTER*132 MBUF

      
C      CALL MVE(1,38,
C     &    ' PIXELS CHANGED   SUM NEGATIVES   BETA',
C     &    MBUF(1),1,1) 
      MBUF(1:38)=' PIXELS CHANGED   SUM NEGATIVES   BETA'
C
      IF (LPIX.LT.4*NPIX*NL2 .OR. LREF.LT.4*NL2*NPIX) THEN
          CALL MABEND('??E - NOT ENOUGH CORE FOR STACKA')
      END IF
C        
C       READ IN IMAGE ESTIMATE DSRN2
C        
      CALL LDIMAG(INUNIT1,PIX) 
      CALL RFT2(PIX,NX,NPIX,1,STATUS)   
C        
C       READ IN REFERENCE IMAGE(BLURRED)  DSRN3  
C        
      CALL LDIMAG(INUNIT2,REF) 
      IF (NPSF.GT.0) CALL FNDSTR(REF)
      CALL RFT2(REF,NX,NPIX,1,STATUS)
      CALL XVMESSAGE(MBUF,' ') 
C      DO KK=1,100
C          MBUF(KK:KK)=' '
C      END DO
      MBUF = ' '
C        
      LOOP=0 
      DO WHILE (LOOP.EQ.0 .OR. 
     &        (NRESET.NE.0 .AND. LOOP.LT.ITER)) 
C
          LOOP=LOOP+1 
C        
          IF (MODE.EQ.1) THEN 
              CALL MAMPL(PIX,REF)
          ELSE 
              CALL MPHASE(PIX,REF)
          END IF
C        
C       TAKE INVERSE FFT OF PIX  
C        
          CALL RFT2(PIX,NX,NPIX,-1,STATUS)
          CALL PICBOU(PIX)
          IF (NRESET.NE.0 .AND. LOOP.LT.ITER) THEN
              IF (NPSF.GT.0) CALL SETSTR(PIX)
C        
C	TAKE DIRECT FFT OF PIX
C        
              CALL RFT2(PIX,NX,NPIX,1,STATUS)
          END IF
C       
      END DO
C        
      CALL OUTLST(PIX) 
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE PARAMP(INUNIT1,INUNIT2)

        implicit none
      INTEGER*4 IPAR(600)
C      BYTE XVPTST
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 I,N,INUNIT1,INUNIT2,ISTATUS,ICNT
      INTEGER*4 NLI,NSI,PSF
      REAL*4  BETA
      LOGICAL*4 XVPTST

      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /PSF/ PSF(7,100)

      CALL XVUNIT(INUNIT1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INUNIT1,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVSIZE(ISL,ISS,NX,NPIX,NLI,NSI)
      NL2 = NX + 2

      CALL XVUNIT(INUNIT2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INUNIT2,ISTATUS,'U_FORMAT','REAL',' ')

      IF (XVPTST('PHASE')) THEN
          MODE=2
      ELSE 
          MODE=1
      END IF

      IF (XVPTST('PRINT')) THEN
          IPRINT=1
      ELSE 
          IPRINT=0
      END IF

      CALL XVP ('ITER',ITER,ICNT)
      CALL XVP ('BETA',BETA,ICNT)

      CALL XVP ('PSF',IPAR,ICNT)
      IF ( ICNT .NE. 0 ) THEN
          NPSF=IPAR(1)
          I=2
          DO N=1,NPSF 
              PSF(5,N) = IPAR(I)
              PSF(6,N) = IPAR(I+1)
              PSF(7,N) = IPAR(I+2)
              I=I+3
          END DO
      ELSE
          NPSF=0
      END IF
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE LDIMAG(INUNIT,BUF)
C     
        implicit none

      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 L,INUNIT,ISTATUS,LINE
      REAL*4  BETA

      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL*4 BUF(NPIX,NL2)

      L=0
      DO LINE=ISL,ISL+NX-1 
              L=L+1 
              CALL XVREAD(INUNIT,BUF(1,L),ISTATUS,'LINE',LINE, 
     +                    'SAMP',ISS,'NSAMPS',NPIX,' ')
      END DO
      CALL XVCLOSE(INUNIT,ISTATUS,' ') 
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MAMPL(PIX,REF)
C 
        implicit none
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L
      REAL*4  BETA,PA,RA,S

       
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)

      DO L=1,NL2,2 
          DO J=1,NPIX 
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (PA.GT.0.0) THEN
                  S=SQRT(RA/PA) 
                  PIX(J,L)=PIX(J,L)*S 
                  PIX(J,L+1)=PIX(J,L+1)*S 
              END IF
          END DO
      END DO
      RETURN
      END
C
C
C
C
C
C**********************************************************************
C
      SUBROUTINE MPHASE(PIX,REF)
        implicit none
C        
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L
      REAL*4  BETA,PA,RA,S
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL PIX(NPIX,NL2),REF(NPIX,NL2)

      DO L=1,NL2,2 
          DO J=1,NPIX
              PA=PIX(J,L)**2+PIX(J,L+1)**2 
              RA=REF(J,L)**2+REF(J,L+1)**2 
              IF (RA.GT.0.0) THEN
                  S=SQRT(PA/RA) 
                  PIX(J,L)=S*REF(J,L) 
                  PIX(J,L+1)=S*REF(J,L+1) 
              ELSE 
                  PIX(J,L)=0.0 
                  PIX(J,L+1)=0.0 
              END IF
          END DO
      END DO
C        
      PIX(1,1)=REF(1,1) 
      PIX(1,2)=REF(1,2) 
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE PICBOU(PIX)
C
        implicit none
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L
      REAL*4  BETA,S, SCALE_NEG,SUMNEG

      REAL*4 PIX(NPIX,NL2)
      CHARACTER*132 MBUF
C
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      COMMON /MBUF/ MBUF

      NRESET=0
      SUMNEG=0.E0
C        
      IF (LOOP.EQ.(LOOP/2)*2) THEN
          SCALE_NEG = -BETA 
      ELSE 
          SCALE_NEG = 0.E0 
      END IF        
      S=NX*NPIX*2 
      DO L=1,NX 
          DO J=1,NPIX 
              PIX(J,L)=PIX(J,L)/S          
              IF (PIX(J,L).LT.0.0) THEN
                  SUMNEG=SUMNEG+PIX(J,L) 
                  PIX(J,L)=PIX(J,L)*SCALE_NEG 
                  NRESET=NRESET+1 
              END IF
          END DO
      END DO
      IF (IPRINT.GT.0) THEN
          WRITE (MBUF,9900) NRESET,SUMNEG,SCALE_NEG
9900  FORMAT ('     ',I5,'          ',F10.1,'  ',F6.2)
          CALL XVMESSAGE(MBUF(2:50),' ')
      END IF
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE OUTLST(PIX)
        implicit none

      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L,IOUTUNIT,ISTATUS
      REAL*4 BETA

      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
C      BYTE XVPTST
      LOGICAL*4 XVPTST
      REAL*4 PIX(NPIX,NL2)
C        
C        
      CALL XVUNIT(IOUTUNIT,'OUT',1,ISTATUS,' ')
      IF (XVPTST('OUTREAL')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','REAL',' ')
      ELSE IF (XVPTST('OUTINTEG')) THEN
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','FULL',' ')
      ELSE IF (XVPTST('OUTHALF')) THEN
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.32767) PIX(J,L)=32767
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','HALF',' ')
      ELSE
          DO L=1,NX
              DO J=1,NPIX
                  IF (PIX(J,L).GT.255) PIX(J,L)=255 
              END DO
          END DO
          CALL XVOPEN(IOUTUNIT,ISTATUS,'OP','WRITE',
     +               'U_NL',NX,'U_NS',NPIX,
     +               'U_FORMAT','REAL','O_FORMAT','BYTE',' ')
      END IF      

      DO L=1,NX
           CALL XVWRIT(IOUTUNIT,PIX(1,L),ISTATUS,'NSAMPS',
     +                 NPIX,' ')
      END DO
C        
      CALL PRNT(4,1,LOOP,' ITERATION ENDED ON PASS .') 
      CALL XVCLOSE(IOUTUNIT,ISTATUS,' ')
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE FNDSTR(REF)
C
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L
      REAL*4 BETA
        
      REAL*4 REF(NPIX,NL2)
      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      INTEGER*4 PSF
      COMMON /PSF/ PSF(7,100)
      INTEGER*4 LINE,SAMP,LEFT,RIGHT,TOP,BOTTOM
      REAL*4 MAXDN,SUMDN
C        
      DO N=1,NPSF  
C        
          LEFT=PSF(6,N)-PSF(7,N)   
          RIGHT=PSF(6,N)+PSF(7,N) 
          TOP=PSF(5,N)-PSF(7,N) 
          BOTTOM=PSF(5,N)+PSF(7,N) 
          IF (LEFT.LT.1) LEFT=1 
          IF (RIGHT.GT.NPIX) RIGHT=NPIX 
          IF (TOP.LT.1) TOP = 1 
          IF (BOTTOM.GT.NX) BOTTOM=NX 
          PSF(1,N)=TOP
          PSF(2,N)=LEFT
          PSF(3,N)=BOTTOM
          PSF(4,N)=RIGHT
C        
          LINE=TOP  
          SAMP=LEFT
          MAXDN=REF(SAMP,LINE) 
          DO L= TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  IF (REF(J,L).GT.MAXDN) THEN
                      MAXDN=REF(J,L) 
                      LINE=L  
                      SAMP=J
                  END IF
              END DO
          END DO
          PSF(5,N)=LINE
          PSF(6,N)=SAMP
C        
          MAXDN=0     
          DO J=LEFT,RIGHT 
              MAXDN=MAXDN+REF(J,TOP)+REF(J,BOTTOM)  
          END DO
          DO L=TOP,BOTTOM 
              MAXDN=MAXDN+REF(LEFT,L)+REF(RIGHT,L)  
          END DO
          MAXDN=MAXDN/(2*(BOTTOM-TOP+1+RIGHT-LEFT+1))   
C        
          SUMDN=0.E0      
          DO L=TOP,BOTTOM 
              DO J=LEFT,RIGHT 
                  SUMDN=SUMDN+REF(J,L)-MAXDN 
              END DO
          END DO
          PSF(7,N)=SUMDN 
          CALL PRNT(4,7,PSF(1,N),' PSF BUF=.') 
C        
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE SETSTR(PIX)
C
      INTEGER*4 ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,NPSF,LOOP,NRESET
      INTEGER*4 J,L,N
      REAL*4 BETA

      COMMON /PARMS/ ITER,ISL,ISS,NX,NPIX,NL2,MODE,IPRINT,
     &    BETA,NPSF,LOOP,NRESET
      REAL*4 PIX(NPIX,NL2)
      INTEGER*4 PSF
      COMMON /PSF/ PSF(7,100)
      REAL*4 MEANDN
C        
      DO N=1,NPSF
C        
          MEANDN=0.E0
          DO L=PSF(1,N),PSF(3,N) 
              MEANDN=MEANDN+PIX(PSF(2,N),L)+PIX(PSF(4,N),L)
          END DO
          DO J=PSF(2,N),PSF(4,N) 
              MEANDN=MEANDN+PIX(J,PSF(1,N))+PIX(J,PSF(3,N))
          END DO
          MEANDN=MEANDN/(2*(PSF(3,N)-PSF(1,N)+1+PSF(4,N)-PSF(2,N)+1)) 
C        
          DO L=PSF(1,N),PSF(3,N) 
              DO J=PSF(2,N),PSF(4,N) 
                  PIX(J,L)=MEANDN 
              END DO
          END DO
          PIX(PSF(6,N),PSF(5,N))=PSF(7,N)  
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftmagic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftmagic

   To Create the build file give the command:

		$ vimake fftmagic		(VMS)
   or
		% vimake fftmagic		(Unix)


************************************************************************/


#define PROGRAM	fftmagic
#define R2LIB

#define MODULE_LIST fftmagic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftmagic.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING 
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM OUTFMT TYPE=KEYWORD VALID=(OUTBYTE,OUTINTEG,OUTHALF,OUTREAL) DEF=OUTBYTE
PARM MODE TYPE=KEYWORD VALID=(PHASE,AMPLITUD) DEF=AMPLITUD
PARM ITER TYPE=INTEGER DEFAULT=100
PARM PRINT TYPE=KEYWORD VALID=PRINT COUNT=0:1 DEFAULT=--
PARM BETA TYPE=REAL DEFAULT=0.5
PARM PSF TYPE=INTEGER COUNT=(4:600) DEFAULT=(0,0,0,0)
END-PROC
.TITLE
fftmagic
.HELP
PURPOSE:
fftmagic provides an iterative method for obtaining either the phase from the
amplitude or the amplitude from the phase of a Fourier transform.  The
technique is controversial and may not work for all images. 

 
EXECUTION:

Examples

fftmagic INP=(EST,REF) OUT=PIC 

This command will read estimate picture EST and reference picture REF as byte
data (default), and create an output picture, PIC, also in byte format, whose
Fourier transform will correspond in amplitude (default) to that of REF.
Up to 100 iterations will be performed (default), and negative pixels on odd
iterations will be multiplied by -0.5 (the negative default of BETA).

fftmagic INP=(EST,REF) OUT=PIC  'OUTINTEG 'PHASE

This command is similar, except that the output picture will be in
in integer format, as specified by the OUTINTEG command, instead
of the default, which is to match the format of the input pictures; other
output format specifications include OUTREAL, OUTBYTE, and OUTHALF.  Finally,
the output picture's Fourier transform will be matched in PHASE to that of
the reference picture (REF), instead of matching amplitude as in the example
above.  The omitted keywords are INPFMT, OUTFMT, and MODE, respectively.

fftmagic INP=(EST,REF) OUT=PIC ITER=200 'PRINT  BETA=0.75

This command will again read the input pictures in byte format, but will this
time perform up to 200 iterations.  Normally, the program will iterate until no
pixels have changed, or 100 loops have been made; in this case, the program
will use up to 200 iterations.  The PRINT option specified above causes the
number of pixels changed and the sum of the negative pixels to be printed at
each iteration.  Finally, negative pixels will be multiplied by -BETA = -0.75
instead of the default -0.5.

fftmagic INP=(EST,REF) OUT=PIC PSF=(1,100,20,40)

This command demonstrates the use of the PSF keyword.  PSF specifies the
(line, sample, radius) triplets needed to describe point spread functions in 
the REF image.  These triplets are preceded by the number of triplets to be
used; in this case, there is one point spread function, centered at line 100,
sample 20, and with a radius of 40.  Additional triplets can follow as
necessary.  When PSF is specified, each center position is re-centered at the
max DN value in each circular area and the sum of the pixels less background
(determined from the border points) is computed for the values within each
circle.  The, for each iteration, all of the points inside each circle are
set to the mean DN plus the sum of the DN's as previously computed.  PSF acts
as an additional boundary condition for stellar-type point spread functions.


Notes and Restrictions

1) Both FFT's must reside in core together.  Together they take NxNx8 bytes if
   N is the picture dimension.
2) (Timing restriction belonging here will be inserted when VAX benchmarks
   have been run.)  (Was:  For 64x64 pictures each iteration consumes about
   4 seconds.)


OPERATION:
fftmagic computes and stores in core the FFT's of the initial estimate picture
and the reference picture.  It then forces the amplitude or phase of the 
estimate to match that of the reference.  The inverse FFT is taken and all
negative DN's are multiplied by -BETA (as specified by the BETA keyword) on the
odd iterations and by 0 on the even iterations.  If PSF has been specified, 
then star images are also set to delta functions.


	       EST             REF
		|		|
		|		|
		V		V
             FFT(EST)       FFT(REF)
		 \             /
		  \           /
		   \         /
		    \       /
		     \     /
		      \   /
			V
		     MATCH <------------------------------+
		     PHASE OR				  |
		     AMPLITUDE				  |
			|				  |
			|				  ^
			V			   RESET NEGATIVES
		     FFT**-1 (EST) -----------> RESET PSF'S






REFERENCE:
Fienup, J.R., Optical Society of America, Optics Letters, Volume 3, No. 1.


WRITTEN BY:  J. J. Lorre, 25 October 1979
COGNIZANT PROGRAMMER:  Ray Bambery 
REVISION:  New

MADE PORTABLE FOR UNIX: CRI     03-OCT-94

  25 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARIABLE INP
STRING - Estimate and reference pictures
.VARIABLE OUT
STRING - Output picture
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE OUTFMT
KEYWORD - Output data format (OUTBYTE, OUTHALF, OUTINTEG, OUTREAL)
.VARIABLE MODE
KEYWORD - Selects phase boundary condition (AMPLITUDE, PHASE)
.VARIABLE ITER
INTEGER - Number of iterations allowed
.VARIABLE PRINT
KEYWORD - Program status trace (PRINT)
.VARIABLE BETA
REAL - Negative-pixel factor
.VARIABLE PSF
INTEGER - Spec for point-spread functions
.LEVEL2
.VARIABLE INP
INP specifies the initial estimate picture (EST) and the reference picture 
(REF).  The initial estimate picture will be used to obtain the unknown part
of the Fourier transform.  EST is usually a random noise scene.  The reference
picture's Fourier transform will be used (either the amplitude or the phase)
as a "known" quantity.
.VARIABLE OUT
OUT specifies the picture whose Fourier transform corresponds either in phase
or amplitude to REF after ITER iterations.
.VARIABLE OUTFMT
OUTFMT specifies the format for output data.  The valid values are OUTBYTE,
OUTHALF, OUTINTEG, and OUTREAL.  The default is to use the format of the
input pictures.
.VARIABLE MODE
MODE=PHASE (or 'PHASE) specifies that the phase of the reference picture (REF)
will be used as a boundary condition, leaving the amplitude to be determined. 
The default is for the amplitude to be used as the boundary condition. 
.VARIABLE ITER
ITER specifies the maximum number of iterations allowed.  If no pixels are 
reset on any pass, the iteration process is terminated.  Default is ITER=100.
.VARIABLE PRINT
'PRINT causes the number of pixels changed and the sum of the
negative pixels to be printed at each iteration.  
.VARIABLE BETA
BETA specifies the real value to be multiplied by each negative pixel in order
to render it positive or zero.  The negative of BETA is used in the program,
and only every other iteration is used.  The default is BETA = 0.5 .
.VARIABLE PSF
The PSF keyword is used in the form PSF=(N,L1,S1,R1,...,Ln,Sn,Rn), where 
each L, S, and R triplet specifies the line, sample, and radius of a 
point-spread function in the reference (REF) image.  If PSF is specified,
each center position is re-centered at the max DN value in each circular area
and the sum of the pixels less background (determined from the border points)
is computed for the values within the circle.  Then, for each iteration, all
of the points inside each circle are set to the mean DN of the border points
and the central pixel is set to the mean DN plus the sum of the DN's as
previously computed.  PSF acts as an additional boundary condition for stellar-
type point-spread functions.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftmagic.pdf
procedure
! Jul 25, 2016- RJB
! TEST SCRIPT FOR FFTMAGIC
! tests BYTE images
!
! Vicar Programs:
!       gausnois boxflt2 stretch
!
! External Programs;
!   <none>
!           
! parameters:
!   <none>
!
! Requires no external test data: 
!

refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="stop"
let $echo="yes"
gausnois a nl=64 ns=64 seed=666
boxflt2 a b nlw=9 nsw=9
stretch b a table=(0.,0.,127.,0.,128.,255.,255.,255.)
gausnois b nl=64 ns=64 seed=1234567
fftmagic (b,a) c 'print
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstfftmagic.log
tstfftmagic
gausnois a nl=64 ns=64 seed=666
Beginning VICAR task gausnois
Gausnois - 11-Aug-2013 - rjb (64bit)
boxflt2 a b nlw=9 nsw=9
Beginning VICAR task boxflt2
BOXFLT2  12-Jul-2016 (64-bit) RJB
stretch b a table=(0.,0.,127.,0.,128.,255.,255.,255.)
Beginning VICAR task stretch
STRETCH - Jun 06, 2012 (64-bit) RJB **
*** TABLE STRETCH MODE ***
gausnois b nl=64 ns=64 seed=1234567
Beginning VICAR task gausnois
Gausnois - 11-Aug-2013 - rjb (64bit)
fftmagic (b,a) c 'print
Beginning VICAR task fftmagic
** FFTMAGIC version 25-JUL-2016 - (64bit) - RJB
 PIXELS CHANGED   SUM NEGATIVES   BETA
      935            -64666.9    0.00
     1140            -53404.1   -0.50
     1266            -40990.5    0.00
     1323            -36705.9   -0.50
     1368            -31463.0    0.00
     1393            -28978.7   -0.50
     1430            -25857.0    0.00
     1441            -24276.5   -0.50
     1432            -22160.0    0.00
     1441            -20979.8   -0.50
     1446            -19355.7    0.00
     1446            -18406.0   -0.50
     1463            -17078.4    0.00
     1464            -16283.2   -0.50
     1462            -15135.2    0.00
     1455            -14462.1   -0.50
     1462            -13516.4    0.00
     1458            -12957.5   -0.50
     1460            -12173.1    0.00
     1462            -11705.5   -0.50
     1462            -11035.8    0.00
     1451            -10629.1   -0.50
     1447            -10055.0    0.00
     1454             -9704.5   -0.50
     1452             -9213.3    0.00
     1451             -8911.1   -0.50
     1449             -8480.1    0.00
     1446             -8218.9   -0.50
     1449             -7842.3    0.00
     1449             -7609.8   -0.50
     1445             -7277.1    0.00
     1444             -7071.1   -0.50
     1436             -6769.7    0.00
     1428             -6580.4   -0.50
     1430             -6308.0    0.00
     1433             -6140.0   -0.50
     1431             -5899.0    0.00
     1430             -5746.3   -0.50
     1427             -5520.9    0.00
     1428             -5378.3   -0.50
     1427             -5174.2    0.00
     1426             -5045.4   -0.50
     1426             -4859.5    0.00
     1422             -4740.3   -0.50
     1427             -4569.2    0.00
     1430             -4463.1   -0.50
     1432             -4308.8    0.00
     1434             -4212.4   -0.50
     1436             -4072.0    0.00
     1431             -3982.9   -0.50
     1428             -3852.3    0.00
     1433             -3770.5   -0.50
     1431             -3653.3    0.00
     1431             -3578.6   -0.50
     1428             -3469.9    0.00
     1428             -3400.1   -0.50
     1425             -3297.8    0.00
     1424             -3231.8   -0.50
     1426             -3135.7    0.00
     1427             -3075.4   -0.50
     1429             -2986.4    0.00
     1426             -2929.2   -0.50
     1429             -2846.8    0.00
     1431             -2794.3   -0.50
     1429             -2717.1    0.00
     1428             -2667.1   -0.50
     1431             -2594.2    0.00
     1432             -2547.9   -0.50
     1424             -2479.6    0.00
     1418             -2435.3   -0.50
     1419             -2370.5    0.00
     1417             -2329.1   -0.50
     1415             -2267.7    0.00
     1416             -2228.2   -0.50
     1412             -2170.2    0.00
     1408             -2132.6   -0.50
     1405             -2076.9    0.00
     1407             -2041.1   -0.50
     1404             -1988.1    0.00
     1404             -1953.7   -0.50
     1404             -1902.9    0.00
     1403             -1870.1   -0.50
     1400             -1821.7    0.00
     1399             -1790.7   -0.50
     1398             -1744.9    0.00
     1398             -1715.4   -0.50
     1397             -1672.0    0.00
     1398             -1644.2   -0.50
     1396             -1603.0    0.00
     1398             -1576.5   -0.50
     1396             -1537.3    0.00
     1396             -1512.2   -0.50
     1399             -1475.2    0.00
     1398             -1451.3   -0.50
     1394             -1416.4    0.00
     1390             -1393.8   -0.50
     1390             -1360.4    0.00
     1390             -1338.8   -0.50
     1391             -1307.0    0.00
     1394             -1286.8   -0.50
 ITERATION ENDED ON PASS         100
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
