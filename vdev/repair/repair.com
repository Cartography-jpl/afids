$!****************************************************************************
$!
$! Build proc for MIPL module repair
$! VPACK Version 1.9, Tuesday, August 16, 2016, 20:04:55
$!
$! Execute by entering:		$ @repair
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
$ write sys$output "*** module repair ***"
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
$ write sys$output "Invalid argument given to repair.com file -- ", primary
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
$   if F$SEARCH("repair.imake") .nes. ""
$   then
$      vimake repair
$      purge repair.bld
$   else
$      if F$SEARCH("repair.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake repair
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @repair.bld "STD"
$   else
$      @repair.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create repair.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack repair.com -mixed -
	-s repair.f -
	-i repair.imake -
	-p repair.pdf -
	-t tstrepair.pdf tstrepair.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create repair.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C 2-JAN-95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C 8-MAR-96 ...TXH... CORRECTING ARRAY INDEXING PROBLEM ON SUBROUTINE RPR
C                    FR 88239; ON INVALID ARRAY INDEXING FOR THE ARRAY PRT
C

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
        implicit none
C
	EXTERNAL SAR,RPR
	INTEGER*4 ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IVAL(4)
        INTEGER*4 ICNT,IDEF,INUNIT,IOUTUNIT,ISL,ISS,ISTAT
        INTEGER*4 NAREA,NL,NLIN,NS,NSIN
        INTEGER*8 II,JJ
        REAL*4    CLEVEL,ROUND,VAR,XMEAN
	LOGICAL*4 XVPTST,QMV,QZOK
	CHARACTER*88   PRT
        CHARACTER*4 FMT

	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C								open datasets
	CALL IFMESSAGE('** REPAIR Version 9-Jul-2016 - (64bit) - RJB')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)

C               determine size parameter for abnormal conditions  
        CALL XVPARM ('SIZE',IVAL,ICNT,IDEF,0) 
        IF (IVAL(3).GT.0) THEN
           IF (IVAL(1).GT.IVAL(3) .OR. IVAL(3).GT.NLIN) THEN
              CALL MABEND (
     +         '??E - Invalid SIZE value.  Program terminated.')
           END IF
        END IF

        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      '??E - Invalid SIZE value.  Program terminated.')

        IF (IVAL(4).GT.0) THEN
           IF (IVAL(2).GT.IVAL(4) .OR. IVAL(4).GT.NSIN) THEN
              CALL MABEND (
     +         '??E - Invalid SIZE value.  Program terminated.') 
           END IF
        END IF

        IF (IVAL(2).GT.NSIN) CALL MABEND (
     +      '??E - Invalid SIZE value.  Program terminated.')

        CALL XVPARM ('SL',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      '??E - Invalid SL value.  Program terminated.')

        CALL XVPARM ('SS',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NSIN) CALL MABEND (
     +      '??E - Invalid SS value.  Program terminated.')

        CALL XVPARM ('NL',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      '??E - Invalid NL value.  Program terminated.')

        CALL XVPARM ('NS',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NSIN) CALL MABEND (
     +      '??E - Invalid NS value.  Program terminated.')

	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,' ')
        
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
C								get parameters
	QZOK = XVPTST('ZOK')
	QMV = XVPTST('MV')
	CALL XVPARM('CORR',CLEVEL,ICNT,IDEF,1)
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,1)
        IF (XMEAN.LT.0.0) CALL MABEND (
     +      '??E - MEAN parameter value must not be negative') 
	IF (IDEF.NE.1) QMV=.TRUE.
	CALL XVPARM('VARIANCE',VAR,ICNT,IDEF,1)
        IF (VAR.LT.0.0) CALL MABEND (
     +      '??E  - VARIANCE parameter value must not be negative') 
	IF (IDEF.NE.1) QMV=.TRUE.

C                           process the 'area', 'badline' & 'lineset' parameters
	CALL PRCESS(ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA)
C
	II = int8(4*NS)                 !REAL*4 (NS)
	JJ = int8(3*II)                 !REAL*4 (NS,3)
	IF (XVPTST('ALL')) THEN
	    CALL STACKA_BIG(16,SAR,2,II,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,
     +			NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
	ELSE 
C								   report limits
	    PRT='Correlation tolerance = '
	    WRITE(PRT(24:29),'(F6.3)' ) CLEVEL
	    CALL XVMESSAGE(PRT,' ')
	    IF (QMV) THEN
	        PRT='Mean tolerance = '
	        WRITE(PRT(17:27), '(F10.2)') XMEAN
		CALL XVMESSAGE(PRT,' ')
		PRT='Variance tolerance = '
		WRITE(PRT(21:31), '(F10.2)') VAR
		CALL XVMESSAGE(PRT,' ')
	    END IF                         
	    CALL STACKA_BIG(16,RPR,2,II,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,
     +			NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR(OBUF,II,BUF,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,
     +			INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
C
C	This routine replaces all areas specified, without any statistical
C	testing. Its function is the same as the old VICAR program SAR.
C
        implicit none
        INTEGER*4 NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS
        INTEGER*4 I,J,IEL,ISLBAD,ISLGOOD,ISTAT,LINE,NLBAD
        INTEGER*8 II,JJ
        REAL*4 ROUND,X
	REAL*4 OBUF(NS),BUF(NS,3)
	INTEGER*4 ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +				'NSAMPS',NS,' ')
		    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
		    LINE = LINE+1
		END DO
		ISLGOOD = ISLBAD+NLBUF(I)
		IF (ISLGOOD.LE.NL) THEN
C							Get the next good line
		    CALL XVREAD(INUNIT,BUF(1,2),ISTAT,'LINE',ISLGOOD,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    NLBAD = NLBUF(I)
C							Fix all lines in set
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL AVE(BUF(1,1),BUF(1,2),OBUF,NS,X,ROUND)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                  'NSAMPS',NS,' ')
			END DO
		    ELSE
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL XVREAD(INUNIT,OBUF,ISTAT,'LINE',LINE,
     +					'SAMP',ISS,'NSAMPS',NS,' ')
			    CALL AVE(BUF(ISSBUF(I),1),BUF(ISSBUF(I),2),
     +				    OBUF(ISSBUF(I)),NSBUF(I),X,ROUND)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			    LINE = LINE+1
			END DO
		    END IF
		ELSE
C						Bad lines go to the bottom of
C						the image, extend last good line
		    NLBAD = NLBUF(I)
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			END DO
		    ELSE
			DO J=1,NLBAD
			    CALL XVREAD(INUNIT,OBUF,ISTAT,'LINE',LINE,
     +					'SAMP',ISS,'NSAMPS',NS,' ')
			    CALL MVE(7,NSBUF(I),BUF(ISSBUF(I),1),
     +				     OBUF(ISSBUF(I)),1,1)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			    LINE = LINE+1
			END DO
		    END IF
		END IF
		LINE = ISLGOOD
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR(OBUF,II,BUF,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,
     +			INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
        implicit none
        INTEGER*4 NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS,IAREA
        INTEGER*4 I,J,N,ISTAT,LINE,NLBAD
        INTEGER*4 IELTEST,ISLTEST,ISSTEST,LAST,LASTGOOD
        INTEGER*4 LINX,LOC,NEXT,NSTEST
        INTEGER*8 II,JJ
        REAL*4 ROUND,X
	REAL*4 OBUF(NS),BUF(NS,3)
C                                ,AVG(3),VAR(3)
	INTEGER*4 ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
	LOGICAL*4 QBAD,DIFFER
	LOGICAL*4 QFIRST/.TRUE./
	CHARACTER*88  PRT
       
        DATA PRT/'                                                                                       '/
C
C       
	DO I=1,NL
	    IF (I.EQ.1) THEN
		LOC = 1			! LOC is the length of the print buffer
		LAST = 1		! LAST is the index of last good line
		LINX = 2		! LINX is the index to the test line
		NEXT = 3		! NEXT is the index to the next line
		LINE = ISL
		LASTGOOD = LINE-1
		IAREA = 0
		IELTEST = -1
	        nstest = ns
		CALL XVREAD(INUNIT,BUF(1,LINX),ISTAT,'LINE',ISL,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		CALL XVREAD(INUNIT,BUF(1,NEXT),ISTAT,'SAMP',ISS,
     +			    'NSAMPS',NS,' ')
		CALL MVE(7,NS,BUF(1,NEXT),BUF(1,LAST),1,1)
	    ELSE IF (I.EQ.NL) THEN
		CALL MVE(7,NS,BUF(1,LAST),BUF(1,NEXT),1,1)
	    ELSE
		CALL XVREAD(INUNIT,BUF(1,NEXT),ISTAT,'SAMP',ISS,
     +			    'NSAMPS',NS,' ')
	    END IF
C						Get the next area to be tested
	    IF (LINE.GT.IELTEST) THEN
		IAREA = IAREA+1
		DO WHILE (NLBUF(IAREA).LE.0 .AND. IAREA.LT.NAREA)
		    IAREA = IAREA+1
		END DO
		IF (IAREA.LE.NAREA) THEN
		    ISLTEST = ISLBUF(IAREA)
		    ISSTEST = ISSBUF(IAREA)
		    IELTEST = ISLTEST + NLBUF(IAREA) -1
		    NSTEST = NSBUF(IAREA)
		ELSE
		    ISLTEST = 999999
		    IELTEST = 999999
		END IF
	    END IF
C
	    QBAD = .FALSE.
	    IF (LINE.GE.ISLTEST) THEN
		QBAD  = DIFFER(BUF(ISSTEST,LAST),BUF(ISSTEST,LINX),
     +			       BUF(ISSTEST,NEXT),NSTEST)
	    END IF

C
	    IF (QBAD) THEN
		IF (QFIRST) THEN
		    CALL XVMESSAGE
     +                   ('The following lines were repaired:',' ')
		    QFIRST = .FALSE.
		END IF
		WRITE(PRT(LOC:LOC+4), '(I5)') LINE
		LOC = LOC+5
		IF (LOC.GE.77) THEN
		    CALL XVMESSAGE(PRT(1:88),' ')
		    LOC = 1 
                ELSE
                    WRITE(PRT(LOC:LOC+4), '(A5)') '     '
		END IF
		LINE = LINE+1
		N = LINX
		LINX = NEXT
		NEXT = N
	    ELSE
C						If good, write out line and any
C						repaired lines that are needed.
C						Update pointers.
		IF (LASTGOOD.NE.LINE-1) THEN
		    NLBAD = LINE-LASTGOOD-1
		    DO J=1,NLBAD
			X = FLOAT(J)/FLOAT(NLBAD+1)
			CALL AVE(BUF(1,LAST),BUF(1,LINX),OBUF,NS,X,
     +				 ROUND)
			CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,'NSAMPS',NS,' ')
		    END DO
		END IF
		CALL XVWRIT(IOUTUNIT,BUF(1,LINX),ISTAT,'NSAMPS',NS,' ')
		LASTGOOD = LINE
		LINE = LINE+1
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = LINE-LASTGOOD-1
	    DO J=1,N
		CALL XVWRIT(IOUTUNIT,BUF(1,LAST),ISTAT,'NSAMPS',NS,' ')
	    END DO
	END IF
	IF (LOC.NE.1) CALL XVMESSAGE(PRT(1:LOC),' ')
	IF (QFIRST) CALL XVMESSAGE('No bad lines found',' ')
	RETURN
	END
C*************************************************************************
	SUBROUTINE PRCESS(ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NX)
C
C	Routine to gather and collate the  AREA, LINESET, and BADLINE 
C	parameters. This includes sorting the regions by SL, and combining
C	overlapping regions.
C
        implicit none
        INTEGER*4 I,J,K,ISS,NL,NS,NX,ICNT,IDEF,IEL,IES1,N1,N2,N3,N4
	INTEGER*4 ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBUF(400)
C
	NX = 0
C						   Get regions specified by AREA
	CALL XVPARM('AREA',IBUF,ICNT,IDEF,400)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,4).NE.0) CALL MABEND( 
     +          '??E - AREA parameter values must be in sets of 4')
	    DO I=1,ICNT,4
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = IBUF(I+1)-ISS+1
		NLBUF(NX) = IBUF(I+2)
		NSBUF(NX) = IBUF(I+3)

C                                      checking invalid AREA parameters 
                IF ((NL.LT.NLBUF(NX)) .OR. (NS.LT.NSBUF(NX))) THEN
                    CALL MABEND (
     +                 '??E - AREA parameters NL or NS may be out of range')
                ELSEIF ((NL.LT.ISLBUF(NX)) .OR. (NS.LT.ISSBUF(NX))) THEN
                    CALL MABEND (
     +                 '??E - AREA parameters SL or SS may be out of range')
                ELSEIF (NSBUF(NX).LE.0 .OR. NLBUF(NX).LE.0) THEN
                    CALL MABEND (
     +                 '??E - AREA parameters NL and NS must be positive')
                ELSEIF ((ISLBUF(NX).LE.0) .OR. (ISSBUF(NX).LE.0)) THEN
                    CALL MABEND (
     +                 '??E - AREA parameters SL and SS must be positive')

                END IF 
	    END DO
	END IF
C						  Get lines specified by LINESET
	CALL XVPARM('LINESET',IBUF,ICNT,IDEF,200)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,2).NE.0) CALL MABEND(
     +		'??E - LINESET parameter values must be in sets of 2')
	    DO I=1,ICNT,2
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = IBUF(I+1)
                IF (NL.LT.NLBUF(NX) .OR. NL.LT.ISLBUF(NX)) THEN
                   CALL MABEND (
     +                  '??E - LINESET parameters may be out of range')
                ELSE IF (NLBUF(NX).LE.0 .OR. ISLBUF(NX).LE.0) THEN
                   CALL MABEND (
     +                  '??E - LINESET parameters must be positive') 
                END IF 
		NSBUF(NX) = NS
	    END DO
	END IF
C						  Get lines specified by BADLINE
	CALL XVPARM('BADLINE',IBUF,ICNT,IDEF,100)
	IF (ICNT.NE.0) THEN
	    DO I=1,ICNT
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
                IF (NL .LT. ISLBUF(NX)) THEN
                   CALL MABEND (
     +                  '??E - BADLINE parameters may be out of range')
                ELSE IF (ISLBUF(NX).LE.0) THEN
                   CALL MABEND (
     +                  '??E - BADLINE parameters must be positive')
                END IF 
		ISSBUF(NX) = 1
		NLBUF(NX) = 1
		NSBUF(NX) = NS
	    END DO
	END IF
C
	IF (NX.LE.0) THEN
	    NX = 1
	    ISLBUF(1) = 1
	    ISSBUF(1) = 1
	    NLBUF(1) = NL
	    NSBUF(1) = NS
	ELSE IF (NX.GT.1) THEN
C					put the areas into line ascending order
	    DO J=1,NX-1
		DO K=J+1,NX
		    IF(ISLBUF(J).GT.ISLBUF(K))  THEN
			N1 = ISLBUF(J)
			N2 = ISSBUF(J)
			N3 = NLBUF(J)
			N4 = NSBUF(J)
			ISLBUF(J) = ISLBUF(K)
			ISSBUF(J) = ISSBUF(K)
			NLBUF(J) = NLBUF(K)
			NSBUF(J) = NSBUF(K)
			ISLBUF(K) = N1
			ISSBUF(K) = N2
			NLBUF(K)= N3
			NSBUF(K)= N4
		    END IF
		END DO
	    END DO
	END IF
C						  take care of overlapping areas
	DO J=1,NX-1
	    IF (NLBUF(J).NE.0) THEN
		IEL = ISLBUF(J)+NLBUF(J)-1
		K = J+1
		DO WHILE (IEL.GE.ISLBUF(K) .AND. K.LE.NX)
		    IEL = MAX(IEL,ISLBUF(K)+NLBUF(K)-1)
		    NLBUF(J) = IEL-ISLBUF(J)+1
		    NLBUF(K) = 0
		    ISSBUF(J) = MIN(ISSBUF(J),ISSBUF(K))
		    IES1 = MAX(NSBUF(J)+ISSBUF(J),NSBUF(K)+ISSBUF(K))
		    NSBUF(J) = IES1-ISSBUF(J)
		    K = K+1
		END DO
	    END IF
	END DO
C
	RETURN
	END
C***********************************************************************
	SUBROUTINE AVE(BUF1,BUF2,OBUF,NS,X,ROUND)
C
C	This routine does a linear interpolation between BUF1 and BUF2,
C	putting the result in OBUF. X is the fractional weight to be given
C	BUF2.
C
        implicit none
        INTEGER*4 I,NS
        REAL*4 X,Y,ROUND
	REAL*4 BUF1(NS),BUF2(NS),OBUF(NS)
C
	Y = 1.0-X
	DO I=1,NS
	    OBUF(I) = Y*BUF1(I) + X*BUF2(I) + ROUND
	END DO
	RETURN
	END
C***********************************************************************
	LOGICAL FUNCTION DIFFER(BUF1,BUF2,BUF3,NS)
C
C	This routine computes the mean and variance of BUF2, and the
C	correlation coefficient between BUF1 and BUF2. It then verifies
C	that the two lines in BUF1 and BUF2 are similar, within the limits
C	specified by the TESTVALS
C		BUF1	Input array, containing the last good line.
C		BUF2	Input array, containing the test line.
C		BUF3	Input array, containing the next line.
C		NS	Input, the number of pixels on each line.
C
        implicit none
        INTEGER*4 I,NS
        REAL*4  AVGX,AVGY,CLEVEL,RHO,VAR,VARX,VARY,X,XMEAN,Y
	REAL*8 SUMX,SUMY,SUMSQX,SUMSQY,XY
	REAL*4 BUF1(NS),BUF2(NS),BUF3(NS)
	LOGICAL*4 QMV,QZOK 

	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	SUMX = 0.0
	SUMY = 0.0
	SUMSQX = 0.0
	SUMSQY = 0.0
	XY = 0.0
C								gather raw stats
	DO I=1,NS
	    X = BUF2(I)
	    Y = BUF1(I)
	    SUMX = SUMX + X
	    SUMY = SUMY + Y
	    SUMSQX = SUMSQX + X*X
	    SUMSQY = SUMSQY + Y*Y
	    XY = XY + X*Y
	END DO
C
	AVGX = SUMX/NS
	AVGY = SUMY/NS
	VARX = SUMSQX/NS - AVGX*AVGX
	VARY = SUMSQY/NS - AVGY*AVGY
	IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
	    IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		RHO = 1.0
	    ELSE
		RHO = 0.0
	    END IF
	ELSE
	    RHO = (XY/NS-AVGX*AVGY)/SQRT(VARX*VARY)
	END IF
C						determine whether the test line
C						is unlike the reference line
        IF (RHO.LT.CLEVEL) THEN
            DIFFER = .TRUE.
        ELSEIF (QMV) THEN 
           IF ((ABS(AVGX-AVGY).GE.XMEAN 
     +                   .OR. ABS(VARX-VARY).GE.VAR)) THEN 
              DIFFER = .TRUE.
           ELSE
              DIFFER = .FALSE.
           ENDIF
	ELSE
	    DIFFER = .FALSE.
        ENDIF
C
C					If the line is different than the last
C					good line, compare to the average of the
C					next line and the last good line.
	IF (DIFFER) THEN
C
	    SUMY = 0.0
	    SUMSQY = 0.0
	    XY = 0.0
	    DO I=1,NS
		Y = (BUF1(I)+BUF3(I))/2.0
		SUMY = SUMY + Y
		SUMSQY = SUMSQY + Y*Y
		XY = XY + BUF2(I)*Y
	    END DO
C
	    AVGY = SUMY/NS
	    VARY = SUMSQY/NS - AVGY*AVGY
	    IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
		IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		    RHO = 1.0
		ELSE
		    RHO = 0.0
		END IF
	    ELSE
		RHO = (XY/NS-AVGX*AVGY)/SQRT(VARX*VARY)
	    END IF
C						determine whether the test line
C						is unlike the possible new line
	    IF (RHO.GE.CLEVEL) THEN
		IF (.NOT.QMV .OR. (ABS(AVGX-AVGY).LE.XMEAN .AND. 
     +		    ABS(VARX-VARY).LE.VAR))    DIFFER = .FALSE.
	    END IF
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create repair.imake
#define  PROGRAM repair

#define MODULE_LIST repair.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create repair.pdf
process help=*
 PARM INP         TYPE=STRING
 PARM OUT         TYPE=STRING
 PARM SIZE        TYPE=INTEGER         COUNT=4 +
             VALID=(1:32767,1:32767,0:32767,0:32767)    DEFAULT=(1,1,0,0)
 PARM SL          TYPE=INTEGER         COUNT=1 +
             VALID=(1:32767)                            DEFAULT=1
 PARM SS          TYPE=INTEGER         COUNT=1 +
             VALID=(1:32767)                            DEFAULT=1
 PARM NL          TYPE=INTEGER         COUNT=1 +
             VALID=(0:32767)                            DEFAULT=0
 PARM NS          TYPE=INTEGER         COUNT=1 +
             VALID=(0:32767)                            DEFAULT=0
 PARM MODE	  TYPE=KEYWORD	VALID=(MV,CORR,ALL)	DEFAULT=CORR
 PARM AREA	  TYPE=INTEGER	       COUNT=(0:400)	DEFAULT=--
 PARM BADLINE	  TYPE=INTEGER	       COUNT=(0:100)	DEFAULT=--
 PARM LINESET	  TYPE=INTEGER	       COUNT=(0:200)	DEFAULT=--
 PARM CORR	  TYPE=REAL	VALID=(0.0:1.0)		DEFAULT=0.7
 PARM MEAN	  TYPE=REAL				DEFAULT=10.0
 PARM VARIANCE	  TYPE=REAL				DEFAULT=200.0
 PARM ZOK	  TYPE=KEYWORD	VALID=ZOK COUNT=(0:1)	DEFAULT=--
 END-PROC
!#  annot function=(corrections/noise)
!#  annot project=all; keywords=(repair, interpolation, "bad lines")
!#  annot icon=repair.xpm
.TITLE
 REPAIR --  Find and replace bad lines (or line segments) in images
.HELP
      REPAIR identifies bad lines (lines that are inconsistent with the
surrounding lines) and replaces them by interpolation of the nearest good 
lines. If the locations of the bad lines or line segments are known, the user 
may list the offending lines and use the ALL mode to repair just those lines.
If the locations of the bad lines are unknown, the program decides which lines
are bad by computing the interline correlation (and optionally the differences
in the means and the variances).
.PAGE
 OPERATION
      There are three modes of operation to REPAIR. The CORR and MV modes 
differ only in the tests employed to determine bad lines, but the ALL mode 
does no statistical testing, and is more closely related to the earlier VICAR 
program SAR. 
      In the ALL mode, lines or regions are specified by the user via the AREA,
LINESET, or BADLINE parameters. These regions are replaced by means of a linear
interpolation, using the lines immediately above and below the region. 
Interpolation is performed in the line direction only, and the area outside the
specified regions is unchanged.
      In the CORR and MV modes, the AREA, LINESET, and BADLINE parameters are
used to identify regions to be examined for bad lines. If all three parameters
are defaulted, the entire image is tested. Each line that is to be checked is
tested by the following procedure:
.PAGE
      A. The line is compared to two reference lines: the last good line, and 
         the average of the next line and the last good line.
      B. If the correlation coefficients between the test line and both of the
         reference lines is less than the value of CORR, then the line is 
         considered bad. There is one exception to this test. If the ZOK (Zero
         OK) keyword is in effect, lines that are all zeroes are passed as
         good. The correlation coefficient is undefined for lines of constant 
         value, requiring this special case. A correlation coefficient of 0.0 
         is assigned to all other cases involving lines of constant DN.
      C. If the MV (Mean and Variance) mode is in effect, then two additional 
         tests are employed. The test line is considered bad if its mean is 
         different than the means of the reference lines by more than the value 
         of MEAN, or if its variance is different than both the reference line 
         variances by more than the value of VARIANCE.
 If a line is found to be bad, the entire line is replaced by a linear
 interpolation of the last good line and the next good line. 
.PAGE
      This algorithm is fairly sensitive to the values of the CORR, MEAN, and
 VARIANCE parameters. It may be necessary to run this program more than once
 in order to find an appropriate set of values. In general, the CORR and 
 VARIANCE values provide the best tests for random noise and hashed lines. The
 test of the means should only rarely be needed to reject a line.
      The coordinates used in the AREA parameter (SL,SS,NL,NS,...) refer to the
 input image, not the output image. This is important only if, in the size
 field, the starting line or starting sample is not one. 
      The user should also be aware that if two regions specified by AREA, 
 LINESET, or BADLINE contain the same line, the two regions will be combined 
 into one larger region that contains both original regions. This can cause
 problems, especially in the ALL mode, where more pixels may be modified than
 were intended.
      This program will run on byte, halfword, fullword, or real data. Up to
 100 regions may by given by each of the AREA, LINESET, and BADLINE parameters.
 If either MEAN or VARIANCE is not defaulted, the MV mode is automatically
 used.
.PAGE
 ORIGINAL PROGRAMMER:  John Addington
 
 CONVERTED TO VAX BY:  Ron Alley,     March 31, 1987

 PORTED to UNIX BY:    Randy Schenk (CRI) 2-Jan-95
 
 CURRENT COGNIZANT PROGRAMMER: Ray Bambery

  19 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
 output image window
 (SL,SS,NL,NS)
.VARIABLE SL
 starting line
.VARIABLE SS
 starting sample
.VARIABLE NL
 number of lines
.VARIABLE NS
 number of samples
.VARIABLE MODE
CORR, MV, or ALL
.VARIABLE AREA
Sets of (SL,SS,NL,NS) to
be tested for bad lines
.VARIABLE LINESET
Sets of lines to be tested
(SL,NL,SL,NL,...)
.VARIABLE BADLINE
Lines to be tested
.VARIABLE CORR
Tolerance level for
interline correlation.
.VARIABLE MEAN
Tolerance level for
difference in means.
.VARIABLE VARIANCE
Tolerance level for
difference in variances.
.VARIABLE ZOK
Are lines of Zero DN OK?
.LEVEL2
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
 image size (SL,SS,NL,NS)
.VARIABLE SL
 The first line of the input image to be output.
.VARIABLE SS
 The first pixel of each input line to be output.
.VARIABLE NL
 The number of lines to be output.
.VARIABLE NS
 The number of pixels to be output for each line.
.VARIABLE MODE
      The program REPAIR operates in three different modes: CORR (the default),
 MV, and ALL. 
      If ALL is specified, no statistical checking is performed, and all 
 regions specified by the AREA, LINESET, and BADLINE parameters are replaced
 by interpolating the nearest two lines. This is similar to the function of
 the defunct VICAR program SAR.
      In the CORRelation mode, a line is considered to be a good line if the
 correlation coefficient between it and the last good line is greater than the
 threshold value specified by CORR. The line is also considered good if its
 correlation with the average of the last good line and the next line is above
 the threshold.
      If MV is specified, two tests are made, in addition to the correlation
 test. The difference in means must be less than the value specified by MEAN,
 and the difference in variances must be less than the value specified by
 VARIANCE. If a line passes all three tests, it is considered good.
.VARIABLE AREA
      The AREA, LINESET, and BADLINE parameters are used to limit the portion
 of the image in which lines may be replaced. BADLINE is used to identify
 individual lines to be checked. LINESET is used to specify sets of lines,
 in the format (SL1,NL1,SL2,NL2,...), when the general region of concern is
 known, but not the exact location. The AREA parameter allows the user to
 select a portion of the lines to be examined. The values for AREA must be 
 specified in sets of four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      Each of these parameters accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE LINESET
      The AREA, LINESET, and BADLINE parameters are used to limit the portion
 of the image in which lines may be replaced. BADLINE is used to identify
 individual lines to be checked. LINESET is used to specify sets of lines,
 in the format (SL1,NL1,SL2,NL2,...), when the general region of concern is
 known, but not the exact location. The AREA parameter allows the user to
 select a portion of the lines to be examined. The values for AREA must be 
 specified in sets of four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      Each of these parameters accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE BADLINE
      The AREA, LINESET, and BADLINE parameters are used to limit the portion
 of the image in which lines may be replaced. BADLINE is used to identify
 individual lines to be checked. LINESET is used to specify sets of lines,
 in the format (SL1,NL1,SL2,NL2,...), when the general region of concern is
 known, but not the exact location. The AREA parameter allows the user to
 select a portion of the lines to be examined. The values for AREA must be 
 specified in sets of four, (SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...). 
      If the MODE is ALL, then all pixels in the regions specified are replaced 
 by interpolation of the adjacent two lines. 
      If MODE is CORR or MV, these values describe the region from which 
 statistics are gathered. If a line is found to be  bad, the entire line is 
 replaced. 
      Each of these parameters accept up to 100 regions, and all three may
 be used in the same task. However, if two of the regions contain a common
 line, then the regions are combined (and perhaps enlarged) into the 
 rectangular region that contains both of them.
.VARIABLE CORR
      The parameter CORR sets the minimum value of the correlation coefficient
 between lines for a line to be considered good. Correlation coefficients may
 range from 1.0 (two lines that match perfectly) to -1.0 (one line perfectly
 matches the complement of the other).
.VARIABLE MEAN
       The value of MEAN is the maximum allowed difference between the means
 of two lines for a line to be considered good.
.VARIABLE VARIANCE
       The value of VARIANCE is the maximum allowed difference between the 
 variances of two lines for a line to be considered good. The variance is the
 square of the standard deviation.
.VARIABLE ZOK
       The keyword ZOK is used when lines that are entirely 0 DN may be
present in the picture, and the user does not want them replaced. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstrepair.pdf
PROCEDURE
! Jul 25, 2016- RJB
! TEST SCRIPT FOR REPAIR
! tests BYTE images
!
! Vicar Programs:
!       gen adl append list
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
BODY
let $echo="no"
let $autousage="none"
let _onfail="stop"
let $echo="yes"
gen A 50 100 IVAL=100 LINC=0 SINC=1
adl A B ADD=(20,10,1,10,60,100,20,1,20,50,-50,20,51,20,100,8,30,1,30,100)
gen C 1 100 IVAL=50 SINC=2
append (B,C,A) D
list D sl=8 ss=8 nl=15 ns=10
list D sl=28 ss=8 nl=6 ns=10
list D sl=48 ss=8 nl=6 ns=10
let $echo="no"
Write "<<<<  This is the original unit test  >>>>"  
write "<<<< bad lines at 10, 20, 30, and 51  >>>>"
let $echo="yes"
repair D E
list E sl=8 ss=8 nl=15 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED LINE 20"

let $echo="yes"
repair D E LINESET=(1,18,25,50)
list E sl=8 ss=8 nl=15 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED NOTHING"

let $echo="yes"
repair D E CORR=.99
list E sl=8 ss=8 nl=15 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED 10 20"

let $echo="yes"
repair D E 'MV
list E sl=8 ss=8 nl=15 ns=10
list E sl=48 ss=8 nl=6 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED 10 20 51"

let $echo="yes"
repair D E MEAN=7
list E sl=8 ss=8 nl=15 ns=10
list E sl=28 ss=8 nl=6 ns=10
list E sl=48 ss=8 nl=6 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED 10 20 30 51"
let $echo="yes"
repair D E 'zok
list E sl=8 ss=8 nl=15 ns=10
let $echo="no"
Write "IT SHOULD HAVE REPAIRED LINE 20 ONLY"
write "  "
Write "*********  EXTREME ******************"
Write "*********  EXTREME ******************"
write "  "
Write "<<<<  The following is the extreme value test  >>>>" 
Write "Most of these processes will generate ABEND termination"
let _onfail = "continue"
write " "
write "<<<<<      1st 10 extreme values cases    >>>>>"
write " "
let $echo="yes"
repair D E size=(1,1,0,0) 

repair D E size=(32767,1,0,0)

repair D E size=(1,32767,0,0)

repair D E size=(1,1,1,0)

repair D E size=(1,1,32767,0)

repair D E size=(1,1,0,1)

repair D E size=(1,1,0,32767)

repair D E sl=1 

repair D E sl=32767

repair D E ss=1
let $echo="no"
write " "
write "<<<<<      2nd 10 extreme values cases  >>>>>"
write " "
let $echo="yes"
repair D E ss=32767 

repair D E nl=1

repair D E nl=0

repair D E nl=32767 

repair D E ns=1

repair D E ns=0

repair D E ns=32767 

repair D E 'mv

repair D E 'corr

repair D E 'all
let $echo="no"
write " "
write "<<<<<      3rd 10 extreme values cases  >>>>>"
write " "
let $echo="yes"
repair D E area=(1,1,100,100)

repair D E area=(0,1,100,100)

repair D E area=(-1,1,100,100)

repair D E area=(3489743,1,100,100)

repair D E area=(1,0,100,100)

repair D E area=(1,-1,100,100)

repair D E area=(1,3489743,100,100)

repair D E area=(1,1,0,100)

repair D E area=(1,1,-1,100)

repair D E area=(1,1,3489743,100)
let $echo="no"
write " "
write "<<<<<      4th 10 extreme values cases  >>>>>"
write " "
let $echo="yes"

repair D E area=(1,1,100,0)

repair D E area=(1,1,100,-1)

repair D E area=(1,1,100,3489743)

repair D E badline=(1,1,100,100)

repair D E badline=(0,1,100,100)

repair D E badline=(-1,1,100,100)

repair D E badline=(3489743,1,100,100)

repair D E badline=(1,0,100,100)

repair D E badline=(1,-1,100,100)

repair D E badline=(1,3489743,100,100)
let $echo="no"
write " "
write "<<<<<      5th 10 extreme values cases  >>>>>"
write " "
let $echo="yes"

repair D E badline=(1,1,0,100)

repair D E badline=(1,1,-1,100)

repair D E badline=(1,1,3489743,100)

repair D E badline=(1,1,100,0)

repair D E badline=(1,1,100,-1)

repair D E badline=(1,1,100,3489743)

repair D E lineset=(1,1,100,100)

repair D E lineset=(0,1,100,100)

repair D E lineset=(-1,1,100,100)

repair D E lineset=(3489743,1,100,100)
let $echo="no"
write " "
write "<<<<<      6th 10 extreme values cases  >>>>>"
write " "
let $echo="yes"

repair D E lineset=(1,0,100,100)

repair D E lineset=(1,-1,100,100)

repair D E lineset=(1,3489743,100,100)

repair D E lineset=(1,1,0,100)

repair D E lineset=(1,1,-1,100)

repair D E lineset=(1,1,3489743,100)

repair D E lineset=(1,1,100,0)

repair D E lineset=(1,1,100,-1)

repair D E lineset=(1,1,100,3489743)

repair D E corr=0.0
let $echo="no"
write " "
write "<<<<<      Final 8 extreme values cases  >>>>>"
write " "
let $echo="yes"

repair D E corr=0.5

repair D E corr=0.999999

repair D E mean=0.0

repair D E mean=-1.0

repair D E mean=3489743.0

list D sl=7 ss=8 nl=15 ns=10
repair D E variance=0.0
list E sl=7 ss=8 nl=15 ns=10

repair D E variance=-1.0

repair D E variance=3489743.0


let $echo="no"

END-PROC
$!-----------------------------------------------------------------------------
$ create tstrepair.log
tstrepair
gen A 50 100 IVAL=100 LINC=0 SINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl A B ADD=(20,10,1,10,60,100,20,1,20,50,-50,20,51,20,100,8,30,1,30,100)
Beginning VICAR task adl
ADL version 16-Feb-06
gen C 1 100 IVAL=50 SINC=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
append (B,C,A) D
Beginning VICAR task append
 OUTPUT: NL=   101, NS=   100, FORMAT=BYTE.

list D sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:APPEND    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     127 128 129 130 131 132 133 134 135 136
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     207 208 209 210 211 212 213 214 215 216
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
list D sl=28 ss=8 nl=6 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:APPEND    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
     28     107 108 109 110 111 112 113 114 115 116
     29     107 108 109 110 111 112 113 114 115 116
     30     115 116 117 118 119 120 121 122 123 124
     31     107 108 109 110 111 112 113 114 115 116
     32     107 108 109 110 111 112 113 114 115 116
     33     107 108 109 110 111 112 113 114 115 116
list D sl=48 ss=8 nl=6 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:APPEND    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
     48     107 108 109 110 111 112 113 114 115 116
     49     107 108 109 110 111 112 113 114 115 116
     50     107 108 109 110 111 112 113 114 115 116
     51      64  66  68  70  72  74  76  78  80  82
     52     107 108 109 110 111 112 113 114 115 116
     53     107 108 109 110 111 112 113 114 115 116
let $echo="no"
<<<<  This is the original unit test  >>>>
<<<< bad lines at 10, 20, 30, and 51  >>>>
repair D E
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     127 128 129 130 131 132 133 134 135 136
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED LINE 20
repair D E LINESET=(1,18,25,50)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
No bad lines found
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     127 128 129 130 131 132 133 134 135 136
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     207 208 209 210 211 212 213 214 215 216
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED NOTHING
repair D E CORR=.99
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.990
The following lines were repaired:
   10   20
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     107 108 109 110 111 112 113 114 115 116
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED 10 20
repair D E 'MV
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =     10.00
Variance tolerance =    200.00
The following lines were repaired:
   10   20   51
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     107 108 109 110 111 112 113 114 115 116
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
list E sl=48 ss=8 nl=6 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
     48     107 108 109 110 111 112 113 114 115 116
     49     107 108 109 110 111 112 113 114 115 116
     50     107 108 109 110 111 112 113 114 115 116
     51     107 108 109 110 111 112 113 114 115 116
     52     107 108 109 110 111 112 113 114 115 116
     53     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED 10 20 51
repair D E MEAN=7
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =      7.00
Variance tolerance =    200.00
The following lines were repaired:
   10   20   30   51
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:22 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     107 108 109 110 111 112 113 114 115 116
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
list E sl=28 ss=8 nl=6 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:22 2016
     Samp     8      10      12      14      16
   Line
     28     107 108 109 110 111 112 113 114 115 116
     29     107 108 109 110 111 112 113 114 115 116
     30     107 108 109 110 111 112 113 114 115 116
     31     107 108 109 110 111 112 113 114 115 116
     32     107 108 109 110 111 112 113 114 115 116
     33     107 108 109 110 111 112 113 114 115 116
list E sl=48 ss=8 nl=6 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:22 2016
     Samp     8      10      12      14      16
   Line
     48     107 108 109 110 111 112 113 114 115 116
     49     107 108 109 110 111 112 113 114 115 116
     50     107 108 109 110 111 112 113 114 115 116
     51     107 108 109 110 111 112 113 114 115 116
     52     107 108 109 110 111 112 113 114 115 116
     53     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED 10 20 30 51
repair D E 'zok
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
list E sl=8 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:22 2016
     Samp     8      10      12      14      16
   Line
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     127 128 129 130 131 132 133 134 135 136
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
     22     107 108 109 110 111 112 113 114 115 116
let $echo="no"
IT SHOULD HAVE REPAIRED LINE 20 ONLY
  
*********  EXTREME ******************
*********  EXTREME ******************
  
<<<<  The following is the extreme value test  >>>>
Most of these processes will generate ABEND termination
 
<<<<<      1st 10 extreme values cases    >>>>>
 
repair D E size=(1,1,0,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E size=(32767,1,0,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SIZE value.  Program terminated.
 ** ABEND called **
continue
repair D E size=(1,32767,0,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SIZE value.  Program terminated.
 ** ABEND called **
continue
repair D E size=(1,1,1,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E size=(1,1,32767,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SIZE value.  Program terminated.
 ** ABEND called **
continue
repair D E size=(1,1,0,1)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E size=(1,1,0,32767)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SIZE value.  Program terminated.
 ** ABEND called **
continue
repair D E sl=1
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E sl=32767
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SL value.  Program terminated.
 ** ABEND called **
continue
repair D E ss=1
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
let $echo="no"
 
<<<<<      2nd 10 extreme values cases  >>>>>
 
repair D E ss=32767
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid SS value.  Program terminated.
 ** ABEND called **
continue
repair D E nl=1
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
No bad lines found
repair D E nl=0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E nl=32767
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid NL value.  Program terminated.
 ** ABEND called **
continue
repair D E ns=1
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16
   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32
   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48
   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64
   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80
   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96
   97   98   99  100  101
repair D E ns=0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E ns=32767
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - Invalid NS value.  Program terminated.
 ** ABEND called **
continue
repair D E 'mv
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =     10.00
Variance tolerance =    200.00
The following lines were repaired:
   10   20   51
repair D E 'corr
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E 'all
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
let $echo="no"
 
<<<<<      3rd 10 extreme values cases  >>>>>
 
repair D E area=(1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
The following lines were repaired:
   20
repair D E area=(0,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL and SS must be positive
 ** ABEND called **
continue
repair D E area=(-1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL and SS must be positive
 ** ABEND called **
continue
repair D E area=(3489743,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL or SS may be out of range
 ** ABEND called **
continue
repair D E area=(1,0,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL and SS must be positive
 ** ABEND called **
continue
repair D E area=(1,-1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL and SS must be positive
 ** ABEND called **
continue
repair D E area=(1,3489743,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters SL or SS may be out of range
 ** ABEND called **
continue
repair D E area=(1,1,0,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL and NS must be positive
 ** ABEND called **
continue
repair D E area=(1,1,-1,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL and NS must be positive
 ** ABEND called **
continue
repair D E area=(1,1,3489743,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL or NS may be out of range
 ** ABEND called **
continue
let $echo="no"
 
<<<<<      4th 10 extreme values cases  >>>>>
 
repair D E area=(1,1,100,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL and NS must be positive
 ** ABEND called **
continue
repair D E area=(1,1,100,-1)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL and NS must be positive
 ** ABEND called **
continue
repair D E area=(1,1,100,3489743)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - AREA parameters NL or NS may be out of range
 ** ABEND called **
continue
repair D E badline=(1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
No bad lines found
repair D E badline=(0,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(-1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(3489743,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters may be out of range
 ** ABEND called **
continue
repair D E badline=(1,0,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,-1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,3489743,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters may be out of range
 ** ABEND called **
continue
let $echo="no"
 
<<<<<      5th 10 extreme values cases  >>>>>
 
repair D E badline=(1,1,0,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,1,-1,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,1,3489743,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters may be out of range
 ** ABEND called **
continue
repair D E badline=(1,1,100,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,1,100,-1)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters must be positive
 ** ABEND called **
continue
repair D E badline=(1,1,100,3489743)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - BADLINE parameters may be out of range
 ** ABEND called **
continue
repair D E lineset=(1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
No bad lines found
repair D E lineset=(0,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(-1,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(3489743,1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters may be out of range
 ** ABEND called **
continue
let $echo="no"
 
<<<<<      6th 10 extreme values cases  >>>>>
 
repair D E lineset=(1,0,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,-1,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,3489743,100,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters may be out of range
 ** ABEND called **
continue
repair D E lineset=(1,1,0,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,1,-1,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,1,3489743,100)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters may be out of range
 ** ABEND called **
continue
repair D E lineset=(1,1,100,0)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,1,100,-1)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters must be positive
 ** ABEND called **
continue
repair D E lineset=(1,1,100,3489743)
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - LINESET parameters may be out of range
 ** ABEND called **
continue
repair D E corr=0.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.000
The following lines were repaired:
   20
let $echo="no"
 
<<<<<      Final 8 extreme values cases  >>>>>
 
repair D E corr=0.5
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.500
The following lines were repaired:
   20
repair D E corr=0.999999
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 1.000
The following lines were repaired:
   10   20
repair D E mean=0.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =      0.00
Variance tolerance =    200.00
The following lines were repaired:
    9   10   19   20   29   30   50   51
repair D E mean=-1.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E - MEAN parameter value must not be negative
 ** ABEND called **
continue
repair D E mean=3489743.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =3489743.00
Variance tolerance =    200.00
The following lines were repaired:
   10   20   51
list D sl=7 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:APPEND    User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
     Samp     8      10      12      14      16
   Line
      7     107 108 109 110 111 112 113 114 115 116
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     127 128 129 130 131 132 133 134 135 136
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     207 208 209 210 211 212 213 214 215 216
     21     107 108 109 110 111 112 113 114 115 116
repair D E variance=0.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =     10.00
Variance tolerance =      0.00
The following lines were repaired:
    9   10   19   20   50   51
list E sl=7 ss=8 nl=15 ns=10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Wed Jul 27 01:11:21 2016
 Task:REPAIR    User:rjb       Date_Time:Wed Jul 27 01:11:26 2016
     Samp     8      10      12      14      16
   Line
      7     107 108 109 110 111 112 113 114 115 116
      8     107 108 109 110 111 112 113 114 115 116
      9     107 108 109 110 111 112 113 114 115 116
     10     107 108 109 110 111 112 113 114 115 116
     11     107 108 109 110 111 112 113 114 115 116
     12     107 108 109 110 111 112 113 114 115 116
     13     107 108 109 110 111 112 113 114 115 116
     14     107 108 109 110 111 112 113 114 115 116
     15     107 108 109 110 111 112 113 114 115 116
     16     107 108 109 110 111 112 113 114 115 116
     17     107 108 109 110 111 112 113 114 115 116
     18     107 108 109 110 111 112 113 114 115 116
     19     107 108 109 110 111 112 113 114 115 116
     20     107 108 109 110 111 112 113 114 115 116
     21     107 108 109 110 111 112 113 114 115 116
repair D E variance=-1.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
??E  - VARIANCE parameter value must not be negative
 ** ABEND called **
continue
repair D E variance=3489743.0
Beginning VICAR task repair
** REPAIR Version 9-Jul-2016 - (64bit) - RJB
Correlation tolerance = 0.700
Mean tolerance =     10.00
Variance tolerance =3489743.00
The following lines were repaired:
   10   20
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
