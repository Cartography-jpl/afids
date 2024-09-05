$!****************************************************************************
$!
$! Build proc for MIPL module destripe
$! VPACK Version 1.9, Tuesday, August 16, 2016, 16:16:15
$!
$! Execute by entering:		$ @destripe
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
$ write sys$output "*** module destripe ***"
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
$ write sys$output "Invalid argument given to destripe.com file -- ", primary
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
$   if F$SEARCH("destripe.imake") .nes. ""
$   then
$      vimake destripe
$      purge destripe.bld
$   else
$      if F$SEARCH("destripe.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake destripe
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @destripe.bld "STD"
$   else
$      @destripe.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create destripe.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack destripe.com -mixed -
	-s destripe.f -
	-i destripe.imake -
	-p destripe.pdf -
	-t tstdestripe.pdf tstdestripe.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create destripe.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
	implicit none

	external horzavg,vertavg			!stacka 

	integer*4 stat,icode,cnt,idef,nb
      	integer*4 ounit,iunit,nl,ns,i
	integer*4 window(4)
	integer*8 i8window(4)
 	integer*8 jj,kk,ll
	
	character*4 orgin 
	character*6 orient
	character*8 fmt(5)/'BYTE','HALF','FULL','REAL','DOUB'/
      	character*8 format
	
	call xvmessage ('** DESTRIPE - Jul 12, 2016 - (64-bit) - rjb',' ')

C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      call xvunit(iunit,'INP',1,stat,' ')
	call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit,stat,'FORMAT',FORMAT,'ORG',orgin,' ')
	call xvget(iunit,stat,'NL',nl,'NS',ns,'NB',nb,' ')
	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (format.eq.'DOUB') icode=5
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
        if (orgin.eq.'BIP') call mabend(
     +  '??E - BIP files not supported, use TRAN to convert to BSQ')

	call xvclose(iunit,stat,' ')

      call xvopen(iunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     +            'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

c      call xvsize(sl,ss,nl,ns,nlin,nsin)
c	the following calls were never implemented
c	call xvp('ONL',onl,cnt)
c	call xvp('ONS',ons,cnt)
      call xvunit(ounit,'OUT',1,STAT,' ')
      call xvopen(ounit,stat,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')

C
	call xvparm ('WINDOW',window,cnt,idef,4)
	if (window(3) .eq. 0 .and. window(4) .eq. 0) then
		window(3) = nl
		window(4) = ns
	endif
C	HORIZ is in line direction.
C	VERT is in sample direction.

	do i=1,4
	   i8window(i) = window(i)
	enddo
	call xvparm ('ORIENT',orient,cnt,idef,1)
c	convert stacka_big variable to int*8
        print *, "informat = ",fmt(icode)
	jj = ns*8		!real*4
	kk = nl*8		!real*4
	ll = nl*ns*4	!real*4 size in bytes
c 	print *, "nl   ns  = ll,kk,jj = ",nl,ns, ll,kk,jj	
	if (orient.eq.'HORIZ') then
	     call stacka_big (10,horzavg,3,ll,kk,kk,i8window,nl,ns,iunit,ounit)
c		call horzavg (iunit,ounit,nl,ns,window)
	else
	     call stacka_big (10,vertavg,3,ll,jj,jj,i8window,nl,ns,iunit,ounit)
c		call vertavg (iunit,ounit,nl,ns,window)
	endif

	call xvclose(iunit,stat,' ')
	call xvclose(ounit,stat,' ')

	return
	end
c==================================================================================
        subroutine horzavg (iimage,ll,avg,jj,adj,kk,
	1 i8window,nl,ns,iunit,ounit)
c
c	computation for horizontal striping - rows
c	you see stripes vary from row to row
        implicit none
c
	integer*4 iunit,ounit,stat,nl,ns,elw,esw,i,j
	integer*4 window(4)
	integer*8 jj,kk,ll,i8nl,i8ns
	integer*8 mem1,mem2,mem3
	integer*8 i8window(4)
	real*4 iimage(ns,nl)
	real*8 avg(nl),adj(nl),avglmax,savg
	character*80 msg
c
        call xvmessage('HORIZ',' ')
	j=0
	do i=1,4
            window(i) = i8window(i)
	enddo
	i8nl = int8(nl)
	i8ns = int8(ns)
        mem1 = i8nl * i8ns * 4
	mem2 = i8nl * 8
	mem3 = i8nl * 8

        write (msg,10100), mem1, mem2, mem3
10100 format ("memory allocated  1 = ",i12, ", 2 = ",i12,", 3 = ",i12 )
	call xvmessage (msg,' ')

	if (ll .lt. mem1 .or. jj .lt. mem2 .or. kk .lt. mem3) then
	   call xvmessage ('??E - Unable to allocate enough memory in stacka',' ')
	   call abend
	endif
c	iunit = i8unit
c	ounit = o8unit

c	stop
c	write (msg,10100) jj,kk,ll,mm,nn,ind
c10100 format (6i5)			!to prevent messages
	elw = window(3) - window(1) + 1		!nl - sl + 1
	esw = window(4) - window(2) + 1		!ns - ss + 1
c	print *, ' sl,ss,nl,ns elw,esw = ',window(1),window(2),nl,ns,elw,esw
        do i = 1,nl
	    call xvread (iunit,iimage(1,i),stat,'NSAMPS',ns,'LINE',i,' ')		!1,ia
	enddo
	savg = 0.0d0
        do i=window(1),elw
	     savg = 0.0d0
             do j=window(2),esw
                savg = savg + dble(iimage(j,i))             !j,i    go thru samples
             enddo
	     avg(i) = savg/window(4)                     !average per row - sample direction
        enddo
c	do i=1,nl
c	   print *,'avg(i) = ',avg(i)
c	enddo
	
	avglmax = -1.00d38
c
c	go across each column
	do i=window(1),elw	!sl,nl
	    if (avg(i) .gt. avglmax) avglmax = avg(i) 	!get the maximum/col
	enddo
	do i=window(1),elw
	    adj(i) = (avglmax - avg(i))			!subtract 
	enddo
	do i=1,nl
	    iimage(j,i)  = iimage(j,i) + real(adj(i))	!j,i
	    call xvwrit(ounit,iimage(1,i),stat,'NSAMPS',ns,' ')	
	enddo

        return
        end

c==================================================================================
        subroutine vertavg (iimage,ll,avg,jj,adj,kk,
	1 i8window,nl,ns,iunit,ounit)
c
        implicit none
c
        integer*4 iunit,ounit,stat,nl,ns,elw,esw,i,j
        integer*4 window(4)
        integer*8 jj,kk,ll,i8nl,i8ns
        integer*8 mem1,mem2,mem3
        integer*8 i8window(4)
        real*4 iimage(ns,nl)
        real*8 avg(ns),adj(ns),avgmax,savg
        character*80 msg

C	4*12 + 4*4 + 4*4 = 80 
c
        call xvmessage('VERT',' ')
        do i=1,4
            window(i) = i8window(i)
        enddo
        i8nl = nl
        i8ns = ns
        mem1 = i8nl * i8ns * 4
        mem2 = i8nl * 8
        mem3 = i8nl * 8
        write (msg,10100), mem1, mem2, mem3
10100 format ("memory allocated  1 = ",i12, ", 2 = ",i12,", 3 = ",i12 )
        call xvmessage (msg,' ')
c        if (nl*ns .gt. 536200000) then
c           call xvmessage ('??E - nl*ns gt 536,200,000 full words (2,144,800,000 bytes)',' ')
c           call abend
c        endif
        if (ll .lt. mem1 .or. jj .lt. mem2 .or. kk .lt. mem3) then
           call xvmessage ('??E - Unable to allocate enough memory in stacka',' ')
           call abend
        endif
c        iunit = i8unit
c        ounit = o8unit
        elw = window(3) - window(1) + 1		!nl - sl + 1
        esw = window(4) - window(2) + 1		!ns - ss + 1
	 print *,'windows 1,2,3,4 = ',window(1), window(2), window(3), window(4)
        do i = 1,nl
            call xvread (iunit,iimage(1,i),stat,'NSAMPS',ns,' ')		!1,i
        enddo

	do i=window(2),esw		!pick sample
	    savg = 0.0d0
            do j=window(1),elw		!go thru lines
                savg = savg + dble(iimage(i,j))	!i,j 
            enddo
	    avg(i) = savg/window(3)	!average per column - line direction
	enddo
        avgmax = -1.00d38
        do i=window(2),esw
            if (avg(i) .gt. avgmax) avgmax = avg(i)
        enddo

        do i=window(2),esw
            adj(i) = avgmax - avg(i) 
        enddo

        do i=1,ns 
            do j=1,nl
                iimage(i,j)  = iimage(i,j)  + real(adj(i))		!i,j
            enddo
        enddo
        do i=1,nl
	    call xvwrit(ounit,iimage(1,i),stat,'NSAMPS',ns,' ')		!1,i
	enddo
        return
        end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create destripe.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM destripe

   To Create the build file give the command:

		$ vimake destripe			(VMS)
   or
		% vimake destripe			(Unix)


************************************************************************/


#define PROGRAM	destripe

#define MODULE_LIST destripe.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create destripe.pdf
PROCESS help=*
  parm inp      type=string count=1 
  parm out      type=string count=(0:1) default=--
  parm window   type=integer count=4 default=(1,1,0,0)
  parm orient   type=keyword    count=(0:1) valid=(HORIZ,VERT) default=HORIZ
end-proc
.title
vicar program destripe
.help
PURPOSE
    A vicar application program to remove striping in iamges caused by
variations in detector response. 

OPERATION

    This program removeds stripe or banding artifacts in images do to  
variations in the response of the detector. This is especially
useful for pushbroom sensors.

    Internally, the program averages rows or columns. Each row or
column is adjusted with reference to the row or column with the

    The WINDOW parameter allows you to select a rectangular window in
the image to do the averaging. The adjustment is applied to all
rows or columns in the image. Otherwise, the entire image is scanned.

    Choose orient=vert when stripes appear in vertical (columnar)
direction and chose orient=horiz (sample or rowwise) direction.
The output image has the banding or striping removed.


INTERNALS

    The internal data is stored in real*4 format. Stacka is called
to allocate the internal buffers. It is limited to 536,200,000 full 
words (2,144,800,000 bytes), which is an image of about
15320x35000 or 21000x25000 in lines and samples.

    The program always lets you know the memory it uses for stacka
by the message, e.g.,
 memory allocated =     262144, 
which is in full words.

RESTRICTIONS

    1) This program does not work on DOUB images.
    2) This program does not work on multiband images
    3) The image is limited to nl and ns equal to 536,200,000 fullwords
        (2,144,800,000 bytes)

PROGRAM HISTORY:

Written by: Ray Bambery, 22 October 2011
Cognizant programmer: Ray Bambery
Revision history:

    22 Oct 2011 - R. J. Bambery - initial release
    27 Oct 2011 - R. J. Bambery - found internal limits
    04 Jun 2012 - R. J. Bambery - removed last debugging statements
    02 Dec 2012 - R. J. Bambery - fixed image array size in horiz routine
    11 Dec 2012 - R. J. Bambery - fixed averaging bug in horiz routine
    12 Jul 2016 - R. J. Bambery - linked against new stacka_big routine
                                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARI INP
Input image.
.VARI OUT
Output image with striping
removed
.VARI WINDOW
Size of the window to use
to compute the destriping
values
.VARI ORIENT
Direction to remove striping or
banding.
HORIZ is in line direction.
VERT is in sample direction.

.END




$ Return
$!#############################################################################
$Test_File:
$ create tstdestripe.pdf
procedure       
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

refgbl $echo
! Aug 29, 2013 - RJB
! TEST SCRIPT FOR DESTRIPE
! tests HALF images, supports all formats
!
! Vicar Programs:
!       gen sargonb f2 difpic flot xvd
! 
! External Programs:
!   <none>
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
!       2) interactive or nobatch is used for display requiring
!       user interaction. 
!
!
! Requires NO external test data: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

body
let _onfail="stop"
let $echo="yes"
gen a nl=512 ns=512 ival=0 modulo=40 linc=1 sinc=1 format=half
gen b nl=512 ns=512 ival=0 linc=0 sinc=0 format=half

!gen horizontal stripes
sargonb b c func1=setto const1=6 vert1=(10,1, 10,512, 20,512, 20,1) +
    func2=setto const2=3 vert2=(21,1, 21,512, 35,512, 35,1) +
    func3=setto const3=1 vert3=(47,1, 47,512, 65,512, 65,1) +
    func4=setto const4=2 vert4=(80,1, 80,512, 125,512, 125,1) +
    func5=setto const5=3 vert5=(80,1, 80,512, 125,512, 125,1) +
    func6=setto const6=1 vert6=(180,1, 180,512, 225,512, 225,1) +
    func7=setto const7=2 vert7=(250,1, 250,512, 300,512, 300,1) +
    func8=setto const8=4 vert8=(340,1, 340,512, 380,512, 380,1) +
    func9=setto const9=6 vert9=(420,1, 420,512, 450,512, 450,1) +
    func10=setto const10=2 vert10=(470,1, 470,512, 501,512, 501,1) 

f2 (a,c) d func=("In1+in2 + 100")
! TEST 1 - remove horizontal destriping by using whole image for
! correction
destripe d e orient=horiz
difpic (d,e)
difpic (e,b)

let $echo="no"
if (mode = "nobatch" or mode = "inter")
    write "Note how the horizontal stripes are removed in e"
! horizontal stripes are removed in h
 xvd  d
 xvd  e
end-if
let $echo="yes"
!
!  TEST 2 - use a window to arrive at destriping correction
destripe d f orient=horiz window=(1,1,512,256)
!xvd f
! 
difpic (d,f)
difpic (f,b)
difpic (e,f)
! rotate the image and do vertical destriping
flot c r mode=clock

f2 (a,r) g func=("In1+in2 + 100")
!
!  TEST 3 - Remove vertical stripes
destripe g h orient=vert
difpic (g,h)
difpic (h,r)
let $echo="no"
if (mode = "nobatch" or mode = "inter")
    write "Note how the vertical stripes are removed in h"
! vertical stripes are removed in h
 xvd  g
 xvd  h
end-if
let $echo="yes"

!  TEST 4 - use a window to arrive at destriping correction
destripe g i orient=vert window=(1,1,512,256)
! xvd i
!
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstdestripe.log
tstdestripe
gen a nl=512 ns=512 ival=0 modulo=40 linc=1 sinc=1 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b nl=512 ns=512 ival=0 linc=0 sinc=0 format=half
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb b c func1=setto const1=6 vert1=(10,1, 10,512, 20,512, 20,1)  +
    func2=setto const2=3 vert2=(21,1, 21,512, 35,512, 35,1)  +
    func3=setto const3=1 vert3=(47,1, 47,512, 65,512, 65,1)  +
    func4=setto const4=2 vert4=(80,1, 80,512, 125,512, 125,1)  +
    func5=setto const5=3 vert5=(80,1, 80,512, 125,512, 125,1)  +
    func6=setto const6=1 vert6=(180,1, 180,512, 225,512, 225,1)  +
    func7=setto const7=2 vert7=(250,1, 250,512, 300,512, 300,1)  +
    func8=setto const8=4 vert8=(340,1, 340,512, 380,512, 380,1)  +
    func9=setto const9=6 vert9=(420,1, 420,512, 450,512, 450,1)  +
    func10=setto const10=2 vert10=(470,1, 470,512, 501,512, 501,1)
Beginning VICAR task sargonb
SARGONB - Apr 20, 2014 - rjb (64-bit)
f2 (a,c) d func=("In1+in2 + 100")
Beginning VICAR task f2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 240 TIMES
destripe d e orient=horiz
Beginning VICAR task destripe
** DESTRIPE - Jul 12, 2016 - (64-bit) - rjb
HORIZ
memory allocated  1 =      1048576, 2 =         4096, 3 =         4096
difpic (d,e)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES = 469
difpic (e,b)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     262144
let $echo="no"
destripe d f orient=horiz window=(1,1,512,256)
Beginning VICAR task destripe
** DESTRIPE - Jul 12, 2016 - (64-bit) - rjb
HORIZ
memory allocated  1 =      1048576, 2 =         4096, 3 =         4096
difpic (d,f)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES = 484
difpic (f,b)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     262144
difpic (e,f)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES = 953
flot c r mode=clock
Beginning VICAR task flot
** flot - 29-Jun-2016  (64-bit) - rjb
f2 (a,r) g func=("In1+in2 + 100")
Beginning VICAR task f2
F2 version 09-Feb-2015 (64-bit) - RJB
F2 using hash table lookup
FUNCTION EVALUATED 240 TIMES
destripe g h orient=vert
Beginning VICAR task destripe
** DESTRIPE - Jul 12, 2016 - (64-bit) - rjb
VERT
memory allocated  1 =      1048576, 2 =         4096, 3 =         4096
difpic (g,h)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     240640
difpic (h,r)
Beginning VICAR task difpic
DIFPIC version 12-Jul-2016 - rjb (64-bit)
 NUMBER OF DIFFERENCES =     262144
let $echo="no"
destripe g i orient=vert window=(1,1,512,256)
Beginning VICAR task destripe
** DESTRIPE - Jul 12, 2016 - (64-bit) - rjb
VERT
memory allocated  1 =      1048576, 2 =         4096, 3 =         4096
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
