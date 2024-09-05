$!****************************************************************************
$!
$! Build proc for MIPL module median
$! VPACK Version 1.9, Tuesday, August 16, 2016, 19:22:42
$!
$! Execute by entering:		$ @median
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
$ write sys$output "*** module median ***"
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
$ write sys$output "Invalid argument given to median.com file -- ", primary
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
$   if F$SEARCH("median.imake") .nes. ""
$   then
$      vimake median
$      purge median.bld
$   else
$      if F$SEARCH("median.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake median
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @median.bld "STD"
$   else
$      @median.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create median.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack median.com -mixed -
	-s median.f -
	-i median.imake -
	-p median.pdf -
	-t tstmedian.pdf tstmedian.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create median.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c VICAR program MEDIAN
c
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit none
      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 nl,ns,nlin,nsin,nlw,nsw,nbuf
      integer*4 status,cnt
      integer*8 n1,n2
      character*10 fmt
      external mainmed

      call xvmessage('MEDIAN 12-Jul-2016 (64-bit) - RJB',' ')
      call xvunit(iunit,'inp',1,status,' ')
      call xvopen(iunit,status,'open_act','sa','io_act','sa',
     +            'u_format','half',' ')
      call xvsize(sl,ss,nl,ns,nlin,nsin)
      if (ns.lt.2 .or. nl.lt.2) 
     1	 call mabend('??-E median cannot handle a file with nl ' //
     +		 'or ns less than 2.')
      call xvget(iunit,status,'format',fmt,' ')
      call uprcase(fmt)
      if (fmt.eq.'BYTE') then
         mindn = 0
         maxdn = 255
      elseif (fmt.eq.'HALF' .or. fmt.eq.'WORD') then
         mindn = -32768
         maxdn = 32767
      else
         call mabend('??E- Only BYTE and HALF images allowed')
      endif

      call xvunit(ounit,'out',1,status,' ')
      call xvopen(ounit,status,'op','write','open_act','sa',
     +          'io_act','sa','u_format','half',' ')

      call xvp('nlw',nlw,cnt)
      call xvp('nsw',nsw,cnt)
      if (nlw.gt.nl) nlw=nl
      if (nsw.gt.ns) nsw=ns
      nlw = 2*(nlw/2) + 1		!force window to be odd
      nsw = 2*(nsw/2) + 1
      nbuf = ns + nsw - 1

      n1 = int8(2*nlw*nbuf)		!integer*2 img(nbuf,nlw)
      n2 = int8(2*ns)			!integer*2 out(ns)

      call stacka_big(9,mainmed,2,n1,n2, nl,ns,nlw,nsw,nbuf)

      call xvclose(iunit,status,' ')
      call xvclose(ounit,status,' ')
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine mainmed(img,n1,out,n2,nl,ns,nlw,nsw,nbuf)
      implicit none
      integer*4 nl,ns,nlw,nsw,nbuf
      integer*2 img(nbuf,nlw),out(ns)
      integer*8 n1,n2

      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 status,cnt,i,i1,i2,j,j1,j2,line,oline
      integer*4 nlw2,nsw2,n50,dclev,minval,maxval
      real*4 perc,dctran
      logical*4 xvptst,high

      minval = mindn
      maxval = maxdn
      call xvp('PERCENT',perc,cnt)
      n50 = nsw*nlw*perc/100
      call xvp('DCTRAN',dctran,cnt)
      high = xvptst('HIGHPASS')
      call xvp('DCLEVEL',dclev,cnt)

      nlw2 = nlw/2
      nsw2 = nsw/2
      j = nlw2 + 1		!index to first line
      i1 = nsw2 + 1		!index to first and
      i2 = i1 + ns - 1		!last pixel of line

c     ...read initial lines into memory, reflecting at left and right margins
      do 20 line=1,nlw2+1
      call xvread(iunit,img(i1,j),status,'nsamps',ns,'samp',ss,
     &                'line',sl+line-1,' ')
      if (nsw.gt.1) then
         do i=1,nsw2
            img(i1-i,j)=img(i1+i,j)
            img(i2+i,j)=img(i2-i,j)
         enddo
      endif
   20 j = j + 1

      j1 = nlw2
      j2 = nlw2 + 2
c     ...reflect lines at top margin 
      do line=1,nlw2
         call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
         j1 = j1 - 1
         j2 = j2 + 1
      enddo

      j = nlw2 + 1		!index to current line (middle of window)
      j1 = nlw			!index to bottom line of window
      line = nlw2 + 1		!image line number at bottom of window

      do 30 oline=1,nl
      call med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval)
      if (high) call hp(img(i1,j),out,ns,dclev,dctran)
      call xvwrit(ounit,out,status,' ')
      j = mod(j,nlw) + 1
      j1 = mod(j1,nlw) + 1
      line = line + 1
      if (line.le.nl) then
         call xvread(iunit,img(i1,j1),status,'nsamps',ns,'samp',ss,
     &               'line',sl+line-1,' ')
         if (nsw.gt.1) then
            do i=1,nsw2
               img(i1-i,j1)=img(i1+i,j1)
               img(i2+i,j1)=img(i2-i,j1)
            enddo
         endif
      else		!reflect lines at bottom of image
          if (oline.eq.nl) goto 30
          if (line.eq.nl+1) then
             j2 = j1 - 2
          else
             j2 = j2 - 1
          endif
          if (j2.lt.1) j2=j2+nlw
          call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
      endif
   30 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c compute median filter for an image line.
c
      subroutine med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,
     &		minval,maxval)
      implicit none
      integer*4 ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval
      integer*2 img(nbuf,nlw),out(ns)

      integer*4 hist(-32768:32767)
      integer med,ltm,dn
      integer*4 i,il,is

      do i=minval,maxval
         hist(i)=0
      enddo

      minval = maxdn
      maxval = mindn
      do il=1,nlw
         do is=1,nsw
            dn = img(is,il)
            if (dn.lt.minval) then
               minval=dn
            elseif (dn.gt.maxval) then
               maxval=dn
            endif
            hist(dn)=hist(dn)+1
         enddo
      enddo

      med = minval
      ltm = 0		!number of samples less than median

      do 50 is=1,ns
      if (ltm.le.n50) goto 15
   10 med = med - 1
      ltm = ltm - hist(med)
      if (ltm.gt.n50) goto 10
      goto 20

   15 ltm = ltm + hist(med)
      med = med + 1
      if (ltm.le.n50) goto 15
      med = med - 1
      ltm = ltm - hist(med)

   20 out(is) = med
      if (is.eq.ns) goto 50

      do 30 il=1,nlw
      dn = img(is,il)
      if (dn.lt.med) ltm=ltm-1
      hist(dn) = hist(dn) - 1
      dn = img(is+nsw,il)
      if (dn.lt.minval) then
         minval=dn
      elseif (dn.gt.maxval) then
         maxval=dn
      endif
      if (dn.lt.med) ltm=ltm+1
      hist(dn) = hist(dn) + 1
   30 continue

   50 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c perform high pass filtering on line
c
      subroutine hp(in,out,ns,dclev,dctran)
      implicit none
      integer*4 ns,dclev
      integer*2 in(ns),out(ns)
      real*4 dctran

      integer i,a

      do i=1,ns				!halfword input & output
         a = in(i) - out(i) + dclev	
         if (dctran.ne.0.0) a=a+dctran*float(out(i))
        out(i) = a
      enddo
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create median.imake
#define  PROGRAM   median
#define R2LIB 

#define MODULE_LIST median.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create median.pdf
process help=*
PARM INP	TYPE=STRING  COUNT=1
PARM OUT	TYPE=STRING  COUNT=1				DEFAULT=MEDIAN
PARM SIZE	TYPE=INTEGER COUNT=0:4				DEFAULT=--
PARM NL		TYPE=INTEGER COUNT=0:1				DEFAULT=--
PARM NS         TYPE=INTEGER COUNT=0:1				DEFAULT=--
PARM NLW        TYPE=INTEGER VALID=(1:999)			DEFAULT=11
PARM NSW        TYPE=INTEGER VALID=(1:999)			DEFAULT=11
PARM HIGHPASS	TYPE=KEYWORD VALID=HIGHPASS COUNT=(0:1)		DEFAULT =--
PARM DCLEVEL	TYPE=INTEGER COUNT=0:1 VALID=(0:32767)		DEFAULT=128
PARM PERCENT	TYPE=REAL    VALID=(0.:100.)			DEFAULT=50.0
PARM DCTRAN     TYPE=REAL    COUNT=0:1 VALID=(0.:1.)		DEFAULT=0.0
END-PROC
.TITLE
VICAR1 program MEDIAN: Spatial median filter
.HELP
PURPOSE:

MEDIAN is a VICAR applications program which performs nonlinear spatial
filtering of an image based upon the local (rectangular window) median of
the input. Output may be in either lowpass or highpass form.

.PAGE
EXECUTION:

	median inp=ipic out=opic
where
  ipic is the input image
  opic is the output image

ipic may be in byte or halfword data format.  opic is output in the same
format.  The input image must be at least 2 lines and 2 samples in size.
There is no upper limit for the image size.

Reference:
  Huang, Yang, and Tang, "A Fast Two-Dimensional Median Filtering Algorithm,"
	IEEE Trans., Vol. ASSP-27, No. 1, February 1979.

.PAGE
OPERATION:

MEDIAN replaces each input pixel by the local median value of an NLW X NSW
window centered at that pixel (see NLW and NSW parameters).

The median is defined as the DN value for which half of the pixels in the
window have a lower value and half have a higher value.  That is, if the
pixels in the window were sorted by DN value, then the median would be the
DN of the pixel exactly half way in the sorted list.  The sorting is performed
by accumulating a histogram of the window (see referenced paper).

The output value can be more generally defined using the PERCENT parameter.
If PERCENT=P, then the output value is that DN for which P*NLW*NSW/100 pixels
in the window have a higher value.  For example, if PERCENT=50 (the default),
then the output is the median.

The effect of replacing each pixel by the local median is to remove all high
frequency scene information.  Thus the median filter functions like a low
pass filter.  See also program BOXFLT2.

If HIGHPASS is specified, the final output is given as:

		OUT = IN - MEDIAN + DCLEVEL
where
	DCLEVEL is an integer value (see DCLEVEL parameter).

The effect of the HIGHPASS keyword is to convert the program into a high pass
filter.

.PAGE
EXAMPLES:

	1) MEDIAN INP=A OUT=B NLW=5 NSW=7

		This example performs the lowpass median filter of size 5 lines
		by 7 samples.

	2) MEDIAN INP=A OUT=B 'HIGH NLW=3 NSW=3

		This example performs the highpass median filter of size
		3x3 pixels.

	3) MEDIAN INP=A OUT=B PERCENT=20 NLW=7 NSW=9

		This example outputs a value corresponding to the 20% level
 		of a histogram generated by a 7 line by 9 sample filter. The
		output is then the 12th lowest value in the local window
		rather than the 31st lowest value (50% level).


.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: W. D. Benton, 27 November 1978
REWRITTEN WITH FASTER ALGORITHM BY:  H. J. Frieden,  22 July 1980
CONVERTED TO VAX BY:  Helen De Rueda,  30 Nov. 1983
COGNIZANT PROGRAMMER:  Ray Bambery

Revisions:
 18 Aug 2005 - G Yagi	Fix DCLEVEL parameter on Linux.
 12 Jul 2016 - R. J. Bambery - Removed 2 GB maximum image size.
                by changing stacka call with stacka_big call
                Centos-7.2 & gcc-4.8.5

.LEVEL1
.VARI INP
Required string
Input image
.VARI OUT
Required string
Output image
.VARI SIZE
Optional 4 integers
VICAR size field,
SIZE=(SL,SS,NL,NS)
.VARI NL
Optional integer
number of output lines
.VARI NS
Optional integer
number of output samples
output file.
.VARI NLW
Optional integer
filter kernel height (lines)
.VARI NSW
filter kernel width (samples)
.VARI HIGHPASS
Optional keyword
output highpass image
.VARI DCLEVEL
Optional integer
DN offset added to output
.VARI PERCENT
Optional REAL
percent of data greater than
output (=50 for median)
.VARI DCTRAN
Optional REAL
DCTRAN*Local Median is
added to the highpass output.

.LEVEL2
.VARI INP
  INP=ipic
where ipic is the input image.  ipic may be in byte or halfword data format.
ipic must be at least 2 lines and 2 samples in size.  ipic may be arbitrarily
large.

.VARI OUT
  OUT=opic
where opic is the output image.  opic will have the same data format as the
input image.

.VARI SIZE
4 INTEGERS - SIZE=(SL,SS,NL,NS) where SL is the starting line, SS is the
 starting sample, NL is the number of lines in the input dataset and NS
 is the number of samples in the input dataset. (SIZE is usually defined
 as SIZE=(1,1,NL,NS)). Default is taken from the VICAR label within the
 program.
.VARI NL
INTEGER - NL=N1 where is N1 is the number of lines in the input dataset.
.VARI NS
INTEGER - NS=N1 where is N1 is the number of samples in the input dataset.
.VARI NLW
INTEGER - NLW=I1 where I1 is an integer and specifies the size of the filter
 kernel in lines. Default is NLW=11.
.VARI NSW
INTEGER - NSW=I2 where I2 is an integer and specifies the size of the filter
 kernel in samples. Default is NSW=11.
.VARI HIGHPASS
KEYWORD - Valid:('HIGH) 'HIGHPASS specifies that the output is to be in
 highpass format, i.e., the input minus the local median value. Default
 is the lowpass mode.
.VARI DCLEVEL
INTEGER - DCLEVEL=I3 where I3 is an integer and
 specifies the offset to be added to the highpass output. Default is
 DCLEVEL=128.
.VARI PERCENT
REAL - PERCENT=R4 where R4 is an floating point number and specifies the
 percentage of the window size which must be satisfied for an output value
 to be found at each point. The default of PERCENT=50.0 specifies that the
 true median value of window is to be found, whereas if a lower (or higher)
 value is specified, the output value will be somewhat lower (or higher)
 than the actual median.
.VARI DCTRAN
REAL - DCTRAN=R where R is a number such that 0.0<=R<=1.0. 
DCTRAN * output(local median value) is added to the highpass output.
Default is DCTRAN=0.0
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmedian.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

refgbl $echo

! Jul 12, 2016 - RJB
! TEST SCRIPT FOR MEDIAN
! tests BYTE, HALF, images
!
! Vicar Programs:
!       gausnois list  
!
! External Programs:
!   <none>
! 
! Parameters:
!   <none>
! Requires external test data: 
!   cartlab or mipl dependent pointers
!            
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!            
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check to see if mipl or cartlab for test images
!cartlab defines env var $AFIDS_ROOT, mipl doesm't

body
let $echo="no"
let _onfail="stop"

translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata

write "afidsroot = &afidsroot"
write "aftestdata = &aftestdata"
if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/carto ct
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/carto ct
    else 
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto ct
    end-if
end-if
let _onfail="goto rm"


let $echo="yes"
!This is a test file for MEDIAN
!This example performs the lowpass median filter of default size 3 lines
!by 3 samples.
gausnois out=a.img nl=10 ns=10 seed=7382382
list a.img
median inp=a.img out=b.img nlw=3 nsw=3
list b.img
!This example performs the highpass median filter of default size 3 lines
!by 3 samples.
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
list b.img
!This example outputs the value corresponding to the 20% level of a 
!histogram
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
list b.img
gausnois out=a.img nl=10 ns=10 seed=7382382 format=half sigma=10. +
  mean=-1000
list a.img
median inp=a.img out=b.img nlw=3 nsw=3
list b.img
!This example performs the highpass median filter of default size 3 lines
!by 3 samples.
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
list b.img
!This example outputs the value corresponding to the 20% level of a 
!histogram
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
list b.img

let $echo="no"
write " "
write " New test of image > 2 GB"
write " "
write "  get median image of a large image"
write " "
let $echo="yes"
median ct/pre_reg.hlf bigmed.img
list ct/pre_reg.hlf size=(5000,5000,10,10)

list bigmed.img  size=(5000,5000,10,10)

!
! clean up
!
rm>
let $echo="no"
ush rm ct

end-proc

$!-----------------------------------------------------------------------------
$ create tstmedian.log
tstmedian
afidsroot = /home/rjb/vicar_afids/install/afids
aftestdata = /media/sf_raid3/test_data
gausnois out=a.img nl=10 ns=10 seed=7382382
Beginning VICAR task gausnois
Gausnois - 11-Aug-2013 - rjb (64bit)
list a.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp     1       3       5       7       9
   Line
      1     111 127 110 138 119 161 133 154 108 141
      2     127 104 105 126 116 113 132 147 123 146
      3     127 124 124 112 121 164 124 114 129 140
      4      94 107 146 107 133 116  92 148 118 134
      5     131 133 148 139 130 131 131 114 143 124
      6     137 139 132 154 157 151 109 134 114 133
      7     117 105 148 110 121 143 129 137 142 147
      8     125 114  98 161 116 135 122 118 147 133
      9     123 126 126 109 130 136 139 139 122 142
     10     135 131 128 121 118 118 143 138 124 120
median inp=a.img out=b.img nlw=3 nsw=3
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp     1       3       5       7       9
   Line
      1     111 110 110 116 119 119 133 132 146 123
      2     124 124 124 119 121 124 133 129 140 129
      3     107 124 112 121 116 121 124 124 134 129
      4     124 127 124 130 130 130 124 124 129 129
      5     133 133 139 139 133 131 131 118 133 124
      6     133 133 139 139 139 131 131 131 134 142
      7     117 125 132 132 143 129 134 129 134 142
      8     117 123 114 121 130 130 136 137 139 142
      9     126 126 126 121 121 130 136 138 133 124
     10     126 126 126 126 121 136 139 139 138 122
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp     1       3       5       7       9
   Line
      1     100 117 100 122 100 142 100 122  62 118
      2     103  80  81 107  95  89  99 118  83 117
      3     120 100 112  91 105 143 100  90  95 111
      4      70  80 122  77 103  86  68 124  89 105
      5      98 100 109 100  97 100 100  96 110 100
      6     104 106  93 115 118 120  78 103  80  91
      7     100  80 116  78  78 114  95 108 108 105
      8     108  91  84 140  86 105  86  81 108  91
      9      97 100 100  88 109 106 103 101  89 118
     10     109 105 102  95  97  82 104  99  86  98
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp     1       3       5       7       9
   Line
      1     105 105 111 112 113 114 116 123 123 114
      2     105 105 107 107 112 114 116 116 118 118
      3     107 107 110 112 113 114 116 116 118 114
      4     107 107 112 113 113 114 114 114 114 114
      5     117 107 112 116 116 114 116 116 114 114
      6     107 107 110 114 116 116 116 118 118 118
      7     114 114 116 116 121 118 121 122 122 118
      8     114 114 116 116 118 118 118 122 122 122
      9     114 110 114 114 118 118 122 122 122 122
     10     114 114 114 114 116 118 118 122 122 122
gausnois out=a.img nl=10 ns=10 seed=7382382 format=half sigma=10.  +
  mean=-1000
Beginning VICAR task gausnois
Gausnois - 11-Aug-2013 - rjb (64bit)
list a.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     -1010 -1000 -1011  -993 -1005  -979  -996  -983 -1012  -991
      2     -1000 -1014 -1014 -1001 -1007 -1008  -996  -988 -1003  -988
      3     -1000 -1001 -1002 -1009 -1003  -976 -1001 -1008  -999  -992
      4     -1021 -1012  -988 -1012  -996 -1007 -1021  -987 -1005  -995
      5      -998  -996  -987  -992  -998  -997  -997 -1008  -990 -1002
      6      -993  -992  -997  -983  -981  -985 -1011  -996 -1008  -996
      7     -1006 -1014  -987 -1010 -1004  -990  -998  -994  -991  -987
      8     -1001 -1008 -1018  -978 -1007  -995 -1003 -1006  -987  -996
      9     -1002 -1000 -1000 -1011  -998  -994  -992  -993 -1003  -990
     10      -995  -997  -999 -1004 -1005 -1005  -990  -993 -1001 -1004
median inp=a.img out=b.img nlw=3 nsw=3
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     -1010 -1011 -1011 -1007 -1005 -1005  -996  -996  -988 -1003
      2     -1001 -1002 -1002 -1005 -1003 -1001  -996  -999  -992  -999
      3     -1012 -1002 -1009 -1003 -1007 -1003 -1001 -1001  -995  -999
      4     -1001 -1000 -1001  -998  -998  -998 -1001 -1001  -999  -999
      5      -996  -996  -992  -992  -996  -997  -997 -1005  -996 -1002
      6      -996  -996  -992  -992  -992  -997  -997  -997  -996  -991
      7     -1006 -1001  -997  -997  -990  -998  -996  -998  -996  -991
      8     -1006 -1002 -1008 -1004  -998  -998  -994  -994  -993  -991
      9     -1000 -1000 -1000 -1004 -1004  -998  -994  -993  -996 -1001
     10     -1000 -1000 -1000 -1000 -1004  -994  -993  -993  -993 -1003
median inp=a.img out=b.img 'high nlw=3 nsw=3 dclevel=100. dctran=0.
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       100   111   100   114   100   126   100   113    76   112
      2       101    88    88   104    96    93   100   111    89   111
      3       112   101   107    94   104   127   100    93    96   107
      4        80    88   113    86   102    91    80   114    94   104
      5        98   100   105   100    98   100   100    97   106   100
      6       103   104    95   109   111   112    86   101    88    95
      7       100    87   110    87    86   108    98   104   105   104
      8       105    94    90   126    91   103    91    88   106    95
      9        98   100   100    93   106   104   102   100    93   111
     10       105   103   101    96    99    89   103   100    92    99
median inp=a.img out=b.img perc=20.0 nlw=5 nsw=5
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list b.img
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GAUSNOIS  User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:17 2016
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1     -1014 -1014 -1010 -1009 -1008 -1008 -1007 -1003 -1003 -1008
      2     -1014 -1014 -1012 -1012 -1009 -1008 -1007 -1007 -1005 -1005
      3     -1012 -1012 -1011 -1009 -1008 -1008 -1007 -1007 -1005 -1008
      4     -1012 -1012 -1009 -1008 -1008 -1008 -1008 -1008 -1008 -1008
      5     -1006 -1012 -1009 -1007 -1007 -1008 -1007 -1007 -1008 -1008
      6     -1012 -1012 -1010 -1008 -1007 -1007 -1007 -1006 -1006 -1006
      7     -1008 -1008 -1007 -1007 -1004 -1006 -1004 -1003 -1003 -1006
      8     -1008 -1008 -1007 -1007 -1005 -1005 -1005 -1003 -1003 -1003
      9     -1008 -1010 -1008 -1008 -1005 -1005 -1003 -1003 -1003 -1003
     10     -1008 -1008 -1008 -1008 -1007 -1006 -1005 -1003 -1003 -1003
let $echo="no"
 
 New test of image > 2 GB
 
  get median image of a large image
 
median ct/pre_reg.hlf bigmed.img
Beginning VICAR task median
MEDIAN 12-Jul-2016 (64-bit) - RJB
list ct/pre_reg.hlf size=(5000,5000,10,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:tllogan   Date_Time:Wed Sep 18 17:16:51 2013
 Task:CFORM     User:tllogan   Date_Time:Wed Sep 18 17:20:45 2013
     Samp    5000  5001  5002  5003  5004  5005  5006  5007  5008  5009
   Line
   5000      3158  3158  3158  3162  3170  3183  3183  3179  3192  3179
   5001      3141  3141  3145  3170  3183  3175  3170  3183  3196  3183
   5002      3170  3158  3158  3166  3166  3175  3179  3170  3166  3187
   5003      3145  3162  3153  3162  3170  3170  3170  3166  3162  3179
   5004      3187  3170  3158  3162  3170  3175  3183  3179  3166  3175
   5005      3170  3162  3153  3158  3166  3170  3179  3179  3166  3175
   5006      3179  3183  3170  3162  3162  3179  3183  3175  3166  3158
   5007      3158  3158  3162  3162  3170  3192  3187  3196  3175  3166
   5008      3162  3162  3162  3162  3166  3166  3175  3179  3179  3166
   5009      3162  3158  3149  3158  3192  3196  3187  3179  3175  3158
list bigmed.img  size=(5000,5000,10,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:tllogan   Date_Time:Wed Sep 18 17:16:51 2013
 Task:MEDIAN    User:rjb       Date_Time:Wed Jul 27 00:43:18 2016
     Samp    5000  5001  5002  5003  5004  5005  5006  5007  5008  5009
   Line
   5000      3158  3162  3166  3170  3170  3170  3170  3170  3170  3170
   5001      3158  3162  3166  3166  3170  3170  3170  3170  3170  3170
   5002      3158  3162  3166  3166  3170  3170  3170  3170  3170  3170
   5003      3158  3162  3166  3166  3170  3170  3170  3175  3175  3170
   5004      3158  3162  3162  3166  3170  3170  3170  3175  3175  3175
   5005      3158  3158  3162  3166  3166  3170  3170  3170  3170  3170
   5006      3158  3158  3162  3166  3166  3170  3170  3170  3170  3170
   5007      3158  3162  3162  3162  3166  3166  3166  3170  3166  3166
   5008      3158  3162  3162  3162  3166  3166  3166  3166  3166  3166
   5009      3158  3162  3162  3162  3166  3166  3166  3166  3166  3166
let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
