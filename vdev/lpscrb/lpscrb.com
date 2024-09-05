$!****************************************************************************
$!
$! Build proc for MIPL module lpscrb
$! VPACK Version 1.9, Wednesday, July 04, 2007, 15:46:23
$!
$! Execute by entering:		$ @lpscrb
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
$ write sys$output "*** module lpscrb ***"
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
$ write sys$output "Invalid argument given to lpscrb.com file -- ", primary
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
$   if F$SEARCH("lpscrb.imake") .nes. ""
$   then
$      vimake lpscrb
$      purge lpscrb.bld
$   else
$      if F$SEARCH("lpscrb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lpscrb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lpscrb.bld "STD"
$   else
$      @lpscrb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lpscrb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lpscrb.com -mixed -
	-s lpscrb.c -
	-i lpscrb.imake -
	-p lpscrb.pdf -
	-t tstlpscrb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lpscrb.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  linked polygon scribe routine  P.Kim    6/6/07    */
#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "/home/pkim/utilities/shapefil.h"
#include "/home/pkim/utilities/LPUtils.h"
#include "/home/pkim/utilities/MemUtils.h"
#include "/home/pkim/utilities/mappingUtils.h"

#define wsiz 2000000

///////////////////////////////
int min(x1, x2)
   int x1, x2;
{
   if(x1 > x2) return x2;
   else return x1;
}

///////////////////////////////
int max(x1, x2)
   int x1, x2;
{
   if(x1 > x2) return x1;
   else return x2;
}

///////////////////////////////
void scribe2(list)
   LPList *list;
{
   int status, geounit1, i, j, index;

/* Al's algorithm
*****************/

   int ll, lu, ld, oldwtop, wtop, ns4, k, l;
   double fxl, fxu, x1, y1, x2, y2, ox2, oy2;
   int ctrev, in_line, isc, jsc, ist, jst, nsqt, lindex, ctspace, xbit, ybit, wsup, rrindex, rrinc, isz, jsz, jstz, istz, zindex;
   char work[wsiz];
   double tr[6], tst[4], xd, yd, xdi, ydi, fyl, fyu, tl, tu, t, dx, dy, xt, yt, xq, yq, xinc, yinc, cycle, pmn, pmj, t1, t2;
   int spacing, sl, ss, nl, ns, zoom, tag, dn, bdn, pdn, outside, cnt, def, refl;
   double thresh;
   double sq, px, x, y, py, phi;
   const double degrad = 57.29578;
   zvp("spacing", &spacing, &cnt);
   zvp("sline", &sl, &cnt);
   zvp("ssamp", &ss, &cnt);
   zvp("nline", &nl, &cnt);
   zvp("nsamp", &ns, &cnt);
   zvp("autozoom", &zoom, &cnt);
   zvparmd("nolong", &thresh, &cnt, &def, 0);
   zvp("tag", &tag, &cnt);
   zvp("dn", &dn, &cnt);
   zvp("bdn", &bdn, &cnt);
   zvp("pdn", &pdn, &cnt);
   zvp("outside", &outside, &cnt);
   zvp("refl", &refl, &cnt);

   zvselpi(0);
   status = zvunit(&geounit1,"out",1,0);
   if(status != 1) IBISSignalU(geounit1, status, 1);

   status = zvopen(geounit1, "OP", "WRITE", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);

   thresh = thresh*thresh*(nl*nl+ns*ns);
   ns4 = ns+4-ns%4;
   lu = sl; ld = wsiz/ns4;
   wsup = ld*ns4; wtop = wsiz;
   fyl = .4; fyu = ns-.4;

bigloop:
   ll = lu;
   lu += ld-1;if (lu>sl+nl) lu = sl+nl;
   if (ll>=lu) goto fin;
   fxl = ll+.4; fxu = lu+.6;
   oldwtop = wtop;
   wtop = (lu-ll+1)*ns4;
   if (ll==sl) for (i=0;i<wtop;i++) work[i] = bdn;
   if (ll>sl)
   {
      for (i=0;i<ns4;i++) work[i] = work[oldwtop-ns4+i];
      for (i=ns4;i<wtop;i++) work[i] = bdn;
   }
   for (i=0;i < list->numOfEntities;i++)
   {
      ctspace = 0;
      dn = getEntityTag(list, i);

      for(j = 0; j < getEntityNumOfVertices(list, i); j++)
      {
	 x1 = ox2; y1 = oy2;
         x2 = getDoubleXYorZ(list, i, j, LP_X);
         y2 = getDoubleXYorZ(list, i, j, LP_Y);

	 //if 1st vertex, we can't draw a line (need 2 vertices)
	 if(j==0)
         {
            ox2 = x2;
            oy2 = y2;
            continue;
         }
	 if (refl)
         {
            ox2 = x2; oy2 = y2;
            x2 = tr[0]*ox2+tr[1]*oy2+tr[2];
            y2 = tr[3]*ox2+tr[4]*oy2+tr[5];
         }

	 ox2 = x2; oy2 = y2;
	 if (outside)
	 {
	    px = y1-y2; py = x2-x1;
	    sq = sqrt((double)(px*px+py*py));
	    px = px/sq; py = py/sq;
	    x1 = x1+px; y1 = y1+py;
	    x2 = x2+px; y2 = y2+py;
	 }
	 if (x1<fxl && x2<fxl) continue;
	 if (x1>fxu && x2>fxu) continue;
	 ctrev = 0;
	 if (x1>x2 || (x1==x2 && y1>y2))
	 {
	    x = x1; x1 = x2; x2 = x;
	    y = y1; y1 = y2; y2 = y;
	    ctrev = 1;
	 }
	 xd = x2-x1+1.141593e-9; xdi = 1./xd;
	 yd = y2-y1+1.141593e-9; ydi = 1./yd;
	 tst[0] = (fxl+.1-x1)*xdi;
	 tst[1] = (fyl+.1-y1)*ydi;
	 tst[2] = (fxu-.1-x1)*xdi;
	 tst[3] = (fyu-.1-y1)*ydi;
	 tl = 3.; tu = -3.; in_line = 0;
	 for (k=0;k<4;k++)
	 {
	    x = xd*tst[k]+x1; y = yd*tst[k]+y1;
	    if (x<fxl || x>fxu) continue;
	    if (y<fyl || y>fyu) continue;
	    in_line = in_line || (tst[k]>=0. && tst[k]<=1.);
	    if (tst[k]<tl) tl = tst[k];
	    if (tst[k]>tu) tu = tst[k];
	 }
	 if (tl>2.) continue;
	 if (tl>0.) t = tl; else t = 0.;
	 if (tu>=1.) tu = 1.;
	 if (in_line==0)
	 if (x1<fxl || x1>fxu || y1<fyl || y1>fyu)
            continue;
	 dx = x2-x1; dy = y2-y1;

	 x = xd*t+x1; y = yd*t+y1;
	 isc = max(min((int)x,lu),ll)-ll;
	 jsc = max(min((int)y,ns),0);
	 xt = xd*tu+x1; yt = yd*tu+y1;
	 ist = max(min((int)xt,lu),ll)-ll;
	 jst = max(min((int)yt,ns),0);
	 nsqt = max(abs(isc-ist),abs(jsc-jst));
	 index = isc*ns4+jsc;
	 lindex = ist*ns4+jst;
	 work[index] = dn;
	 if (t<.001) work[index] = pdn;
	 if (nsqt==0) continue;
	 xq = isc+ll; yq = jsc;
	 xinc = fabs(xdi);
	 yinc = fabs(ydi);

	 xbit = ns4; ybit = 1;
	 if (xd<0.) xbit = -xbit;
	 if (yd<0.) ybit = -ybit;
	 if (xinc<=yinc)
	 {
	    ybit += xbit; yinc -= xinc;
	    cycle = (xinc+yinc)/xinc;
	    pmn = y-yq; if (yd<0.) pmn = -pmn;
	    pmj = x-xq; if (xd<0.) pmj = -pmj;
	    t1 = xinc*.5;
	    t2 = yinc*(.5-pmn+pmj/cycle);
	 }
	 else
	 {
	    xbit += ybit; xinc -= yinc;
	    cycle = (xinc+yinc)/yinc;
	    pmn = x-xq; if (xd<0.) pmn = -pmn;
	    pmj = y-yq; if (yd<0.) pmj = -pmj;
	    t1 = xinc*(.5-pmn+pmj/cycle);
	    t2 = yinc*.5;
	 }
	 if (ctrev && (ctspace+nsqt)>spacing)
		ctspace = (100*spacing-nsqt-ctspace)%spacing;
	 for (l=0;l<nsqt;l++)
	 {
	    if (t1>=t2)
            {
               index += ybit;
               t2 += yinc;
            }
	    else
            {
               index += xbit;
               t1 += xinc;
            }
	    if (index>wsup) index -= ns4;
	    if (index<0) index += ns4;
	    work[index] = dn;
	    ctspace++;
	    if (ctspace==spacing)
	    {
	       ctspace = 0;
	       rrindex = index+rrinc;
	       if (rrindex<=wsup && rrindex>=0)
                  work[rrindex] = dn;
	       rrindex = index-rrinc;
	       if (rrindex<=wsup && rrindex>=0)
                  work[rrindex] = dn;
	    }
	 }
	 isz = index/ns4; jsz = index-isz*ns4;
	 istz = (ist+isz)/2; jstz = (jst+jsz)/2;
	 zindex = istz*ns4+jstz;
	 work[zindex] = dn; work[lindex] = dn;
	 if (tu>.999) work[lindex] = pdn;
      }
   }
   for (l=ll;l<lu;l++) zvwrit(geounit1, &work[(l-ll)*ns4],0);

   goto bigloop;

fin:
   status = zvclose(geounit1, 0);
   if(status != 1) IBISSignalU(geounit1, status, 1);

   destroyLPList(list);
}

///////////////////////////////
main44()
{
   int ibis;
   int status;
   int i, j, index, geounit1;
   int numOfXYs;
   LPList *list;
   char *lineBuf;
   double minX, maxX, minY, maxY;
   int inpCnt;

   ibis = openIBISInp(1);
   list = getLPList(ibis);
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);

   zvpcnt("inp", &inpCnt);
   printf("file count: %d\n", inpCnt);
   if(inpCnt == 2) lpPixMapConversion(list, 1, 2);

   scribe2(list);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lpscrb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lpscrb

   To Create the build file give the command:

		$ vimake lpscrb		(VMS)
   or
		% vimake lpscrb		(Unix)


************************************************************************/


#define PROGRAM	lpscrb

#define MODULE_LIST shpopen.c fileutils.c MemUtils.c LPUtils.c mappingUtils.c lpscrb.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lpscrb.pdf
process help=*
PARM INP       TYPE=STRING COUNT=1:2
PARM OUT       TYPE=STRING
PARM SPACING   TYPE=INTEGER
PARM SLINE     TYPE=INTEGER
PARM SSAMP     TYPE=INTEGER
PARM NLINE     TYPE=INTEGER
PARM NSAMP     TYPE=INTEGER
PARM AUTOZOOM  TYPE=INTEGER
PARM NOLONG    TYPE=REAL
PARM TAG       TYPE=INTEGER
PARM DN        TYPE=INTEGER
PARM BDN       TYPE=INTEGER
PARM PDN       TYPE=INTEGER
PARM OUTSIDE   TYPE=INTEGER
PARM REFL      TYPE=INTEGER VALID=(0,1)

END-PROC
.TITLE
.HELP
PURPOSE:
EXECUTION:

Example

lpscrb inp=A out=B spacing=1 sline=1 ssamp=1 nline=3674 nsamp=5295 autozoom=0 nolong=0.0 dn=255 bdn=0 pdn=0 outside=0 tag=0 refl=0

 TIMING:

As fast as C can read and write the lines.  

ORIGINAL PROGRAMMER:    P. KIM          26 APR 2007
 
.LEVEL1
.VARIABLE INP
.VARIABLE OUT
.VARIABLE SPACING
.VARIABLE SLINE
.VARIABLE SSAMP
.VARIABLE NLINE
.VARIABLE NSAMP
.VARIABLE AUTOZOOM
.VARIABLE NOLONG
.VARIABLE TAG
.VARIABLE DN
.VARIABLE BDN
.VARIABLE PDN
.VARIABLE OUTSIDE
.VARIABLE REFL
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlpscrb.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

lpscrb inp=(lpxxx2.slp) out=lpxxximg2 spacing=1 sline=1 ssamp=1 nline=3674 nsamp=5295 autozoom=0 nolong=0.0 dn=255 bdn=0 pdn=0 outside=0 tag=0 refl=0

xvd lpxxximg2

theend>
end-proc
$ Return
$!#############################################################################
