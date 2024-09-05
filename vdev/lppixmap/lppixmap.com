$!****************************************************************************
$!
$! Build proc for MIPL module lppixmap
$! VPACK Version 1.9, Sunday, August 05, 2007, 20:20:28
$!
$! Execute by entering:		$ @lppixmap
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
$ write sys$output "*** module lppixmap ***"
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
$ write sys$output "Invalid argument given to lppixmap.com file -- ", primary
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
$   if F$SEARCH("lppixmap.imake") .nes. ""
$   then
$      vimake lppixmap
$      purge lppixmap.bld
$   else
$      if F$SEARCH("lppixmap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lppixmap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lppixmap.bld "STD"
$   else
$      @lppixmap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lppixmap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lppixmap.com -mixed -
	-s lppixmap.c -
	-i lppixmap.imake -
	-p lppixmap.pdf -
	-t tstlppixmap.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lppixmap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "mappingUtils.h"
#include "VicHdrs.h"
#include "fileutils.h"

/***********************************************************************
lppixmap.c

Written by Al Zobrist, P. Kim

This program was originally ported and modified from pixmap.  It
converts the X, Y, Z values in the LP shape entities to either
its corresponding map value or image value.  The algorithm
was originally written by Al Zobrist in pixmap.c.

9-May-07 - finalized
/************************************************************************/

void main44()
{
   int ibis1,ibis2,status;
   int maptopix, pixtomap;
           
   zifmessage("lppixmap version 09-May-07");
   /* get the basic parameters and calculate the mapping */
   maptopix = zvptst("maptopix");
   pixtomap = zvptst("pixtomap");
   if (maptopix&&pixtomap)
      zmabend("Only one keyword for mapping direction can be given");
   if (!maptopix&&!pixtomap)
      zmabend("One keyword for mapping direction must be given");

   /* read in points from the ibis interface file */
   ibis1 = openIBISInp(1);
   LPList *list = slp2LPList(ibis1);
   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);

   lpPixMapConversion(list, maptopix, 2);

   /* Output points to the ibis interface file */
   ibis2 = openIBISOutSLP(list->totDataSize, list->fmt, 1);
   printSLP(ibis2, list);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lppixmap.imake
#define  PROGRAM   lppixmap

#define MODULE_LIST dbfopen.c shpopen.c MemUtils.c fileutils.c LPUtils.c mappingUtils.c lppixmap.c

#define MAIN_LANG_C
/*#define R2LIB*/

/* Comment this out before delivery.
#define DEBUG
*/

#define USES_ANSI_C

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create lppixmap.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,99) COUNT=2
PARM OUT      TYPE=(STRING,99) COUNT=1
PARM MAPTOPIX TYPE=KEYWORD COUNT=(0:1) VALID=MAPTOPIX DEFAULT=--
PARM PIXTOMAP TYPE=KEYWORD COUNT=(0:1) VALID=PIXTOMAP DEFAULT=--
END-PROC
.TITLE
VICAR Program lppixmap
.HELP
PURPOSE

.PAGE
WRITTEN BY:    A. Zobrist  P. Kim, 8 Aug 2007

REVISIONS: 
  none

.LEVEL1
.VARIABLE INP
Input files of the .slp file
and the GEOTiff file with
the convertion header. The
".slp" MUST be specified
before the GEOTiff file.

For example:
inp=(lake.slp, geotiff)
.VARIABLE OUT
Output file.  This file is
a .slp file with the
translated vertices.
.VARIABLE MAPTOPIX
Keyword to convert map to pixel.
If specified, PIXTOMAP keyword
cannot be specified.
.VARIABLE PIXTOMAP
Keyword to convert pixel to map.
If specified, MAPTOPIX keyword
cannot be specified.

.LEVEL2
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlppixmap.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-2"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"

! TEST SCRIPT FOR lppixmap

gen xim0 nl=100 ns=200
gtgen xim0 xim 'tiecnvrt +
   geotiff=("ModelTiePointTag=(2,3,0,350807.4,5317081.3,0.0)", +
          "ModelTiePointTag=(102,83,0,351807.4,5316081.3,0.0)", +
          "ModelTiePointTag=(2,83,0,350807.4,5316081.3,0.0)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)")
gtlist xim

lppixmap inp=(lpxxx9.slp,xim) out=lpxxxPix.slp 'maptopix

ibis-list lpxxx9.slp 'format
ibis-list lpxxxPix.slp 'format

end-proc
$ Return
$!#############################################################################
