$!****************************************************************************
$!
$! Build proc for MIPL module detclst
$! VPACK Version 1.9, Monday, October 04, 2010, 12:19:32
$!
$! Execute by entering:		$ @detclst
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
$ write sys$output "*** module detclst ***"
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
$ write sys$output "Invalid argument given to detclst.com file -- ", primary
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
$   if F$SEARCH("detclst.imake") .nes. ""
$   then
$      vimake detclst
$      purge detclst.bld
$   else
$      if F$SEARCH("detclst.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake detclst
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @detclst.bld "STD"
$   else
$      @detclst.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create detclst.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack detclst.com -mixed -
	-s detclst.c -
	-i detclst.imake -
	-p detclst.pdf -
	-t tstdetclst.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create detclst.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "ImageUtils.h"

/***************************************************************/
double centerDiff(VICAR_TILE_IMAGE *vti, double subtract)
{
   int i, j;
   double sum;

   sum = .0;
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += fabs(vti->tile[i][j] - subtract);
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
double getMean(VICAR_TILE_IMAGE *vti)
{
   int i, j;
   double sum;

   sum = .0;
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += vti->tile[i][j];
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
double meanCenterDiff(VICAR_TILE_IMAGE *vti)
{
   double mean;

   mean = getMean(vti);
   return centerDiff(vti, mean);
}

/***************************************************************/
double variance(VICAR_TILE_IMAGE *vti)
{
   int i, j;
   double mean, sum;

   sum = .0;
   mean = getMean(vti);
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += pow(vti->tile[i][j]-mean, 2.);
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
void main44(void)
{
   int i, j, mode, status, cnt, dum, win;
   VICAR_IMAGE *vi, *out;
   VICAR_TILE_IMAGE *vti;

   status = zvparm("win", &win, &cnt, &dum, 1, 0);
   assert(status == 1);
   vi = getVI_inp(1);
   vti = getVTI(vi, win, win);
   status = zvparm("mode", &mode, &cnt, &dum, 1, 0);
   assert(status == 1);
   out = getVI_out("REAL", 1, vi->nl, vi->ns);

   for(i = 0; i < vi->nl; i++)
   {
      for(j = 0; j < vi->ns; j++)
      {
         readVicarTileImage(vti, i, j);
         switch(mode)
         {
            case 1:
               out->buffer[j] = centerDiff(vti, vti->tile[win/2][win/2]);
               break;
            case 2:
               out->buffer[j] = meanCenterDiff(vti);
               break;
            case 3:
               out->buffer[j] = variance(vti);
               break;
         }
      }

      writeVicarImageLine(out, i);
   }

   deleteVTI(&vti);
   deleteAndCloseImage(&vi);
   deleteAndCloseImage(&out);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create detclst.imake
#define  PROGRAM   detclst

#define MODULE_LIST detclst.c ImageUtils.h ImageUtils.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.*/
#define DEBUG

#define USES_ANSI_C

#define LIB_CARTO
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create detclst.pdf
PROCESS      HELP=*
PARM INP       TYPE=(STRING) COUNT=1
PARM OUT       TYPE=(STRING) COUNT=1
PARM MODE      TYPE=INTEGER  COUNT=1 VALID=(1,2,3) DEFAULT=1
PARM WIN       TYPE=INTEGER  COUNT=1 DEFAULT=5
END-PROC

.TITLE
VICAR/IBIS Program "detclst"
.HELP
PURPOSE
 Outputs statistical image of the input image
 measuring the variance of the neighboring pixels.

.LEVEL1
.VARIABLE INP
 Input image file

.VARIABLE OUT
 Output image file

.VARIABLE MODE
 Stat mode
 1 for sum of difference
 between center and
 it's neighbors
 2 for sum of difference
 between mean and
 the neighbors,
 excluding the center
 3 for variance
 excluding the center

.VARIABLE WIN
 window size

.END











$ Return
$!#############################################################################
$Test_File:
$ create tstdetclst.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let $echo="yes"

gen x 10 10

detclst x x1 1
detclst x x2 2
detclst x x3 3


END-PROC













$ Return
$!#############################################################################
