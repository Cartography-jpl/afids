$!****************************************************************************
$!
$! Build proc for MIPL module despike2
$! VPACK Version 1.9, Tuesday, October 12, 2010, 14:31:06
$!
$! Execute by entering:		$ @despike2
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
$ write sys$output "*** module despike2 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to despike2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("despike2.imake") .nes. ""
$   then
$      vimake despike2
$      purge despike2.bld
$   else
$      if F$SEARCH("despike2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake despike2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @despike2.bld "STD"
$   else
$      @despike2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create despike2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack despike2.com -mixed -
	-s despike2.c -
	-i despike2.imake -
	-p despike2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create despike2.c
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
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sort.h>

#define WINSIZE 10

void main44(void)
{
   int i, j, status, dumcnt, nChanged;
   float scale, tol;
   double *medBuf;
   char *threshFlags;
   VICAR_IMAGE *inp, *out;
   VICAR_TILE_IMAGE *tile;

   inp = getVI_inp(1);
   out = getVI_out(inp->format, 1, inp->nl, inp->ns);
   tile = getVTI(inp, 1, WINSIZE);
   status = zvp("SCALE", &scale, &dumcnt);
   assert(status == 1);
   status = zvp("TOL", &tol, &dumcnt);
   assert(status == 1);
   medBuf = (double*)malloc(sizeof(double)*inp->ns);
   threshFlags = (char*)malloc(sizeof(double)*inp->ns);
   nChanged = 0;

   for(i = 0; i < inp->nl; i++)
   {
      readVicarImageLine(inp, i);
      memcpy(out->buffer, inp->buffer, sizeof(double)*inp->ns);

      // create median buffer
      for(j = 0; j < inp->ns; j++)
      {
         int k;
         double tmp[WINSIZE];

         readVicarTileImage(tile, i, j);
         for(k = 0; k < WINSIZE; k++)
            tmp[k] = tile->tile[0][k];
         /*
         if(i == 1)
         {
            printf("%d %lf %lf %lf\n", j, tile->tile[0][2], tile->buffer[0][j], inp->buffer[j]);
            for(k = 0; k < WINSIZE; k++)
               printf("%lf ",tile->tile[0][k]);
            printf("\n");
         }
         */
         gsl_sort(tmp, 1, WINSIZE);
         medBuf[j] = gsl_stats_median_from_sorted_data(tmp, 1, WINSIZE);
         //         if(i == 1) printf("%lf ", medBuf[j]);
         medBuf[j] = fabs(inp->buffer[j]-medBuf[j]);
         //         if(i == 1) printf("%lf\n", medBuf[j]);
      }

      // perform 1st test and trigger threshold flags
      for(j = 0; j < inp->ns; j++)
      {
         // k, l is the left and right indices of medBuf
         int k, l, m;
         double thresh, sum;

         threshFlags[j] = 0;
         readVicarTileImage(tile, i, j);

         // make sure left and right indices of medBuf is not out of bounds
         if(j == 0) k = 0;
         else k = j-1;
         if(j == inp->ns - 1) l = inp->ns-1;
         else l = j+1;

         sum = 0.;
         for(m = k; m <= l; m++) sum += medBuf[m];
         thresh = (scale/(double)WINSIZE)*(sum)+tol;
         if(medBuf[j] > thresh) threshFlags[j] = 1;
         //         if(i == 1) printf("%d: %d %lf\n", j, threshFlags[j], thresh); 
      }

      // perform 2nd test to determine spike then fix
      for(j = 0; j < inp->ns; j++)
      {
         int k, l, m;
         double thresh, expected, sum, leftDiff, rightDiff, avgDiff, diffCnt, sumCnt;

         if(!threshFlags[j])
            continue;

         if(j == 0) k = 0;
         else k = j-(WINSIZE/2);
         if(j == inp->ns - 1) l = inp->ns-1;
         else l = j+(WINSIZE/2);

         leftDiff = 0;
         rightDiff = 0;
         avgDiff = 0;
         diffCnt = 0;
         sum = 0;
         sumCnt = 0;
         for(m = k; m <= l; m++)
         {
            if(m == j) continue;

            sum += inp->buffer[m];
            sumCnt++;
            //            printf("%lf\n", sum);
            if(m+1 < j)
            {
               leftDiff += fabs(inp->buffer[m+1]-inp->buffer[m]);
               diffCnt++;
            }
            else if(m-1 > j)
            {
               rightDiff += fabs(inp->buffer[m-1]-inp->buffer[m]);
               diffCnt++;
            }
         }
         //         printf("diff cnt: %lf window size: %d sum: %lf\n", diffCnt, WINSIZE, sum);
         if(diffCnt > 0) avgDiff = (leftDiff+rightDiff)/diffCnt;
         else avgDiff = 0;

         expected = sum/sumCnt;
         //         expected = (inp->buffer[k]+inp->buffer[l])/2.;
         thresh = scale*avgDiff + tol;
         //         printf("scale: %lf tol: %lf threshold 2: %lf\n", scale, tol, thresh);
         // check to see if fails threshold and positive spike
         if(fabs(inp->buffer[j] - expected) > thresh && inp->buffer[j] > expected)
         {
            out->buffer[j] = expected;
            nChanged++;
         }

         //         if(i == 1) printf("%d: leftDiff: %lf rightDiff: %lf inp-expected: %lf sum: %lf inp: %lf thresh: %lf\n", j, leftDiff, rightDiff, fabs(inp->buffer[j]-expected), sum, inp->buffer[j], thresh); 
      }

      writeVicarImageLine(out, i);
   }

   printf("despiking from %s to %s - %d pixels changed.\n", inp->fname, out->fname, nChanged);
   deleteVTI(&tile);
   deleteAndCloseImage(&inp);
   deleteAndCloseImage(&out);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create despike2.imake
#define  PROGRAM   despike2

#define MODULE_LIST despike2.c ImageUtils.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.*/
#define DEBUG

#define USES_ANSI_C

#define LIB_GSL
#define LIB_CARTO
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create despike2.pdf
PROCESS      HELP=*
PARM INP       TYPE=(STRING) COUNT=1
PARM OUT       TYPE=(STRING) COUNT=1
PARM SCALE     TYPE=REAL     COUNT=1 DEFAULT=3.
PARM TOL       TYPE=REAL     COUNT=1 DEFAULT=3.
END-PROC

.TITLE
VICAR/IBIS Program "despike2"
.HELP
PURPOSE

.LEVEL1
.VARIABLE INP
 Input image file

.VARIABLE OUT
 Output ascii file

.END

$ Return
$!#############################################################################
