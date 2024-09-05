$!****************************************************************************
$!
$! Build proc for MIPL module lpgen
$! VPACK Version 1.9, Sunday, August 05, 2007, 20:20:28
$!
$! Execute by entering:		$ @lpgen
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
$ write sys$output "*** module lpgen ***"
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
$ write sys$output "Invalid argument given to lpgen.com file -- ", primary
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
$   if F$SEARCH("lpgen.imake") .nes. ""
$   then
$      vimake lpgen
$      purge lpgen.bld
$   else
$      if F$SEARCH("lpgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lpgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lpgen.bld "STD"
$   else
$      @lpgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lpgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lpgen.com -mixed -
	-s lpgen.c -
	-i lpgen.imake -
	-p lpgen.pdf -
	-t tstlpgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lpgen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "VicHdrs.h"
#include "fileutils.h"
#include "MemUtils.h"
#include "strings.h"

/***********************************************************
lpgen.c

Written by P. Kim

This program creates a .dlp file from the given .slp file

8/3/07 - finalized
***********************************************************/

void main44()
{
   LPList* list;
   LPTble* tble;
   int ibis, i;

   ibis = openIBISInp(1);
   list = slp2LPList(ibis);
   IBISFileClose(ibis, 0);

   printf("\tGenerating LPTble from LPList data.\n");
   tble = (LPTble*)calloc(1, sizeof(LPTble));
   tble->numOfEntities = list->numOfEntities;
   tble->recordSize = sizeof(int) + 17;
   tble->numOfColumns = 2;
   mz_alloc1((unsigned char**)&(tble->colLoc), tble->numOfColumns, sizeof(int));
   (tble->colLoc)[0] = 0;
   (tble->colLoc)[1] = 4;
   strcpy(tble->fmt[0], "FULL");
   strcpy(tble->fmt[1], "FULL");
   strcpy(tble->fmt[2], "A16");

   mz_alloc1((unsigned char**)&(tble->data), tble->recordSize*tble->numOfEntities, 1);
   for(i = 0; i < list->numOfEntities; i++)
   {
      int index;
      char hwyName[16];
      index = getTbleIndex(tble, i, 0);
      ((int*)((char*)(tble->data)+index))[0] = i+1;
      index = getTbleIndex(tble, i, 1);
      sprintf(hwyName, "HWY %d", i+1);
      strcpy(((char*)(tble->data)+index), hwyName);
   }
   ibis = openIBISOutDLP(tble->numOfColumns, tble->numOfEntities, tble->fmt, 1);
   printDLP(ibis, tble);
   IBISFileClose(ibis, 0);
   destroyLPTble(tble);
   destroyLPList(list);
}




$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lpgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lpgen

   To Create the build file give the command:

		$ vimake lpgen			(VMS)
   or
		% vimake lpgen			(Unix)


************************************************************************/


#define PROGRAM	lpgen

#define MODULE_LIST dbfopen.c shpopen.c LPUtils.c fileutils.c MemUtils.c lpgen.c

#define MAIN_LANG_C
#define R2LIB
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lpgen.pdf
process help=*
PARM INP         TYPE=(STRING,99)
PARM OUT         TYPE=(STRING,99)
END-PROC
.TITLE
.HELP
PURPOSE:
This proc is to call the c program lpgen.
EXECUTION:

Example

lpgen inp=A out=B

TIMING: 

As fast as C can read and write the lines.  

 ORIGINAL PROGRAMMER:    P. Kim          26 Apr 2007

.LEVEL1
.VARIABLE INP
This variable specifies the
".slp" file.
.VARIABLE OUT
This variable specifies the
generated ".dlp" file.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlpgen.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

lpgen inp=lpxxx9.slp out=lpxxx9a.dlp 

ibis-list lpxxx9a.dlp

theend>
end-proc
$ Return
$!#############################################################################
