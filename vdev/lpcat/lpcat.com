$!****************************************************************************
$!
$! Build proc for MIPL module lpcat
$! VPACK Version 1.9, Sunday, August 05, 2007, 20:20:28
$!
$! Execute by entering:		$ @lpcat
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
$ write sys$output "*** module lpcat ***"
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
$ write sys$output "Invalid argument given to lpcat.com file -- ", primary
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
$   if F$SEARCH("lpcat.imake") .nes. ""
$   then
$      vimake lpcat
$      purge lpcat.bld
$   else
$      if F$SEARCH("lpcat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lpcat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lpcat.bld "STD"
$   else
$      @lpcat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lpcat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lpcat.com -mixed -
	-s lpcat.c -
	-i lpcat.imake -
	-p lpcat.pdf -
	-t tstlpcat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lpcat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"

#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "fileutils.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "VicHdrs.h"

/*************************************************************
lpcat.c

Written by Peter Kim

This program concatenates 2 LP files

8/3/07 - finalized
*************************************************************/

/*************************************************************/
void main44()
{
   int ibis1, ibis2, ibis3, inpCnt, outCnt;
   
   ibis1 = openIBISInp(1);
   ibis2 = openIBISInp(2);
   
   LPList *list1 = slp2LPList(ibis1);
   LPList *list2 = slp2LPList(ibis2);
   LPList *list3 = addLists(list1, list2);
   destroyLPList(list1);
   destroyLPList(list2);

   IBISFileClose(ibis1, 0);
   IBISFileClose(ibis2, 0);

   ibis3 = openIBISOutSLP(list3->totDataSize, list3->fmt, 1);
   printSLP(ibis3, list3);
   destroyLPList(list3);
   IBISFileClose(ibis3, 0);
   
   zvpcnt("inp", &inpCnt);
   zvpcnt("out", &outCnt);
   //   printf("inp: %d out: %d\n", inpCnt, outCnt);
   if(inpCnt == 4 && outCnt == 2)
   {
      ibis1 = openIBISInp(3);
      ibis2 = openIBISInp(4);
      LPTble *tble1 = dlp2LPTble(ibis1);
      LPTble *tble2 = dlp2LPTble(ibis2);
      LPTble *tble3 = addTbles(tble1, tble2);
      destroyLPTble(tble1);
      destroyLPTble(tble2);

      IBISFileClose(ibis1, 0);
      IBISFileClose(ibis2, 0);

      ibis3 = openIBISOutDLP(tble3->numOfColumns, tble3->numOfEntities, tble3->fmt, 2);
      printDLP(ibis3, tble3);
      destroyLPTble(tble3);
      IBISFileClose(ibis3, 0);
   }
}














$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lpcat.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lpcat

   To Create the build file give the command:

		$ vimake lpcat			(VMS)
   or
		% vimake lpcat			(Unix)


************************************************************************/


#define PROGRAM	lpcat

#define MODULE_LIST dbfopen.c shpopen.c LPUtils.c MemUtils.c fileutils.c lpcat.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lpcat.pdf
process help=*
PARM INP         TYPE=(STRING,99) COUNT=(2:4)
PARM OUT         TYPE=(STRING,99) COUNT=(1:2)
END-PROC
.TITLE
.HELP
PURPOSE:
EXECUTION:

Example

lpcat inp=(A, B) out=C

TIMING: 

As fast as C can read and write the lines.  

 ORIGINAL PROGRAMMER:    P. Kim          26 Apr 2007

.LEVEL1
.VARIABLE INP
This variable specifies
the files to concatenate.
There must be 2 .slp files
to concatenate whereas
the 2 .dlp files are
optional.  The .slp files
must be listed before the
".dlp" files.

.VARIABLE OUT
This variable specifies
the output files.
If only the .slp files
were specified in the
input, then only the
".slp" output file needs
to be specified.  If both
".slp" and .dlp files were
specified in the input
variable then both .slp 
and .dlp output files 
should be specified. The 
".slp" file must be specified 
before the .dlp file.

.LEVEL2
.VARIABLE INP
For example:
to concatenate lake1.slp
and lake2.slp - 
inp=(lake1.slp, lake2.slp)
to concatenate their
corresponding .dlp files
inp=(lake1.slp, lake2.slp,
lake1.dlp, lake2.dlp)

.VARIABLE OUT
For example:
if only .slp files were
specified in inp, then -
out=(output.slp)
if both .slp and .dlp
files were specified
in inp, then -
out=(output.slp, output.dlp)

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlpcat.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

lpcat inp=(lpxxx9.slp, lpxxx9.slp, lpxxx9.dlp, lpxxx9.dlp) out=(lpxxx2.slp, lpxxx2.dlp)

lplist inp=(lpxxx2.slp, lpxxx2.dlp) start=1 count=10
lplist inp=(lpxxx2.slp, lpxxx2.dlp) start=360 count=10
lplist inp=(lpxxx2.slp, lpxxx2.dlp) start=730 count=10
theend>
end-proc
$ Return
$!#############################################################################
