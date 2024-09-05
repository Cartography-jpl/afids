$!****************************************************************************
$!
$! Build proc for MIPL module lplist
$! VPACK Version 1.9, Sunday, August 05, 2007, 20:20:28
$!
$! Execute by entering:		$ @lplist
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
$ write sys$output "*** module lplist ***"
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
$ write sys$output "Invalid argument given to lplist.com file -- ", primary
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
$   if F$SEARCH("lplist.imake") .nes. ""
$   then
$      vimake lplist
$      purge lplist.bld
$   else
$      if F$SEARCH("lplist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lplist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lplist.bld "STD"
$   else
$      @lplist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lplist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lplist.com -mixed -
	-s lplist.c -
	-i lplist.imake -
	-p lplist.pdf -
	-t tstlplist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lplist.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"

/*#include <stdio.h>*/
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "fileutils.h"
#include "VicHdrs.h"

/************************************************************
lplist.c

Written by P. Kim

This program displays the LP shape and tabular data.

5/8/07 - finalized
************************************************************/

void main44()
{
   int i, ibis;
   int startEntity, numOfEntities, endEntity, nVertices;
   int parmct, parmdf, fileCount;
   LPList *list;
   LPTble *tble;

   ibis = openIBISInp(1);
   list = slp2LPList(ibis);
   IBISFileClose(ibis, 0);
   zvpcnt("inp", &fileCount);
   if(fileCount == 2)
   {
      ibis = openIBISInp(2);
      tble = dlp2LPTble(ibis);
      IBISFileClose(ibis, 0);
   }

   zvparm("start", &startEntity, &parmct, &parmdf, 1, 0);
   zvparm("count", &numOfEntities, &parmct, &parmdf, 1, 0);
   zvparm("enum", &nVertices, &parmct, &parmdf, 1, 0);

   if(startEntity < 1)
      zmabend("\nStart Entity must be greater than 0.\n");
   if(numOfEntities < 0)
      zmabend("\nCount must not be less than 0.\n");
   --startEntity;

   if(startEntity+numOfEntities > list->numOfEntities || numOfEntities == 0)
      endEntity = list->numOfEntities;
   else
      endEntity = startEntity+numOfEntities;

   printf("\n==========================================\n");
   printf("          Listing LP Entities             \n\n");
   printf(" start entity: %d end entity: %d enum: %d \n", startEntity+1, endEntity, nVertices);
   printf("==========================================\n");

   for(i = startEntity; i < endEntity; i++)
   {
      if(fileCount == 2) printTbleEntity(tble, i);
      printListEntity(list, i, nVertices);
      printf("\n");
   }

   destroyLPList(list);
   if(fileCount == 2) destroyLPTble(tble);
}







$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lplist.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lplist

   To Create the build file give the command:

		$ vimake lplist			(VMS)
   or
		% vimake lplist			(Unix)


************************************************************************/


#define PROGRAM	lplist

#define MODULE_LIST dbfopen.c shpopen.c fileutils.c MemUtils.c LPUtils.c lplist.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lplist.pdf
process help=*
PARM INP         TYPE=(STRING,99) COUNT=(1:2)
PARM START       TYPE=INTEGER DEFAULT=1
PARM COUNT       TYPE=INTEGER DEFAULT=0
PARM ENUM        TYPE=INTEGER DEFAULT=-1
END-PROC
.TITLE
.HELP
PURPOSE:
EXECUTION:

Example

lplist inp=A start=B count=C enum=D

TIMING: 

As fast as C can read and write the lines.  

 ORIGINAL PROGRAMMER:    P. Kim          26 Apr 2007

.LEVEL1
.VARIABLE INP
The user needs to specify
the .slp to list.  The user
can also specify the
corresponding .dlp file
to list together.  The
".slp" MUST be specified
whereas the .dlp file
is optional.  The .slp
file must be specified
before the .dlp file.

For example:
inp=(lake.slp)
inp=(lake.slp, lake.dlp)
.VARIABLE START
This variable specifies the
entity number to start
listing.  Default is the
1st entity if not user
specified.  FIRST ENTITY
STARTS AT 1.

For example:
to start listing at entity
20 - 
start=20
.VARIABLE COUNT
This variable specifies
how many entities to
list.  0 value for
count means list all
entities.

For example:
to list 10 entities - 
count=10
to list ALL entities - 
count=0
.VARIABLE ENUM
This variable specifies
the number of vertices to
list for each entity.
Default is -1 (list all
vertices.)

For example:
to list 5 vertices per
entity - 
enum=5
to list all vertices
for each entity - 
enum=-1

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlplist.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

lplist inp=(lpxxx9.slp, lpxxx9.dlp) start=1 count=20 enum=5

theend>
end-proc
$ Return
$!#############################################################################
