$!****************************************************************************
$!
$! Build proc for MIPL module shp2lp
$! VPACK Version 1.9, Tuesday, August 28, 2007, 17:53:46
$!
$! Execute by entering:		$ @shp2lp
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
$ write sys$output "*** module shp2lp ***"
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
$ write sys$output "Invalid argument given to shp2lp.com file -- ", primary
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
$   if F$SEARCH("shp2lp.imake") .nes. ""
$   then
$      vimake shp2lp
$      purge shp2lp.bld
$   else
$      if F$SEARCH("shp2lp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake shp2lp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @shp2lp.bld "STD"
$   else
$      @shp2lp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create shp2lp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack shp2lp.com -mixed -
	-s shp2lp.c -
	-i shp2lp.imake -
	-p shp2lp.pdf -
	-t tstshp2lp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create shp2lp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "shapefil.h"

#include "fileutils.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "VicHdrs.h"

/*************************************************************
shp2lp.c
  
Written by P. Kim

This program creates a .slp and .dlp file from shape files.

8/3/07 - finalized
*************************************************************/

/*************************************************************/
void main44()
{
   int parmct, parmdf, ibis, fmt, dlp, status;
   char inFile[95];
   SHPHandle shpHandle;
   DBFHandle dbfHandle;
   LPList *list;
   LPTble *tble;
   FILE *file;
   char shpFileName[99], dbfFileName[99];
   int index, fileCount;
   
   zvparm("inp", inFile, &parmct, &parmdf, 1, 99);
   zvparm("fmt", &fmt, &parmct, &parmdf, 1, 99);

   strcat(shpFileName, inFile);
   strcat(shpFileName, ".shp");
   file = fopen(shpFileName, "r");
   if(file != 0)
   {
      fclose(file);
      shpHandle = SHPOpen(inFile, "rb");
      list = shp2LPList(shpHandle);
      ibis = openIBISOutSLP(list->totDataSize, list->fmt, 1);
      printSLP(ibis, list);
      destroyLPList(list);
      status = IBISFileClose(ibis, 0);
      if(status != 1) IBISSignal(ibis, status, 1);
   }
   else
   {
      printf("%s not found... !!!\nProceeding to tabular data.\n", shpFileName);
      fclose(file);
   }

   strcat(dbfFileName, inFile);
   strcat(dbfFileName, ".dbf");
   file = fopen(dbfFileName, "r");
   zvpcnt("out", &fileCount);
   if(file != 0 && fileCount == 2)
   {
      fclose(file);
      dbfHandle = DBFOpen(inFile, "rb");
      tble = dbf2LPTble(dbfHandle);
      ibis = openIBISOutDLP(tble, 2);
      printDLP(ibis, tble);
      destroyLPTble(tble);
      status = IBISFileClose(ibis, 0);
   }
   else
   {
      printf("%s not found or output .dlp file not specified.\n", dbfFileName );
      fclose(file);
   }
}









$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create shp2lp.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM shp2lp

   To Create the build file give the command:

		$ vimake shp2lp			(VMS)
   or
		% vimake shp2lp			(Unix)


************************************************************************/


#define PROGRAM	shp2lp

#define MODULE_LIST dbfopen.c fileutils.c shpopen.c MemUtils.c LPUtils.c shp2lp.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create shp2lp.pdf
process help=*
PARM INP TYPE=(STRING,99)
PARM OUT TYPE=STRING  COUNT=(1:2)
PARM FMT TYPE=INTEGER DEFAULT=8
END-PROC
.TITLE
VICAR/IBIS Program shp2lp
.HELP
PURPOSE:

This program creates a .slp from
a .shp file and also optionally,
a .dlp file from a .dbf file.  This
program is the first step in
processing a shape file through
the lp programs.

In the input, format 4 is to 
convert the shape file into an 
integer .slp file, 7 to a float 
.slp file, and 8 to a double .slp 
file.
.PAGE
EXAMPLES:

shp2lp inp=lake out=lake.slp fmt=4

The above example will take lake.shp
and create a lake.slp file.  The format
of the lake.slp file will be integer.

shp2lp inp=lake out=(lake.slp, lake.dlp) fmt=7

The above example will take lake.shp
and create a lake.slp, lake.dlp files.
The format of the lake.slp file will be
float.
.PAGE
RESTRICTIONS:

This program requires programs in the
PKUtils.com.

Only a format of 4(int), 7(float),
8(double) can be used.

.PAGE
OPERATION:

algorithm steps:
1. read and map each entity in the shape data
2. output shape data into .slp file
3. read and map each record in the .dbf file
4. output tabular data into .dlp file

.PAGE
WRITTEN BY:

ORIGINAL PROGRAMMER:    PETER KIM      26 APR 2007
 
.LEVEL1
.VARIABLE INP
Base root name of the .shp
and .dbf files.

.VARIABLE OUT
Name of the .slp and .dlp
output files.

.LEVEL2
.VARIABLE INP
STRING - Input file
(maximum string length 99)

The user needs to specify the
base name of the shape file.
For example, if the shape files
are "lake.shp", "lake.shx",
"lake.dbf", then variable inp
should be "lake".

.VARIABLE OUT
STRING - Output file
(maximum string length 99)

The user needs to specify the
the name of the .slp and .dlp
files.  The .slp MUST be
specified whereas .dlp file
is optional.  The .slp file
MUST be specified before the
".dlp" file.
For example,
out=(lake.slp)
out=(lake.slp, lake.dlp)
.VARIABLE FMT
INTEGER - Format
(4 - integer (full) format
 7 - float (real) format
 8 - double format)

This variable allows the
user to specify the .slp
format.  The default format
is double if not specified.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstshp2lp.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

shp2lp inp=testShapes out=(lpxxx9.slp, lpxxx9.dlp) fmt=8

ibis-list lpxxx9.slp
ibis-list lpxxx9.dlp

theend>
end-proc
$ Return
$!#############################################################################
