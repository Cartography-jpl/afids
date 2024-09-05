$!****************************************************************************
$!
$! Build proc for MIPL module lpselect
$! VPACK Version 1.9, Wednesday, July 04, 2007, 15:56:00
$!
$! Execute by entering:		$ @lpselect
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
$ write sys$output "*** module lpselect ***"
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
$ write sys$output "Invalid argument given to lpselect.com file -- ", primary
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
$   if F$SEARCH("lpselect.imake") .nes. ""
$   then
$      vimake lpselect
$      purge lpselect.bld
$   else
$      if F$SEARCH("lpselect.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lpselect
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lpselect.bld "STD"
$   else
$      @lpselect.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lpselect.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lpselect.com -mixed -
	-s lpselect.c -
	-i lpselect.imake -
	-p lpselect.pdf -
	-t tstlpselect.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lpselect.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "/home/pkim/utilities/LPUtils.h"
#include "/home/pkim/utilities/shapefil.h"

#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>

int getRowsNeeded(data1, data2, nr1, nr2)
   void *data1;
   unsigned int *data2;
   int nr1, nr2;
{
   int index1, index2, rowsNeeded, tempIndex;
   int fmt, type, cycle;

   rowsNeeded = 0;
   for(index2 = 0; index2 < nr2; index2++)
   {
      index1 = 0;

      while(index1 < nr1 && data2[index2] != getTag(data1))
      {
         tempIndex = nextEntityIndex(data1);
         type = getType(data1);
         if(type == 8) (double *)data1 += tempIndex;
         else if(type == 4 || type == 7) (unsigned int*)data1 += tempIndex;
         index1 += tempIndex;
      }

      if(data2[index2] == getTag(data1))
         rowsNeeded += nextEntityIndex(data1);
      else
         zmabend("An index requested is not in the .slp file");
   }

   return rowsNeeded;
}

void writeData(ibis, data1, data2, rowsNeeded, nEntities)
   int ibis, nEntities, rowsNeeded;
   void *data1;
   unsigned int *data2;
{
   void *data3;
   int i, j, fmt, fmtCheck, index, length;

   fmt = getType(data1);
   if(fmt == 8) mz_alloc1((unsigned char**)&data3, rowsNeeded, sizeof(double));
   else if(fmt == 4 || fmt == 7) mz_alloc1((unsigned char **)&data3, rowsNeeded, sizeof(float));

   index = i = 0;
   while(i < nEntities)
   {
      fmtCheck = getType(data1);
      if(fmt != fmtCheck) zmabend("Different formats in the same file!!!");
      length = getLength(data1);
      if(fmt == 8 && getTag(data1) == data2[i])
      {
         for(j = 0; j < length; j++)
            ((double *)data3)[index++] = ((double *)data1)[j+2];
         ++i;
      }
      else if(fmt == 7 && getTag(data1) == data2[i])
      {
         for(j = 0; j < length; j++)
            ((float *)data3)[index++] = ((float *)data1)[j+4];
         ++i;
      }
      else if(fmt == 4 && getTag(data1) == data2[i])
      {
         for(j = 0; j < length; j++)
            ((unsigned int*)data3)[index++] = ((unsigned int*)data1)[j+4];
         ++i;
      }

      if(fmt == 8)(double *)data1 += nextEntityIndex(data1);
      else if(fmt == 7)(float *)data1 += nextEntityIndex(data1);
      else if(fmt == 4)(unsigned int *)data1 += nextEntityIndex(data1);
   }

   if(fmt == 8)
      IBISColumnWrite(ibis, (double *)data3, 1, 1, rowsNeeded);
   else if(fmt == 7)
      IBISColumnWrite(ibis, (float *)data3, 1, 1, rowsNeeded);
   else if(fmt == 4)
      IBISColumnWrite(ibis, (unsigned int *)data3, 1, 1, rowsNeeded);

   free(data3);
}
      

main44()
{
   int status, ibis1, ibis2, ibis3;
   int nr1, nr2, key, parmct, parmdf, rowsNeeded;
   void *data1;
   unsigned int *data2;

   ibis1 = openIBISInp(1);
   ibis2 = openIBISInp(2);
   IBISFileGet(ibis1, "nr", &nr1, 0, 0, 0);
   IBISFileGet(ibis2, "nr", &nr2, 0, 0, 0);
   zvparm("key", &key, &parmct, &parmdf, 1, 0);

   mz_alloc1((unsigned char**)&data1, nr1, sizeof(double));
   mz_alloc1((unsigned char**)&data2, nr2, sizeof(double));
   status = IBISColumnRead(ibis1, data1, 1, 1, nr1);
   if(status != 1) IBISSignal(ibis1, status, 1);
   status = IBISColumnRead(ibis2, data2, key, 1, nr2);
   if(status != 1) IBISSignal(ibis2, status, 1);
   status = IBISFileClose(ibis1, 0);
   if(status != 1) IBISSignal(ibis1, status, 1);
   status = IBISFileClose(ibis2, 0);
   if(status != 1) IBISSignal(ibis2, status, 1);
   printf("nr1: %d nr2: %d\n", nr1, nr2);

   rowsNeeded = getRowsNeeded(data1, data2, nr1, nr2);
   ibis3 = openIBISOut(1, rowsNeeded, 1);
   writeData(ibis3, data1, data2, rowsNeeded, nr2);

   free(data1);
   free(data2);
   IBISFileClose(ibis3, 0);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lpselect.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lpselect

   To Create the build file give the command:

		$ vimake lpselect			(VMS)
   or
		% vimake lpselect			(Unix)


************************************************************************/


#define PROGRAM	lpselect

#define MODULE_LIST LPUtils.c fileutils.c shpopen.c MemUtils.c lpselect.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/

$ Return
$!#############################################################################
$PDF_File:
$ create lpselect.pdf
process help=*
PARM INP         TYPE=(STRING,99) COUNT=2
PARM OUT         TYPE=(STRING,99)
PARM KEY         TYPE=INTEGER
END-PROC
.TITLE
.HELP
PURPOSE:
EXECUTION:

Example

lpselect A B C D

TIMING: 

As fast as C can read and write the lines.  

 ORIGINAL PROGRAMMER:    P. Kim          26 Apr 2007

.LEVEL1
.VARIABLE INP
.VARIABLE OUT
.VARIABLE KEY
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlpselect.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"


ibis-gen lpxxx5 nc=4 nr=5 index=2 datacol=1 +
     data=(0,2,3,4,5) 'ibis-2 format=(FULL,FULL,A12,A12) +
     strcol=(3,4) +
     string=(aaaaaa1,bbbbbb1,aaaaaa2,bbbbbb2,aaaaaa3, +
      bbbbbb3,aaaaaa4,bbbbbb4,aaaaaa5,bbbbbb5)
   
lpselect inp=(lpxxx4.slp, lpxxx5) out=lpxxx6 key=1
ibis-list lpxxx6

theend>
end-proc
$ Return
$!#############################################################################
