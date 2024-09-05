$!****************************************************************************
$!
$! Build proc for MIPL module crtshp
$! VPACK Version 1.9, Tuesday, November 12, 2019, 10:15:34
$!
$! Execute by entering:		$ @crtshp
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
$ write sys$output "*** module crtshp ***"
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
$ write sys$output "Invalid argument given to crtshp.com file -- ", primary
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
$   if F$SEARCH("crtshp.imake") .nes. ""
$   then
$      vimake crtshp
$      purge crtshp.bld
$   else
$      if F$SEARCH("crtshp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake crtshp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @crtshp.bld "STD"
$   else
$      @crtshp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create crtshp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack crtshp.com -mixed -
	-s crtshp.c -
	-i crtshp.imake -
	-p crtshp.pdf -
	-t tstcrtshp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create crtshp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include "vicmain_c.h"

#include "shapefil.h"
#include "carto/cartoMemUtils.h"
#include <string.h>

/* prototype */
void createDBF(const char *fileName, int nEntities);
void createTxt(const char *fileName, int minX, int maxX, int minY, int maxY, int inc);



/****************************************************************/
void createDBF(fileName, nEntities)
     const char* fileName;
     int nEntities;
{
  DBFHandle dbfHandle;
    char message[100];

  dbfHandle = DBFCreate(fileName);
  

  DBFAddField(dbfHandle, "ID", FTInteger, 9, 0);
  DBFAddField(dbfHandle, "HWY NAME", FTString, 10, 0);
  int i;
  for(i = 0; i < nEntities; i++)
    {
      DBFWriteDoubleAttribute(dbfHandle, i, 0, i);
      char hwyName[10];
      sprintf(hwyName, "HWY %d", i+1);
      DBFWriteStringAttribute(dbfHandle, i, 1, hwyName);
    }
  DBFClose(dbfHandle);

  DBFOpen(fileName, "rb");
  for(i = 0; i < DBFGetRecordCount(dbfHandle); i++)
    {
      sprintf(message,"id: %d ", DBFReadIntegerAttribute(dbfHandle, i, 0));
      zvmessage(message," ");
      sprintf(message,"name: %s\n", DBFReadStringAttribute(dbfHandle, i, 1));
      zvmessage(message," ");
    }
  DBFClose(dbfHandle);
}
/*****************************************************************/
void createTxt(fileName, minX, maxX, minY, maxY, inc)
     const char*fileName;
     int minX, maxX, minY, maxY, inc;
{
//  int nEntities = maxY/inc;

  FILE *file = fopen(fileName, "w");
  int j;
  for(j = 0; j < maxY; j+=inc)
    fprintf(file, "2\n2\n%d\n%d\n%d\n%d\n",minX, maxX, minY + j, minY + j);

  fclose(file);
}

/******************************************************************/
void main44()
{
   SHPHandle shpHandle;
   int cycle, nEntities=0;
    int ct, def;
    char inFileName[99], outFileName[99];
    FILE *infile;
    char message[100];
     int nVertices;
     SHPObject *shpObject;
     double *xBuf, *yBuf, *zBuf;

    zifmessage ("crtshp - 11-11-2019 - rjb - 64-bit");
    
   createTxt("inPolys.txt", 0, 5295, 0, 3674, 10);

   zvparm("inp",inFileName,&ct,&def,1,0); 
   infile = fopen(inFileName, "r");

   zvparm("out",outFileName,&ct,&def,1,0);

   shpHandle = SHPCreate(outFileName, SHPT_POLYGON);
   while(fscanf(infile, "%d", &cycle) != -1)
   {

     sprintf(message,"cycle: %d\n", cycle);
     zvmessage(message," ");

     fscanf(infile, "%d", &nVertices);
     sprintf(message,"nVertices: %d\n", nVertices);
     zvmessage(message," ");

     mz_alloc1((unsigned char**)&xBuf, nVertices, sizeof(double));
     mz_alloc1((unsigned char**)&yBuf, nVertices, sizeof(double));
     if(cycle == 3)
       mz_alloc1((unsigned char**)&zBuf, nVertices, sizeof(double));

     int i;
     for(i = 0; i < nVertices; i++)
       fscanf(infile, "%lf", &xBuf[i]);
     for(i = 0; i < nVertices; i++)
       fscanf(infile, "%lf", &yBuf[i]);
     if(cycle == 3)
       for(i = 0; i < nVertices; i++)
	 fscanf(infile, "%lf", &zBuf[i]);

     for(i = 0; i < nVertices; i++) {
       sprintf(message,"x = %f y = %f\n", xBuf[i], yBuf[i]);
       zvmessage(message," ");  
    }
     if(cycle == 2)
       shpObject = SHPCreateSimpleObject(SHPT_POLYGON, nVertices, xBuf, yBuf, NULL);
     else if(cycle == 3)
       shpObject = SHPCreateSimpleObject(SHPT_POLYGON, nVertices, xBuf, yBuf, zBuf);

     SHPWriteObject(shpHandle, -1, shpObject);
     SHPDestroyObject(shpObject);
     free(xBuf);
     free(yBuf);
     if(cycle == 3) free(zBuf);

     ++nEntities;
   }
    sprintf (message,"nEntities = %d\n",nEntities);
    zvmessage(message," ");
   createDBF("testShapes", nEntities);

   SHPClose(shpHandle);
   fclose(infile);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create crtshp.imake
#define  PROGRAM   crtshp

#define MODULE_LIST crtshp.c

#define MAIN_LANG_C
#define R2LIB 

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
$ create crtshp.pdf
process help=*
PARM INP         TYPE=(STRING,99)
PARM OUT         TYPE=(STRING,99)
END-PROC
.TITLE
.HELP
PURPOSE:
to create test shapes for lp programs

EXECUTION:

Currently, makes only one data set, inPolys.


Example

crtshp INP=l    OUT=testShapes

 TIMING: 

As fast as C can read and write the lines.  

ORIGINAL PROGRAMMER:    P. KIM          4 JUN 2007

11-11-2019 - Ray Bambery

 
.LEVEL1
.VARIABLE INP
This variable specifies the
text file.
.VARIABLE OUT
filename for 3 output files.
(out.dbf, out.shp, out shx)

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcrtshp.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $echo="yes"
let $autousage="none"

crtshp inp=inPolys.txt out=testShapes

theend>
let $echo="no"
end-proc
$ Return
$!#############################################################################
