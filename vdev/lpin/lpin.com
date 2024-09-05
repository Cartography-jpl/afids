$!****************************************************************************
$!
$! Build proc for MIPL module lpin
$! VPACK Version 1.9, Monday, May 22, 2000, 23:22:07
$!
$! Execute by entering:		$ @lpin
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
$ write sys$output "*** module lpin ***"
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
$ write sys$output "Invalid argument given to lpin.com file -- ", primary
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
$   if F$SEARCH("lpin.imake") .nes. ""
$   then
$      vimake lpin
$      purge lpin.bld
$   else
$      if F$SEARCH("lpin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lpin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lpin.bld "STD"
$   else
$      @lpin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lpin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lpin.com -
	-s lpin.c -
	-i lpin.imake -
	-p lpin.pdf -
	-t tstlpin.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lpin.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"

/*  copy file, n bytes   A. Zobrist    5/21/00   */

main44()
{
   int i,nb,ct,def;
   char infilename[99],outfilename[99],buf[2];
   FILE *infile,*outfile;
   
   /*zifmessage("lpin version 21-may-00"); won't run with this*/
 
   zvp("NB",&nb,&ct);
   zvparm("inp",infilename,&ct,&def,1,0);
   zvparm("out",outfilename,&ct,&def,1,0);
   
   infile = fopen(infilename,"r");
   outfile = fopen(outfilename,"w");
   
   for (i=0;i<nb;i++)
      {
      fread(buf,1,1,infile);
      fwrite(buf,1,1,outfile);
      }
   
   fclose(outfile);
   printf("Copied %d bytes from %s to %s\n",nb,infilename,outfilename);
   return 0;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lpin.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lpin

   To Create the build file give the command:

		$ vimake lpin			(VMS)
   or
		% vimake lpin			(Unix)


************************************************************************/


#define PROGRAM	lpin

#define MODULE_LIST lpin.c

#define MAIN_LANG_C
#define R2LIB
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lpin.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM NB TYPE=INTEGER DEFAULT=1000
END-PROC
.TITLE
lpin - chop off the start of a file
.HELP
PURPOSE:
lpin copies NB bytes of the input to the output
EXECUTION:

Example

lpin A B 200

OPERATION:
lpin does not use VICAR IO.
 TIMING: 

As fast as C can read and write the lines.  

 ORIGINAL PROGRAMMER:    A. Zobrist          21 May 2000
 COGNIZANT PROGRAMMER:   Barbara McGuffie    21 May 2000
 
 REVISION HISTORY
  00-5-21    AZ   Initial version
  
.LEVEL1
.VARIABLE INP
STRING - Input file
.VARIABLE OUT
STRING - Output file
.VARIABLE NB
INTEGER - Number of bytes to copy
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlpin.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"

gen a nl=10 ns=10

lpin a b 20

od -x b

end-proc
$ Return
$!#############################################################################
