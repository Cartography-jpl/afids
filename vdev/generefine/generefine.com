$!****************************************************************************
$!
$! Build proc for MIPL module generefine
$! VPACK Version 1.9, Monday, March 23, 2009, 11:56:43
$!
$! Execute by entering:		$ @generefine
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
$ write sys$output "*** module generefine ***"
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
$ write sys$output "Invalid argument given to generefine.com file -- ", primary
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
$   if F$SEARCH("generefine.imake") .nes. ""
$   then
$      vimake generefine
$      purge generefine.bld
$   else
$      if F$SEARCH("generefine.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake generefine
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @generefine.bld "STD"
$   else
$      @generefine.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create generefine.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack generefine.com -mixed -
	-s generefine.c -
	-i generefine.imake -
	-p generefine.pdf -
	-t tstgenerefine.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create generefine.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "ibishelper.h"
#include <assert.h>

#define CHROMOSOME_LENGTH 30
#define N_POPULATION      500
#define N_EVOLUTION       100

typedef struct
{
   double a, b, c, d, e, f;
   char chromosome[30];
}PERSON;

/**************************************************/
void binary(char *infile, char *outfile, int thresh)
{
   int i, nl, ns;
   int unit1, unit2, status;
   short int *inBuf;
   char *outBuf;

   /* open files */
   printf("creating file: %s\n", outfile);
   zvselpi(0);
   status = zvunit(&unit1, "none", 1, "u_name", infile, 0);
   if(status != 1) zabend();
   status = zvopen(unit1, "OP", "READ", 0);
   if(status != 1) zabend();
   status = zvget(unit1, "NL", &nl, "NS", &ns, 0);
   if(status != 1) zabend();

   status = zvunit(&unit2, "none", 2, "u_name", outfile, 0);
   if(status != 1) zabend();
   status = zvopen(unit2, "OP", "WRITE", "U_NL", nl, "U_NS", ns, "O_FORMAT", "BYTE",
                   "U_FORMAT", "BYTE", "OPEN_ACT", "SA", "LAB_ACT", "SA", 0);
   if(status != 1) zabend();

   /* initialize data for output */
   inBuf = (short int *)malloc(ns*sizeof(short int));
   outBuf = (char *)malloc(ns*sizeof(char));

   for(i = 0; i < nl; i++)
   {
      int j;

      status = zvread(unit1, inBuf, "LINE", i+1, 0);
      if(status != 1) zabend();

      for(j = 0; j < ns; j++)
         if(inBuf[j] > thresh) outBuf[j] = 1;
         else outBuf[j] = 0;

      status = zvwrit(unit2, outBuf, "LINE", i+1, 0);
      if(status != 1) zabend();
   }

   free(inBuf);
   free(outBuf);

   /* close image files */
   status = zvclose(unit1, 0);
   assert(status == 1);
   status = zvclose(unit2, 0);
   assert(status == 1);
}

/**************************************************/
void addToMasterImg(char *infile, short int masterImg[][1920])
{
   int i, j;
   int unit1, nl, ns, status;
   char *inBuf;

   status = zvunit(&unit1, "none", 1, "u_name", infile, 0);
   if(status != 1) zabend();
   status = zvopen(unit1, "OP", "READ", 0);
   if(status != 1) zabend();
   status = zvget(unit1, "NL", &nl, "NS", &ns, 0);
   if(status != 1) zabend();

   inBuf = (char*)malloc(sizeof(char)*ns);
   for(i = 0; i < nl; i++)
   {
      status = zvread(unit1, inBuf, "LINE", i+1, 0);
      assert(status == 1);

      for(j = 0; j < ns; j++) masterImg[i][j] += inBuf[j];
   }

   free(inBuf);

   status = zvclose(unit1, 0);
   assert(status == 1);
}

/**************************************************/
void getRouletteWheel(PERSON **population, double *roulette_wheel)
{
   int i;

   roulette_wheel[0] =  (population[0])->fitness;
   for(i = 1; i < N; i++)
      roulette_wheel[i] = roulette_wheel[i-1] + (population[i])->fitness;
}

/**************************************************/
void calcFitness(PERSON *p, short int masterImg[][1920], char img[][1920])
{
   int i, j, difcnt;
   char trialImg[1080][1920];
   double a, b, c, d, e, f;

   a = p->a;
   b = p->b;
   c = p->c;
   d = p->d;
   e = p->e;
   f = p->f;

   /* initialize the trial image */
   for(i = 0; i < 1080; i++)
      for(j = 0; j < 1920; j++)
         trialImg[i][j] = 0;

   /* generate the trial image */
   for(i = 0; i < 1080; i++)
      for(j = 0; j < 1920; j++)
      {
         int x, y;

         x = a*j + b*i + c;
         y = d*j + e*i + f;

         if(x < 0 || x > 1920) continue;
         if(y < 0 || y > 1080) continue;

         trialImg[y][x] = img[i][j];
      }

   /* diff the trial image with master image */
   difcnt = 0;
   for(i = 0; i < 1080; i++)
      for(j = 0; j < 1920; j++)
      {
         if(fabs(img[i][j] - trialImg[i][j]) >= 1)
            difcnt++;
      }

   p->fitness = 8002/difcnt;
}

/**************************************************/
double subdecode(PERSON *p, int i)
{
   int j;
   double answer;

   j = i;
   answer = 0.0;
   for(; i < j + 5; i++)
      answer += p->chromosome[i]*pow(0.1, i-j);

   return answer;
}

/**************************************************/
void decode(PERSON *p)
{
   p->a = subdecode(p, 0);
   p->b = subdecode(p, 5);
   p->c = subdecode(p, 10);
   p->d = subdecode(p, 15);
   p->e = subdecode(p, 20);
   p->f = subdecode(p, 25);
}

/**************************************************/
void generateRandomPerson(PERSON *p)
{
   int i;

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
       p->chromosome[i] = rand()%10;
}

/**************************************************/
void createRandomPopulation(PERSON **pop, int n)
{
   int i;

   srand(0);
   for(i = 0; i < n; i++) generateRandomPerson(pop[i]);
}

/**************************************************/
void main44(void)
{
   int i, j, status, unit;
   int thresh, cnt, filecnt;
   IBISStruct *ibis;
   char **outfiles;
   short int masterImg[1080][1920];
   PERSON **pop;

   /* UNIT TEST OF GENETIC CODE */
   pop = (PERSON**)malloc(sizeof(PERSON*)*2);
   pop[0] = (PERSON*)malloc(sizeof(PERSON));
   pop[1] = (PERSON*)malloc(sizeof(PERSON));
   createRandomPopulation(pop, 2);
   printf("chromosome A: ");
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      printf("%d ", pop[0]->chromosome[i]);
   printf("\n");
   decode(pop[0]);
   printf("a: %f b: %f c: %f d: %f e: %f f: %f\n", pop[0]->a,
          pop[0]->b, pop[0]->c, pop[0]->d, pop[0]->e, pop[0]->f);
   printf("chromosome B: ");
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      printf("%d ", pop[1]->chromosome[i]);
   printf("\n");
   decode(pop[1]);
   printf("a: %f b: %f c: %f d: %f e: %f f: %f\n", pop[1]->a,
          pop[1]->b, pop[1]->c, pop[1]->d, pop[1]->e, pop[1]->f);

   free(pop[0]);
   free(pop[1]);
   free(pop);
   return;
   /* END UNIT TEST */

   /* get IBIS data */
   status = zvp("thresh", &thresh, &cnt);
   ibis = IBISHELPER_openIBIS("inp", 1, "read");

   /* open input and output images */
   outfiles = (char**)malloc(sizeof(char*)*329);
   for(i = 0; i < 329; i++) outfiles[i] = (char*)calloc(50, sizeof(char));

   filecnt = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      double fileno, valid;
      char infile[50];

      valid = IBISHELPER_getDouble(ibis, 24, i);
      if(valid < 10E-10) continue;

      fileno = IBISHELPER_getDouble(ibis, 0, i);

      sprintf(infile, "/raid1/fft192/nir_%d_ff_reg.hlf", (int)fileno);
      sprintf(outfiles[filecnt], "binaryNIRs/nir_%d_ff_reg.bin", (int)fileno);
      //      binary(infile, outfiles[filecnt++], thresh);
   }

   /* create a master image */
   for(i = 0; i < 1080; i++)
      for(j = 0; j < 1920; j++) masterImg[i][j] = 0;
   for(i = 0; i < filecnt; i++)
      addToMasterImg(outfiles[i], masterImg);

   status = zvunit(&unit, "out", 1, 0);
   assert(status == 1);
   status = zvopen(unit, "OP", "WRITE", "U_NL", 1080, "U_NS", 1920,
                   "U_FORMAT", "HALF", "O_FORMAT", "HALF", 0);
   assert(status == 1);

   for(i = 0; i < 1080; i++)
   {
      for(j = 0; j < 1920; j++)
         if(masterImg[i][j] < 100) masterImg[i][j] = 0;
         else masterImg[i][j] = 1;

      status = zvwrit(unit, masterImg[i], "LINE", i+1, 0);
      assert(status == 1);
   }

   status = zvclose(unit, 0);
   assert(status == 1);

   for(i = 0; i < filecnt; i++) free(outfiles[i]);
   free(outfiles);

   IBISHELPER_closeIBIS(ibis);
}



























/*
void subencode(double val, char *a)
{
   int i;

   for(i = 0; i < 5; i++)
      a[i] = val*pow(10,i)%10;
}

void encode(PERSON *p)
{
   int i;
   char subchromosome[5];

   subencode(p->a, subchromosome);
   memcpy(p->chromosome, subchromsome, 5);
   subencode(p->b, subchromosome);
   memcpy(p->chromosome+5, subchromsome, 5);
   subencode(p->c, subchromosome);
   memcpy(p->chromosome+10, subchromsome, 5);
   subencode(p->d, subchromosome);
   memcpy(p->chromosome+15, subchromsome, 5);
   subencode(p->e, subchromosome);
   memcpy(p->chromosome+20, subchromsome, 5);
   subencode(p->f, subchromosome);
   memcpy(p->chromosome+25, subchromsome, 5);
}
*/

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create generefine.imake
#define  PROGRAM   generefine

#define MODULE_LIST generefine.c ibishelper.c ibishelper.h

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
$ create generefine.pdf
PROCESS      HELP=*
PARM INP       TYPE=(STRING) COUNT=1
PARM OUT       TYPE=(STRING) COUNT=1
PARM THRESH    TYPE=(INTEGER) COUNT=1 DEFAULT=200
END-PROC

.TITLE
VICAR/IBIS Program "generefine"
.HELP
PURPOSE

.LEVEL1
.VARIABLE INP
 Input image file

.VARIABLE OUT
 Output image file

.END











$ Return
$!#############################################################################
$Test_File:
$ create tstgenerefine.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let $echo="yes"

generefine out.int generefine.img

END-PROC
$ Return
$!#############################################################################
