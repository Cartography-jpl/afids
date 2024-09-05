$!****************************************************************************
$!
$! Build proc for MIPL module genesearch
$! VPACK Version 1.9, Monday, May 18, 2009, 16:35:50
$!
$! Execute by entering:		$ @genesearch
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
$ write sys$output "*** module genesearch ***"
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
$ write sys$output "Invalid argument given to genesearch.com file -- ", primary
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
$   if F$SEARCH("genesearch.imake") .nes. ""
$   then
$      vimake genesearch
$      purge genesearch.bld
$   else
$      if F$SEARCH("genesearch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake genesearch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @genesearch.bld "STD"
$   else
$      @genesearch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create genesearch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack genesearch.com -mixed -
	-s genesearch.c -
	-i genesearch.imake -
	-p genesearch.pdf -
	-t tstgenesearch.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create genesearch.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "gsl_matrix.h"
#include "gsl_vector.h"
#include "gsl_fit.h"
#include "gsl_linalg.h"
#include "cartoSortUtils.h"

#define NBANDS        14
#define NRANDPICKS    10000

#define VNIRFILTSIZE  7
#define SWIRFILTSIZE  3

#define COMPUTECOEFF  1
#define CHROMOSOME_LENGTH 196
#define N_POPULATION  500
#define N_EVOLUTIONS  100

typedef struct
{
   char chromosome[196];
   double fitness;
}PERSON;

typedef struct
{
   float coeffs[196];
   int nl, ns, nFiles;
   int coeffFlags[196];
   char dirname[99];
   int *lefts, *rights;
   int *randPicks;
   int *intersectingPixels[2];
   int pixcnt; // number of intersecting pixels
   float **A, *B;
   float **result, *tmp, *tmp2, *tmp3;
   int normalizedNightUnit, units[196];
   int night12unit, day12unit;
}ALGORITHM_VARS;

/***********************************************************************/
void openFiles(ALGORITHM_VARS *vars)
{
   int i, j, fileno, status;
   char fname[99];

   fileno = 0;
   for(i = 0; i < NBANDS; i++)
   {
      sprintf(fname, "%s/day/normalized%d", vars->dirname, i+1);
      status = zvunit(&(vars->units[fileno]), "NONE", fileno+1, "U_NAME", fname, 0);
      assert(status == 1);
      status = zvopen(&(vars->units[fileno]), "OP", "READ", "U_FORMAT", "REAL", 0);
      assert(status == 1);
      ++fileno;
   }

   for(i = 0; i < NBANDS; i++)
      for(j = 0; j < NBANDS; j++)
      {
         if(i == j) continue;
         sprintf(fname, "%s/day/normalizedratio%d_%d", vars->dirname, i+1, j+1);
         status = zvunit(&(vars->units[fileno]), "NONE", fileno+1, "U_NAME", fname, 0);
         assert(status == 1);
         status = zvopen(&(vars->units[fileno]), "OP", "READ", "U_FORMAT", "REAL", 0);
         assert(status == 1);
         ++fileno;
      }

   assert(fileno == NBANDS*NBANDS);

   sprintf(fname, "%s/gtpresult2.img", vars->dirname);
   status = zvunit(&(vars->night12unit), "OTHER", 1, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->night12unit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();
   sprintf(fname, "%s/day/b12ref", vars->dirname);
   status = zvunit(&(vars->day12unit), "OTHER", 2, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->day12unit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();
   sprintf(fname, "%s/day/normalizednight", vars->dirname);
   status = zvunit(&(vars->normalizedNightUnit), "OTHER", 3, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->normalizedNightUnit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();
   status = zvget(vars->night12unit, "U_NL", vars->nl, "U_NS", vars->nl, 0);
}

/***********************************************************************/
void sortRand(int *array)
{
   int sortedIndices[NRANDPICKS], tmpArray[NRANDPICKS], i;

   memcpy(tmpArray, array, NRANDPICKS*sizeof(int));
   getSelectionSortIndices(array, sortedIndices, NRANDPICKS, CART_FLOAT);

   for(i = 0; i < NRANDPICKS; i++)
      array[i] = tmpArray[sortedIndices[i]];
}

/***********************************************************************/
void getAandB(ALGORITHM_VARS *vars)
{
   int status;
   int i, oldLine, nightOldLine;

   oldLine = -1;
   nightOldLine = -1;
   for(i = 0; i < NBANDS*NBANDS; i++)
   {
      int j;

      if(!(vars->coeffFlags[i])) continue;
      for(j = i; j < NBANDS*NBANDS; j++)
      {
         int k;

         if(!(vars->coeffFlags[j])) continue;
         for(k = 0; k < NRANDPICKS; k++)
         {
            int line, sample;

            line = vars->intersectingPixels[0][vars->randPicks[k]];
            sample = vars->intersectingPixels[1][vars->randPicks[k]];

            if(line != oldLine)
            {
               status = zvread(vars->units[i], vars->tmp, "LINE", line + 1, 0);
               assert(status == 1);
               status = zvread(vars->units[j], vars->tmp2, "LINE", line + 1, 0);
               assert(status == 1);

               oldLine = line;
            }

            vars->A[i][j] += vars->tmp[sample]*vars->tmp2[sample];

            /* B is a vector and A is a matrix... only fill B when j equals i */
            if(j == i)
            {
               if(line != nightOldLine)
               { 
                  status = zvread(vars->normalizedNightUnit, vars->tmp3, "LINE", line + 1, 0);
                  assert(status == 1);

                  nightOldLine = line;
               }

               vars->B[i] += vars->tmp[sample]*vars->tmp3[sample];
            }
         }

         if(i != j) vars->A[j][i] = vars->A[i][j];
      }
   }
}

/***********************************************************************/
void solveCoeffs(ALGORITHM_VARS *vars)
{
   int i, indexI, indexJ;
   gsl_matrix *mA, *mV;
   gsl_vector *vB, *vS, *vwork, *vX;

   mA    = gsl_matrix_alloc(vars->nFiles, vars->nFiles);
   mV    = gsl_matrix_alloc(vars->nFiles, vars->nFiles);
   vB    = gsl_vector_alloc(vars->nFiles);
   vS    = gsl_vector_alloc(vars->nFiles);
   vwork = gsl_vector_alloc(vars->nFiles);
   vX    = gsl_vector_alloc(vars->nFiles);

   for(i = indexI = 0; i < NBANDS*NBANDS; i++)
   {
      int j;

      if(!(vars->coeffFlags[i])) continue;
      for(j = indexJ = 0; j < NBANDS*NBANDS; j++)
      {
         if(!(vars->coeffFlags[j])) continue;
         gsl_matrix_set(mA, indexI, indexJ, (double)(vars->A[i][j]));
         ++indexJ;
      }

      gsl_vector_set(vB, indexI, (double)(vars->B[i]));
      ++indexI;
   }
   assert(indexI == vars->nFiles && indexJ == vars->nFiles); // sanity check

   gsl_linalg_SV_decomp(mA, mV, vS, vwork);

   for(i = 0; i < vars->nFiles; i++)
      if(gsl_vector_get(vS, i) < 10e-4*gsl_vector_get(vS, 0)) gsl_vector_set(vS, i, 0.0);

   gsl_linalg_SV_solve(mA, mV, vS, vB, vX);

   for(i = 0; i < vars->nFiles; i++) vars->coeffs[i] = gsl_vector_get(vX, i);
}

/***********************************************************************/
void computeCoefficients(ALGORITHM_VARS *vars)
{
   int i, indexCnt;

   getAandB(vars);
   solveCoeffs(vars);

   indexCnt = 0;
   for(i = 0; i < NBANDS; i++)
   {
      if(vars->coeffFlags[indexCnt]) printf("BAND %d : %d\n", i+1, indexCnt);
      ++indexCnt;
   }
   for(i = 1; i < NBANDS+1; i++)
   {
      int j;
      for(j = 1; j < NBANDS+1; j++)
      {
         if(i == j) continue;
         if(vars->coeffFlags[indexCnt]) printf("BAND %d/%d : %d\n", i, j, indexCnt);
         ++indexCnt;
      }
   }
   for(i = 0; i < vars->nFiles; i++)
   {
      printf("PKCOEFF %d: %f\n", i, vars->coeffs[i]);
   }
}

/***********************************************************************/
void applyCoefficients(ALGORITHM_VARS *vars)
{
   int i, j, k, status, coeffCnt;

   for(i = 0; i < vars->nl; i++)
      memset(vars->result[i], 0, sizeof(float)*vars->ns);
   coeffCnt = 0;
   for(i = 0; i < NBANDS*NBANDS; i++)
   {
      if(!(vars->coeffFlags[i])) continue;

      for(j = 0; j < vars->nl; j++)
      {
         status = zvread(vars->units[i], vars->tmp, "LINE", j+1, 0);
         assert(status == 1);

         for(k = vars->lefts[j]; k < vars->rights[j]; k++)
            vars->result[j][k] += vars->tmp[k]*vars->coeffs[coeffCnt];
      }

      ++coeffCnt;
   }

   assert(coeffCnt == vars->nFiles);
}

/**************************************************/
/*
double getError(ALGORITHM_VARS *vars)
{
   int i, j;
   double l1Err;

   l1Err = 0.0;
   for(i = 0; i < nl; i++)
   {

      for(j = 0; j < ns; j++)
      {







      }

   }

}
*/
/**************************************************/
void calculateFitness(PERSON *p, ALGORITHM_VARS *vars)
{
   int i;
   double L1err;

   /* set coeffFlags to match chromosome */
   vars->nFiles = 0;
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
   {
      if(p->chromosome[i])
      {
         vars->coeffFlags[i] = 1;
         vars->nFiles++;
      }
      else vars->coeffFlags[i] = 0;
   }

   computeCoefficients(vars);
   applyCoefficients(vars);
   //   L1err = getError(vars);
}

/**************************************************/
int calculatePopFitness(PERSON **pop, ALGORITHM_VARS *vars)
{
   int i, best;

   best = 0;
   for(i = 0; i < N_POPULATION; i++)
   {
      calculateFitness(pop[i], vars);
      if(pop[best]->fitness < pop[i]->fitness) best = i;
   }

   return best;
}

/**************************************************/
ALGORITHM_VARS* getVars()
{
   int status, i, j;
   char fname[99];
   ALGORITHM_VARS* vars;
   int randcnt;

   vars = malloc(sizeof(ALGORITHM_VARS*));

   strcpy(vars->dirname, "../../set0");
   openFiles(vars);

   status = zvget(vars->units[0], "U_NL", &(vars->nl), "U_NS", &(vars->ns), 0);

   vars->lefts = (int*)malloc(sizeof(int)*vars->nl);
   vars->rights = (int*)malloc(sizeof(int)*vars->nl);
   vars->tmp = (float*)malloc(vars->ns*sizeof(float));
   vars->tmp2 = (float*)malloc(vars->ns*sizeof(float));
   vars->tmp3 = (float*)malloc(vars->ns*sizeof(float));
   for(i = 0; i < vars->nl; i++)
   {
      vars->lefts[i] = vars->ns;
      vars->rights[i] = 0;

      status = zvread(vars->units[0], vars->tmp, "LINE", i+1, 0);
      assert(status == 1);
      for(j = 0; j < vars->ns; j++)
      {
         if(fabs(vars->tmp[j]) < 10E-10) continue;
         if(vars->lefts[i] == vars->ns) vars->lefts[i] = j;
         vars->rights[i] = j;
      }
   }

   vars->result = (float**)malloc(vars->nl*sizeof(float*));
   for(i = 0; i < vars->nl; i++)
      vars->result[i] = (float*)malloc(vars->ns*sizeof(float));
   vars->A = (float**)malloc(NBANDS*NBANDS*sizeof(float*));
   assert(vars->A != 0);
   for(i = 0; i < NBANDS*NBANDS; i++)
   {
      vars->A[i] = (float*)calloc(NBANDS*NBANDS, sizeof(float));
      assert(vars->A[i] != 0);
   }
   vars->B = (float*)calloc(NBANDS*NBANDS, sizeof(float));
   assert(vars->B != 0);

   sprintf(fname, "%s/gtpresult2.img", vars->dirname);
   status = zvunit(&(vars->night12unit), "OTHER", 1, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->night12unit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();
   sprintf(fname, "%s/day/b12ref", vars->dirname);
   status = zvunit(&(vars->day12unit), "OTHER", 2, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->day12unit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();
   sprintf(fname, "%s/day/normalizednight", vars->dirname);
   status = zvunit(&(vars->normalizedNightUnit), "OTHER", 3, "U_NAME", fname, 0);
   if(status != 1) zabend();
   status = zvopen(vars->normalizedNightUnit, "OP", "READ", "U_FORMAT", "REAL", 0);
   if(status != 1) zabend();

   vars->intersectingPixels[0] = (int *)malloc(sizeof(int)*vars->ns*vars->nl);
   vars->intersectingPixels[1] = (int *)malloc(sizeof(int)*vars->ns*vars->nl);

   /* Get intersecting pixels */
   vars->pixcnt = 0;
   for(i = 0; i < vars->nl; i++)
   {
      int j;

      status = zvread(vars->night12unit, vars->tmp, "LINE", i+1, 0);
      assert(status == 1);
      status = zvread(vars->day12unit, vars->tmp2, "LINE", i+1, 0);
      assert(status == 1);

      for(j = 0; j < vars->ns; j++)
      {
         if(fabs(vars->tmp[j]) > 10e-10 && fabs(vars->tmp2[j]) > 10e-10)
         {
            vars->intersectingPixels[0][vars->pixcnt] = i;
            vars->intersectingPixels[1][vars->pixcnt] = j;

            vars->pixcnt++;
         }
      }
   }

   if(vars->pixcnt < NRANDPICKS)
   {
      printf("Not enough pixels overlap between the 2 images to find coefficients");
      zabend();
   }

   vars->randPicks = (int*)malloc(sizeof(int)*NRANDPICKS);
   randcnt = 0;
   while(randcnt < NRANDPICKS)
   {
      int randnum, alreadypicked;

      randnum = rand() % vars->pixcnt;

      /* check to see if randnum is already picked */
      alreadypicked = 0;
      for(i = 0; i < randcnt; i++)
         if(vars->randPicks[i] == randnum) alreadypicked = 1;

      /* if randnum is not already picked, then pick */
      if(!alreadypicked) vars->randPicks[randcnt++] = randnum;
   }
   sortRand(vars->randPicks);

   return vars;
}

/**************************************************/
void generateRandomPop(PERSON **pop)
{
   int i, j;

   for(i = 0; i < N_POPULATION; i++)
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
          pop[i]->chromosome[j] = rand()%2;
}

/***********************************************************************/
void main44(void)
{
   PERSON **pop, **oldPop;
   int i;
   ALGORITHM_VARS *vars;

   /* set up variables to calculate resulting image */
   srand(0);
   vars = getVars();

   /* allocate memory for population */
   pop = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   oldPop = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   for(i = 0; i < N_POPULATION; i++)
   {
      pop[i] = (PERSON*)malloc(sizeof(PERSON));
      oldPop[i] = (PERSON*)malloc(sizeof(PERSON));
   }

   generateRandomPop(pop);

   for(i = 0; i < N_EVOLUTIONS; i++)
   {
      calculatePopFitness(pop, vars);
      break;
      //      generateNewPop(pop, oldPop);
   }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create genesearch.imake
#define  PROGRAM   genesearch

#define MODULE_LIST genesearch.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.*/
#define DEBUG

#define USES_ANSI_C

#define LIB_CARTO
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
#define LIB_GSL
$ Return
$!#############################################################################
$PDF_File:
$ create genesearch.pdf
PROCESS      HELP=*

END-PROC

.TITLE
VICAR/IBIS Program "genesearch"
.HELP
PURPOSE

   This program creates a compressed output file by compressing
the input file.  The compression type can by specified by specifying
COMPRESS="NONE", COMPRESS="BASIC", or COMPRESS="BASIC2".  By
specifying COMPRESS="NONE", it will create an uncompressed output
file.  Compression type BASIC and BASIC2 are similar in algorithm
and they only differ in the way the compressed indices are placed
inside the compressed file.  NOTE: The user does not have to be
concerned with the input compression type as this will be handled
by the VICAR rtl.

EXECUTION

   The program simply reads a line from the input file and 
writes it out to the output file using the compression type
specified.

EXAMPLES

   genesearch inp=x1 out=x2 compress=BASIC

This command will compress x1 using BASIC compression method and
create a compressed file called x2.

   genesearch inp=x1 out=x2 compress=NONE

This command will uncompress x1 (it does not matter if x1 is
compressed or not as we are only concerned with the output
file) and create a file x2.

RESTRICTIONS

The size of image cannot be greater than 
18,446,744,073,709,551,616 bytes.  64 bit long integer is
used to keep track of EOL label position.

Original Programmer: P. Kim, 14 May 2008

.LEVEL1
.VARIABLE INP
 Input image file
.VARIABLE OUT
 Output image file
.VARIABLE COMPRESS
 Compression type

.LEVEL2
.VARIABLE COMPRESS 

 This variable specifies the output compression
 type.  The valid choices are NONE (uncompressed)
 BASIC and BASIC2.

.END











$ Return
$!#############################################################################
$Test_File:
$ create tstgenesearch.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let $echo="yes"

genesearch

END-PROC













$ Return
$!#############################################################################
