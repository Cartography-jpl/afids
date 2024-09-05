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
#define CROSSOVER_RATE 0.7
#define MUTATION_RATE 0.005
#define N_POPULATION  500
#define N_EVOLUTIONS  100
#define ALPHA         1
#define DATASET       "../../set6"

typedef struct
{
   char chromosome[196];
   double fitness, L1err;
   int nFiles;
}PERSON;

typedef struct
{
   float coeffs[196];
   int coeffFlags[196];
   char dirname[99];
   int units[196];
   int *intersectingPixels[2];
   int *lefts, *rights;
   int *randPicks;
   float **A, *B;
   float **result, *tmp, *tmp2, *tmp3;
   float **normNightImage;
   float ***images;
   int pixcnt; // number of intersecting pixels
   int normalizedNightUnit;
   int night12unit, day12unit;
   int nl, ns, nFiles;
   int bandCnt[196];
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
      status = zvopen(vars->units[fileno], "OP", "READ", "U_FORMAT", "REAL", 0);
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
         status = zvopen(vars->units[fileno], "OP", "READ", "U_FORMAT", "REAL", 0);
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
}

/***********************************************************************/
void readImages(ALGORITHM_VARS *vars)
{
   int i, j, status;

   for(i = 0; i < 196; i++)
      for(j = 0; j < vars->nl; j++)
      {
         status = zvread(vars->units[i], vars->images[i][j], "LINE", j+1, 0);
         assert(status == 1);
      }

   for(i = 0; i < vars->nl; i++)
   {
      status = zvread(vars->normalizedNightUnit, vars->normNightImage[i], "LINE", i+1, 0);
      assert(status == 1);
   }
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
   int i, oldLine;

   oldLine = -1;
   for(i = 0; i < NBANDS*NBANDS; i++)
   {
      int j;

      for(j = i; j < NBANDS*NBANDS; j++)
      {
         int k;

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
               vars->B[i] += vars->tmp[sample]*vars->normNightImage[line][sample];
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
void applyCoefficients(ALGORITHM_VARS *vars)
{
   int i, j, k, coeffCnt;

   for(i = 0; i < vars->nl; i++)
      memset(vars->result[i], 0, sizeof(float)*vars->ns);
   coeffCnt = 0;
   for(i = 0; i < NBANDS*NBANDS; i++)
   {
      if(!(vars->coeffFlags[i])) continue;

      for(j = 0; j < vars->nl; j++)
      {
        /*
         status = zvread(vars->units[i], vars->tmp, "LINE", j+1, 0);
         assert(status == 1);
        */
         for(k = vars->lefts[j]; k < vars->rights[j]; k++)
           //            vars->result[j][k] += vars->tmp[k]*vars->coeffs[coeffCnt];
            vars->result[j][k] += vars->images[i][j][k]*vars->coeffs[coeffCnt];
      }

      ++coeffCnt;
   }

   /*
   {
      int unit, status;

      status = zvunit(&unit, "OTHER2", 1, "U_NAME", "testresult.img", 0);
      assert(status == 1);
      status = zvopen(unit, "OP", "WRITE", "U_NL", vars->nl, "U_NS", vars->ns, "U_FORMAT", "REAL", "O_FORMAT", "REAL", 0);
      assert(status == 1);

      for(i = 0; i < vars->nl; i++)
      {
         status = zvwrit(unit, vars->result[i], "LINE", i+1, 0);
         assert(status == 1);
      }

      status = zvclose(unit, 0);
      assert(status == 1);
      return;
   }
   */

   assert(coeffCnt == vars->nFiles);
}

/**************************************************/
double getError(ALGORITHM_VARS *vars)
{
   int i, j;
   double l1Err;
   int cnt;

   cnt = 0;
   l1Err = 0.0;
   for(i = 0; i < vars->nl; i++)
   {
      for(j = 0; j < vars->ns; j++)
         if(fabs(vars->result[i][j]) > 10E-10 && fabs(vars->normNightImage[i][j]) > 10E-10)
         {
            l1Err += fabs(vars->result[i][j] - vars->normNightImage[i][j]);
            ++cnt;
         }
   }

   return l1Err/cnt;
}

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

   if(!vars->nFiles)
   {
      p->fitness = 0.;
      p->L1err = 1.;
      p->nFiles = 0;

      return;
   }
   solveCoeffs(vars);
   applyCoefficients(vars);
   L1err = getError(vars);
   //   printf("error: %f %f\n", L1err, ALPHA*vars->nFiles/(double)196);

   //   p->fitness = (1+ALPHA) - (L1err + ALPHA*vars->nFiles/(double)196);
   p->fitness = exp((196-vars->nFiles)/(L1err*50));
   //   p->fitness = exp(-L1err);

   p->L1err = L1err;
   p->nFiles = vars->nFiles;
}

/**************************************************/
int calculatePopFitness(PERSON **pop, ALGORITHM_VARS *vars)
{
   int i, j, best;

   best = 0;
   for(i = 0; i < N_POPULATION; i++)
   {
      calculateFitness(pop[i], vars);
      printf("person: %d fitness: %f ", i+1, pop[i]->fitness);
      for(j = 0; j < CHROMOSOME_LENGTH; j++) printf("%d", pop[i]->chromosome[j]);
      printf("\t%f %d\n", pop[i]->L1err, pop[i]->nFiles);
      if(pop[best]->fitness < pop[i]->fitness) best = i;

      for(j = 0; j < CHROMOSOME_LENGTH; j++)
         if(pop[i]->chromosome[j]) vars->bandCnt[j]++;

      //      if(i == 10) exit(0);
   }

   return best;
}

/**************************************************/
ALGORITHM_VARS* getVars()
{
   int status, i, j;
   ALGORITHM_VARS* vars;
   int randcnt;

   vars = (ALGORITHM_VARS*)malloc(sizeof(ALGORITHM_VARS));
   assert(vars != NULL);

   strcpy(vars->dirname, DATASET);
   openFiles(vars);

   status = zvget(vars->units[0], "NL", &(vars->nl), "NS", &(vars->ns), 0);
   assert(status == 1);
   vars->images = (float***)malloc(sizeof(float**)*196);
   for(i = 0; i < 196; i++)
   {
      vars->images[i] = (float**)malloc(sizeof(float*)*vars->nl);
      for(j = 0; j < vars->nl; j++)
         vars->images[i][j] = (float*)malloc(sizeof(float)*vars->ns);
   }
   vars->normNightImage = (float**)malloc(sizeof(float*)*vars->nl);
   for(i = 0; i < vars->nl; i++)
      vars->normNightImage[i] = (float*)malloc(sizeof(float)*vars->ns);
   readImages(vars);

   vars->lefts = (int*)malloc(sizeof(int)*vars->nl);
   vars->rights = (int*)malloc(sizeof(int)*vars->nl);
   vars->tmp = (float*)malloc(vars->ns*sizeof(float));
   vars->tmp2 = (float*)malloc(vars->ns*sizeof(float));
   vars->tmp3 = (float*)malloc(vars->ns*sizeof(float));

   /* initialize the lefts and rights to unset flag of -1 */
   for(i = 0; i < vars->nl; i++)
      vars->lefts[i] = vars->rights[i] = -1;
   for(i = 0; i < 14; i++)
   {
      for(j = 0; j < vars->nl; j++)
      {
         int k;

         status = zvread(vars->units[i], vars->tmp, "LINE", j+1, 0);
         assert(status == 1);

         if(!fabs(vars->tmp[0]) < 10E-10 && vars->lefts[j] == -1)
            vars->lefts[j] = 0;
         if(vars->rights[j] == -1 && !fabs(vars->tmp[vars->ns-1]) < 10E-10) 
            vars->rights[j] = vars->ns-1;

         for(k = 0; k < vars->ns-1; k++)
         {
            /* check for start of image in line - inclusive */
            if(fabs(vars->tmp[k]) < 10E-10 && !fabs(vars->tmp[k+1]) < 10E-10
               && vars->lefts[j] < k)
               vars->lefts[j] = k;
            /* check for end of image in line - exclusive */
            if(!fabs(vars->tmp[k]) < 10E-10 && fabs(vars->tmp[k+1]) < 10E-10
               && (vars->rights[j] > k+1 || vars->rights[j] == -1))
               vars->rights[j] = k+1;
         }
      }
   }
   /* if some lefts and rights are still unset then set them */
   for(i = 0; i < vars->nl; i++)
   {
      if(vars->lefts[i] == -1) vars->lefts[i] = vars->ns;
      if(vars->rights[i] == -1) vars->rights[i] = 0;
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

   vars->intersectingPixels[0] = (int *)malloc(sizeof(int)*vars->ns*vars->nl);
   vars->intersectingPixels[1] = (int *)malloc(sizeof(int)*vars->ns*vars->nl);

   /* Get intersecting pixels */
   vars->pixcnt = 0;
   for(i = 0; i < vars->nl; i++)
   {
      int j;

      status = zvread(vars->normalizedNightUnit, vars->tmp, "LINE", i+1, 0);
      assert(status == 1);
      /*
      status = zvread(vars->units[0], vars->tmp2, "LINE", i+1, 0);
      assert(status == 1);
      */
      for(j = 0; j < vars->ns; j++)
      {
         if(fabs(vars->tmp[j]) > 10e-10 && j >= vars->lefts[i] && j < vars->rights[i])
         {
            vars->intersectingPixels[0][vars->pixcnt] = i;
            vars->intersectingPixels[1][vars->pixcnt] = j;

            vars->pixcnt++;
         }
      }
   }

   printf("intersecting pixels: %d\n", vars->pixcnt);
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

   getAandB(vars);

   for(i = 0; i < CHROMOSOME_LENGTH; i++) vars->bandCnt[i] = 0;

   return vars;
}

/**************************************************/
void generateRandomPop(PERSON **pop)
{
   int i, j;
   char trialgene1[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
/* 1 */                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
/* 2 */                 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 3 */                 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
/* 4 */                 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 5 */                 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 1, 0, 0, 
/* 6 */                 0, 0, 0, 0, 0,    1, 0, 0, 0, 0, 0, 0, 0, 
/* 7 */                 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 
/* 8 */                 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 
/* 9 */                 0, 0, 0, 0, 0, 0, 1, 0,    0, 0, 0, 0, 0, 
/* 10 */                0, 1, 0, 0, 0, 0, 0, 0, 0,    0, 0, 1, 0, 
/* 11 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 
/* 12 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 
/* 13 */                0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 
/* 14 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    };

   char trialgene2[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* 1 */                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 2 */                 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 3 */                 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 4 */                 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 5 */                 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 
/* 6 */                 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 
/* 7 */                 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 
/* 8 */                 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 
/* 9 */                 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 
/* 10 */                0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 
/* 11 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 
/* 12 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 
/* 13 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 
/* 14 */                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    };


   for(i = 0; i < N_POPULATION; i++)
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
      {
         if(i < 196)
         {
            if(i == j) pop[i]->chromosome[j] = 1;
            else pop[i]->chromosome[j] = 0;
         }
         else
            pop[i]->chromosome[j] = rand()%2;
      }

   /* inserting the trial genes
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      pop[0]->chromosome[i] = trialgene1[i];

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      pop[1]->chromosome[i] = trialgene2[i];
   */
}

/**************************************************/
void getRouletteWheel(PERSON **population, double *roulette_wheel)
{
   int i;

   roulette_wheel[0] =  (population[0])->fitness;
   for(i = 1; i < N_POPULATION; i++)
      roulette_wheel[i] = roulette_wheel[i-1] + (population[i])->fitness;
}

/**************************************************/
int searchRoulette(double *roulette, double val)
{
   int left, right, mid;

   left = 0;
   right = N_POPULATION - 1;
   mid = (left + right)/2;
   //   printf("val: %lf mid-1: %d lval: %lf mid: %d rval: %lf\n", val, mid-1, roulette[mid-1], mid, roulette[mid]);
   while(!(val <= roulette[mid] && val >= roulette[mid - 1]))
   {
     //      printf("val: %lf mid-1: %d lval: %lf mid: %d rval: %lf\n", val, mid-1, roulette[mid-1], mid, roulette[mid]);
      if(val > roulette[mid]) left = mid + 1;
      if(val < roulette[mid - 1]) right = mid - 1;
      mid = (left + right)/2;

      if(mid == 0) break;
   }

   return mid;
}

/**************************************************/
void crossover(PERSON *p1, PERSON *p2)
{
   double random = rand()/(double)(RAND_MAX);

   /* if random is greater than crossover rate then crossover chromosomes */
   if(random < CROSSOVER_RATE)
   {
      int index;
      char tmp[CHROMOSOME_LENGTH];

      index = rand()%CHROMOSOME_LENGTH;
      memcpy(tmp, p1->chromosome, index);
      memcpy(p1->chromosome, p2->chromosome, index);
      memcpy(p2->chromosome, tmp, index);
   }
}

/**************************************************/
void mutate(PERSON *p)
{
   int i;

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      if(rand()/(double)(RAND_MAX) < MUTATION_RATE) 
         (p->chromosome)[i] = rand()%2;
}

/**************************************************/
void copyPerson(PERSON *from, PERSON *to)
{
   memcpy(to->chromosome, from->chromosome, CHROMOSOME_LENGTH);
   to->fitness = from->fitness;
   to->L1err = from->L1err;
   to->nFiles = from->nFiles;
}

/**************************************************/
void createNewPopulation(PERSON **oldPopulation, PERSON **newPopulation, double *roulette_wheel)
{
   int i, roulette1, roulette2;
   double pick1, pick2;
   double sum = roulette_wheel[N_POPULATION-1];
   PERSON *person1, *person2;

   person1 = (PERSON*)malloc(sizeof(PERSON));
   person2 = (PERSON*)malloc(sizeof(PERSON));

   printf("avg: %f\n", sum/N_POPULATION);

   for(i = 0; i < N_POPULATION; i+=2)
   {
      pick1 = rand()/(double)(RAND_MAX)*sum;
      pick2 = rand()/(double)(RAND_MAX)*sum;

      roulette1 = searchRoulette(roulette_wheel, pick1);
      roulette2 = searchRoulette(roulette_wheel, pick2);

      copyPerson(oldPopulation[roulette1], person1);
      copyPerson(oldPopulation[roulette2], person2);

      crossover(person1, person2);
      mutate(person1);
      mutate(person2);

      copyPerson(person1, newPopulation[i]);
      copyPerson(person2, newPopulation[i+1]);
   }
}

/**************************************************/
void copyPopulation(PERSON **from, PERSON **to)
{
   int i;

   for(i = 0; i < N_POPULATION; i++)
   {
      memcpy(to[i]->chromosome, from[i]->chromosome, CHROMOSOME_LENGTH);
      to[i]->fitness = from[i]->fitness;
   }
}

/***********************************************************************/
void printPerson(PERSON *p)
{
   int i, j, index;

   index = 0;
   for(i = 0; i < NBANDS; i++)
      printf("%d: %d\n", i+1, p->chromosome[index++]);

   for(i = 0; i < NBANDS; i++)
   {
      for(j = 0; j < NBANDS; j++) 
      {
         if(i == j) continue;

         printf("%d %d: %d\n", i+1, j+1, p->chromosome[index++]);
      }
   }

   printf("fitness: %f\n", p->fitness);
   printf("L1err:   %f\n", p->L1err);
   printf("nFiles:  %d\n", p->nFiles);
}

/***********************************************************************/
void main44(void)
{
   PERSON **pop, **oldPop, *bestP;
   int i, best;
   ALGORITHM_VARS *vars;
   double *roulette_wheel;

   /* set up variables to calculate resulting image */
   srand(0);
   vars = getVars();

   /* allocate memory for population */
   bestP = (PERSON*)malloc(sizeof(PERSON));
   pop = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   oldPop = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   for(i = 0; i < N_POPULATION; i++)
   {
      pop[i] = (PERSON*)malloc(sizeof(PERSON));
      oldPop[i] = (PERSON*)malloc(sizeof(PERSON));
   }
   roulette_wheel = (double*)malloc(sizeof(double)*N_POPULATION);

   generateRandomPop(pop);

   for(i = 0; i < N_EVOLUTIONS; i++)
   {
      printf("EVOLUTION: %d\n", i+1);
      printf("=============\n");
      best = calculatePopFitness(pop, vars);
      if(pop[best]->fitness > bestP->fitness)
         copyPerson(pop[best], bestP);
      printf("best: %f\n", bestP->fitness);
      //      printPerson(bestP);
      getRouletteWheel(pop, roulette_wheel);
      copyPopulation(pop, oldPop);
      createNewPopulation(oldPop, pop, roulette_wheel);
      //      break;
      //      generateNewPop(pop, oldPop);
   }

   /* display result */
   {
      int j, index;

      index = 0;
      printPerson(bestP);
      for(i = 0; i < 14; i++)
         printf("band %d: %d\n", i+1, vars->bandCnt[index++]);

      for(i = 0; i < 14; i++)
         for(j = 0; j < 14; j++)
         {
            if(i == j) continue;

            printf("band %d/%d: %d\n", i+1, j+1, vars->bandCnt[index++]);
         }

      assert(index == CHROMOSOME_LENGTH);
   }
}
