#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "carto/cartoLinkedList.h"
#include "carto/cartoClassUtils.h"
#include "carto/ibishelper.h"
#include "carto/ImageUtils.h"
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#define MAX_IDS 100
#define CHROMOSOME_LENGTH 28

////////////////////////////////////////////////
typedef struct
{
   char gene[CHROMOSOME_LENGTH];
   int nCorrect;
   double score;
}PERSON;

////////////////////////////////////////////////
typedef struct
{
   double mutationRate;
   double crossoverRate;
   int nEvolutions;
   int correctClass;
}ENVIRONMENT;

////////////////////////////////////////////////
typedef struct
{
   int size;
   int evolutionCnt;
   double* roulette;
   PERSON **people;
   PERSON *bestInThisPopulation;
}POPULATION;

////////////////////////////////////////////////
typedef struct
{
   char *name;
   double *ratios;
   double *categoryDistances;
}RECORD;

////////////////////////////////////////////////
typedef struct
{
   PERSON *p;
   int evolutionNum;
}BEST_CANDIDATE;

////////////////////////////////////////////////
typedef struct
{
   gsl_matrix *diff;
   gsl_matrix *result;
   gsl_matrix *result2;
}TMP;

/*prototypes */
TMP* getTemp(void);
void printPerson(PERSON *p);
ENVIRONMENT* setEnvironment(void);
PERSON* initializePerson(void);
RECORD **getRecords(int nCategories, int *nRecords);
double getMahalanobisDistance(double *dns, gsl_vector *means, gsl_matrix *inv, TMP *t);
CLASS* categorize(CLASS **categories, RECORD *record, PERSON *p, int nCategories, TMP *t);
void score(PERSON *p, CLASS **categories, RECORD **records, int nCategories, int nRecords, TMP *t);
void setRoulette(POPULATION *pop);
PERSON* getBestInPopulation(POPULATION *pop);
POPULATION* initializePopulation(CLASS **categories, RECORD **records, int nCategories, int nRecords, TMP *t);
PERSON *pick(POPULATION *pop);
void setChildren(PERSON *parent1, PERSON *parent2, PERSON *child1, PERSON *child2, ENVIRONMENT *env);
void copyPerson(PERSON *dest, PERSON *src);
void evolve(POPULATION *oldPopulation, POPULATION *newPopulation, BEST_CANDIDATE *best, CLASS **categories, RECORD **records, ENVIRONMENT *env, int nCategories, int nRecords, TMP *t);
void printPopulation(POPULATION *pop, int evolutions);
void printBest(BEST_CANDIDATE *best, int nRecords);
CLASS *findCategoryByName(CLASS **cats, char* name, int nCategories);
int isInIDs(char **ids, char *id, int nIDs);
void getUniqueIDs(RECORD **records, char** ids, int *nIDs, int nRecords);
int countOnes(PERSON *p);
void outIBIS(BEST_CANDIDATE *best, CLASS **categories, RECORD **records, int nCategories, int nRecords);
void outCovIBIS(BEST_CANDIDATE *best, CLASS **categories, RECORD **records, int nCategories, int nRecords);



/**********************************************/
TMP* getTemp(void)
{
   TMP *t = (TMP*)malloc(sizeof(TMP));

   t->diff = gsl_matrix_alloc(1, 28);
   t->result = gsl_matrix_alloc(1, 28);
   t->result2 = gsl_matrix_alloc(1, 1);

   return t;
}

/**********************************************/
void printPerson(PERSON *p)
{
   int i;
    char message[100];

   zvmessage("gene: "," ");
   for(i = 0; i < CHROMOSOME_LENGTH; i++) {
      sprintf(message,"%d ", p->gene[i]);
      zvmessage(message," ");
    }
   zvmessage("\n"," ");
   sprintf(message,"score: %lf\n", p->score);
    zvmessage(message," ");
   sprintf(message,"num of correct: %d\n", p->nCorrect);
    zvmessage(message," ");
}

/**********************************************/
ENVIRONMENT* setEnvironment(void)
{
   int status, dumcnt, dumdef;
   ENVIRONMENT *env;

   env = (ENVIRONMENT*)malloc(sizeof(ENVIRONMENT));

   status = zvparmd("mutation", &(env->mutationRate), &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("!!!Error while acquiring mutation parameter!!!");

   status = zvparmd("crossover", &(env->crossoverRate), &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("!!!Error while acquiring crossover parameter!!!");

   status = zvparm("evolutions", &(env->nEvolutions), &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("!!!Error while acquiring evolutions parameter!!!");

   return env;
}

/**********************************************/
PERSON* initializePerson(void)
{
   int i;
   PERSON *p;

   p = (PERSON*)malloc(sizeof(PERSON));
   for(i = 0; i < CHROMOSOME_LENGTH; i++) p->gene[i] = (char)rand() % 2;

   return p;
}

/**********************************************/
RECORD **getRecords(int nCategories, int *nRecords)
{
   int i, isStringID;
   RECORD **records;
   IBISStruct *ibis;

   ibis = IBISHELPER_openIBIS("inp", 2, "read");
   //   printf("ibis format: %s isStringID: %d\n", (ibis->formats)[0], isStringID);
   if(strcmp((ibis->formats)[0], "real") == 0) isStringID = 0;
   else isStringID = 1;

   records = (RECORD**)malloc(sizeof(RECORD*)*(long unsigned int)ibis->nr);
   *nRecords = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      int j;
      char tmp[20];

      records[i] = (RECORD*)malloc(sizeof(RECORD));
      records[i]->categoryDistances = (double*)malloc(sizeof(double)*(long unsigned int)nCategories);
      records[i]->ratios = (double*)calloc(CHROMOSOME_LENGTH, sizeof(double));
      records[i]->name = (char*)malloc(sizeof(char)*20);

      if(isStringID)
         IBISHELPER_getString(ibis, tmp, 0, i);
      else
      {
         int id = (int)(IBISHELPER_getDouble(ibis, 0, i));
         sprintf(tmp, "CLASS %d", id);
      }
      strcpy(records[i]->name, tmp);
      for(j = 1; j < ibis->nc; j++)
         records[i]->ratios[j-1] = IBISHELPER_getDouble(ibis, j, i);

      (*nRecords)++;
   }

   //   printf("before close\n");
   IBISHELPER_closeIBIS(&ibis);
   return records;
}

/**********************************************/
double getMahalanobisDistance(double *dns, gsl_vector *means, gsl_matrix *inv, TMP *t)
{
   int i;
   double distance;

   for(i = 0; i < 28; i++) gsl_matrix_set(t->diff, 0, (size_t)i, dns[i] - gsl_vector_get(means, (size_t)i));

   gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1, t->diff, inv, 0, t->result);
   gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1, t->result, t->diff, 0, t->result2);

   /* This is where the application and theory diverge... in theory the result should
   be positive since the covariance matrix is positive semi-definite.  However,
   due to the fact that some variables in the covariance matrix may be linearly
   independent, we sometimes get a negative distance.  This is a quick fix... may
   not be the best but don't know a better way without disregarding the mahalanobis
   distance altogether. */
   distance = sqrt(fabs(gsl_matrix_get(t->result2, 0, 0)));

   return distance;
}

/**********************************************/
CLASS* categorize(CLASS **categories, RECORD *record, PERSON *p, int nCategories, TMP *t)
{
   int i, j, minIndex;

   minIndex = 0;

   //   printf("nCategories: %d\n", nCategories);
   for(i = 0; i < nCategories; i++)
   {
      record->categoryDistances[i] = 0;
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
         if(p->gene[j])
         {
            record->categoryDistances[i] += fabs(record->ratios[j]-gsl_vector_get(categories[i]->means, (size_t)j))/sqrt(gsl_matrix_get(categories[i]->cov_matrix, (size_t)j, (size_t)j));
            //            printf("id: %s j: %d cov: %lf\n", categories[i]->name, j, gsl_matrix_get(categories[i]->cov_matrix, j, j));
         }

      //      printf("minIndex: %d index: %d distance: %lf\n", minIndex, i, record->categoryDistances[i]);
      if(record->categoryDistances[i] < record->categoryDistances[minIndex]) minIndex = i;
   }

   return categories[minIndex];
}

/**********************************************/
void score(PERSON *p, CLASS **categories, RECORD **records, int nCategories, int nRecords, TMP *t)
{
   int i, oneCnt;
   double score;

   oneCnt = 0;
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      if(p->gene[i]) ++oneCnt;
   if(oneCnt == 0)
   {
      p->score = 0;
      return;
   }

   score = 0.;
   for(i = 0; i < nRecords; i++)
   {
      CLASS *category;

      category = categorize(categories, records[i], p, nCategories, t);
      //      printf("category name: %s record name: %s\n", category->name, records[i]->name);
      if(strcmp(category->name, records[i]->name) == 0) score = score + 1;
   }

   p->nCorrect = (int)score;
   p->score = pow(score, 1.5);
}

/**********************************************/
void setRoulette(POPULATION *pop)
{
   int i;
   double sum;

   sum = 0;
   for(i = 0; i < pop->size; i++)
      sum += pop->people[i]->score;
   if(sum < 10E-10) zmabend("No correct classifications.  Check input files for accuracy.\n");

   pop->roulette[0] = 0.;
   for(i = 0; i < pop->size; i++)
   {
      if(fabs(sum) > 10E-10) pop->roulette[i+1] = pop->roulette[i] + pop->people[i]->score/sum;
      else pop->roulette[i+1] = pop->roulette[i] + 1/(double)(pop->size);
   }

   // sanity check, make sure the last element of the roulette wheel
   // is 1.
   assert(fabs(pop->roulette[pop->size] - 1.) < 10E-10);
}

/**********************************************/
PERSON* getBestInPopulation(POPULATION *pop)
{
   int i;
   PERSON *best;

   best = pop->people[0];
   for(i = 1; i < pop->size; i++)
      if(pop->people[i]->score > best->score) best = pop->people[i];

   return best;
}

/**********************************************/
POPULATION* initializePopulation(CLASS **categories, RECORD **records, int nCategories, int nRecords, TMP *t)
{
   int i, status, dumcnt, dumdef;
   POPULATION *pop;
    char message[100];

   pop = (POPULATION*)malloc(sizeof(POPULATION));

   status = zvparmd("population", &(pop->size), &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("!!!Error while acquiring population size parameter!!!");
   if(pop->size%2)
   {
      sprintf(message,"Population size must be even.  Adding 1 to population.\n");
      zvmessage(message," ");
      pop->size++;
   }

   pop->people = (PERSON**)malloc(sizeof(PERSON*)*(long unsigned int)pop->size);
   for(i = 0; i < pop->size; i++)
   {
      pop->people[i] = initializePerson();
      //      printf("herea\n");
      score(pop->people[i], categories, records, nCategories, nRecords, t);
      //      printf("hereb\n");
   }
   pop->roulette = (double*)calloc((size_t)pop->size + 1, sizeof(double));
   setRoulette(pop);
   pop->evolutionCnt = 0;
   pop->bestInThisPopulation = getBestInPopulation(pop);

   return pop;
}

/**********************************************/
PERSON *pick(POPULATION *pop)
{
   int i;
   double rand_pick = rand()/(double)RAND_MAX;

   for(i = 0; i < pop->size; i++)
      if(rand_pick >= pop->roulette[i] && rand_pick < pop->roulette[i+1])
         return pop->people[i];

   assert(0);
   return NULL;
}

/**********************************************/
void setChildren(PERSON *parent1, PERSON *parent2, PERSON *child1, PERSON *child2, ENVIRONMENT *env)
{
   int swapPosition, i, j;
   double rand_num = rand()/(double)RAND_MAX;

   //   printf("setchildren 1\n");
   if(rand_num < env->crossoverRate)
   {
      swapPosition = (rand() % (CHROMOSOME_LENGTH-2))+1;

      //      printf("setchildren 1 a\n");
      //printPerson(child1);
      //      printf("setchildren 2 a %x\n", child2);
      //printPerson(child2);
      for(j = 0; j < swapPosition; j++)
      {
         child1->gene[j] = parent1->gene[j];
         child2->gene[j] = parent2->gene[j];
      }
      for(j = swapPosition; j < CHROMOSOME_LENGTH; j++)
      {
         child1->gene[j] = parent2->gene[j];
         child2->gene[j] = parent1->gene[j];
      }
   }
   else
   {
      //      printf("setchildren 1 b\n");
      //      printPerson(child1);
      //      printf("setchildren 2 b %x\n", child2);
      //      printPerson(child2);
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
      {
         child1->gene[j] = parent1->gene[j];
         child2->gene[j] = parent2->gene[j];
      }
   }
   //   printf("setchildren 2\n");
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
   {
//      int rand_int;

      rand_num = rand()/(double)RAND_MAX;
      if(rand_num < env->mutationRate && !child1->gene[i])
         child1->gene[i] = 1;
      else if(rand_num < env->mutationRate && child1->gene[i])
         child1->gene[i] = 0;

      rand_num = rand()/(double)RAND_MAX;
      if(rand_num < env->mutationRate && !child2->gene[i])
         child2->gene[i] = 1;
      else if(rand_num < env->mutationRate && child2->gene[i])
         child2->gene[i] = 0;
   }
   //   printf("setchildren 3\n");
}

/**********************************************/
void copyPerson(PERSON *dest, PERSON *src)
{
   int i;

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      dest->gene[i] = src->gene[i];

   dest->score = src->score;
   dest->nCorrect = src->nCorrect;
}

/**********************************************/
void evolve(POPULATION *oldPopulation, POPULATION *newPopulation, BEST_CANDIDATE *best, CLASS **categories, RECORD **records, ENVIRONMENT *env, int nCategories, int nRecords, TMP *t)
{
   int i, minIndex;

   newPopulation->size = oldPopulation->size;
   newPopulation->evolutionCnt = oldPopulation->evolutionCnt + 1;

   //   printf("evolve 1\n");
   for(i = 0; i < newPopulation->size; i += 2)
   {
      PERSON *parent1, *parent2;

      //      printf("1evolve loop %d\n", i);
      parent1 = pick(oldPopulation);
      //      printf("2evolve loop %d\n", i);
      parent2 = pick(oldPopulation);
      //      printf("3evolve loop %d\n", i);
      //      printPerson(parent1);
      //      printPerson(parent2);

      //      printf("i: %d\n", i);
      setChildren(parent1, parent2, newPopulation->people[i], newPopulation->people[i+1], env);
      //      printf("4evolve loop %d\n", i);
      score(newPopulation->people[i], categories, records, nCategories, nRecords, t);
      //      printf("5evolve loop %d\n", i);
      score(newPopulation->people[i + 1], categories, records, nCategories, nRecords, t);
      //      printf("6evolve loop %d\n", i);
   }
   //   printf("evolve 2\n");
   // replace the least fit with the best candidate so far
   minIndex = 0;
   for(i = 1; i < newPopulation->size; i++)
      if(newPopulation->people[i]->score < newPopulation->people[minIndex]->score) minIndex = i;
   copyPerson(newPopulation->people[minIndex], best->p);

   setRoulette(newPopulation);
   newPopulation->evolutionCnt = oldPopulation->evolutionCnt + 1;
   newPopulation->bestInThisPopulation = getBestInPopulation(oldPopulation);
}

/**********************************************/
void printPopulation(POPULATION *pop, int evolutions)
{
    char message[100];

   sprintf(message,"\n----------> evolution: %d/%d population: %d\n", pop->evolutionCnt, evolutions, pop->size);

   zvmessage(message," ");
   zvmessage("========================\n"," ");
   zvmessage("Best in this Population:\n"," ");
   zvmessage("========================\n"," ");

   printPerson(pop->bestInThisPopulation);
}

/**********************************************/
void printBest(BEST_CANDIDATE *best, int nRecords)
{
   char message[100];

   zvmessage("=====> THE BEST <=====\n"," ");
   printPerson(best->p);
   sprintf(message,"evolution number: %d\n", best->evolutionNum);
    zvmessage(message," ");
}

/**********************************************/
CLASS *findCategoryByName(CLASS **cats, char* name, int nCategories)
{
   int i;
    char message[100];
   for(i = 0; i < nCategories; i++)
      if(strcmp(cats[i]->name, name) == 0) return cats[i];

   // code should not reach here
   // if code reaches here then the id
   // was not in one of the categories
   zvmessage ("code should not reach here\n"," ");
    sprintf (message,"if it does then id %s was not in one of the categories\n",name);
    zvmessage (message," ");
    zmabend ("assert(0)");
   assert(0);
   return cats[0];
}

/**********************************************/
int isInIDs(char **ids, char *id, int nIDs)
{ 
   int i;

   for(i = 0; i < nIDs; i++)
      if(!strcmp(ids[i], id)) return 1;

   return 0;
}

/**********************************************/
void getUniqueIDs(RECORD **records, char** ids, int *nIDs, int nRecords)
{
   int i;

   *nIDs = 0;
   for(i = 0; i < nRecords; i++)
   {
      char* id;

      id = records[i]->name;
      if(!isInIDs(ids, id, *nIDs))
      {
         ids[*nIDs] = id;
         (*nIDs)++;
      }
   }
}

/**********************************************/
int countOnes(PERSON *p)
{
   int i, cnt;

   cnt = 0;
   for(i = 0; i < CHROMOSOME_LENGTH; i++) if(p->gene[i]) cnt++;

   return cnt;
}

/**********************************************/
void outIBIS(BEST_CANDIDATE *best, CLASS **categories, RECORD **records, int nCategories, int nRecords)
{
   int oneCnt, i, j, k, indexCnt, rowCnt, nCols, nIDs;
   char **ids;
   IBISPrep *prep;
   IBISStruct *ibis;
   CLASS *category;
//   double *sigma;

   oneCnt = countOnes(best->p);
   ids = (char**)malloc(sizeof(char*)*(long unsigned int)nRecords);
   for(i = 0; i < nRecords; i++) ids[i] = (char*)malloc(sizeof(char)*20);
   getUniqueIDs(records, ids, &nIDs, nRecords);

   //   printf("nIDs: %d\n", nIDs);
   // 2 for band ratios and 3 for each id
   nCols = 2 + nIDs*3;

   prep = IBISHELPER_openIBIS_out2("out", 1, oneCnt);
   for(i = 0; i < 2; i++)
   {
     //      printf("col: %d\n", i);
      IBISHELPER_addColumn(prep, "FULL");
   }
   //   printf("col: 2\n");
   IBISHELPER_addColumn(prep, "DOUB");
   for(i = 3; i < nCols; i++)
   {
     //      printf("col: %d\n", i);
      IBISHELPER_addColumn(prep, "DOUB");
   }
   ibis = IBISHELPER_getIBISStruct(&prep);

   indexCnt = rowCnt = 0;
   for(i = 0; i < 8; i++)
   {
      for(j = i+1; j < 8; j++)
      {
         if(best->p->gene[indexCnt])
         {
            IBISHELPER_setDouble(ibis, 0, rowCnt, (double)(i+1));
            IBISHELPER_setDouble(ibis, 1, rowCnt, (double)(j+1));
            for(k = 0; k < nIDs; k++)
            {
               int numID;

               category = findCategoryByName(categories, ids[k], nCategories);
               sscanf(ids[k], "CLASS %d", &numID);

               //               IBISHELPER_setString(ibis, k*3+2, rowCnt, ids[k]);
               IBISHELPER_setDouble(ibis, k*3+2, rowCnt, (double)numID);
               IBISHELPER_setDouble(ibis, k*3+3, rowCnt, (double)(gsl_vector_get(category->means, (size_t)indexCnt)));
               IBISHELPER_setDouble(ibis, k*3+4, rowCnt, (double)(sqrt(gsl_matrix_get(category->cov_matrix, (size_t)indexCnt, (size_t)indexCnt))));
            }

            ++rowCnt;
         }

         ++indexCnt;
      }
   }

   IBISHELPER_closeIBIS(&ibis);
}

/**********************************************/
void outCovIBIS(BEST_CANDIDATE *best, CLASS **categories, RECORD **records, int nCategories, int nRecords)
{
   int oneCnt, i, j, k, nCols, colIndex, nIDs;
   IBISStruct *ibis;
   IBISPrep *prep;
   char **ids;
   CLASS *category;

   ids = (char**)malloc(sizeof(char*)*(long unsigned int)nRecords);
   for(i = 0; i < nRecords; i++) ids[i] = (char*)malloc(sizeof(char)*20);
   getUniqueIDs(records, ids, &nIDs, nRecords);

   oneCnt = countOnes(best->p);
   nCols = oneCnt*(oneCnt + 1)/2 + 1;
   prep = IBISHELPER_openIBIS_out2("out", 2, nIDs);
   //   printf("col: 0\n");
   IBISHELPER_addColumn(prep, "A12");
   for(i = 1; i < nCols; i++)
   {
      //      printf("col: %d\n", i);
      IBISHELPER_addColumn(prep, "REAL");
   }
   ibis = IBISHELPER_getIBISStruct(&prep);
   for(i = 0; i < nIDs; i++)
   {
      category = findCategoryByName(categories, ids[i], nCategories);
      IBISHELPER_setString(ibis, 0, i, category->name);

      colIndex = 1;
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
      {
         for(k = 0; k <= j; k++)
         {
            if(best->p->gene[j] && best->p->gene[k])
               IBISHELPER_setDouble(ibis, colIndex++, i, gsl_matrix_get(category->cov_matrix_inv, (size_t)j, (size_t)k));
         }
      }

      assert(colIndex - 1 == oneCnt*(oneCnt + 1)/2);
   }

   IBISHELPER_closeIBIS(&ibis);
}

/**********************************************/
void main44(void)
{
   int i, status, evolutions, dumcnt, nCategories, nRecords, outCnt;
   POPULATION *oldPopulation, *newPopulation, *tmp;
   //   CATEGORY **categories;
   RECORD **records;
   ENVIRONMENT *env;
   BEST_CANDIDATE *best;
   TMP *tmp2;
   CLASS **categories;
   IBISStruct *ibis;

    zvmessage("genealg2 - 10-14-2019 - rjb - 64-bit"," ");
   ibis = IBISHELPER_openIBIS("inp", 1, "read");
   //   for(i = 0; i < 10; i++) printf("%d: %s\n", i, (ibis->formats)[i]);
   categories = CARTOCLASS_loadClassesFromIBIS(ibis);
   nCategories = ibis->nr;

   /*
   for(i = 0; i < ibis->nr; i++)
   {
      CARTOCLASS_printClass(categories[i]);
      printf("\n");
   }
   */
   records = getRecords(nCategories, &nRecords);

    tmp2=0;
   srand(0);
    tmp2 = 0;
   oldPopulation = initializePopulation(categories, records, nCategories, nRecords, tmp2);
   newPopulation = initializePopulation(categories, records, nCategories, nRecords, tmp2);
   best = (BEST_CANDIDATE*)malloc(sizeof(BEST_CANDIDATE));
   best->p = (PERSON*)malloc(sizeof(PERSON));
   copyPerson(best->p, oldPopulation->bestInThisPopulation);
   best->evolutionNum = 0;
   env = setEnvironment();

   //   printf("main here 1\n");
   status = zvp("evolutions", &evolutions, &dumcnt);
   if(status != 1) zmabend("!!! Error while acquiring evolutions parameter !!!\n");
   //   printf("main here 2\n");
   for(i = 0; i < evolutions; i++)
   {
      //      printf("main loop %d\n", i);
      evolve(oldPopulation, newPopulation, best, categories, records, env, nCategories, nRecords, tmp2);
      //      printf("main loop %d\n", i);
      if(best->p->score < newPopulation->bestInThisPopulation->score)
      {
//         int j;

         copyPerson(best->p, newPopulation->bestInThisPopulation);
         best->evolutionNum = i+1;
      }

      printPopulation(newPopulation, evolutions);
      printBest(best, nRecords);
      if(best->p->nCorrect == nRecords) break;
      tmp = newPopulation;
      newPopulation = oldPopulation;
      oldPopulation = tmp;
   }

   //   printf("outIBIS\n");
   outIBIS(best, categories, records, nCategories, nRecords);
   //   printf("outCovIBIS\n");
   status = zvpcnt("out", &outCnt);
   if(outCnt == 2)
      outCovIBIS(best, categories, records, nCategories, nRecords);
}
