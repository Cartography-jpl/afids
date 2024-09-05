#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "carto/ibishelper.h"
#include "carto/ImageUtils.h"

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
   int id;
   double *mean;
   double *sigma;
}CATEGORY;

////////////////////////////////////////////////
typedef struct
{
   int id;
   double *ratios;
   double *categoryDistances;
}RECORD;

////////////////////////////////////////////////
typedef struct
{
   PERSON *p;
   int evolutionNum;
}BEST_CANDIDATE;

/* prototypes */
void printPerson(PERSON *p);
ENVIRONMENT* setEnvironment(void);
PERSON* initializePerson(void);
CATEGORY **getCategories(int *nCategories);
RECORD **getRecords(int nCategories, int *nRecords);
CATEGORY* categorize(CATEGORY **categories, RECORD *record, PERSON *p, int nCategories);
void score(PERSON *p, CATEGORY **categories, RECORD **records, int nCategories, int nRecords);
void setRoulette(POPULATION *pop);
PERSON* getBestInPopulation(POPULATION *pop);
POPULATION* initializePopulation(CATEGORY **categories, RECORD **records, int nCategories, int nRecords);
PERSON *pick(POPULATION *pop);
void setChildren(PERSON *parent1, PERSON *parent2, PERSON *child1, PERSON *child2, ENVIRONMENT *env);
void copyPerson(PERSON *dest, PERSON *src);
void evolve(POPULATION *oldPopulation, POPULATION *newPopulation, BEST_CANDIDATE *best, CATEGORY **categories, RECORD **records, ENVIRONMENT *env, int nCategories, int nRecords);
void printPopulation(POPULATION *pop, int evolutions);
void printBest(BEST_CANDIDATE *best);
CATEGORY *findCategoryByID(CATEGORY **cats, int id, int nCategories);
int isInIDs(int *ids, int id, int nIDs);
void getIDs(RECORD **records, int* ids, int *nIDs, int nRecords);
void outIBIS(BEST_CANDIDATE *best, CATEGORY **categories, RECORD **records, int nCategories, int nRecords);


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
CATEGORY **getCategories(int *nCategories)
{
   int i;
   CATEGORY **categories;
   IBISStruct *ibis;

   ibis = IBISHELPER_openIBIS("inp", 1, "read");
   categories = (CATEGORY**)malloc(sizeof(CATEGORY*)*(long unsigned int)ibis->nr);
   *nCategories = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      int j, ratioIndex;

      categories[i] = (CATEGORY*)malloc(sizeof(CATEGORY));
      categories[i]->mean = (double*)calloc(CHROMOSOME_LENGTH, sizeof(double));
      categories[i]->sigma = (double*)calloc(CHROMOSOME_LENGTH, sizeof(double));
      categories[i]->id = (int)IBISHELPER_getDouble(ibis, 0, i);
      ratioIndex = 0;
      for(j = 1; j < ibis->nc; j++)
      {
         if(j % 2)
            categories[i]->mean[ratioIndex] = IBISHELPER_getDouble(ibis, j, i);
         else
         {
            categories[i]->sigma[ratioIndex] = IBISHELPER_getDouble(ibis, j, i);
            ratioIndex++;
         }
      }

      (*nCategories)++;
   }

   IBISHELPER_closeIBIS(&ibis);
   return categories;
}

/**********************************************/
RECORD **getRecords(int nCategories, int *nRecords)
{
   int i;
   RECORD **records;
   IBISStruct *ibis;
    char message[100];

   ibis = IBISHELPER_openIBIS("inp", 2, "read");
   records = (RECORD**)malloc(sizeof(RECORD*)*(long unsigned int)ibis->nr);
   *nRecords = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      int j;

      //      printf("here %d\n", i);
      records[i] = (RECORD*)malloc(sizeof(RECORD));
      records[i]->categoryDistances = (double*)malloc(sizeof(double)*(long unsigned int)nCategories);
      records[i]->ratios = (double*)calloc(CHROMOSOME_LENGTH, sizeof(double));
      records[i]->id = (int)IBISHELPER_getDouble(ibis, 0, i);
      for(j = 1; j < ibis->nc; j++)
         records[i]->ratios[j-1] = IBISHELPER_getDouble(ibis, j, i);

      (*nRecords)++;
   }

   zvmessage("before close\n"," ");
   IBISHELPER_closeIBIS(&ibis);
   return records;
}

/**********************************************/
CATEGORY* categorize(CATEGORY **categories, RECORD *record, PERSON *p, int nCategories)
{
   int i, minIndex;

   //   printPerson(p);
   minIndex = 0;
   for(i = 0; i < nCategories; i++)
   {
      int j;

      record->categoryDistances[i] = 0.;
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
      {
         if(p->gene[j]) record->categoryDistances[i] += fabs(record->ratios[j]-categories[i]->mean[j])/categories[i]->sigma[j];
         //         printf("gene on/off: %d dist: %lf\n", p->gene[j], record->categoryDistances[i]);
      }

      // printf("i: %d dist: %lf minIndex: %d dist:%lf\n", i, record->categoryDistances[i], minIndex, record->categoryDistances[minIndex]);
      if(record->categoryDistances[i] < record->categoryDistances[minIndex]) minIndex = i;
   }

   return categories[minIndex];
}

/**********************************************/
void score(PERSON *p, CATEGORY **categories, RECORD **records, int nCategories, int nRecords)
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
      CATEGORY *category;

      category = categorize(categories, records[i], p, nCategories);
      if(category->id == records[i]->id) score = score + 1;
      //      printf("category id: %d record id: %d score %lf\n", category->id, records[i]->id, score);
   }

   p->nCorrect = (int)score;
   p->score = pow(score, 1.5);
}

/**********************************************/
void setRoulette(POPULATION *pop)
{
   int i;
   double sum;
    char message[100];

   sum = 0;
   for(i = 0; i < pop->size; i++)
   {
      sum += pop->people[i]->score;
      //      printf("sum: %lf\n", sum);
   }

   pop->roulette[0] = 0.;
   for(i = 0; i < pop->size; i++)
   {
      if(fabs(sum) > 10E-10) pop->roulette[i+1] = pop->roulette[i] + pop->people[i]->score/sum;
      else pop->roulette[i+1] = pop->roulette[i] + 1/(double)(pop->size);
      //      printf("pop->roulette[i]: %lf\n", pop->roulette[i]);
      //      printf("pop->people[i]->score: %lf\n", pop->people[i]->score);
      //      printf("%lf %lf\n", pop->roulette[i], pop->roulette[i+1]);
   }

   // sanity check, make sure the last element of the roulette wheel
   // is 1.
   sprintf(message,"sum: %lf\n", fabs(pop->roulette[pop->size]));
    zvmessage(message," ");
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
POPULATION* initializePopulation(CATEGORY **categories, RECORD **records, int nCategories, int nRecords)
{
   int i, status, dumcnt, dumdef;
   POPULATION *pop;

   pop = (POPULATION*)malloc(sizeof(POPULATION));

   status = zvparmd("population", &(pop->size), &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("!!!Error while acquiring population size parameter!!!");

   pop->people = (PERSON**)malloc(sizeof(PERSON*)*(long unsigned int)pop->size);
   for(i = 0; i < pop->size; i++)
   {
      pop->people[i] = initializePerson();
      //      printf("herea\n");
      score(pop->people[i], categories, records, nCategories, nRecords);
      //      printf("hereb\n");
   }
   pop->roulette = (double*)calloc((long unsigned int)pop->size + 1, sizeof(double));
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

   if(rand_num < env->crossoverRate)
   {
      swapPosition = (rand() % (CHROMOSOME_LENGTH-2))+1;

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
      for(j = 0; j < CHROMOSOME_LENGTH; j++)
      {
         child1->gene[j] = parent1->gene[j];
         child2->gene[j] = parent2->gene[j];
      }
   }

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
void evolve(POPULATION *oldPopulation, POPULATION *newPopulation, BEST_CANDIDATE *best, CATEGORY **categories, RECORD **records, ENVIRONMENT *env, int nCategories, int nRecords)
{
   int i, minIndex;

   newPopulation->size = oldPopulation->size;
   newPopulation->evolutionCnt = oldPopulation->evolutionCnt + 1;

   for(i = 0; i < newPopulation->size; i += 2)
   {
      PERSON *parent1, *parent2;

      parent1 = pick(oldPopulation);
      parent2 = pick(oldPopulation);

      setChildren(parent1, parent2, newPopulation->people[i], newPopulation->people[i+1], env);
      score(newPopulation->people[i], categories, records, nCategories, nRecords);
      score(newPopulation->people[i + 1], categories, records, nCategories, nRecords);
   }

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
void printBest(BEST_CANDIDATE *best)
{
    char message[100];

   zvmessage("=====> THE BEST <=====\n"," ");
   printPerson(best->p);
   sprintf(message,"evolution number: %d\n", best->evolutionNum);
    zvmessage(message," ");
}

/**********************************************/
CATEGORY *findCategoryByID(CATEGORY **cats, int id, int nCategories)
{
   int i;
    char message[100];

   for(i = 0; i < nCategories; i++)
      if(cats[i]->id == id) return cats[i];

   // code should not reach here
   // if code reaches here then the id
   // was not in one of the categories
   zvmessage ("code should not reach here\n"," ");
    sprintf (message,"if it does then id %d was not in one of the categories\n",id);
    zvmessage (message," ");
    zmabend ("assert(0)");
   assert(0);
   return cats[0];
}

/**********************************************/
int isInIDs(int *ids, int id, int nIDs)
{ 
   int i;

   for(i = 0; i < nIDs; i++)
      if(ids[i] == id) return 1;

   return 0;
}

/**********************************************/
void getIDs(RECORD **records, int* ids, int *nIDs, int nRecords)
{
   int i;

   *nIDs = 0;
   for(i = 0; i < nRecords; i++)
   {
      int id = records[i]->id;

      if(!isInIDs(ids, id, *nIDs))
      {
         ids[*nIDs] = id;
         (*nIDs)++;
      }
   }
}

/**********************************************/
void outIBIS(BEST_CANDIDATE *best, CATEGORY **categories, RECORD **records, int nCategories, int nRecords)
{
   int oneCnt, i, j, k, indexCnt, rowCnt, nCols, *ids, nIDs;
   IBISStruct *ibis;
//   CATEGORY *category;
   char **outFormat;

   oneCnt = 0;
   for(i = 0; i < CHROMOSOME_LENGTH; i++) if(best->p->gene[i]) oneCnt++;

   ids = (int*)malloc(sizeof(int)*(long unsigned int)nRecords);
   getIDs(records, ids, &nIDs, nRecords);

   // 2 for band ratios and 3 for each id
   nCols = 2 + nIDs*3;

   outFormat = (char**)malloc(sizeof(char*)*(long unsigned int)nCols);
   for(i = 0; i < nCols; i++) outFormat[i] = (char*)malloc(sizeof(char)*6);
   for(i = 0; i < 2; i++) strncpy(outFormat[i], "FULL", 6);
   for(i = 2; i < nCols; i++) strncpy(outFormat[i], "DOUB", 6);

   ibis = IBISHELPER_openIBIS_out(outFormat, 1, oneCnt, 2 + nIDs*3);
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
               CATEGORY *category = findCategoryByID(categories, ids[k], nCategories);

               IBISHELPER_setDouble(ibis, k*3+2, rowCnt, (double)(ids[k]));
               IBISHELPER_setDouble(ibis, k*3+3, rowCnt, (double)(category->mean[indexCnt]));
               IBISHELPER_setDouble(ibis, k*3+4, rowCnt, (double)(category->sigma[indexCnt]));
            }

            ++rowCnt;
         }

         ++indexCnt;
      }
   }

   IBISHELPER_closeIBIS(&ibis);
}

/**********************************************/
void main44(void)
{
   int i, status, evolutions, dumcnt, nCategories, nRecords;
   POPULATION *oldPopulation, *newPopulation, *tmp;
   CATEGORY **categories;
   RECORD **records;
   ENVIRONMENT *env;
   BEST_CANDIDATE *best;

    zvmessage("genealg - 10-14-2019 - rjb - 64-bit"," ");
   categories = getCategories(&nCategories);

   for(i = 0; i < nCategories; i++)
   {
   //   int j;

      //      printf("id: %d\n", categories[i]->id);
      //      for(j = 0; j < CHROMOSOME_LENGTH; j++) printf("mean: %lf sig: %lf\n", categories[i]->mean[j], categories[i]->sigma[j]);
   }
   //   printf("here1\n");
   records = getRecords(nCategories, &nRecords);

   //   printf("here1\n");
   srand(0);
   oldPopulation = initializePopulation(categories, records, nCategories, nRecords);
   newPopulation = initializePopulation(categories, records, nCategories, nRecords);
   best = (BEST_CANDIDATE*)malloc(sizeof(BEST_CANDIDATE));
   best->p = (PERSON*)malloc(sizeof(PERSON));
   copyPerson(best->p, oldPopulation->bestInThisPopulation);
   best->evolutionNum = 0;
   env = setEnvironment();
   //   printf("here2\n");

   status = zvp("evolutions", &evolutions, &dumcnt);
   if(status != 1) zmabend("!!! Error while acquiring evolutions parameter !!!\n");

   for(i = 0; i < evolutions; i++)
   {
      // count of the number of 1's in the chromosome
     //      oneCnt = 1;
      evolve(oldPopulation, newPopulation, best, categories, records, env, nCategories, nRecords);
      if(best->p->score < newPopulation->bestInThisPopulation->score)
      {
//         int j;

         copyPerson(best->p, newPopulation->bestInThisPopulation);
         best->evolutionNum = i+1;
         //         oneCnt = 0;
         //         for(j = 0; j < CHROMOSOME_LENGTH; j++) if(best->p->gene[j]) oneCnt++;
      }
      printPopulation(newPopulation, evolutions);
      printBest(best);
      tmp = newPopulation;
      newPopulation = oldPopulation;
      oldPopulation = tmp;
      //      if(oneCnt == 0) break;

      //      break;
   }

   outIBIS(best, categories, records, nCategories, nRecords);
}
