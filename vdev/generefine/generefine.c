#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include "cartoSortUtils.h"

#define CHROMOSOME_LENGTH 30
#define N_POPULATION      100
#define N_EVOLUTIONS      100
#define CROSSOVER_RATE    0.7
#define MUTATION_RATE     0.1

typedef struct
{
   double a, b, c, d, e, f;
   char chromosome[CHROMOSOME_LENGTH];
   double fitness;
}PERSON;

typedef struct
{
   double x, y;
}POINT;

/* prototypes */
void debugprintfile(char img[][1920]);
void getRouletteWheel(PERSON **population, double *roulette_wheel);
double subdecode1(PERSON *p, int i);
double subdecode2(PERSON *p, int i);
void decode(PERSON *p);
void displayPerson(PERSON *p);
double searchClosestPt(int **master, POINT *pt, int cnt, double yp, double xp);
void findCentroidPt(int **master, POINT *cent, int cnt);
void printPoint(POINT* p);
double distance(double x1, double y1, double x2, double y2);
void calculateDistances(POINT *pt, int **master, double* distances, int cnt);
void getMasterDistances(int **master, POINT* p[5], int **sortedIndices,
     double **distances, int cnt, int nl, int ns);
int binaryRange(double *distances, int *sortedIndices, double dist, 
     int mastercnt);
double searchLimitedClosestPt(int **master, int **sortedIndices, 
     POINT *controlPts[5],double **distances, int mastercnt, double yp,
     double xp,int nl, int ns);
double calcFitness(int **master, int **img, PERSON *p, POINT *controlPts[5],
     double **distances, int **sortedIndices, int mastercnt, int imgcnt,
     int nl, int ns);
double calcPopFitness(PERSON **pop, int **master, int **img, 
     POINT *controlPts[5],double **distances, int **sortedIndices,
     int mastercnt, int imgcnt, int nl, int ns);
void generateRandomPerson(PERSON *p);
void createRandomPopulation(PERSON **pop, int n);
void getImage(int **img, int unit);
int countPixels(int unit);
void printPopulation(PERSON **pop, int n);
int searchRoulette(double *roulette, double val);
void crossover(PERSON *p1, PERSON *p2);
void mutate(PERSON *p);
void copyPerson(PERSON *from, PERSON *to);
void copyPopulation(PERSON **from, PERSON **to);
void freePopulation(PERSON **pop);
void evolve(PERSON **pop);


/**************************************************/
void debugprintfile(char img[][1920])
{
   int unit, status, i;

   status = zvunit(&unit, "NONE", 2, "U_NAME", "debug.img", NULL);
   assert(status == 1);
   status = zvopen(unit, "op", "write", "u_nl", 1080, "u_ns",
                   1920, "u_format", "byte", "o_format", "byte", NULL);
   assert(status == 1);

   for(i = 0; i < 1080; i++)
   {
      status = zvwrit(unit, img[i], "LINE", i+1, NULL);
      assert(status == 1);
   }

   status = zvclose(unit, NULL);
   assert(status == 1);
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
double subdecode1(PERSON *p, int i)
{
   int j;
   double answer;

   j = i;
   answer = 0.0;
   for(; i < j + 4; i++)
      answer += p->chromosome[i]*pow(0.1, i-j);
   if(p->chromosome[i] < 5) answer *= -1;

   return answer;
}

/**************************************************/
double subdecode2(PERSON *p, int i)
{
   int j;
   double answer;

   j = i;
   answer = 0.0;
   for(; i < j + 4; i++)
      answer += p->chromosome[i]*pow(0.1, i-j);
   if(p->chromosome[i] < 5) answer *= -1;

   return answer;
}

/**************************************************/
void decode(PERSON *p)
{
   p->a = subdecode1(p, 0);
   p->b = subdecode1(p, 5);
   p->c = subdecode2(p, 10);
   p->d = subdecode1(p, 15);
   p->e = subdecode1(p, 20);
   p->f = subdecode2(p, 25);
}

/**************************************************/
void displayPerson(PERSON *p)
{
   int i;
    char message[100];

   decode(p);
   for(i = 0; i < CHROMOSOME_LENGTH; i++)
   {
      if(!(i % 5)) {
        zvmessage(" "," "); 
        sprintf(message,"%d", p->chromosome[i]);
        zvmessage(message," ");
      }
   }
   zvmessage(" "," ");
   sprintf(message,"a: %lf ", p->a);
    zvmessage(message," ");
   sprintf(message,"b: %lf ", p->b);
    zvmessage(message," ");
   sprintf(message,"c: %lf ", p->c);
    zvmessage(message," ");
   sprintf(message,"d: %lf ", p->d);
    zvmessage(message," ");
   sprintf(message,"e: %lf ", p->e);
    zvmessage(message," ");
   sprintf(message,"f: %lf ", p->f);
    zvmessage(message," ");
   sprintf(message,"fit: %.9lf\n", p->fitness);
    zvmessage(message," ");
}

/**************************************************/
double searchClosestPt(int **master, POINT *pt, int cnt, double yp, double xp)
{
   int i, index;
   double minDist;

    index= 0;
   minDist = -1;
   for(i = 0; i < cnt; i++)
   {
      double dx, dy, dist;

      dy = pow(yp - master[i][0], 2.0);
      dx = pow(xp - master[i][1], 2.0);
      dist = pow(dx+dy, 0.5);

      if(minDist == -1)
      {
          minDist = dist;
          index = 0;
      }
      else if(minDist > dist)
      {
         minDist = dist;
         index = i;
      }
   }

   pt->y = master[index][0];
   pt->x = master[index][1];

   return minDist;
}

/**************************************************/
void findCentroidPt(int **master, POINT *cent, int cnt)
{
   int i;
   double xsum, ysum;

   xsum = ysum = 0.0;

   for(i = 0; i < cnt; i++)
   {
      ysum += master[i][0];
      xsum += master[i][1];
   }

   ysum /= cnt;
   xsum /= cnt;

   searchClosestPt(master, cent, cnt, ysum, xsum);
}

/**************************************************/
void printPoint(POINT* p)
{
    char message[100];

   sprintf(message,"x: %f y: %f\n", p->x, p->y);
    zvmessage(message," ");
}

/**************************************************/
double distance(double x1, double y1, double x2, double y2)
{
   double dx, dy;

   dx = pow(x2 - x1, 2.);
   dy = pow(y2 - y1, 2.);

   return pow(dx + dy, 0.5);
}

/**************************************************/
void calculateDistances(POINT *pt, int **master, double* distances, int cnt)
{
   int i;
   double origX, origY;

   origX = pt->x;
   origY = pt->y;

   for(i = 0; i < cnt; i++)
   {
      double x, y;

      x = master[i][1];
      y = master[i][0];

      distances[i] = distance(origX, origY, x, y);
   }
}

/*
   p1 = upper left
   p2 = upper right
   p3 = lower right
   p4 = lower left
   p5 = centroid
*/
/**************************************************/
void getMasterDistances(int **master, POINT* p[5], int **sortedIndices,
                        double **distances, int cnt, int nl, int ns)
{
   int i;

   for(i = 0; i < 5; i++) p[i] = (POINT*)malloc(sizeof(POINT));
   searchClosestPt(master, p[0], cnt, 0.0, 0.0);
   searchClosestPt(master, p[1], cnt, (double)0, (double)ns);
   searchClosestPt(master, p[2], cnt, (double)nl, (double)0);
   searchClosestPt(master, p[3], cnt, (double)nl, (double)ns);
   findCentroidPt(master, p[4], cnt);

   for(i = 0; i < 5; i++)
   {
      calculateDistances(p[i], master, distances[i], cnt);
      //      for(j = 0; j < cnt; j++) printf("unsorted %d: %f\n", j, distances[i][j]);

      getSelectionSortIndices(distances[i], sortedIndices[i], cnt, CART_DOUBLE);

      /*
      for(j = 0; j < cnt; j++)
         printf("sorted %d: %f %d\n", j, distances[i][sortedIndices[i][j]], sortedIndices[i][j]);
      */
   }
}

/**************************************************/
int binaryRange(double *distances, int *sortedIndices, double dist, int mastercnt)
{
   int left, right, mid;

   left = 0;
   right = mastercnt - 1;
   mid = (left + right)/2;
   //   printf("val: %lf mid-1: %d lval: %lf mid: %d rval: %lf\n", val, mid-1, roulette[mid-1], mid, roulette[mid]);
   while(!(dist <= distances[sortedIndices[mid]] && dist >= distances[sortedIndices[mid - 1]]))
   {
     //      printf("val: %lf mid-1: %d lval: %lf mid: %d rval: %lf\n", val, mid-1, roulette[mid-1], mid, roulette[mid]);
      if(dist > distances[sortedIndices[mid]]) left = mid + 1;
      if(dist < distances[sortedIndices[mid - 1]]) right = mid - 1;
      mid = (left + right)/2;

      if(mid == 0 || mid == mastercnt - 1) break;
   }

   return mid;
}

/**************************************************/
double searchLimitedClosestPt(int **master, int **sortedIndices, POINT *controlPts[5],
                              double **distances, int mastercnt, double yp, double xp,
                              int nl, int ns)
{
   int i, closestControlPt, maxRange, closest, *indices;
   double closestControlDist, controlDist, closestDist, dist;

   closest = 0;
   closestControlDist = distance(controlPts[0]->x, controlPts[0]->y, xp, yp);
   closestControlPt = 0;
   for(i = 1; i < 5; i++)
   {
      controlDist = distance(controlPts[i]->x, controlPts[i]->y, xp, yp);
      if(controlDist < closestControlDist)
      {
         closestControlDist = controlDist;
         closestControlPt = i;
      }
   }

   indices = sortedIndices[closestControlPt];
   maxRange = binaryRange(distances[closestControlPt], indices,
                          closestControlDist*2, mastercnt);
   closestDist = distance(master[indices[0]][1], master[indices[0]][0], xp, yp);
   if(xp < 0 || yp < 0 || xp > ns || yp > nl) return closestDist;
   for(i = 1; i <= maxRange; i++)
   {
      dist = distance(master[indices[i]][1], master[indices[i]][0], xp, yp);
      if(dist < closestDist)
      {
         closestDist = dist;
         closest = i;
      }
   }
   if (closest == 0) {
        zvmessage ("closestDist not found"," ");
        zmabend ("ABEND");
    }
   return closestDist;
}

/**************************************************/
double calcFitness(int **master, int **img, PERSON *p, POINT *controlPts[5],
                   double **distances, int **sortedIndices, int mastercnt, int imgcnt,
                   int nl, int ns)
{
   int i;
   double a, b, c, d, e, f;
   double xp, yp, distsum;

   //   printf("here\n");

   decode(p);
   a = p->a;
   b = p->b;
   c = p->c;
   d = p->d;
   e = p->e;
   f = p->f;

   distsum = 0.0;
   //   printf("start searching closest point mastercnt: %d imgcnt: %d\n", mastercnt, imgcnt);
   for(i = 0; i < imgcnt; i++)
   {
      xp = a*img[i][1] + b*img[i][0] + c;
      yp = d*img[i][1] + e*img[i][0] + f;

      distsum += searchLimitedClosestPt(master, sortedIndices, controlPts, distances, mastercnt, yp, xp, nl, ns);
   }
   //   printf("end searching closest point\n");

   if(distsum < 10E-10) p->fitness = -1;
   else p->fitness = imgcnt/distsum;

   return p->fitness;
}

/**************************************************/
double calcPopFitness(PERSON **pop, int **master, int **img, POINT *controlPts[5],
                      double **distances, int **sortedIndices, int mastercnt, int imgcnt,
                      int nl, int ns)
{
   int i;
   double popfit;

   popfit = 0.0;

   for(i = 0; i < N_POPULATION; i++)
   {
      double fit;

      //      printf("i: before %d pop: %x master: %x\n", i, pop, master);
      //      printf("Calculating fitness for person %d: ", i);
      fit = calcFitness(master, img, pop[i], controlPts, distances, sortedIndices, mastercnt, imgcnt, nl, ns);
      //      printf("%lf\n", fit);
      //      printf("i: after %d  pop: %x master: %x\n", i, pop, master);
      if(fit == -1.) return -1.;
      popfit += fit;

      //      displayPerson(pop[i]);
      //      if(i == 5) return popfit;
   }

   return popfit;
}

/**************************************************/
void generateRandomPerson(PERSON *p)
{
   int i;

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
       p->chromosome[i] = (char)rand()%10;

   p->fitness = 0.0;
}

/**************************************************/
void createRandomPopulation(PERSON **pop, int n)
{
   int i;

   for(i = 0; i < n; i++) generateRandomPerson(pop[i]);
}

/**************************************************/
void getImage(int **img, int unit)
{
   int i, j, status, imgcnt;
   char *buf;

   imgcnt = 0;
   buf = (char*)malloc(sizeof(char)*1920);
   for(i = 0; i < 1080; i++)
   {
      status = zvread(unit, buf, "LINE", i+1, NULL);
      assert(status == 1);

      for(j = 0; j < 1920; j++)
         if(buf[j])
         {
            img[imgcnt][0] = i;
            img[imgcnt++][1] = j;
         }
   }
}

/**************************************************/
int countPixels(int unit)
{
   int i, j, status, tot;
   char *buf;

   tot = 0;
   buf = (char*)malloc(sizeof(char)*1920);
   for(i = 0; i < 1080; i++)
   {
      status = zvread(unit, buf, "LINE", i+1, NULL);
      assert(status == 1);

      for(j = 0; j < 1920; j++) if(buf[j]) ++tot;
   }

   return tot;
}

/**************************************************/
void printPopulation(PERSON **pop, int n)
{
   int i;
    char message[100];

   for(i = 0; i < n; i++)
   {
      sprintf(message,"%d: ", i+1);
        zvmessage(message," ");
      displayPerson(pop[i]);
   }
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

      index = rand()%(CHROMOSOME_LENGTH-2);
      memcpy(tmp, p1->chromosome, (size_t)index+1);
      memcpy(p1->chromosome, p2->chromosome, (size_t)index+1);
      memcpy(p2->chromosome, tmp, (size_t)index+1);
   }
}

/**************************************************/
void mutate(PERSON *p)
{
   int i;

   for(i = 0; i < CHROMOSOME_LENGTH; i++)
      if(rand()/(double)(RAND_MAX) < MUTATION_RATE) 
         (p->chromosome)[i] = (char)rand()%10;
}

/**************************************************/
void copyPerson(PERSON *from, PERSON *to)
{
   memcpy(to->chromosome, from->chromosome, CHROMOSOME_LENGTH);
   to->fitness = from->fitness;
}

/**************************************************/
void copyPopulation(PERSON **from, PERSON **to)
{
   int i;

   for(i = 0; i < N_POPULATION; i++)
      copyPerson(from[i], to[i]);
}

/**************************************************/
void freePopulation(PERSON **pop)
{
   int i;

   for(i = 0; i < N_POPULATION; i++)
      free(pop[i]);

   free(pop);
}

/**************************************************/
void evolve(PERSON **pop)
{
   int i, roulette1, roulette2;
   double roulette_wheel[N_POPULATION], sum, pick1, pick2;
   PERSON *person1, *person2;
   PERSON **newPopulation;

   newPopulation = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   for(i = 0; i < N_POPULATION; i++)
      newPopulation[i] = (PERSON*)malloc(sizeof(PERSON));

   getRouletteWheel(pop, roulette_wheel);
   //   printf("ROULETTE WHEEL\n");
   //   for(i = 0; i < N_POPULATION; i++) printf("%d: %f\n", i, roulette_wheel[i]);
   sum = roulette_wheel[N_POPULATION-1];

   person1 = (PERSON*)malloc(sizeof(PERSON));
   person2 = (PERSON*)malloc(sizeof(PERSON));

   for(i = 0; i < N_POPULATION; i+=2)
   {
      pick1 = rand()/(double)(RAND_MAX)*sum;
      pick2 = rand()/(double)(RAND_MAX)*sum;

      roulette1 = searchRoulette(roulette_wheel, pick1);
      roulette2 = searchRoulette(roulette_wheel, pick2);
      //      printf("pick: %lf roulette: %d roulette_wheel: %f\n", pick1, roulette1, roulette_wheel[roulette1] - roulette_wheel[roulette1-1]);

      copyPerson(pop[roulette1], person1);
      copyPerson(pop[roulette2], person2);

      crossover(person1, person2);
      mutate(person1);
      mutate(person2);

      copyPerson(person1, newPopulation[i]);
      copyPerson(person2, newPopulation[i+1]);
   }

   copyPopulation(newPopulation, pop);

   free(person1);
   free(person2);
   freePopulation(newPopulation);
}

/**************************************************/
void main44(void)
{
   int unit, i, cnt, def, status, masterunit;
   int bestfit, masterPixels, imgPixels;
   char masterfname[50];
   int **master, **img, **sortedIndices;
   double **distances;
   PERSON **pop;
   PERSON *best;
   POINT* p[5];
    char message[100];

    zvmessage("generefine - 10-17-2019 - rjb - 64-bit"," ");
   srand(0);
   /* UNIT TEST OF GENETIC CODE */
   /*
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
   */
   /* END UNIT TEST */

   status = zvunit(&unit, "inp", 1, NULL);
   assert(status == 1);
   status = zvopen(unit, "op", "read", NULL);
   assert(status == 1);
   imgPixels = countPixels(unit);
   img = (int**)malloc(sizeof(int*)*(long unsigned int)imgPixels);
   for(i = 0; i < imgPixels; i++) img[i] = (int*)calloc(2, sizeof(int));
   getImage(img, unit);
   status = zvclose(unit, NULL);
   assert(status == 1);

   status = zvparm("master", masterfname, &cnt, &def, 1, 50);
   status = zvunit(&masterunit, "none", 1, "u_name", masterfname, NULL);
   assert(status == 1);
   status = zvopen(masterunit, "op", "read", NULL);
   assert(status == 1);
   masterPixels = countPixels(masterunit);
   master = (int**)malloc(sizeof(int*)*(long unsigned int)masterPixels);
   for(i = 0; i < masterPixels; i++) master[i] = (int*)calloc(2, sizeof(int));
   getImage(master, masterunit);
   status = zvclose(masterunit, NULL);
   assert(status == 1);

   best = (PERSON*)malloc(sizeof(PERSON));
   pop = (PERSON**)malloc(sizeof(PERSON*)*N_POPULATION);
   for(i = 0; i < N_POPULATION; i++) pop[i] = (PERSON*)malloc(sizeof(PERSON));
   distances = (double**)malloc(sizeof(double*)*5);
   sortedIndices = (int**)malloc(sizeof(int*)*5);
   for(i = 0; i < 5; i++)
   {
      distances[i] = (double*)malloc(sizeof(double)*(long unsigned int)masterPixels);
      sortedIndices[i] = (int*)malloc(sizeof(int)*(long unsigned int)masterPixels);
   }

   createRandomPopulation(pop, N_POPULATION);
   printPopulation(pop, N_POPULATION);
   getMasterDistances(master, p, sortedIndices, distances, masterPixels, 1080, 1920);

   i = 0;
   while(i < N_EVOLUTIONS)
   {
      int j;
      double popFit;

      popFit = calcPopFitness(pop, master, img, p, distances, sortedIndices, masterPixels, imgPixels, 1080, 1920);

      if(popFit == -1.) break;
      evolve(pop);

      if(i == 0) copyPerson(pop[0], best);
      for(j = 0; j < N_POPULATION; j++)
         if(pop[j]->fitness > best->fitness)
            copyPerson(pop[j], best);
      sprintf(message,"EVOLUTION: %d totfitness: %f bestfitness: %f", ++i, popFit, best->fitness);
      zvmessage(message," ");
      displayPerson(best);
      //      return;
   }

   /* search for the best person */
   bestfit = 0;
   for(i = 0; i < N_POPULATION; i++)
   {
      if(pop[i]->fitness > pop[bestfit]->fitness)
         bestfit = i;
      if(pop[i]->fitness == -1.)
      {
         bestfit = i;
         break;
      }
   }

   displayPerson(best);

   for(i = 0; i < N_POPULATION; i++) free(pop[i]);
   free(pop);
   //   displayPerson(pop[bestfit]);
}
