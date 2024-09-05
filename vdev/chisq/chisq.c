#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "cartoMemUtils.h"

/*=========================================================*/
/* This function gets the distances for datapoints in      */
/* pos and stores it into distance buffer.  The distance   */
/* is measured from origin.                                */
/*                                                         */
/* pos - buffer that has the datapoints (input)            */
/* distance - buffer that the distances will be stored     */
/*            into (output)                                */
/* indcount - number of independent variables in pos and   */
/*            origin. (input)                              */
/* controlCnt - number of entities in pos. (input)         */
/* origin - the point from where the distances are         */
/*          calculated.                                    */
/*=========================================================*/

/* prototypes */
void getDistances(double **pos, double *distance, int indcount, int controlCnt, double *origin);
double getStandardDeviation(double *vector, int n);


/*************************************************************************/
void getDistances(double **pos, double *distance, int indcount, int controlCnt, double *origin)
{
   int i, j;

   for(i = 0; i < controlCnt; i++)
   {
      double sum;

      sum = 0.0;
      for(j = 0; j < indcount; j++)
	 sum += pow(origin[j]-pos[j][i], 2);
      distance[i] = sqrt(sum);
   }
}

/*=========================================================*/
double getStandardDeviation(double *vector, int n)
{
   int i;
   double avg=0.0, deviation=0.0;

   for(i = 0; i < n; i++) avg += vector[i];
   avg /= n;

   for(i = 0; i < n; i++) deviation += pow(vector[i] - avg, 2.0);
   deviation /= (n-1);

   return pow(deviation, 0.5);
}

/*=========================================================*/
void main44(void)
{
   int iunit, iibis, ounit, oibis;
   int i, status, nc, nr, ngoods, nbads;
   double **data, **results, *tmp, *stan_dev;
   double *goodImgs, *badImgs;

   zifmessage("chisq version 10-05-2019 - rjb - 64-bit");
   

   /* read in data from the ibis interface file */
   /* open input files */
   status = zvunit(&iunit,"inp",1,NULL);
   status = IBISFileOpen(iunit,&iibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(iunit,status,1);
   IBISFileGet(iibis,"nr",&nr,1,1,0);
   IBISFileGet(iibis,"nc",&nc,1,1,0);

   /* allocate memories and read data */
   mz_alloc2((unsigned char **)&data, nc, nr, sizeof(double));
   mz_alloc2((unsigned char **)&results, nc+1, nr, sizeof(double));
   mz_alloc1((unsigned char **)&stan_dev, nr, sizeof(double));

   mz_alloc1((unsigned char *)&goodImgs, nr, sizeof(double));
   mz_alloc1((unsigned char *)&badImgs, nr, sizeof(double));
   tmp = (double*)calloc(nr, sizeof(double));
   ngoods = nbads = 0;
   
   for (i = 0; i < nc; i++)
   {
      status = IBISColumnSet(iibis, "U_FORMAT", "DOUB", i+1);
      if (status!=1) IBISSignal(iibis, status, 1);
      status = IBISColumnRead(iibis, (char *)data[i], i+1, 1, nr);
      if (status!=1) IBISSignal(iibis, status, 1);
   }

   for(i = 0; i < nc; i++)
   {
      int j;

      results[i][0] = 0.0;
      for(j = 1; j < nr; j++)
         results[i][j] = fabs(data[i][j] - data[i][j-1]);
   }

   for(i = 1; i < nc; i++)
   {
      stan_dev[i] = getStandardDeviation(results[i], nr);
      printf("i: %d std: %f\n", i, stan_dev[i]);
   }

   /* identify good and bad images */
   for(i = 0; i < nr; i++)
   {
      int j;
      int outlier;

      outlier = 0;
      for(j = 1; j < nc; j++)
         outlier += (results[j][i] > stan_dev[j]);

      if(!outlier)
         results[j][i] = 1.0;
      else
         results[j][i] = 0.0;
   }

   /* images are good if there are 5 consistent good images before or after current frame */
   for(i = 0; i < nr; i++)
   {
      int before, after, j;

      if(results[nc][i] < 10E-10) continue;

      /* checking before */
      if(i - 5 < 0) before = 0;
      else
      {
         before = 1.0;
         for(j = i - 5; j < i; j++)
            if(results[nc][j] < 10E-10)
            {
               before = 0.0;
               break;
            }
      }

      /* checking after */
      if(i + 5 >= nr) after = 0;
      else
      {
         after = 1.0;
         for(j = i + 1; j < i + 6; j ++)
            if(results[nc][j] < 10E-10)
            {
               after = 0.0;
               break;
            }
      }

      if(before > 10E-10 || after > 10E-10)
      {
         results[nc][i] = 1.0;
         goodImgs[ngoods++] = data[0][i];
      }
      else
      {
         results[nc][i] = 0.0;
         badImgs[nbads++] = data[0][i];
      }
   }

   for(i = 0; i < nr; i++)
   {
      int mod;

      mod = i%10;

      if(mod == 8)
      {
         int j, window[5];

         for(j = 0; j < 5 && i + j < nr; j++) window[j] = i + j;

         /* search for good window in specific order */
         if(!(results[nc][window[2]] < 10E-10)) tmp[window[2]] = 1.0;
         else if(!(results[nc][window[1]] < 10E-10)) tmp[window[1]] = 1.0;
         else if(!(results[nc][window[3]] < 10E-10)) tmp[window[3]] = 1.0;
         else if(!(results[nc][window[0]] < 10E-10)) tmp[window[0]] = 1.0;
         else if(!(results[nc][window[4]] < 10E-10)) tmp[window[4]] = 1.0;

         i += 5;
      }
   }
   memcpy(results[nc], tmp, nr*sizeof(double));     

   /*
   printf("Good Images: %d\n", ngoods);
   for(i = 0; i < ngoods; i++) printf("%lf\n", goodImgs[i]);
   printf("\nBad Images: %d\n", nbads);
   for(i = 0; i < nbads; i++) printf("%lf\n", badImgs[i]);
   */

   status = zvunit(&ounit,"out",1,NULL);
   status = IBISFileUnit(ounit, &oibis, "write", nc*2+1, nr, 0, 0);
   status = IBISFileSet(oibis, "fmt_default", "doub", 0);
   status = IBISFileUnitOpen(oibis);
   if (status!=1) IBISSignalU(ounit,status,1);
   for(i = 0; i < nc; i++)
   {
      status = IBISColumnSet(oibis,"U_FORMAT","DOUB",i+1);
      if (status!=1) IBISSignal(oibis,status,1);
      status = IBISColumnWrite(oibis,(char *)data[i],i+1,1,nr);
      if (status!=1) IBISSignal(oibis,status,1);
   }
   for(i = nc; i < nc*2+1; i++)
   {
      status = IBISColumnSet(oibis,"U_FORMAT","DOUB",i+1);
      if (status!=1) IBISSignal(oibis,status,1);
      status = IBISColumnWrite(oibis, results[i-nc],i+1,1,nr);
      if (status!=1) IBISSignal(oibis,status,1);
   }

   mz_free2((unsigned char**)data, nc);
   mz_free2((unsigned char**)results, nc+1);
   free(stan_dev);
   free(goodImgs);
   free(badImgs);
   free(tmp);

   status = IBISFileClose(iibis,0);
   if (status!=1) IBISSignal(iibis,status,1);
}












