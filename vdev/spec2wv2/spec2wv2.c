#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "gsl/gsl_vector.h"
#include "carto/ImageUtils.h"
#include "carto/ibishelper.h"
#include "carto/WV2Manager.h"

#define SPEC2WV2_MAX_FILE_LEN 500
#define SPEC2WV2_MAXFILES 100

typedef struct
{
   char fname[SPEC2WV2_MAX_FILE_LEN];
   char name[SPEC2WV2_MAX_FILE_LEN];
   gsl_vector *x, *y;
}SPEC;

/****************************************************************/
SPEC* getSpec(char fname[])
{
   int index, vectorsSet;
   FILE *f = NULL;
   char line[SPEC2WV2_MAX_FILE_LEN];
   SPEC* spec;

   vectorsSet = 0;
   spec = (SPEC*)malloc(sizeof(SPEC));
   strcpy(spec->fname, fname);

   index = 0;
   f = fopen(fname, "r");
   while(fgets(line, SPEC2WV2_MAX_FILE_LEN, f) != NULL)
   {
      double x, y;
      char *ptr;

      ptr = strstr(line, "Name:");
      if(ptr != NULL)
      {
         sscanf(line, "Name:  %s", spec->name);
         continue;
      }

      ptr = strstr(line, "Number of X Values");
      if(ptr != NULL)
      {
         int nXValues;

         sscanf(line, "Number of X Values:  %d", &nXValues);
         spec->x = gsl_vector_alloc(nXValues);
         spec->y = gsl_vector_alloc(nXValues);
         vectorsSet = 1;
         continue;
      }

      if(!vectorsSet) continue;
      // scan for values
      if(sscanf(line, "%lf%*[ \t]%lf", &x, &y) == 2)
      {
         gsl_vector_set(spec->x, index, x);
         gsl_vector_set(spec->y, index, y);
         ++index;
      }
   }

   return spec;
}

/****************************************************************/
SPEC** getSpecs(char fnames[][SPEC2WV2_MAX_FILE_LEN], int cnt)
{
   int i;
   SPEC **specs;

   specs = (SPEC**)malloc(sizeof(SPEC*)*cnt);
   for(i = 0; i < cnt; i++)
      specs[i] = getSpec(fnames[i]);

   return specs;
}

/****************************************************************/
void printSpecs(SPEC **specs, int nSpecs)
{
   int i, j;

   for(i = 0; i < nSpecs; i++)
   {
      printf("NAME: %s (%s)\n", specs[i]->name, specs[i]->fname);
      printf("===============================\n");
      for(j = 0; j < specs[i]->x->size; j++)
         printf("%lf %lf\n", gsl_vector_get(specs[i]->x, j),
                gsl_vector_get(specs[i]->y, j));
      printf("\n");
   }
}

/****************************************************************/
double* getBandWeightedReflectances(SPEC *spec)
{
   int i;
   double *bwr;

   bwr = (double*)malloc(sizeof(double)*WV2_N_BANDS);
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      int j;

      bwr[i] = 0.;
      for(j = 0; j < spec->x->size; j++)
      {
         double x = gsl_vector_get(spec->x, j);
         if(x >= WV2_LBW[i] && x <= WV2_UBW[i])
         {
            int nSum;
            double dw;

            nSum = 0;
            dw = 0.;
            if(j-1 >= 0)
            {
               double leftX = gsl_vector_get(spec->x, j-1);
               if(leftX < WV2_LBW[i]) leftX = WV2_LBW[i];
               dw += fabs(leftX - x);
               nSum++;
            }
            if(j+1 < spec->x->size)
            {
               double rightX = gsl_vector_get(spec->x, j+1);
               if(rightX > WV2_UBW[i]) rightX = WV2_UBW[i];
               dw += fabs(rightX - x);
               nSum++;
            }
            if(nSum == 2) dw /= 2;

            bwr[i] += gsl_vector_get(spec->y, j)*dw;
         }
      }
      bwr[i] /= (WV2_UBW[i] - WV2_LBW[i]);
   }

   return bwr;
}

/****************************************************************/
void outputIBIS(SPEC** specs, double **bwr, int cnt)
{
   int i;
   IBISStruct *ibis;
   IBISPrep *prep;

   prep = IBISHELPER_openIBIS_out2("out", 1, cnt);
   IBISHELPER_addColumn(prep, "A100");
   for(i = 0; i < WV2_N_BANDS; i++)
      IBISHELPER_addColumn(prep, "DOUB");

   ibis = IBISHELPER_getIBISStruct(&prep);
   for(i = 0; i < cnt; i++)
   {
      int j;

      IBISHELPER_setString(ibis, 0, i, specs[i]->name);
      for(j = 0; j < WV2_N_BANDS; j++)
         IBISHELPER_setDouble(ibis, j+1, i, bwr[i][j]);
   }

   IBISHELPER_closeIBIS(&ibis);
}

/****************************************************************/
void main44(void)
{
   int i;
   int status, dumdef, cnt;
   SPEC **specs;
   char fnames[SPEC2WV2_MAXFILES][SPEC2WV2_MAX_FILE_LEN];
   double **bwrs;

   status = zvparm("SPECFILES", fnames, &cnt, &dumdef,
                   SPEC2WV2_MAXFILES, SPEC2WV2_MAX_FILE_LEN);
//   printf("status: %d\n", status);
   assert(status == 1);

   specs = getSpecs(fnames, cnt);
   bwrs = (double**)malloc(sizeof(double*)*cnt);
   for(i = 0; i < cnt; i++)
   {
//      printf("Band Weighted Reflectances for %s\n", specs[i]->name);
//      printf("===============================================\n");
      bwrs[i] = getBandWeightedReflectances(specs[i]);
//      printf("\n");
   }

   outputIBIS(specs, bwrs, cnt);
}
