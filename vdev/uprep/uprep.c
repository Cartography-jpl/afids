#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <float.h>

#include "carto/ibishelper.h"
#include "carto/ImageUtils.h"
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_blas.h>

#define MAX_CLASS_NAME_LEN 100
#define N_GRID_1D 100
#define N_GRID_2D 100*100

/**********************************************/
typedef struct
{
   char *className;
   gsl_vector *means;
   gsl_matrix *cov_mat;
   gsl_matrix *inv_mat;
   int nDim;
}CLASS;

/**********************************************/
typedef struct
{
   int line, sample;
   gsl_vector *dns;
   CLASS *class;
}SAMPLE;

/**********************************************/
void printSample(SAMPLE *sample)
{
   int i;

   if(sample->class == NULL) printf("class: unclassified\n");
   else printf("class: %s\n", sample->class->className);
   printf("line: %d sample: %d\n", sample->line, sample->sample);
   printf("dn: ");
   for(i = 0; i < sample->dns->size; i++) printf("%lf ", gsl_vector_get(sample->dns, i));
   printf("\n");
}

/**********************************************/
int compare_sample_by_line(const SAMPLE **s1, const SAMPLE **s2)
{
  /*
   SAMPLE* const *ia;
   SAMPLE* const *ib;

   ia = s1;
   ib = s2;

   printf("ia: %x\n", ia);
   printf("struct pointer size: %d\n", sizeof(SAMPLE*));
   printf("struct size: %d\n", sizeof(SAMPLE));
   printf("comparing: %d %d\n", sizeof(*ia), sizeof(**ia));
   printSample(*ia);
   printSample(*ib);
  */
   if((*s1)->line < (*s2)->line)
   {
     //      printf("less than\n");
      return -1;
   }
   else if((*s1)->line > (*s2)->line)
   {
     //      printf("more than\n");
      return 1;
   }

   //   printf("same\n");
   return 0;
}

/**********************************************/
void printClass(CLASS *class)
{
   int i, j;

   printf("name: %s\n", class->className);
   printf("means: ");
   for(i = 0; i < class->means->size; i++)
      printf("%lf ", gsl_vector_get(class->means, i));
   printf("\n");
   printf("covariance matrix:\n");
   for(i = 0; i < class->nDim; i++)
   {
      for(j = 0; j < class->nDim; j++)
         printf("%5.8lf\t", gsl_matrix_get(class->cov_mat, i, j));
      printf("\n");
   }
   printf("inverse of covariance matrix:\n");
   for(i = 0; i < class->nDim; i++)
   {
      for(j = 0; j < class->nDim; j++)
         printf("%5.8lf\t", gsl_matrix_get(class->inv_mat, i, j));
      printf("\n");
   }
}

/**********************************************/
CLASS* getClass(char* className, gsl_vector* means, gsl_matrix *cov_mat, int nDim)
{
   int i, j;
   gsl_matrix *inv;
   gsl_permutation *p;
   double signum;

   p = gsl_permutation_alloc(means->size);
   inv = gsl_matrix_alloc(cov_mat->size1, cov_mat->size2);
   /*
   printf("here1\n");
   for(i = 0; i < cov_mat->size1; i++)
   {
      for(j = 0; j < cov_mat->size2; j++)
         printf("%lf ", gsl_matrix_get(cov_mat, i, j));
      printf("\n");
   }
   */

   gsl_linalg_LU_decomp(cov_mat, p, &signum);
   //   printf("here2\n");
   gsl_linalg_LU_invert(cov_mat, p, inv);
   //   printf("here3\n");

   // fix -0s in inverse and covariance matrix
   for(i = 0; i < inv->size1; i++)
   {
      for(j = 0; j < inv->size2; j++)
      {
         if(fabs(gsl_matrix_get(inv, i, j)) < 10E-10)
            gsl_matrix_set(inv, i, j, 0.);
         if(fabs(gsl_matrix_get(cov_mat, i, j)) < 10E-10)
            gsl_matrix_set(cov_mat, i, j, 0.);
      }
   }

   CLASS* class = (CLASS*)malloc(sizeof(CLASS));
   class->className = className;
   class->means = means;
   class->cov_mat = cov_mat;
   class->inv_mat = inv;
   class->nDim = nDim;

   gsl_permutation_free(p);

   return class;
}

/**********************************************/
CLASS** getClasses(IBISStruct *ibis)
{
   int i;
   CLASS **classes;
   gsl_permutation *p;

   classes = (CLASS**)malloc(sizeof(CLASS*)*ibis->nr);
   for(i = 0; i < ibis->nr; i++)
   {
      char *className;
      int nDim;
      gsl_vector *means;
      gsl_matrix *cov_mat;
      int j, rowIndex, colIndex;

      // get number of bands
      nDim = IBISHELPER_getInt(ibis, 2, i);

      // set class name
      className = (char*)calloc(MAX_CLASS_NAME_LEN, sizeof(char));
      IBISHELPER_getString(ibis, className, 0, i);

      // set class means
      means = gsl_vector_alloc(nDim);
      for(j = 3; j < 3+nDim; j++)
         gsl_vector_set(means, j-3, IBISHELPER_getDouble(ibis, j, i));

      // set class cov matrix
      cov_mat = gsl_matrix_alloc(nDim, nDim);

      rowIndex = colIndex = 0;
      for(; j < ibis->nc; j++)
      {
         double val = IBISHELPER_getDouble(ibis, j, i);

         //         if(val < -255. || val > 255.) val = 0.;

         gsl_matrix_set(cov_mat, rowIndex, colIndex, val);
         gsl_matrix_set(cov_mat, colIndex, rowIndex, val);

         ++colIndex;
         if(colIndex > rowIndex)
         {
           rowIndex++;
           colIndex = 0;
         }
      }

      classes[i] = getClass(className, means, cov_mat, nDim);
      //      printClass(classes[i]);
   }

   return classes;
}

/**********************************************/
VICAR_IMAGE** getVicarImages(int cnt)
{
   int i;
   VICAR_IMAGE **inps;

   inps = (VICAR_IMAGE**)malloc(sizeof(VICAR_IMAGE*)*cnt);
   for(i = 0; i < cnt-1; i++)
      inps[i] = getVI_inp(i+1);

   return inps;
}

/**********************************************/
void printClasses(CLASS **classes, int nClasses)
{
   int i;

   for(i = 0; i < nClasses; i++)
   {
      printf("********************\n");
      printClass(classes[i]);
   }
}

/**********************************************/
void printSamples(SAMPLE **samples, int nSamples)
{
   int i;

   for(i = 0; i < nSamples; i++)
   {
      printf("********************\n");
      printSample(samples[i]);
   }
}

/**********************************************/
void readSamples(SAMPLE **samples, VICAR_IMAGE **inps, int nDim)
{
   // assumed samples are sorted in reading order at this point
   int i, j;

   for(i = 0; i < N_GRID_2D; i++)
   {
      // only read line if necessary
      if(i == 0 || samples[i]->line != samples[i-1]->line)
         for(j = 0; j < nDim; j++) readVicarImageLine(inps[j], samples[i]->line);
      //     printf("here\n");

      for(j = 0; j < nDim; j++)
         gsl_vector_set(samples[i]->dns, j, inps[j]->buffer[samples[i]->sample]);
   }
}

/**********************************************/
SAMPLE** getStratifiedSamples(VICAR_IMAGE **inps, int nDim)
{
   SAMPLE **samples;
   int gridX, gridY, i, sampleCnt;
   SAMPLE **readSampleIndices;
   srand(0);

   gridX = inps[0]->ns/N_GRID_1D;
   gridY = inps[0]->nl/N_GRID_1D;

   //   printf("gridX: %d gridY: %d\n", gridX, gridY);

   //   printf("herea %d\n", sizeof(SAMPLE*)*N_GRID_2D);
   samples = (SAMPLE**)malloc(sizeof(SAMPLE*)*N_GRID_2D);
   //   printf("herea2 \n");
   sampleCnt = 0;
   for(i = 0; i < N_GRID_1D; i++)
   {
      int j;
      int x, y;

      for(j = 0; j < N_GRID_1D; j++)
      {
         x = gridX*i + rand()%gridX;
         y = gridY*j + rand()%gridY;

         samples[sampleCnt] = (SAMPLE*)malloc(sizeof(SAMPLE));
         samples[sampleCnt]->line = y;
         samples[sampleCnt]->sample = x;
         samples[sampleCnt]->dns = gsl_vector_alloc(nDim);
         sampleCnt++;
      }
   }

   //   printf("=====> before sort\n");
   //   printSamples(samples, N_GRID_2D);
   //   for(i = 0; i < 10; i++)
   //      printf("before sort address: %d\n", samples[i+1]-samples[i]);
   qsort(samples, N_GRID_2D, sizeof(SAMPLE*), compare_sample_by_line);
   //   printf("=====> after sort\n");
   //   printSamples(samples, N_GRID_2D);
   readSamples(samples, inps, nDim);
   //   printf("herec \n");

   return samples;
}

/**********************************************/
double getMahalanobisDistance(gsl_vector *dns, gsl_vector *means, gsl_matrix *inv)
{
   int i, j;
   double distance;
   gsl_matrix_view diffView;
   gsl_matrix *result, *result2;

   //   if(!gsl_matrix_isnonneg(inv)) return -1.;

   result = gsl_matrix_alloc(1, means->size);
   result2 = gsl_matrix_alloc(1, 1);

   /*
   printf("original dns: ");
   for(i = 0; i < dns->size; i++) printf("%lf ", gsl_vector_get(dns, i));
   printf("\n");

   printf("mean dns: ");
   for(i = 0; i < dns->size; i++) printf("%lf ", gsl_vector_get(means, i));
   printf("\n");
   */

   gsl_vector_sub(dns, means);

   /*
   printf("after mean subtraction: ");
   for(i = 0; i < dns->size; i++) printf("%lf ", gsl_vector_get(dns, i));
   printf("\n");
   */

   diffView = gsl_matrix_view_vector(dns, 1, dns->size);

   //   printf("diff View dimensions: %d %d\n", diffView.matrix.size1, diffView.matrix.size2);
   //   printf("covariance dimensions: %d %d\n", covariance->size1, covariance->size2);
   //   printf("result dimensions: %d %d\n", result->size1, result->size2);

   gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1, &diffView.matrix, inv, 0, result);
   gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1, result, &diffView.matrix, 0, result2);

   distance = sqrt(gsl_matrix_get(result2, 0, 0));

   gsl_matrix_free(result);
   gsl_matrix_free(result2);

   return distance;
}

/**********************************************/
void classifySamples(SAMPLE **samples, CLASS **classes, int nSamples, int nClasses)
{
   int i, j, min;
   double mahalanobis[nClasses];
   gsl_vector *tmpSamp = gsl_vector_alloc(samples[0]->dns->size);

   for(i = 0; i < nSamples; i++)
   {
      for(j = 0; j < nClasses; j++)
      {
         gsl_vector_memcpy(tmpSamp, samples[i]->dns);
         mahalanobis[j] = getMahalanobisDistance(tmpSamp, classes[j]->means, classes[j]->inv_mat);
         if(mahalanobis[j] < 0.) mahalanobis[j] = DBL_MAX;
         //         printf("mahalanobis result: %lf\n", mahalanobis[j]);
      }

      min = 0;
      for(j = 1; j < nClasses; j++)
         if(mahalanobis[j] < mahalanobis[min]) min = j;

      samples[i]->class = classes[min];
   }

   gsl_vector_free(tmpSamp);
}

/**********************************************/
void outIBIS(SAMPLE **samples, int nDim)
{
   int nr, nc, i, j;
   char **formats;
   IBISStruct *out;

   nr = N_GRID_2D;
   nc = nDim+3;
   formats = (char**)malloc(nc*sizeof(char*));
   formats[0] = "a12";
   for(i = 1; i < nc; i++) formats[i] = "REAL";
   //   for(i = 0; i < nc; i++) printf("%d: %s\n", i+1, formats[i]);

   out = IBISHELPER_openIBIS_out(formats, 1, nr, nc);

   for(i = 0; i < N_GRID_2D; i++)
   {
      IBISHELPER_setString(out, 0, i, samples[i]->class->className);
      IBISHELPER_setDouble(out, 1, i, (double)samples[i]->line+1);
      IBISHELPER_setDouble(out, 2, i, (double)samples[i]->sample+1);
      for(j = 3; j < nc; j++)
         IBISHELPER_setDouble(out, j, i, (double)gsl_vector_get(samples[i]->dns, j-3));
   }

   IBISHELPER_closeIBIS(&out);

   //   for(i = 0; i < nc; i++) free(formats[i]);
   free(formats);
}

/**********************************************/
void main44(void)
{
   int status, i, inpCnt;
   VICAR_IMAGE **inps;
   IBISStruct *ibis;
   IBISStruct *out;
   CLASS **classes;
   SAMPLE **samples;
   //   CLASSIFIED_SAMPLES **classifiedSamples;

   status = zvpcnt("inp", &inpCnt);
   if(status != 1) zmabend("Error getting input count.\n");
   //   printf("input count: %d\n", inpCnt);
   inps = getVicarImages(inpCnt);
   ibis = IBISHELPER_openIBIS("inp", inpCnt, "read");
   //   printf("here\n");
   classes = getClasses(ibis);
   //   printClasses(classes, ibis->nr);
   samples = getStratifiedSamples(inps, inpCnt-1);
   //   printSamples(samples, N_GRID_2D);
   classifySamples(samples, classes, N_GRID_2D, ibis->nr);
   //   printSamples(samples, N_GRID_2D);
   outIBIS(samples, inpCnt-1);
}
