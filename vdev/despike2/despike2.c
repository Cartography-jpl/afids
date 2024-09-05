#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "ImageUtils.h"
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sort.h>

#define WINSIZE 5

void main44(void)
{
   int i, j, status, dumcnt, nChanged;
   float scale, tol;
   double *medBuf;
   char *threshFlags;
   VICAR_IMAGE *inp, *out;
   VICAR_TILE_IMAGE *tile;

    zvmessage ("despike2 - 10-11-2019 - rjb - 64-bit"," ");
   inp = getVI_inp(1);
   out = getVI_out(inp->format, 1, inp->nl, inp->ns);
   tile = getVTI(inp, 1, WINSIZE);
   status = zvp("SCALE", &scale, &dumcnt);
//    printf ("status for SCALE = %d\n",status);

   assert(status == 1);
   status = zvp("TOL", &tol, &dumcnt);
//    printf ("status for TOL = %d\n",status);
   assert(status == 1);
   medBuf = (double*)malloc(sizeof(double)*(long unsigned int)inp->ns);
   threshFlags = (char*)malloc(sizeof(double)*(long unsigned int)inp->ns);
   nChanged = 0;

   for(i = 0; i < inp->nl; i++)
   {
      readVicarImageLine(inp, i);
      memcpy(out->buffer, inp->buffer, sizeof(double)*(long unsigned int)inp->ns);

      // create median buffer
      for(j = 0; j < inp->ns; j++)
      {
         int k;
         double tmp[WINSIZE];

         readVicarTileImage(tile, i, j);
         for(k = 0; k < WINSIZE; k++)
            tmp[k] = tile->tile[0][k];
         /*
         if(i == 1)
         {
            printf("%d %lf %lf %lf\n", j, tile->tile[0][2], tile->buffer[0][j], inp->buffer[j]);
            for(k = 0; k < WINSIZE; k++)
               printf("%lf ",tile->tile[0][k]);
            printf("\n");
         }
         */
         gsl_sort(tmp, 1, WINSIZE);
         medBuf[j] = gsl_stats_median_from_sorted_data(tmp, 1, WINSIZE);
         //         if(i == 1) printf("%lf ", medBuf[j]);
         medBuf[j] = fabs(inp->buffer[j]-medBuf[j]);
         //         if(i == 1) printf("%lf\n", medBuf[j]);
      }

      // perform 1st test and trigger threshold flags
      for(j = 0; j < inp->ns; j++)
      {
         // k, l is the left and right indices of medBuf
         int k, l, m;
         double thresh, sum;

         threshFlags[j] = 0;
         readVicarTileImage(tile, i, j);

         // make sure left and right indices of medBuf is not out of bounds
         if(j == 0) k = 0;
         else k = j-1;
         if(j == inp->ns - 1) l = inp->ns-1;
         else l = j+1;

         sum = 0.;
         for(m = k; m <= l; m++) sum += medBuf[m];
         thresh = (scale/(double)WINSIZE)*(sum)+tol;
         if(medBuf[j] > thresh) threshFlags[j] = 1;
         //         if(i == 1) printf("%d: %d %lf\n", j, threshFlags[j], thresh); 
      }

      // perform 2nd test to determine spike then fix
      for(j = 0; j < inp->ns; j++)
      {
         int k, l, m;
         double thresh, expected, sum, leftDiff, rightDiff, avgDiff, diffCnt, sumCnt;

         if(!threshFlags[j])
            continue;

         if(j == 0) k = 0;
         else k = j-(WINSIZE/2);
         if(j == inp->ns - 1) l = inp->ns-1;
         else l = j+(WINSIZE/2);

         leftDiff = 0;
         rightDiff = 0;
         avgDiff = 0;
         diffCnt = 0;
         sum = 0;
         sumCnt = 0;
         for(m = k; m <= l; m++)
         {
            if(m == j) continue;

            sum += inp->buffer[m];
            sumCnt++;
            //            printf("%lf\n", sum);
            if(m+1 < j)
            {
               leftDiff += fabs(inp->buffer[m+1]-inp->buffer[m]);
               diffCnt++;
            }
            else if(m-1 > j)
            {
               rightDiff += fabs(inp->buffer[m-1]-inp->buffer[m]);
               diffCnt++;
            }
         }
         //         printf("diff cnt: %lf window size: %d sum: %lf\n", diffCnt, WINSIZE, sum);
         if(diffCnt > 0) avgDiff = (leftDiff+rightDiff)/diffCnt;
         else avgDiff = 0;

         expected = sum/sumCnt;
         //         expected = (inp->buffer[k]+inp->buffer[l])/2.;
         thresh = scale*avgDiff + tol;
         //         printf("scale: %lf tol: %lf threshold 2: %lf\n", scale, tol, thresh);
         // check to see if fails threshold and positive spike
         if(fabs(inp->buffer[j] - expected) > thresh && inp->buffer[j] > expected)
         {
            out->buffer[j] = expected;
            nChanged++;
         }

         //         if(i == 1) printf("%d: leftDiff: %lf rightDiff: %lf inp-expected: %lf sum: %lf inp: %lf thresh: %lf\n", j, leftDiff, rightDiff, fabs(inp->buffer[j]-expected), sum, inp->buffer[j], thresh); 
      }

      writeVicarImageLine(out, i);
   }

   printf("despiking from %s to %s - %d pixels changed.\n", inp->fname, out->fname, nChanged);
   deleteVTI(&tile);
   deleteAndCloseImage(&inp);
   deleteAndCloseImage(&out);
}
