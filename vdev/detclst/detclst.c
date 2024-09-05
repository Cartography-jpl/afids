#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>


#include "carto/ImageUtils.h"
/* prototypes */
double centerDiff(VICAR_TILE_IMAGE *vti, double subtract);
double getMean(VICAR_TILE_IMAGE *vti);
double meanCenterDiff(VICAR_TILE_IMAGE *vti);
double variance(VICAR_TILE_IMAGE *vti);

/***************************************************************/
double centerDiff(VICAR_TILE_IMAGE *vti, double subtract)
{
   int i, j;
   double sum;

   sum = .0;
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += fabs(vti->tile[i][j] - subtract);
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
double getMean(VICAR_TILE_IMAGE *vti)
{
   int i, j;
   double sum;

   sum = .0;
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += vti->tile[i][j];
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
double meanCenterDiff(VICAR_TILE_IMAGE *vti)
{
   double mean;

   mean = getMean(vti);
   return centerDiff(vti, mean);
}

/***************************************************************/
double variance(VICAR_TILE_IMAGE *vti)
{
   int i, j;
   double mean, sum;

   sum = .0;
   mean = getMean(vti);
   for(i = 0; i < vti->tile_nl; i++)
      for(j = 0; j < vti->tile_ns; j++)
      {
         if(i == vti->tile_nl/2 && j == vti->tile_ns/2) continue;
         sum += pow(vti->tile[i][j]-mean, 2.);
      }

   return sum/(vti->tile_nl*vti->tile_ns - 1);
}

/***************************************************************/
void main44(void)
{
   int i, j, mode, status, cnt, dum, win;
   VICAR_IMAGE *vi, *out;
   VICAR_TILE_IMAGE *vti;

    zvmessage ("detclst - 10-11-2019 -rjb - 64-bit"," ");
   status = zvparm("win", &win, &cnt, &dum, 1, 0);
   assert(status == 1);
   vi = getVI_inp(1);
   vti = getVTI(vi, win, win);
   status = zvparm("mode", &mode, &cnt, &dum, 1, 0);
   assert(status == 1);
   out = getVI_out("REAL", 1, vi->nl, vi->ns);

   for(i = 0; i < vi->nl; i++)
   {
      for(j = 0; j < vi->ns; j++)
      {
         readVicarTileImage(vti, i, j);
         switch(mode)
         {
            case 1:
               out->buffer[j] = centerDiff(vti, vti->tile[win/2][win/2]);
               break;
            case 2:
               out->buffer[j] = meanCenterDiff(vti);
               break;
            case 3:
               out->buffer[j] = variance(vti);
               break;
         }
      }

      writeVicarImageLine(out, i);
   }

   deleteVTI(&vti);
   deleteAndCloseImage(&vi);
   deleteAndCloseImage(&out);
}
