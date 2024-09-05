#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "carto/ibishelper.h"
//#include "lsqdat.h"
//#include "lsqdatlist.h"
#include "carto/lsqibis.h"
#include "carto/lsqequation.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoSortUtils.h"

/*=========================================================*/
void markOutliers(LsqIbis *lsq, int *outliers)
{
   int i;
   int *thrownout;
   double thresh, *dist;

   dist = lsq->dist;
   thresh = lsq->thresh;
   thrownout = lsq->throwout;

   for(i = 0; i < lsq->ibisfile->nr; i++)
      if(dist[i] > thresh && !(thrownout[i]))
      {
         outliers[i] = 1;
         //         printf("i: %d %f\n", i, dist[i]);
      }
      else outliers[i] = 0;
}

/*=========================================================*/
void main44(void)
{
   int i, changed;
   int *outlier;
   LsqIbis *lsqIbis;
   //   LsqEquation *lsqEq;

   zifmessage("ibislsq9 version 24 Nov 2008");
   
   lsqIbis = LSQIBIS_getLsqIbis();

   LSQIBIS_solveAll(lsqIbis, 1);

   changed = 0;
   outlier = (int*)calloc(lsqIbis->ibisfile->nr, sizeof(int));
   if(lsqIbis->thresh > 0.0)
   {
      markOutliers(lsqIbis, outlier);

      /* for each point, if it is an outlier, do local fit on 10% of neighboring control points */
      for(i = 0; i < lsqIbis->ibisfile->nr; i++)
      {
         int npts;

         npts = (lsqIbis->IBIS2control)[i][0];
         npts = (lsqIbis->controlIDCnts)[npts];
         npts *= 0.1;
         assert(npts >= 10);
         if(outlier[i] && !LSQIBIS_passesLocalFit(lsqIbis, i, npts, 1))
         {
            (lsqIbis->throwout)[i] = 1;
            changed = 1;
         }
      }
   }

   if(!(lsqIbis->noprint) && changed)
   {
      for(i = 0; i < lsqIbis->ibisfile->nr; i++)
         if((lsqIbis->throwout)[i])
            printf("  ** Point %d thrown out.\n", i);
      printf("\n");
   }

   if(changed) LSQIBIS_solveAll(lsqIbis, 0);

   LSQIBIS_writeToIBIS(lsqIbis, 0);
   LSQIBIS_deleteLsqIbis(lsqIbis);

   free(outlier);
}
