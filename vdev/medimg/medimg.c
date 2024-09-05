#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_statistics.h>

#include "carto/ImageUtils.h"

/**********************************************/
double getMedian(double *buf, int cnt)
{
   int upper, lower, mid, med_avg, index;

   upper = zvptst("upper");
   lower = zvptst("lower");
   mid = zvptst("mid");

   if(cnt%2 == 0 && (lower||mid)) index = cnt/2 - 1;
   else index = cnt/2;
   if(cnt%2 == 0 && mid) med_avg = 1;
   else med_avg = 0;

   gsl_sort(buf, 1, cnt);

   if(med_avg)
      return (buf[index] + buf[index+1])/2;

   return buf[index];
}

/**********************************************/
void fillPixBuf(VICAR_IMAGE **vi, double* buf, int pix, int cnt)
{
   int i;

   for(i = 0; i < cnt; i++)
      buf[i] = vi[i]->buffer[pix];
}

/**********************************************/
void main44(void)
{
   VICAR_IMAGE **vi;
   VICAR_IMAGE *out;
   double *pixBuf;
   int cnt, status, i, j, nl, ns;
   int median, mean, sig, var, min, max;
   char fmt[8];
   int fmtcnt, dumdef;

   nl = ns = 0;
   median = zvptst("median");
   mean = zvptst("mean");
   sig = zvptst("sig");
   var = zvptst("var");
   min = zvptst("min");
   max = zvptst("max");
   status = zvparm("outfmt", fmt, &fmtcnt, &dumdef, 1, 8);
   assert(status == 1);

   status = zvpcnt("inp", &cnt);
   assert(status == 1);
   pixBuf = (double*)calloc(cnt, sizeof(double));
   vi = (VICAR_IMAGE**)malloc(sizeof(VICAR_IMAGE*)*cnt);
   assert(vi != NULL);
   for(i = 0; i < cnt; i++)
   {
      vi[i] = getVI_inp(i+1);

      if(i == 0)
      {
         nl = vi[i]->nl;
         ns = vi[i]->ns;
         continue;
      }
      if(nl != vi[i]->nl || ns != vi[i]->ns)
      {
         printf("-->All input images must be the same size.\n");
         zabend();
      }
   }

   if(fmtcnt == 1) out = getVI_out(fmt, 1, nl, ns);
   else if(sig || var) out = getVI_out("real", 1, nl, ns);
   else out = getVI_out(vi[0]->format, 1, nl, ns);

   for(i = 0; i < nl; i++)
   {
      for(j = 0; j < cnt; j++) readVicarImageLine(vi[j], i);

      for(j = 0; j < ns; j++)
      {
         fillPixBuf(vi, pixBuf, j, cnt);

         if(median) out->buffer[j] = getMedian(pixBuf, cnt);
         else if(mean) out->buffer[j] = gsl_stats_mean(pixBuf, 1, cnt);
         else if(sig) out->buffer[j] = gsl_stats_sd(pixBuf, 1, cnt);
         else if(var) out->buffer[j] = gsl_stats_variance(pixBuf, 1, cnt);
         else if(min) out->buffer[j] = gsl_stats_min(pixBuf, 1, cnt);
         else if(max) out->buffer[j] = gsl_stats_max(pixBuf, 1, cnt);
         else assert(0);
      }

      writeVicarImageLine(out, i);
   }

   for(i = 0; i < cnt; i++) deleteAndCloseImage(&(vi[i]));
   free(vi);
   deleteAndCloseImage(&out);
   free(pixBuf);
}
