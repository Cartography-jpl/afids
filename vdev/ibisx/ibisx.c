#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

#include "ibisfile.h"
#include "ibiserrs.h"
#include "carto/ibishelper.h"

#ifndef MAXCOLS
#define MAXCOLS 500
#endif

void printHeader(IBISStruct *ibis)
{
   int i, j, line[MAXCOLS];
   char *formats[MAXCOLS];

   printf("\n");
   for(i = 0; i < ibis->nc; i++)
   {
      formats[i] = (ibis->formats)[i];

      if(!strcmp("byte", formats[i]) ||
         !strcmp("half", formats[i]) ||
         !strcmp("full", formats[i])) line[i] = 13;
      else if(!strcmp("real", formats[i]) ||
              !strcmp("doub", formats[i])) line[i] = 13;
      else if(!strcmp("comp", formats[i])) line[i] = 21;
      else if(formats[i][0] == 'a') line[i] = (ibis->colLens)[i] + 1;
      else IBISHELPER_wrongFormatError(ibis, i);
   }

   printf("      ");
   for(i = 0; i < ibis->nc; i++)
   {
      for(j = 0; j < line[i] - 1; j++) printf("-");
      printf("+");
   }
   printf("\n");

   printf("      ");
   for(i = 0; i < ibis->nc; i++)
   {
      int numlen;

      numlen = 1;
      while(i/(numlen*10) != 0) numlen++;
      for(j = 0; j < line[i] - (3 + numlen); j++) printf(" ");
      printf("C:");
      printf("%d", i);
      printf(" ");
   }
   printf("\n");

   printf("      ");
   for(i = 0; i < ibis->nc; i++)
   {
      for(j = 0; j < line[i] - 1; j++) printf("-");
      printf("+");
   }
   printf("\n");
}

void printRecord(IBISStruct *ibis, int index, char fmtstr[MAXCOLS][30])
{
   int i, *lengths;
   char *elem;

   printf("     ");

   lengths = ibis->colLens;
   for(i = 0; i < ibis->nc; i++)
   {
      //      getElement(ibis, i, index, &elem);
      elem = IBISHELPER_getBufPtr(ibis, i, index);
      IBISPRINT( elem, (ibis->formats)[i][0], fmtstr[i]);
   }

   printf("\n");
}

void getStatistics(IBISStruct *ibis, int *printFlags, double *mean, double *stdv, int nr)
{
   int i, nElems;
   double *sum;

   sum = (double *)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   if(!sum) zabend();

   nElems = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      if(printFlags[i])
      {
         int j;

         for(j = 0; j < ibis->nc; j++)
         {
            char *format;
            char *elem;

            format = (ibis->formats)[j];
            if(format[0] != 'a') elem = IBISHELPER_getBufPtr(ibis, j, i); //getElement(ibis, j, i, &elem);
            else continue;

            if(!strcmp(format, "byte")) sum[j*2] += *((char*)elem);
            if(!strcmp(format, "half")) sum[j*2] += *((short int*)elem);
            if(!strcmp(format, "full")) sum[j*2] += *((int*)elem);
            if(!strcmp(format, "real")) sum[j*2] += *((float*)elem);
            if(!strcmp(format, "doub")) sum[j*2] += *((double*)elem);
            if(!strcmp(format, "comp"))
            {
               sum[j*2] += *((float*)elem);
               sum[j*2 + 1] += *((float*)(elem + sizeof(float)));
            }
         }

         ++nElems;
      }
   }

   if(nElems > 0)
      for(i = 0; i < ibis->nc; i++) mean[i*2] = sum[i*2]/nElems;

   for(i = 0; i < ibis->nr; i++)
   {
      if(printFlags[i])
      {
         int j;

         for(j = 0; j < ibis->nc; j++)
         {
            char *format;
            char *elem;

            format = (ibis->formats)[j];
            if(format[0] != 'a') elem = IBISHELPER_getBufPtr(ibis, j, i); //getElement(ibis, j, i, &elem);
            else continue;

            if(!strcmp(format, "byte")) stdv[j*2] += pow(*((char*)elem) - mean[j*2], 2);
            if(!strcmp(format, "half")) stdv[j*2] += pow(*((short int*)elem) - mean[j*2], 2);
            if(!strcmp(format, "full")) stdv[j*2] += pow(*((int*)elem) - mean[j*2], 2);
            if(!strcmp(format, "real")) stdv[j*2] += pow(*((float*)elem) - mean[j*2], 2);
            if(!strcmp(format, "doub")) stdv[j*2] += pow(*((double*)elem) - mean[j*2], 2);
            if(!strcmp(format, "comp"))
            {
               stdv[j*2] += pow(*((float*)elem) - mean[j*2], 2);
               stdv[j*2 + 1] += pow(*((float*)(elem + sizeof(float))) - mean[j*2 + 1], 2);
            }
         }
      }
   }

   for(i = 0; i < ibis->nc; i++)
   {
      if(stdv[i*2] > 10e-10) stdv[i*2] = sqrt(stdv[i*2]/(nElems - 1));
      if(stdv[i*2 + 1] > 10e-10) stdv[i*2 + 1] = sqrt(stdv[i*2 + 1]/(nElems - 1));
   }
}

void printStatistics(IBISStruct *ibis, char fmtstrs[MAXCOLS][30], double *mean, double *stdv)
{
   int i, line[MAXCOLS];
   char doubfmt[30], compfmt[30], *formats[MAXCOLS];

   /* print separating line */
   for(i = 0; i < ibis->nc; i++)
   {
      formats[i] = (ibis->formats)[i];

      if(!strcmp("byte", formats[i]) ||
         !strcmp("half", formats[i]) ||
         !strcmp("full", formats[i])) line[i] = 13;
      else if(!strcmp("real", formats[i]) ||
              !strcmp("doub", formats[i])) line[i] = 13;
      else if(!strcmp("comp", formats[i])) line[i] = 21;
      else if(formats[i][0] == 'a') line[i] = (ibis->colLens)[i] + 1;
      else IBISHELPER_wrongFormatError(ibis, i);
   }

   printf("      ");
   for(i = 0; i < ibis->nc; i++)
   {
      int j;

      for(j = 0; j < line[i] - 1; j++) printf("-");
      printf("+");
   }
   printf("\n");

   IBISFORMAT(doubfmt, 'd', 13);
   IBISFORMAT(compfmt, 'c', 21);

   /* print the mean */
   printf("MEAN:");
   for(i = 0; i < ibis->nc; i++)
   {
      char *type;

      type = (ibis->formats)[i];
      if(type[0] == 'a')
      {
         printf(fmtstrs[i]," ");
         continue;
      }
      else if(!strcmp("comp", type))
         printf(compfmt, (float)(mean[i*2]), (float)(mean[i*2 + 1]));
      else
         printf(doubfmt, mean[i*2]);
   }
   printf("\n");

   /* print the standard deviation */
   printf("STDV:");
   for(i = 0; i < ibis->nc; i++)
   {
      char *type;

      type = (ibis->formats)[i];
      if(type[0] == 'a')
      {
         printf(fmtstrs[i]," ");
         continue;
      }
      else if(!strcmp("comp", type))
         printf(compfmt, (float)(stdv[i*2]), (float)(stdv[i*2 + 1]));
      else
         printf(doubfmt, stdv[i*2]);
   }
   printf("\n");
}

void main44(void)
{
   int i, skip, randflag, seed, sr, nr, parmcnt, *printFlags, cnt, status, percent;
   IBISStruct *ibis;
   char **data;
   double *mean, *stdv;
   char fmtstrs[MAXCOLS][30];

   /* get user input parameters */
   status = zvp("skip", &skip, &parmcnt);
   if(status != 1) zabend();
   randflag = zvptst("random");
   seed = zvptst("seed");
   status = zvp("sr", &sr, &parmcnt);
   if(status != 1) zabend();
   status = zvp("percent", &percent, &parmcnt);
   if(status != 1) zabend();
   status = zvp("nr", &nr, &parmcnt);
   if(status != 1) zabend();

   /* open ibis file */
   ibis = IBISHELPER_openIBIS("inp", 1, "read");

   /* allocate buffers */
   mean = (double*)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   stdv = (double*)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   printFlags = (int*)calloc(sizeof(int)*ibis->nr, sizeof(int));
   if(!printFlags || !stdv || !printFlags)
   {
      printf("Error while allocating buffer.\n");
      if(!mean) printf("CHECK MEAN BUFFER ALLOCATION.\n");
      if(!stdv) printf("CHECK STDV BUFFER ALLOCATION.\n");
      if(!printFlags) printf("CHECK PRINTFLAGS BUFFER ALLOCATION.\n");
      zabend();
   }

   /* preprocess parameters and decide which rows to print */
   if(sr > ibis->nr)
   {
      printf("---> Number of rows in ibis file: %d\n", ibis->nr);
      printf("---> Start row specified        : %d\n", sr);
   }

   if(skip && randflag)
      printf("---> Both skip and random chosen... Ignoring random and doing skip.\n");

   if(nr && percent)
      printf("---> Both nr and percent specified... Ignoring nr and doing percent.\n");

   cnt = 0;
   if(percent > 0) nr = (percent/100.0)*ibis->nr;
   else if(nr == 0 || nr > ibis->nr - sr) nr = ibis->nr - sr;
   if((!randflag && !skip) || nr == ibis->nr - sr)
   {
      for(i = sr; i < ibis->nr; i++)
      {
         printFlags[i] = 1;
         cnt++;
         if(cnt >= nr) break;
      }
   }
   else if(skip)
   {

      for(i = sr; i < ibis->nr && cnt < nr; i+=skip)
      {
         printFlags[i] = 1;
         cnt++;
      }
   }
   else
   {
      if(seed) srand(time(0));

      while(cnt < nr)
      {
         int randIndex;

         randIndex = (rand()/((double)RAND_MAX))*ibis->nr;

         if(!printFlags[randIndex] && randIndex >= sr)
         {
            printFlags[randIndex] = 1;
            cnt++;
         }
      }
   }

   /* get data and display */
   data = ibis->data;

   IBISHELPER_getFormats(ibis, fmtstrs);
   printHeader(ibis);
   for(i = sr; i < ibis->nr; i++)
      if(printFlags[i]) printRecord(ibis, i, fmtstrs);

   /* get the statistics */
   getStatistics(ibis, printFlags, mean, stdv, nr);

   /* print statistics */
   printStatistics(ibis, fmtstrs, mean, stdv);

   /* free data */
   free(printFlags);
   free(mean);
   free(stdv);

   /* close ibis file */
   IBISHELPER_closeIBIS(ibis);
}
