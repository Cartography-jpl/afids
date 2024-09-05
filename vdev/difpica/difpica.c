#include "vicmain_c.h"
#include "applic.h"
#include "carto/cartoVicarProtos.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Quick and dirty difpic to abort if 2 input files
   differ. If they differ, then abort. */
void main44(void)
{
   int i, status, allocsize, totns;
   int unit1, nl1, ns1, pixsize1;
   int unit2, nl2, ns2, pixsize2;
   char fmt1[8], fmt2[8];
   double *row1, *row2;

   zifmessage("difpica version Fri Apr 18 2008");

   /* open files */
   status = zvunit(&unit1, "inp", 1, NULL);
   if(status != 1) zabend();
   status = zvopen(unit1, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   if(status != 1) zabend();
   status = zvget(unit1, "NL", &nl1, "NS", &ns1, "PIX_SIZE", &pixsize1, "FORMAT", fmt1, NULL);
   if(status != 1) zabend();

   status = zvunit(&unit2, "inp", 2, NULL);
   if(status != 1) zabend();
   status = zvopen(unit2, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   if(status != 1) zabend();
   status = zvget(unit2, "NL", &nl2, "NS", &ns2, "PIX_SIZE", &pixsize2, "FORMAT", fmt2, NULL);
   if(status != 1) zabend();

   if(nl1 != nl2)
   {
      printf("Number of lines differ.\n");
      zabend();
   }
   if(ns1 != ns2)
   {
      printf("Number of samples differ.\n");
      zabend();
   }
   if(strcmp(fmt1, fmt2) != 0)
   {
      printf("Formats differ.\n");
      zabend();
   }
   if(pixsize1 != pixsize2)
   {
      printf("Pixel sizes differ.\n");
      zabend();
   }

   allocsize = ns1*sizeof(double);
   totns = ns1;
   if(strcmp(fmt1, "COMP") == 0)
   {
      allocsize *= 2;
      totns *= 2;
   }

   row1 = malloc(allocsize);
   row2 = malloc(allocsize);

   for(i = 0; i < nl1; i++)
   {
      int j;

      status = zvread(unit1, row1, "LINE", i+1, NULL);
      if(status != 1) zabend();
      status = zvread(unit2, row2, "LINE", i+1, NULL);
      if(status != 1) zabend();

      for(j = 0; j < totns; j++)
 	 if(fabs(row1[j] - row2[j]) > 1.0E-10)
         {
 	    printf("line: %d samp: %d\n", i, j);
	    printf("row1: %f row2: %f\n", row1[j], row2[j]);
            printf("Data differs.\n");
            zabend();
         }
   }

   printf("***** The two files do not differ. *****\n\n");

   free(row1);
   free(row2);

   /* close image and dem files */
   zvclose(unit1, NULL);
   zvclose(unit2, NULL);
}
