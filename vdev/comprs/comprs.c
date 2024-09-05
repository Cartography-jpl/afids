#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

void main44(void)
{
   int i, unit1, unit2, status;
   int nl, ns, pixsize, cnt;
   char fmt[8];
   char *buf, comprstype[16];

   status = zvp("compress", comprstype, &cnt);
   if(status != 1) zabend();

   /* open files */
   status = zvunit(&unit1, "inp", 1, NULL);
   if(status != 1) zabend();
   status = zvopen(unit1, "OP", "READ", NULL);
   if(status != 1) zabend();
   status = zvget(unit1, "NL", &nl, "NS", &ns, "PIX_SIZE", &pixsize, "FORMAT", fmt, NULL);
   if(status != 1) zabend();

   status = zvunit(&unit2, "out", 1, NULL);
   if(status != 1) zabend();
   status = zvopen(unit2, "OP", "WRITE", "U_NL", nl, "U_NS", ns, "O_FORMAT", fmt, "U_FORMAT", fmt,
                   "COMPRESS", comprstype, "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
   if(status != 1) zabend();

   /* initialize data for output */
   buf = (char *)malloc(ns*pixsize);

   for(i = 0; i < nl; i++)
   {
      status = zvread(unit1, buf, "LINE", i+1, NULL);
      if(status != 1) zabend();
      status = zvwrit(unit2, buf, "LINE", i+1, NULL);
      if(status != 1) zabend();
   }

   free(buf);

   /* close image and dem files */
   zvclose(unit1, NULL);
   zvclose(unit2, NULL);
}
