#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoTaeUtils.h"

/*  parse ascii file to fill a TAE TCL variable   P. Kim  4/17/09   */
void main44(void)
{
   int cnt, def, keycnt, i;
   double dval;
   char numchars[14] = "+-.0123456789";
   char fname[50], keyword[100], *fbuf, *bufptr;
   int lineno, seq, fsize, first_occurrence;
   FILE *f;

   zifmessage("ascnum version Wed Apr 17 2009");
   
   /* get the parameters */
   
   zvparm("inp", fname, &cnt, &def, 1, 0);
   zvp("lineno", &lineno, &cnt);
   zvp("sequence", &seq, &cnt);
   zvparm("keyword",keyword, &keycnt, &def, 1, 100);

   if (def==1) keycnt = 1;

   f = fopen(fname, "r");
   fseek(f, 0, SEEK_SET);
   fsize = ftell(f);
   fseek(f, 0, SEEK_END);
   fsize = ftell(f) - fsize;
   fseek(f, 0, SEEK_SET);

   fbuf = malloc(fsize+1);
   fread(fbuf, sizeof(char), fsize, f);
   fbuf[fsize] = 0;
   bufptr = fbuf;

   /* skip to lineno */
   for(i = 0; i < lineno-1; i++)
   {
      bufptr = strchr(bufptr, '\n');
      if(bufptr == NULL)
         zmabend("Lineno exceeds the number of lines in the ascii file.\n");
      else
         bufptr++;
   }

   /* skip to sequence */
   for(i = 0; i < seq; i++)
   {
      bufptr = strstr(bufptr, keyword);
      if(bufptr == NULL)
         zmabend("Not enough occurrences of the keyword in the ascii file.\n");
      else
         bufptr++;
   }
   bufptr += strlen(keyword)-1;

   first_occurrence = strcspn(bufptr, numchars);
   if(first_occurrence == strlen(bufptr))
      zmabend("No number found after keyword.\n");
   bufptr += strcspn(bufptr, numchars);
   dval = atof(bufptr);

   mq_out_real("val", dval);

   /* close and exit */
   free(fbuf);   
   fclose(f);

   return;
}
