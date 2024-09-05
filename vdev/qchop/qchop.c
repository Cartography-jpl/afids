#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"

/*  copy file, n bytes   A. Zobrist    5/21/00   */

void main44(void)
{
   int i,nb,ct,def;
   char infilename[99],outfilename[99],buf[2];
   FILE *infile,*outfile;
   
   zifmessage("qchop version Thu Jan  3 2008"); /*won't run with this*/
 
   zvp("NB",&nb,&ct);
   zvparm("inp",infilename,&ct,&def,1,0);
   zvparm("out",outfilename,&ct,&def,1,0);
   
   infile = fopen(infilename,"r");
   outfile = fopen(outfilename,"w");
   
   for (i=0;i<nb;i++)
      {
      fread(buf,1,1,infile);
      fwrite(buf,1,1,outfile);
      }
   
   fclose(outfile);
   printf("Copied %d bytes from %s to %s\n",nb,infilename,outfilename);
   return;
}
