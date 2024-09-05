#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"

/*  copy file, n bytes   A. Zobrist    5/21/00   */

void main44(void)
{
   int i,nb,ct,def;
   char infilename[99],outfilename[99],buf[2];
   FILE *infile,*outfile;
    char message[100];
  
   /*zifmessage("lpin version 21-may-00"); won't run with this*/
 
    zvmessage ("lpin - 10-18-2019 -rjb - 64-bit"," ");    
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
   sprintf(message,"Copied %d bytes from %s to %s\n",nb,infilename,outfilename);
    zvmessage(message," ");
}
