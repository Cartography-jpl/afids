#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

/*=========================================================*/
int getCorrespondingIndex(i, ns)
  int i, ns;
{
   if(i < ns/2) return (ns+1)/2 + i;

   return i-(ns/2);
}

/*=========================================================*/
void output(inBuf, outBuf, i, j, inI, inJ, fmt)
   char **inBuf, **outBuf, fmt[8];
   int i, j, inI, inJ;
{
   if(strcmp(fmt, "REAL") == 0)
      ((float*)(outBuf[i]))[j] = ((float*)(inBuf[inI]))[inJ];
   else if(strcmp(fmt, "FULL") == 0)
      ((int*)(outBuf[i]))[j] = ((int*)(inBuf[inI]))[inJ];
   else if(strcmp(fmt, "DOUB") == 0)
      ((double*)(outBuf[i]))[j] = ((double*)(inBuf[inI]))[inJ];
   else if(strcmp(fmt, "HALF") == 0)
      ((short int*)(outBuf[i]))[j] = ((short int*)(inBuf[inI]))[inJ];
   else
      outBuf[i][j] = inBuf[inI][inJ];
}

/*=========================================================*/
void swap(inBuf, outBuf, nl, ns, fmt, forward, reverse)
   int nl, ns, forward, reverse;
   char **inBuf, **outBuf, fmt[];
{
   int i, j;

   for(i = 0; i < nl; i++)
   {
      for(j = 0; j < ns; j++)
      {
	 int inI, inJ;
	 inI = getCorrespondingIndex(i, nl);
	 inJ = getCorrespondingIndex(j, ns);

	 if(strcmp(fmt, "COMP") == 0 && forward)
	 {
	    ((float*)(outBuf[i]))[j*2] = ((float*)(inBuf[inI]))[inJ*2];
	    ((float*)(outBuf[i]))[j*2+1] = ((float*)(inBuf[inI]))[inJ*2+1];
	 }
	 else if(strcmp(fmt, "COMP") == 0 && reverse)
	 {
	    ((float*)(outBuf[inI]))[inJ*2] = ((float*)(inBuf[i]))[j*2];
	    ((float*)(outBuf[inI]))[inJ*2+1] = ((float*)(inBuf[i]))[j*2+1];
	 }
	 else if(forward) output(inBuf, outBuf, i, j, inI, inJ, fmt);
	 else if(reverse) output(inBuf, outBuf, inI, inJ, i, j, fmt);
	 else zmabend("Error in swap function in qswp2005.");
      }
   }
}

/*=========================================================*/

void main44(void)
{
   int inUnit, outUnit;
   char **inBuf, **outBuf;
   int nl, ns, pixSize, bufNS;
   int forward, reverse;
   int status, i;
   char fmt[8];

   zifmessage("qswp2005 version Thu Jan  3 2008");

   forward = zvptst("forward");
   reverse = zvptst("reverse");

   /*open input file*/
   status = zvunit(&inUnit, "inp", 1, NULL);
   if(status != 1) IBISSignalU(inUnit, status, 1);
   status = zvopen(inUnit, "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
   if(status != 1) IBISSignalU(inUnit, status, 1);
   status = zvget(inUnit, "PIX_SIZE", &pixSize, "NL", &nl, "NS", &ns, "FORMAT", fmt, NULL);
   if(status != 1) IBISSignalU(inUnit, status, 1);

   /*open output file*/
   status = zvunit(&outUnit, "out", 1, NULL);
   if(status != 1) IBISSignalU(outUnit, status, 1);
   status = zvopen(outUnit, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "U_NL", nl, "U_NS", ns, "U_FORMAT", fmt, "O_FORMAT", fmt, NULL);
   if(status != 1) IBISSignalU(outUnit, status, 1);

   /*allocate buffer memory*/
   if(strcmp(fmt, "COMP") == 0) bufNS = ns*2;
   else bufNS = ns;
   mz_alloc2((unsigned char***) &inBuf, nl, bufNS, pixSize);
   mz_alloc2((unsigned char***) &outBuf, nl, bufNS, pixSize);

   /*read input file into input buffer*/
   for(i = 0; i < nl; i++)
   {
      status = zvread(inUnit, inBuf[i], "LINE", i+1, NULL);
      if(status != 1) IBISSignalU(inUnit, status, 1);
   }

   /*fill out buffer*/
   swap(inBuf, outBuf, nl, ns, fmt, forward, reverse);

   /*write to output file*/
   for(i = 0; i < nl; i++)
   {
      status = zvwrit(outUnit, outBuf[i], "LINE", i+1, NULL);
      if(status != 1) IBISSignalU(outUnit, status, 1);
   }

   /*close files*/
   zvclose(inUnit, NULL);
   zvclose(outUnit, NULL);

   /*free buffers*/
   mz_free2((unsigned char**) inBuf, nl);
   mz_free2((unsigned char**) outBuf, nl);
}
