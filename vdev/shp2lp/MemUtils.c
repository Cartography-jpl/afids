#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ibiserrs.h"
#include "VicHdrs.h"
/********************************************************/
void mz_alloc1(buf,d1,w)
   int d1,w;
   unsigned char **buf;
{
   if ((*buf=(unsigned char *)malloc(1+d1*w))==NULL)
                            zmabend("malloc failed");
   return;
}

/********************************************************/
void mz_alloc2(buf,d1,d2,w)
   int d1,d2,w;
   unsigned char ***buf;
{
   int i;
   if ((*buf=(unsigned char **)malloc(1+d1*4))==NULL)
                            zmabend("malloc failed");
   for (i=0;i<d1;i++)
      if (((*buf)[i]=(unsigned char *)malloc(1+d2*w))==NULL)
                            zmabend("malloc failed");
   return;
}

/********************************************************/
void mz_free2(buf,d1)
   int d1; unsigned char **buf;
{
   int i;
   for (i=0;i<d1;i++) free(buf[i]);
   free(buf);
   return;
}                                                                  
