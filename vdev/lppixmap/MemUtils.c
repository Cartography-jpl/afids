#include <math.h>
#include <stdio.h>
#include <string.h>

#include "ibiserrs.h"

/*=========================================================
 
mz_alloc1
 
allocate a one dimensional array of any type
 
arguments:
     1. buf: output, unsigned char **buf;
        contents set to pointer to array.
     2. d1: input, int d1;
        dimension of the array
     3. w: input, int w;
        number of bytes per array element
 
memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space may be
released with the free(buf) statement.

example:
   mz_alloc1((unsigned char **)&buf,totwid,1);
*/
void mz_alloc1(buf,d1,w)
   int d1,w;
   unsigned char **buf;
{
   if ((*buf=(unsigned char *)malloc(1+d1*w))==NULL)
                            zmabend("malloc failed");
   return;
}
                                                                                                                                                             
/*=========================================================
                                                                                                                                                             
mz_alloc2
                                                                                                                                                             
allocate a two dimensional array of any type
                                                                                                                                                             
arguments:
     1. buf: output, unsigned char ***buf;
        contents set to pointer to array.
     2. d1: input, int d1;
        first dimension of the array
     3. d2: input, int d2;
        second dimension of the array
     4. w: input, int w;
        number of bytes per array element
                                                                                                                                                             
memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free2(buf,d1) so all of the parts can be freed
in reverse order.

example:
   mz_alloc2((unsigned char ***)&buf,row,col,8);
*/
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
                                                                                                                                                             
/*=========================================================
                                                                                                                                                             
mz_free2
                                                                                                                                                             
free a two dimensional array created by mz_alloc2
                                                                                                                                                             
arguments:
     1. buf: output, unsigned char **buf;
        array to be freed (not a pointer)
     2. d1: input, int d1;
        first dimension of the array
                                                                                                                                                             
The subparts are freed first and then the top part.  Use
the first dimension from the mz_alloc2 call.
                                                                                                                                                             
*/
void mz_free2(buf,d1)
   int d1; unsigned char **buf;
{
   int i;
   for (i=0;i<d1;i++) free(buf[i]);
   free(buf);
   return;
}                                                                  
/*=========================================================*/
