#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

#include "carto/ImageUtils.h"
#include "carto/ibishelper.h"

/* prototypes */
int getPaint(IBISStruct *colormap, int *cols, int dn, int bdn);

/*================================================================*/
int getPaint(IBISStruct *colormap, int *cols, int dn, int bdn)
{
   int i;

   for(i = 0; i < colormap->nr; i++)
      if(IBISHELPER_getInt(colormap, cols[0], i) == dn)
         return IBISHELPER_getInt(colormap, cols[1], i);

   return bdn;
}

void main44(void)
{
   int i, bdn, status, dumcnt, dumdef, cols[2];
   VICAR_IMAGE *inp, *out;
   IBISStruct *colormap;

    zvmessage ("paintc - 10-06-2019 - rjb - 64-bit"," ");
   inp = getVI_inp(1);
   colormap = IBISHELPER_openIBIS("inp", 2, "read");
   out = getVI_out(inp->format, 1, inp->nl, inp->ns);
   status = zvp("bdn", &bdn, &dumcnt);
   assert(status == 1);
   status = zvparm("cols", cols, &dumcnt, &dumdef, 2, 0);
   assert(status == 1);

   (cols[0])--;
   (cols[1])--;
   for(i = 0; i < inp->nl; i++)
   {
      int j;

      readVicarImageLine(inp, i);
      for(j = 0; j < inp->ns; j++)
         out->buffer[j] = getPaint(colormap, cols, inp->buffer[j], bdn);

      writeVicarImageLine(out, i);
   }

   IBISHELPER_closeIBIS(&colormap);
   deleteAndCloseImage(&inp);
   deleteAndCloseImage(&out);
}
