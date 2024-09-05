#include "vicmain_c.h"
#include "applic.h"

#include "carto/ImageUtils.h"

void main44(void)
{
   int i, j, nl, ns;

   VICAR_IMAGE *inp1, *inp2;
   VICAR_IMAGE *out;

   inp1=getVI_inp(1);
   inp2=getVI_inp(2);
   nl = inp1->nl;
   ns = inp1->ns;
   out=getVI_out("BYTE", 1, nl, ns);

   getValidMask(&inp2);
   for(i = 0; i < nl; i++)
   {
      readVicarImageLine(inp1, i);
      for(j = 0; j < ns; j++)
      {
         if(!inp2->valid[i][j]) out->buffer[j] = 0;
         else if(inp1->buffer[j] < 0.) out->buffer[j] = 1;
         else if(inp1->buffer[j] > .5) out->buffer[j] = 255;
         else out->buffer[j] = inp1->buffer[j]*510+1;
         /*
         else if(inp1->buffer[j] > 1.) out->buffer[j] = 255;
         else out->buffer[j] = inp1->buffer[j]*254+1;
         */
      }
      writeVicarImageLine(out, i);
   }

   deleteAndCloseImage(&inp1);
   deleteAndCloseImage(&inp2);
   deleteAndCloseImage(&out);
}
