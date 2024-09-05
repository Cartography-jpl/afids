#include "vicmain_c.h"
#include "applic.h"
#include <assert.h>
#include <stdio.h>

#include "carto/ImageUtils.h"
#include "LandsatManager.h"

void main44(void)
{
   int i;
   int status, band, dumcnt;
   char meta[IU_MAX_FNAME_LEN];
   LANDSAT_MANAGER *lm;
   FILE *f;
   VICAR_IMAGE *out;
   VICAR_IMAGE *inp[LANDSAT_N_BANDS];

   status = zvp("band", &band, &dumcnt);
   assert(status == 1);
   status = zvp("meta", meta, &dumcnt);
   assert(status == 1);

   if(band <= 5) --band;
   for(i = 0; i < LANDSAT_N_BANDS; i++)
      inp[i] = NULL;
   inp[band] = getVI_inp(1);

   out = getVI_out("REAL", 1, inp[band]->nl, inp[band]->ns);

   f = fopen(meta, "r");

   lm = LM_getLandsatManager2(inp, f);
   LM_createReflectanceImage(lm, out, band);

   LM_deleteLandsatManager(&lm);
   deleteAndCloseImage(&out);
}
