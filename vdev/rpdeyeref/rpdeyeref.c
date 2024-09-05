#include "vicmain_c.h"
#include "applic.h"
#include <assert.h>
#include <stdio.h>

#include "carto/ImageUtils.h"
#include "carto/RapidEyeManager.h"

void main44(void)
{
   int i;
   int status, band, dumcnt;
   char meta[IU_MAX_FNAME_LEN];
   RAPIDEYE_MANAGER *rem;
   FILE *f;
   VICAR_IMAGE *out;
   VICAR_IMAGE *inp[RAPIDEYE_N_BANDS];

   status = zvp("band", &band, &dumcnt);
   assert(status == 1);
   status = zvp("meta", meta, &dumcnt);
   assert(status == 1);

   --band;
   for(i = 0; i < RAPIDEYE_N_BANDS; i++)
      inp[i] = NULL;
   inp[band] = getVI_inp(1);

   out = getVI_out("REAL", 1, inp[band]->nl, inp[band]->ns);

   f = fopen(meta, "r");

   rem = RE_getRapidEyeManager(inp, f);
   RE_createReflectanceImage(rem, out, band);

   RE_deleteRapidEyeManager(&rem);
   deleteAndCloseImage(&inp[band]);
   deleteAndCloseImage(&out);
}
