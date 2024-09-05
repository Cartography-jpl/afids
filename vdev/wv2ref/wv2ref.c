#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carto/WV2Manager.h"
#include "carto/ImageUtils.h"

void setInps(VICAR_IMAGE **inps, char *multiMeta, char *panMeta, int *band)
{
   int i, status, inpcnt, dumcnt, dumdef;
   char fnames[9][IU_MAX_FNAME_LEN];

   status = zvparm("multimeta", multiMeta, &dumcnt, &dumdef, 1, IU_MAX_FNAME_LEN);
   if(status != 1) printf("Error getting multi band meta file.\n");
   status = zvparm("panmeta", panMeta, &dumcnt, &dumdef, 1, IU_MAX_FNAME_LEN);
   if(status != 1) printf("Error getting pan band meta file.\n");

   status = zvp("band", band, &dumcnt);
   if(status != 1) zmabend("Error getting band.\n");

   if(*band > 0)
   {
      inps[*band-1] = getVI_inp(1);
      return;
   }

   status = zvparm("inp", fnames, &inpcnt, &dumdef, 9, IU_MAX_FNAME_LEN);
   if(status != 1) zmabend("Check input files.\n");
   if(inpcnt != WV2_N_BANDS) zmabend("The number of input files must be 9.\n");

   for(i = 0; i < WV2_N_BANDS; i++)
   {
      printf("%d: %s %d\n", i, fnames[i], strlen(fnames[i]));
      if(strlen(fnames[i]) > 0)
         inps[i] = getVI_inp(i+1);
   }

}

void main44(void)
{
   int i, band;
   VICAR_IMAGE *inps[WV2_N_BANDS];
   WV2_MANAGER *wv2;
   char m_metafile[IU_MAX_FNAME_LEN], p_metafile[IU_MAX_FNAME_LEN];
   char line[100];

   for(i = 0; i < WV2_N_BANDS; i++)
      inps[i] = NULL;

   setInps(inps, m_metafile, p_metafile, &band);
   wv2 = WV2_getWV2Manager(inps, m_metafile, p_metafile);

   if(band == 0)
   {
      for(i = 0; i < WV2_N_BANDS; i++)
         WV2_createTOAReflectanceImage(wv2, i+1, i);
   }
   else
      WV2_createTOAReflectanceImage(wv2, 1, band-1);

   WV2_print(wv2);
}
