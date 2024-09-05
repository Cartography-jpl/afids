#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carto/PleiadesManager.h"
#include "carto/ImageUtils.h"

void setInps(VICAR_IMAGE **inps, char *multiMeta, char *panMeta, int *band)
{
   int i, status, inpcnt, dumcnt, dumdef;
   char fnames[PLEIADES_N_BANDS][IU_MAX_FNAME_LEN];

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

   status = zvparm("inp", fnames, &inpcnt, &dumdef, PLEIADES_N_BANDS, IU_MAX_FNAME_LEN);
   if(status != 1) zmabend("Check input files.\n");
   if(inpcnt != PLEIADES_N_BANDS) zmabend("The number of input files must be 5.\n");

   for(i = 0; i < PLEIADES_N_BANDS; i++)
   {
      printf("%d: %s %d\n", i, fnames[i], (int)strlen(fnames[i]));
      if(strlen(fnames[i]) > 0)
         inps[i] = getVI_inp(i+1);
   }

}

void main44(void)
{
   int i, band;
   VICAR_IMAGE *inps[PLEIADES_N_BANDS];
   PLDS_MANAGER *plds;
   char m_metafile[IU_MAX_FNAME_LEN], p_metafile[IU_MAX_FNAME_LEN];

   for(i = 0; i < PLEIADES_N_BANDS; i++)
      inps[i] = NULL;

   setInps(inps, m_metafile, p_metafile, &band);
   plds = PLDS_getPLDSManager(inps, m_metafile, p_metafile);

   if(band == 0)
   {
      for(i = 0; i < PLEIADES_N_BANDS; i++) {
         printf("creating reflectance image: %d\n", i);
         PLDS_createTOAReflectanceImage(plds, i+1, i);
      }
   }
   else {
      printf("creating reflectance image for band %d.\n", band);
      PLDS_createTOAReflectanceImage(plds, 1, band-1);
   }

   PLDS_print(plds);
}
