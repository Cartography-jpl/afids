#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carto/QBManager.h"
#include "carto/ImageUtils.h"

void setInps(VICAR_IMAGE **inps, char *multiMeta, char *panMeta, int *band)
{
   int i, status, inpcnt, dumcnt, dumdef;
   char fnames[QB_N_BANDS][IU_MAX_FNAME_LEN];

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

   status = zvparm("inp", fnames, &inpcnt, &dumdef, QB_N_BANDS, IU_MAX_FNAME_LEN);
   if(status != 1) zmabend("Check input files.\n");
   if(inpcnt != QB_N_BANDS) zmabend("The number of input files must be 9.\n");

   for(i = 0; i < QB_N_BANDS; i++)
   {
      printf("%d: %s %d\n", i, fnames[i], strlen(fnames[i]));
      if(strlen(fnames[i]) > 0)
         inps[i] = getVI_inp(i+1);
   }

}

void main44(void)
{
   int i, band;
   VICAR_IMAGE *inps[QB_N_BANDS];
   QB_MANAGER *qb;
   char m_metafile[IU_MAX_FNAME_LEN], p_metafile[IU_MAX_FNAME_LEN];

   for(i = 0; i < QB_N_BANDS; i++)
      inps[i] = NULL;

   setInps(inps, m_metafile, p_metafile, &band);
   qb = QB_getQBManager(inps, m_metafile, p_metafile);

   if(band == 0)
   {
      for(i = 0; i < QB_N_BANDS; i++)
         QB_createTOAReflectanceImage(qb, i+1, i);
   }
   else
      QB_createTOAReflectanceImage(qb, 1, band-1);

   QB_print(qb);
}
