#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carto/GE1Manager.h"
#include "carto/ImageUtils.h"

void setInps(VICAR_IMAGE **inps, char *meta, int *band)
{
   int i, status, inpcnt, dumcnt, dumdef;
   char fnames[GE1_N_BANDS][IU_MAX_FNAME_LEN];

   status = zvparm("meta", meta, &dumcnt, &dumdef, 1, IU_MAX_FNAME_LEN);
   if(status != 1) printf("Error getting multi band meta file.\n");

   status = zvp("band", band, &dumcnt);
   if(status != 1) zmabend("Error getting band.\n");

   if(*band > 0)
   {
      inps[*band-1] = getVI_inp(1);
      return;
   }

   status = zvparm("inp", fnames, &inpcnt, &dumdef, GE1_N_BANDS, IU_MAX_FNAME_LEN);
   if(status != 1) zmabend("Check input files.\n");
   if(inpcnt != GE1_N_BANDS) zmabend("If band parameter is not specified then the number of input files must be 5.\n");

   for(i = 0; i < GE1_N_BANDS; i++)
   {
      printf("%d: %s %d\n", i, fnames[i], strlen(fnames[i]));
      if(strlen(fnames[i]) > 0)
         inps[i] = getVI_inp(i+1);
   }
}

void main44(void)
{
   int i, band;
   VICAR_IMAGE *inps[GE1_N_BANDS];
   GE1_MANAGER *ge1;
   char metafile[IU_MAX_FNAME_LEN];
   char line[100];

   for(i = 0; i < GE1_N_BANDS; i++)
      inps[i] = NULL;

   setInps(inps, metafile, &band);
   ge1 = GE1_getGE1Manager(inps, metafile);

   if(band == 0)
   {
      for(i = 0; i < GE1_N_BANDS; i++)
         GE1_createTOAReflectanceImage(ge1, i+1, i);
   }
   else
      GE1_createTOAReflectanceImage(ge1, 1, band-1);

   GE1_print(ge1);
}
