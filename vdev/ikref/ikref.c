#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carto/IKManager.h"
#include "carto/ImageUtils.h"

void setInps(VICAR_IMAGE **inps, char *meta, int *band)
{
   int i, status, inpcnt, dumcnt, dumdef;
   char fnames[IK_N_BANDS][IU_MAX_FNAME_LEN];

   status = zvparm("meta", meta, &dumcnt, &dumdef, 1, IU_MAX_FNAME_LEN);
   if(status != 1) printf("Error getting multi band meta file.\n");

   status = zvp("band", band, &dumcnt);
   if(status != 1) zmabend("Error getting band.\n");

   if(*band > 0)
   {
      inps[*band-1] = getVI_inp(1);
      return;
   }

   status = zvparm("inp", fnames, &inpcnt, &dumdef, IK_N_BANDS, IU_MAX_FNAME_LEN);
   if(status != 1) zmabend("Check input files.\n");
   if(inpcnt != IK_N_BANDS) zmabend("If band parameter is not specified then the number of input files must be 5.\n");

   for(i = 0; i < IK_N_BANDS; i++)
   {
      printf("%d: %s %d\n", i, fnames[i], strlen(fnames[i]));
      if(strlen(fnames[i]) > 0)
         inps[i] = getVI_inp(i+1);
   }
}

void main44(void)
{
   int i, band;
   VICAR_IMAGE *inps[IK_N_BANDS];
   IK_MANAGER *ik;
   char metafile[IU_MAX_FNAME_LEN];
   char line[100];

   for(i = 0; i < IK_N_BANDS; i++)
      inps[i] = NULL;

   setInps(inps, metafile, &band);
   ik = IK_getIKManager(inps, metafile);

   if(band == 0)
   {
      for(i = 0; i < IK_N_BANDS; i++)
         IK_createTOAReflectanceImage(ik, i+1, i);
   }
   else
      IK_createTOAReflectanceImage(ik, 1, band-1);

   IK_print(ik);
}
