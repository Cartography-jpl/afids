#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "shapefil.h"
#include "VicHdrs.h"
#include "fileutils.h"
#include "MemUtils.h"

#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>

/*****************************************************
lpselect.c

Written by Peter Kim

This program display on screen the selected LP
entities.

8/3/07 - finalized
*****************************************************/

void main44()
{
   LPList* list;
   LPTble* tble;
   int status, ibis, i, inpParm;
   int nr, key, parmct, parmdf, fileCnt;
   unsigned int *data;

   inpParm = 1;
   ibis = openIBISInp(inpParm++);
   list = slp2LPList(ibis);
   status = IBISFileClose(ibis, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   zvpcnt("inp", &fileCnt);

   if(fileCnt == 3)
   {
      ibis = openIBISInp(inpParm++);
      tble = dlp2LPTble(ibis);
      status = IBISFileClose(ibis, 0);
      if(status != 1) IBISSignal(ibis, status, 1);
   }

   ibis = openIBISInp(inpParm);
   IBISFileGet(ibis, "nr", &nr, 0, 0, 0);
   zvparm("key", &key, &parmct, &parmdf, 1, 0);
   mz_alloc1((unsigned char**)&data, nr, sizeof(double));
   status = IBISColumnRead(ibis, (void*)data, key, 1, nr);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileClose(ibis, 0);
   if(status != 1) IBISSignal(ibis, status, 1);

   printf("\n=============   Selected Entities   ===========\n");
   for(i = 0; i < nr; i++)
   {
      if(data[i]-1 < 0 || data[i]-1 > list->numOfEntities || data[i]-1 > tble->numOfEntities)
         zmabend("Selected Entity is out of bounds.\n");
      printf("Selecting entity %d.\n", data[i]);
      if(fileCnt == 3) printTbleEntity(tble, data[i]-1);
      printListEntity(list, data[i], -1);
      printf("\n");
   }

   free(data);
   destroyLPList(list);
   IBISFileClose(ibis, 0);
}

