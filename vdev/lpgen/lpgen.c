#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "VicHdrs.h"
#include "fileutils.h"
#include "MemUtils.h"
#include "strings.h"

/***********************************************************
lpgen.c

Written by P. Kim

This program creates a .dlp file from the given .slp file

8/3/07 - finalized
***********************************************************/

void main44()
{
   LPList* list;
   LPTble* tble;
   int ibis, i;

   ibis = openIBISInp(1);
   list = slp2LPList(ibis);
   IBISFileClose(ibis, 0);

   printf("\tGenerating LPTble from LPList data.\n");
   tble = (LPTble*)calloc(1, sizeof(LPTble));
   tble->numOfEntities = list->numOfEntities;
   tble->recordSize = sizeof(int) + 17;
   tble->numOfColumns = 2;
   mz_alloc1((unsigned char**)&(tble->colLoc), tble->numOfColumns, sizeof(int));
   (tble->colLoc)[0] = 0;
   (tble->colLoc)[1] = 4;
   strcpy(tble->fmt[0], "FULL");
   strcpy(tble->fmt[1], "FULL");
   strcpy(tble->fmt[2], "A16");

   mz_alloc1((unsigned char**)&(tble->data), tble->recordSize*tble->numOfEntities, 1);
   for(i = 0; i < list->numOfEntities; i++)
   {
      int index;
      char hwyName[16];
      index = getTbleIndex(tble, i, 0);
      ((int*)((char*)(tble->data)+index))[0] = i+1;
      index = getTbleIndex(tble, i, 1);
      sprintf(hwyName, "HWY %d", i+1);
      strcpy(((char*)(tble->data)+index), hwyName);
   }
   ibis = openIBISOutDLP(tble->numOfColumns, tble->numOfEntities, tble->fmt, 1);
   printDLP(ibis, tble);
   IBISFileClose(ibis, 0);
   destroyLPTble(tble);
   destroyLPList(list);
}




