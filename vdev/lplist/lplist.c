#include "vicmain_c.h"
#include "applic.h"

/*#include <stdio.h>*/
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "fileutils.h"
#include "VicHdrs.h"

/************************************************************
lplist.c

Written by P. Kim

This program displays the LP shape and tabular data.

5/8/07 - finalized
************************************************************/

void main44()
{
   int i, ibis;
   int startEntity, numOfEntities, endEntity, nVertices;
   int parmct, parmdf, fileCount;
   LPList *list;
   LPTble *tble;

   ibis = openIBISInp(1);
   list = slp2LPList(ibis);
   IBISFileClose(ibis, 0);
   zvpcnt("inp", &fileCount);
   if(fileCount == 2)
   {
      ibis = openIBISInp(2);
      tble = dlp2LPTble(ibis);
      IBISFileClose(ibis, 0);
   }

   zvparm("start", &startEntity, &parmct, &parmdf, 1, 0);
   zvparm("count", &numOfEntities, &parmct, &parmdf, 1, 0);
   zvparm("enum", &nVertices, &parmct, &parmdf, 1, 0);

   if(startEntity < 1)
      zmabend("\nStart Entity must be greater than 0.\n");
   if(numOfEntities < 0)
      zmabend("\nCount must not be less than 0.\n");
   --startEntity;

   if(startEntity+numOfEntities > list->numOfEntities || numOfEntities == 0)
      endEntity = list->numOfEntities;
   else
      endEntity = startEntity+numOfEntities;

   printf("\n==========================================\n");
   printf("          Listing LP Entities             \n\n");
   printf(" start entity: %d end entity: %d enum: %d \n", startEntity+1, endEntity, nVertices);
   printf("==========================================\n");

   for(i = startEntity; i < endEntity; i++)
   {
      if(fileCount == 2) printTbleEntity(tble, i);
      printListEntity(list, i, nVertices);
      printf("\n");
   }

   destroyLPList(list);
   if(fileCount == 2) destroyLPTble(tble);
}







