#include "vicmain_c.h"
#include "applic.h"

#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "fileutils.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "VicHdrs.h"

/*************************************************************
lpcat.c

Written by Peter Kim

This program concatenates 2 LP files

8/3/07 - finalized
*************************************************************/

/*************************************************************/
void main44()
{
   int ibis1, ibis2, ibis3, inpCnt, outCnt;
   
   ibis1 = openIBISInp(1);
   ibis2 = openIBISInp(2);
   
   LPList *list1 = slp2LPList(ibis1);
   LPList *list2 = slp2LPList(ibis2);
   LPList *list3 = addLists(list1, list2);
   destroyLPList(list1);
   destroyLPList(list2);

   IBISFileClose(ibis1, 0);
   IBISFileClose(ibis2, 0);

   ibis3 = openIBISOutSLP(list3->totDataSize, list3->fmt, 1);
   printSLP(ibis3, list3);
   destroyLPList(list3);
   IBISFileClose(ibis3, 0);
   
   zvpcnt("inp", &inpCnt);
   zvpcnt("out", &outCnt);
   //   printf("inp: %d out: %d\n", inpCnt, outCnt);
   if(inpCnt == 4 && outCnt == 2)
   {
      ibis1 = openIBISInp(3);
      ibis2 = openIBISInp(4);
      LPTble *tble1 = dlp2LPTble(ibis1);
      LPTble *tble2 = dlp2LPTble(ibis2);
      LPTble *tble3 = addTbles(tble1, tble2);
      destroyLPTble(tble1);
      destroyLPTble(tble2);

      IBISFileClose(ibis1, 0);
      IBISFileClose(ibis2, 0);

      ibis3 = openIBISOutDLP(tble3->numOfColumns, tble3->numOfEntities, tble3->fmt, 2);
      printDLP(ibis3, tble3);
      destroyLPTble(tble3);
      IBISFileClose(ibis3, 0);
   }
}














