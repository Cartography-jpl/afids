#include "vicmain_c.h"
#include "applic.h"
#include <math.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "LPUtils.h"
#include "mappingUtils.h"
#include "VicHdrs.h"
#include "fileutils.h"

/***********************************************************************
lppixmap.c

Written by Al Zobrist, P. Kim

This program was originally ported and modified from pixmap.  It
converts the X, Y, Z values in the LP shape entities to either
its corresponding map value or image value.  The algorithm
was originally written by Al Zobrist in pixmap.c.

9-May-07 - finalized
/************************************************************************/

void main44()
{
   int ibis1,ibis2,status;
   int maptopix, pixtomap;
           
   zifmessage("lppixmap version 09-May-07");
   /* get the basic parameters and calculate the mapping */
   maptopix = zvptst("maptopix");
   pixtomap = zvptst("pixtomap");
   if (maptopix&&pixtomap)
      zmabend("Only one keyword for mapping direction can be given");
   if (!maptopix&&!pixtomap)
      zmabend("One keyword for mapping direction must be given");

   /* read in points from the ibis interface file */
   ibis1 = openIBISInp(1);
   LPList *list = slp2LPList(ibis1);
   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);

   lpPixMapConversion(list, maptopix, 2);

   /* Output points to the ibis interface file */
   ibis2 = openIBISOutSLP(list->totDataSize, list->fmt, 1);
   printSLP(ibis2, list);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}
