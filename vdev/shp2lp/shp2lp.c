#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "shapefil.h"

#include "fileutils.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "VicHdrs.h"

/*************************************************************
shp2lp.c
  
Written by P. Kim

This program creates a .slp and .dlp file from shape files.

8/3/07 - finalized
*************************************************************/

/*************************************************************/
void main44()
{
   int parmct, parmdf, ibis, fmt, dlp, status;
   char inFile[95];
   SHPHandle shpHandle;
   DBFHandle dbfHandle;
   LPList *list;
   LPTble *tble;
   FILE *file;
   char shpFileName[99], dbfFileName[99];
   int index, fileCount;
   
   zvparm("inp", inFile, &parmct, &parmdf, 1, 99);
   zvparm("fmt", &fmt, &parmct, &parmdf, 1, 99);

   strcat(shpFileName, inFile);
   strcat(shpFileName, ".shp");
   file = fopen(shpFileName, "r");
   if(file != 0)
   {
      fclose(file);
      shpHandle = SHPOpen(inFile, "rb");
      list = shp2LPList(shpHandle);
      ibis = openIBISOutSLP(list->totDataSize, list->fmt, 1);
      printSLP(ibis, list);
      destroyLPList(list);
      status = IBISFileClose(ibis, 0);
      if(status != 1) IBISSignal(ibis, status, 1);
   }
   else
   {
      printf("%s not found... !!!\nProceeding to tabular data.\n", shpFileName);
      fclose(file);
   }

   strcat(dbfFileName, inFile);
   strcat(dbfFileName, ".dbf");
   file = fopen(dbfFileName, "r");
   zvpcnt("out", &fileCount);
   if(file != 0 && fileCount == 2)
   {
      fclose(file);
      dbfHandle = DBFOpen(inFile, "rb");
      tble = dbf2LPTble(dbfHandle);
      ibis = openIBISOutDLP(tble, 2);
      printDLP(ibis, tble);
      destroyLPTble(tble);
      status = IBISFileClose(ibis, 0);
   }
   else
   {
      printf("%s not found or output .dlp file not specified.\n", dbfFileName );
      fclose(file);
   }
}









