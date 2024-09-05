#include <strings.h>
#include "math.h"

#include "defines.h"
#include "shapefil.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "VicHdrs.h"
#include "LPUtils.h"
#include "MemUtils.h"
#include "fileutils.h"

/**************************************************************
Written by Peter Kim

Please see LPUtils.h for function documentation

6 aug 07  - finalized
15 aug 07 - added insertEntity() to LPList.
28 aug 07 - when reading in from file, all format is set
            to double
**************************************************************/

/*************************************************************/
void retagList(list)
   LPList* list;
{
   int i;

   for(i = 0; i < list->numOfEntities; i++)
   {
      if(list->fmt == LP_DOUBLE)
         ((double*)list->data)[list->map[i]] = (double)i+1;
      else if(list->fmt == LP_FLOAT)
	 ((float*)list->data)[list->map[i]] = (float)i+1;
      else if(list->fmt == LP_FULL)
 	 ((int*)list->data)[list->map[i]] = i+1;
   }
}

/*************************************************************/
void printTbleEntity(tble, entityNum)
   LPTble* tble;
   int entityNum;
{
   int j;

   printf("%d:\t", entityNum+1);
   for(j = 0; j < tble->numOfColumns; j++)
   {
      if(strcmp((tble->fmt)[j], "A16") == 0)
         printf("%s\t", (char*)(tble->data)+getTbleIndex(tble, entityNum, j));
      else if(strcmp((tble->fmt)[j], "DOUB") == 0)
	 printf("%f\t", ((double*)((char*)(tble->data)+getTbleIndex(tble, entityNum, j)))[0]);
      else if(strcmp((tble->fmt)[j], "FULL") == 0)
	 printf("%d\t", ((int*)((char*)(tble->data)+getTbleIndex(tble, entityNum, j)))[0]);
      else
      {
         printf("entityNum: %d column: %d fmt: %s\n", entityNum, j, (tble->fmt)[j]);
         zmabend("Invalid column format.\n");
      }
   }
   printf("\n");
}

/*************************************************************/
void printLPData(list)
   LPList* list;
{
   int i;

   for(i = 0; i < 20; i++)
      if(list->fmt == LP_DOUBLE)
         printf("i: %d %f\n", i, (double)((list->data)[i]));
      else if(list->fmt == LP_FLOAT)
         printf("i: %d %f\n", i, (float)((list->data)[i]));
      else if(list->fmt == LP_FULL)
         printf("i: %d %d\n", (int)((list->data)[i]));
}

/*************************************************************/
void printTbleData(tble)
   LPTble* tble;
{
   int i;

   for(i = 0; i < tble->numOfEntities; i++)
      printTbleEntity(tble, i);
}

/*************************************************************/
int rowCount(shpHandle, nEntities, cycle)
   SHPHandle shpHandle;
   int nEntities, cycle;
{
   int i, nParts, nVertices, count = 0;
   SHPObject *shpObj;

   for(i = 0; i < nEntities; i++)
   {
      shpObj = SHPReadObject(shpHandle, i);
      nVertices = shpObj->nVertices;
      nParts = shpObj->nParts;
      count += (nVertices+(nParts-1))*cycle+3;
      SHPDestroyObject(shpObj);
   }

   return count;
}

/*************************************************************/
/*
int initLength(list, index)
   LPList *list;
   int index;
{
   if(list->fmt == LP_DOUBLE)
      return (int)(((double*)(list->data))[index+1] + 0.5);
   if(list->fmt == LP_FLOAT)
      return (int)(((float*)(list->data))[index+1] + 0.5);
   if(list->fmt == LP_FULL)
      return (int)(((int*)(list->data))[index+1]);

   zmabend("Invalid format while initializing LP list.\n");
}
*/
/*************************************************************/
/*
int initCycle(list, index)
   LPList *list;
   int index;
{
   if(list->fmt == LP_DOUBLE)
      return (int)(((double*)(list->data))[index+2] + 0.5);
   if(list->fmt == LP_FLOAT)
      return (int)(((float*)(list->data))[index+2] + 0.5);
   if(list->fmt == LP_FULL)
      return (int)(((int*)(list->data))[index+2]);

   zmabend("Invalid format while initializing LP list.\n");
}
*/
/*************************************************************/
void mapLP(list)
   LPList *list;
{
   int *tmpMap, i, entityCount;
   int *tmpVertMap;

   // nr/3: assuming max, the file consists of only headers
   mz_alloc1((unsigned char**)&tmpMap, list->totDataSize/3, sizeof(int));
   mz_alloc1((unsigned char**)&tmpVertMap, list->totDataSize/3, sizeof(int));

   i = entityCount = 0;

   while(i < list->totDataSize)
   {
      int length, cycle;

      length = (int)((list->data)[i+1] + 0.5);
      cycle = (int)((list->data)[i+2] + 0.5);

      tmpMap[entityCount] = i;
      tmpVertMap[entityCount] = length/(int)cycle;
 
      ++entityCount;
      i += length+3;
   }

   mz_alloc1((unsigned char**)&(list->map), entityCount, sizeof(int));
   mz_alloc1((unsigned char**)&(list->vertCnt), entityCount, sizeof(int));
   for(i = 0; i < entityCount; i++)
   {
      (list->map)[i] = tmpMap[i];
      (list->vertCnt)[i] = tmpVertMap[i];
   }
   list->numOfEntities = entityCount;

   free(tmpMap);
   free(tmpVertMap);
}

/*************************************************************/
LPList* slp2LPList(ibis)
   int ibis;
{
   LPList *list;
   int nr, type, status;
   char fmt[5];

   printf("\tAcquiring LPList from .slp file.\n");
   list = (LPList*)calloc(1, sizeof(LPList));

   status = IBISColumnSet(ibis, "U_FORMAT", "DOUB", 1);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISColumnGet(ibis, "format", fmt, 1);
   if(status != 1) IBISSignal(ibis, status, 1);
   if(strcmp(fmt, "FULL") == 0) list->fmt = LP_FULL;
   else if(strcmp(fmt, "REAL") == 0) list->fmt = LP_FLOAT;
   else if(strcmp(fmt, "DOUB") == 0) list->fmt = LP_DOUBLE;
   else zmabend("Unable to get LP File format.");

   status = IBISFileGet(ibis, "nr", &nr, 0, 0, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   list->totDataSize = nr;
   mz_alloc1((unsigned char **)&list->data, nr, sizeof(double));
   IBISColumnRead(ibis, (char *)(list->data), 1, 1, nr);
   /*
   if(strcmp(fmt, "DOUB") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(double));
   else if(strcmp(fmt, "REAL") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(float));
   else if(strcmp(fmt, "FULL") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(int));
   */

   mapLP(list);

   return list;
}

/*************************************************************/
LPList* shp2LPList(handle)
   SHPHandle handle;
{
   LPList *list;
   int nr, status, fmtSize, cycle, row;
   int nShpTypes, nEntities, iEntity;
   char fmt[5];
   double xyzmMin[4], xyzmMax[4];

   printf("\tAcquiring LPList from .shp file.\n");
   if(!handle) zmabend("Failed to open shape (.shp) file.\n");
   list = (LPList*)calloc(1, sizeof(LPList));
   SHPGetInfo(handle, &nEntities, &nShpTypes, xyzmMin, xyzmMax);
   list->fmt = 8; // assume shape files are always in double precision

   if(nShpTypes == SHPT_POINTZ || nShpTypes == SHPT_ARCZ ||nShpTypes == SHPT_POLYGONZ || nShpTypes == SHPT_MULTIPOINTZ) cycle = 3;
   else cycle = 2;
   
   nr = rowCount(handle, nEntities, cycle);
   /*
   if(list->fmt == LP_FULL) fmtSize = sizeof(int);
   else if(list->fmt == LP_FLOAT) fmtSize = sizeof(float);
   else if(list->fmt == LP_DOUBLE) fmtSize = sizeof(double);
   */
   mz_alloc1((unsigned char**)&list->data, nr, sizeof(double));

   row = 0;

   for(iEntity=0; iEntity < nEntities; iEntity++)
   {
      int part, vertex, nVertices, nParts, startRow;
      SHPObject *shpObj;

      shpObj = SHPReadObject(handle, iEntity);
      nVertices = shpObj->nVertices;
      nParts = shpObj->nParts;
      
      (list->data)[row++] = (double)iEntity + 1;
      row++; // save an entry for length
      (list->data)[row++] = (double)cycle;

      startRow = row;
      for(part = 0, vertex=0; vertex < nVertices; vertex++)
      {
         if(part > nParts) break;
         if(shpObj->panPartStart[part]==vertex) part++;
         if(shpObj->panPartStart[part]==vertex && part > 1)
         {
 	    (list->data)[row++] = (double)0.0;
            (list->data)[row++] = (double)0.0;
            if(cycle == 3) (list->data)[row++] = (double)0.0;
	    //            printf("ent: %d\n", iEntity);
         }

         (list->data)[row++] = shpObj->padfX[vertex];
         (list->data)[row++] = shpObj->padfY[vertex];
         if(cycle == 3) (list->data)[row++] = shpObj->padfZ[vertex];
      }
      
      (list->data)[startRow-2] = (double)(row - startRow);
      SHPDestroyObject(shpObj);
   }
   
   list->totDataSize = row;

   mapLP(list);

   return list;
}

/*************************************************************/
int getTbleIndex(tble, record, field)
   LPTble *tble;
   int field, record;
{
   int index;

   index = (tble->colLoc)[field] * tble->numOfEntities;
   /*
   int i;
   for(i = 0; i < tble->numOfColumns; i++)
     printf("%d colLoc: %d\n", i, (tble->colLoc)[field]);
   */
   if(strcmp((tble->fmt)[field], "A16") == 0)
      return index + record*17;
   else if(strcmp((tble->fmt)[field], "DOUB") == 0)
      return index + record*sizeof(double);
   else if(strcmp((tble->fmt)[field], "FULL") == 0)
      return index + record*sizeof(int);
   else
   {
      printf("format: %s field: %d\n", (tble->fmt)[field], field);
      zmabend("Table indexing failed.\n");
   }

   return index;
}

/*************************************************************/
void writeTbleString(tble, string, field, record)
   LPTble *tble;
   char* string;
   int field, record;
{
   int i, index;

   index = getTbleIndex(tble, record, field);
   if(string)
      for(i = 0; i < 17; i++)
         ((char*)(tble->data))[index + i] = string[i];
   else
      ((char*)(tble->data))[index] = 0;
}

/*************************************************************/
void writeTbleDouble(tble, value, field, record)
   LPTble *tble;
   double value;
   int field, record;
{
   int index;

   index = getTbleIndex(tble, record, field);
   ((double*)(((char*)(tble->data))+index))[0] = value;
}

/*************************************************************/
void writeTbleInt(tble, value, field, record)
   LPTble *tble;
   int field, record, value;
{
   int index;

   index = getTbleIndex(tble, record, field);
   ((int*)(((char*)(tble->data))+index))[0] = value;
}

/*************************************************************/
LPTble* dlp2LPTble(ibis)
   int ibis;
{
   int nr, nc, status, i;
   char tmpFmt[99][6];
   LPTble *tble;

   printf("\tAcquiring LPTble from .dlp file.\n");
   tble = (LPTble*)calloc(1, sizeof(LPTble));
   status = IBISFileGet(ibis, "nr", &nr, 1, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileGet(ibis, "nc", &nc, 1, 1, 0);
   if(status != 1) IBISSignal(ibis, status, 1);
   tble->numOfColumns = nc-1;
   status = IBISFileGet(ibis, "formats", tmpFmt, 1, 99, 6);
   for(i = 0; i <= tble->numOfColumns; i++)
      strcpy(tble->fmt[i], tmpFmt[i+1]);
   tble->numOfEntities = nr;

   mz_alloc1((unsigned char**)&(tble->colLoc), tble->numOfColumns, sizeof(int));
   tble->colLoc[0] = 0;
   for(i = 1; i < tble->numOfColumns; i++)
   {
      if(strcmp((tble->fmt)[i-1], "A16") == 0)
	 (tble->colLoc)[i] = (tble->colLoc)[i-1] + 17;
      else if(strcmp((tble->fmt)[i-1], "FULL") == 0)
	 (tble->colLoc)[i] = (tble->colLoc)[i-1] + sizeof(int);
      else if(strcmp((tble->fmt)[i-1], "DOUB") == 0)
	 (tble->colLoc)[i] = (tble->colLoc)[i-1] + sizeof(double);
      else zmabend("++Invalid column format in .dlp file.\n");
   }

   if(strcmp((tble->fmt)[i-1], "A16") == 0)
      tble->recordSize = tble->colLoc[i-1] + 17;
   else if(strcmp((tble->fmt)[i-1], "FULL") == 0)
      tble->recordSize = tble->colLoc[i-1] + sizeof(int);
   else if(strcmp((tble->fmt)[i-1], "DOUB") == 0)
      tble->recordSize = tble->colLoc[i-1] + sizeof(double);
   else zmabend("++Invalid column format in .dlp file.\n");
   
   mz_alloc1((unsigned char**)&(tble->data), tble->recordSize*tble->numOfEntities + 10, sizeof(char));

   for(i = 0; i < tble->numOfColumns; i++)
   {
      int index;

      index = getTbleIndex(tble, 0, i);
      IBISColumnRead(ibis, ((char*)(tble->data)+index), i+2, 1, tble->numOfEntities);
   }

   return tble;
}

/*************************************************************/
LPTble* dbf2LPTble(handle)
   DBFHandle handle;
{
   int validIndex, fmtIncrement, dataIndex, i, j, totFields;
   int* fieldTypes, *tmpColLoc, *validFields;
   LPTble *tble;

   printf("\tAcquiring LPTble from .dbf file.\n");
   tble = (LPTble*)calloc(1, sizeof(LPTble));
   if(!handle) zmabend("Failed to open data (.dbf) file.\n");
   totFields = DBFGetFieldCount(handle);
   tble->numOfEntities = DBFGetRecordCount(handle);

   mz_alloc1((unsigned char**)&fieldTypes, totFields, sizeof(int));
   mz_alloc1((unsigned char**)&tmpColLoc, totFields, sizeof(int));
   mz_alloc1((unsigned char**)&validFields, totFields, sizeof(int));

   validIndex = 0;
   //   strcpy((tble->fmt)[validIndex], "FULL");
   tble->recordSize = 0;
   //   tmpColLoc[validIndex++] = tble->recordSize;
   for(i = 0; i < totFields; i++)
   {
      fieldTypes[i] = DBFGetFieldInfo(handle, i, NULL, NULL, NULL);
      //printf("dbf format: %d\n", fieldTypes[i]);
      if(fieldTypes[i] == FTString)
      {
         strcpy((tble->fmt)[validIndex], "A16");
	 tmpColLoc[validIndex++] = tble->recordSize;
	 tble->recordSize += 17;
	 validFields[i] = 1;
      }
      else if(fieldTypes[i] == FTDouble)
      {
         strcpy((tble->fmt)[validIndex], "DOUB");
	 tmpColLoc[validIndex++] = tble->recordSize;
	 tble->recordSize += sizeof(double);
	 validFields[i] = 1;
      }
      else if(fieldTypes[i] == FTInteger)
      {
         strcpy((tble->fmt)[validIndex], "FULL");
	 tmpColLoc[validIndex++] = tble->recordSize;
	 tble->recordSize += sizeof(int);
	 validFields[i] = 1;
      }
      else
         validFields[i+1] = 0;
   }

   tble->numOfColumns = validIndex;
   mz_alloc1((unsigned char**)&tble->colLoc, tble->numOfColumns, sizeof(int));
   mz_alloc1((unsigned char**)&tble->data, tble->numOfEntities*tble->recordSize, 1);

   memcpy(tble->colLoc, tmpColLoc, sizeof(int)*tble->numOfColumns);
   free(tmpColLoc);

   validIndex = dataIndex = 0;
   for(i = 0; i < totFields; i++)
   {
      char *stringValue;
      double dval;
      void *data;
      int ival;

      if(!validFields[i]) continue;
      if(fieldTypes[i] == FTString)
      {
         for(j = 0; j < tble->numOfEntities; j++)
	 {
            stringValue = (char*)DBFReadStringAttribute(handle, j, i);
	    writeTbleString(tble, stringValue, validIndex, j);
	 }
      }
      else if(fieldTypes[i] == FTDouble)
	 for(j = 0; j < tble->numOfEntities; j++)
	 {
	    dval = DBFReadDoubleAttribute(handle, j, i);
	    writeTbleDouble(tble, dval, validIndex, j);
	 }
      else if(fieldTypes[i] == FTInteger)
	 for(j = 0; j < tble->numOfEntities; j++)
	 {
	    ival = DBFReadIntegerAttribute(handle, j, i);
	    writeTbleInt(tble, ival, validIndex, j);
	 }
      ++validIndex;
   }

   free(fieldTypes);
   free(validFields);

   return tble;
}

/*************************************************************/
void destroyLPList(list)
   LPList* list;
{
   printf("\tDestroying LPList.\n");
   free(list->data);
   free(list->map);
   free(list->vertCnt);
   free(list);
}

/*************************************************************/
void destroyLPTble(tble)
   LPTble* tble;
{
   printf("\tDestroying LPTble.\n");
   free(tble->colLoc);
   free(tble->data);
   free(tble);
}

/*************************************************************/
double getDoubleXYorZ(list, entity, vertex, xyOrz)
   LPList *list;
   int entity, vertex, xyOrz;
{
   int cycle = getCycle(list, entity);

   if(xyOrz == LP_X) 
      return (list->data)[(list->map)[entity]+vertex*cycle+3];
   if(xyOrz == LP_Y)
      return (list->data)[(list->map)[entity]+vertex*cycle+4];
   if(xyOrz == LP_Z && cycle == 3)
      return (list->data)[(list->map)[entity]+vertex*cycle+5];

   zmabend("Requested entity-vertex-xyOrz value is not valid.");
}

/*************************************************************/
void setDoubleXYorZ(list, entity, vertex, xyOrz, val)
   LPList *list;
   int entity, vertex, xyOrz;
   double val;
{
   int cycle = getCycle(list, entity);

   if(xyOrz == LP_X)
      (list->data)[(list->map)[entity]+vertex*cycle+3] = val;
   else if(xyOrz == LP_Y)
      (list->data)[(list->map)[entity]+vertex*cycle+4] = val;
   else if(xyOrz == LP_Z && cycle == 3)
      (list->data)[(list->map)[entity]+vertex*cycle+5] = val;
   else
      zmabend("requested entity-vertex-xyOrz is not valid.");
}

/*************************************************************/
void setLength(list, entity, len)
   LPList *list;
   int entity;
   double len;
{
   if(list->fmt == LP_DOUBLE)
      ((double*)(list->data))[list->map[entity]+1] = len;
   else if(list->fmt == LP_FLOAT)
      ((float*)(list->data))[list->map[entity]+1] = (float)len;
   else if(list->fmt == LP_FULL)
      ((int*)(list->data))[list->map[entity]+1] = (int)len;
}

/*************************************************************/
void setCycle(list, entity, cycle)
   LPList *list;
   int entity;
   double cycle;
{
   if(list->fmt == LP_DOUBLE)
      ((double*)(list->data))[list->map[entity]+2] = cycle;
   else if(list->fmt == LP_FLOAT)
      ((float*)(list->data))[list->map[entity]+2] = (float)cycle;
   else if(list->fmt == LP_FULL)
      ((int*)(list->data))[list->map[entity]+2] = (int)cycle;

   zmabend("Invalid format in the List.\n");
}

/*************************************************************/
int getLength(list, entity)
   LPList *list;
   int entity;
{
   return (int)((list->data)[list->map[entity]+1]+.5);

   /*
   if(list->fmt == LP_DOUBLE)
      return (int)(((double *)(list->data))[list->map[entity]+1]+.5);
   if(list->fmt == LP_FLOAT)
      return (int)(((float *)(list->data))[list->map[entity]+1]+.5);
   if(list->fmt == LP_FULL)
      return ((int *)(list->data))[list->map[entity]+1];
   */
}

/*************************************************************/
int getTag(list, entity)
   LPList *list;
   int entity;
{
   return (int)((list->data)[list->map[entity]]+.5);
}

/************************************************************/
int getCycle(list, entity)
   LPList *list;
   int entity;
{
   return (int)((list->data)[list->map[entity]+2]+.5);
}

/*************************************************************/
void setTag(list, entity, val)
   int entity, val;
   LPList *list;
{
   if(val < 1 || val > list->numOfEntities)
      zmabend("Attempt to assign invalid tag number.\n");

   (list->data)[list->map[entity]] = (double)val;
}

/*************************************************************/
void printListEntity(list, entityNum, nVertices)
   LPList* list;
   int entityNum, nVertices;
{
   int len = getLength(list, entityNum);
   int cycle = getCycle(list, entityNum);
   int tag = getTag(list, entityNum);
   printf("tag: %d cycle: %d length: %d\n", tag, cycle, len);
   if(nVertices == -1 || nVertices > list->vertCnt[entityNum])
      nVertices = list->vertCnt[entityNum];

   int i, index;
   for(i = 0; i < nVertices; i++)
   {
      printf("\tx: %f ", getDoubleXYorZ(list, entityNum, i, LP_X));
      printf("\ty: %f ", getDoubleXYorZ(list, entityNum, i, LP_Y));
      if(cycle == 3)
	 printf("\tz: %f", getDoubleXYorZ(list, entityNum, i, LP_Z));
      printf("\n");
   }
}

/*************************************************************/
void allocLPList(list)
   LPList* list;
{
   mz_alloc1((unsigned char**)&list->data, list->totDataSize, sizeof(double));
   mz_alloc1((unsigned char**)&list->map, list->totDataSize, sizeof(int));
   mz_alloc1((unsigned char**)&list->vertCnt, list->totDataSize, sizeof(int));
}

/*************************************************************/
LPList* addLists(LPList *list1, LPList *list2)
{
   LPList *list;

   if(list1->fmt != list2->fmt) 
      zmabend("The 2 lists are not in the same format.");

   printf("\tConcatenating 2 LPLists.\n");
   list = (LPList*)calloc(1, sizeof(LPList));
   list->numOfEntities = list1->numOfEntities + list2->numOfEntities;
   list->totDataSize = list1->totDataSize + list2->totDataSize;
   list->fmt = list1->fmt;

   allocLPList(list);
   memcpy(list->data, list1->data, list1->totDataSize*sizeof(double));
   memcpy((void*)((list->data)+list1->totDataSize), 
          list2->data, list2->totDataSize*sizeof(double)); 

   //printLPData(list);
   mapLP(list);

   retagList(list);

   return list;
}

/*************************************************************/
int openIBISOutSLP(nRow, fmt, outParm)
   int nRow, fmt, outParm;
{
   int unit, ibis, status, i;
   int parmct, parmdf;
   char fileNames[10][99];

   getFileName(IBIS_OUT, outParm, fileNames);

   zvselpi(0);
   zvunit(&unit, "out", outParm, 0);
   status = IBISFileUnit(unit, &ibis, "write", 1, nRow, 0, "column");
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("\tOutputting LPList to %s - nr: %d ibis: %d\n", fileNames[outParm-1], nRow, ibis);

   if(fmt == 8)
      status = IBISFileSet(ibis, "fmt_default", "doub", 1);
   else if(fmt == 4)
      status = IBISFileSet(ibis, "fmt_default", "full", 1);
   else if(fmt == 7)
      status = IBISFileSet(ibis, "fmt_default", "real", 1);
   if(status != 1) IBISSignal(ibis, status, 1);

   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   if(fmt == 8)
      status = IBISColumnSet(ibis, "U_FORMAT", "DOUB", 1);
   else if(fmt == 4)
      status = IBISColumnSet(ibis, "U_FORMAT", "FULL", 1);
   else if(fmt == 7)
      status = IBISColumnSet(ibis, "U_FORMAT", "REAL", 1);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

/*************************************************************/
LPTble* addTbles(tble1, tble2)
   LPTble* tble1, *tble2;
{
   int i, index, index1, index2;
   LPTble *tble;

   printf("\tConcatenating 2 LPTbles.\n");
   if(tble1->numOfColumns != tble2->numOfColumns)
      zmabend("The 2 LPTbles have different number of columns.\n");

   for(i = 0; i < tble1->numOfColumns+1; i++)
      if(strcmp((tble1->fmt)[i], (tble2->fmt)[i]) != 0)
	 zmabend("The column formats of the 2 LPTbles do not match.\n");

   if(tble1->recordSize != tble2->recordSize)
      zmabend("The record sizes of the 2 LPTbles do not match.\n");

   tble = (LPTble*)calloc(1, sizeof(LPTble));
   tble->numOfEntities = tble1->numOfEntities + tble2->numOfEntities;
   tble->numOfColumns = tble1->numOfColumns;
   tble->recordSize = tble1->recordSize;
   mz_alloc1((unsigned char**)&(tble->colLoc), tble->numOfColumns, sizeof(int));
   for(i = 0; i < tble->numOfColumns+1; i++)
   {
      strcpy((tble->fmt)[i], (tble1->fmt)[i]);
      if(i < tble->numOfColumns) (tble->colLoc)[i] = (tble1->colLoc)[i];
   }

   mz_alloc1((unsigned char**)&(tble->data), tble->recordSize*tble->numOfEntities, sizeof(char));
   
   //printTbleData(tble1);
   //printTbleData(tble2);
   index = index1 = index2 = 0;
   for(i = 0; i < tble->numOfColumns; i++)
   {
      if(strcmp((tble->fmt)[i], "A16") == 0)
      {
         memcpy((char*)(tble->data)+index, (char*)(tble1->data)+index1, 17*tble1->numOfEntities);
	 index += 17*tble1->numOfEntities;
 index1 += 17*tble1->numOfEntities;
	 memcpy((char*)(tble->data)+index, (char*)(tble2->data)+index2, 17*tble2->numOfEntities);
	 index += 17*tble2->numOfEntities;
	 index2 += 17*tble2->numOfEntities;
      }
      else if(strcmp((tble->fmt)[i], "DOUB") == 0)
      {
         memcpy((char*)(tble->data)+index, (char*)(tble1->data)+index1, sizeof(double)*tble1->numOfEntities);
	 index += sizeof(double)*tble1->numOfEntities;
	 index1 += sizeof(double)*tble1->numOfEntities;
	 memcpy((char*)(tble->data)+index, (char*)(tble2->data)+index2, sizeof(double)*tble2->numOfEntities);
	 index += sizeof(double)*tble2->numOfEntities;
	 index2 += sizeof(double)*tble2->numOfEntities;
      }
      else if(strcmp((tble->fmt)[i], "FULL") == 0)
      {
         memcpy((char*)(tble->data)+index, (char*)(tble1->data)+index1, sizeof(int)*tble1->numOfEntities);
	 index += sizeof(int)*tble1->numOfEntities;
	 index1 += sizeof(int)*tble1->numOfEntities;
	 memcpy((char*)(tble->data)+index, (char*)(tble2->data)+index2, sizeof(int)*tble2->numOfEntities);
	 index += sizeof(int)*tble2->numOfEntities;
	 index2 += sizeof(int)*tble2->numOfEntities;
      }
      else
      {
	 printf("fmt: %s column: %d\n", (tble->fmt)[i+1], i);
         zmabend("Invalid format found in .dlp file.\n");
      }
   }

   //   printTbleData(tble);

   return tble;
}


/*************************************************************/
void printSLP(ibis, list)
   int ibis;
   LPList* list;
{
   int status, i;
   void *tmpData;

   if(list->fmt == LP_DOUBLE)
      mz_alloc1((unsigned char**)&tmpData, list->totDataSize, sizeof(double));
   else if(list->fmt == LP_FULL || list->fmt == LP_FLOAT)
      mz_alloc1((unsigned char**)&tmpData, list->totDataSize, sizeof(int));

   for(i = 0; i < list->totDataSize; i++)
      if(list->fmt == LP_DOUBLE)
         ((double*)tmpData)[i] = list->data[i];
      else if(list->fmt == LP_FLOAT)
         ((float*)tmpData)[i] = (float)(list->data[i]);
      else ((int*)tmpData)[i] = (int)(list->data[i]);

   status = IBISColumnWrite(ibis, tmpData, 1, 1, list->totDataSize);
   if(status != 1) IBISSignal(ibis, status, 1);

   free(tmpData);
}

/*************************************************************/
void printDLP(ibis, tble)
   int ibis;
   LPTble* tble;
{
   int status, i, *tagCol;

   mz_alloc1((unsigned char**)&tagCol, tble->numOfEntities, sizeof(int));
   for(i = 0; i < tble->numOfEntities; i++) tagCol[i] = i+1;
   status = IBISColumnWrite(ibis, (int*)tagCol, 1, 1, tble->numOfEntities);
   if(status != 1) IBISSignal(ibis, status, 1);

   for(i = 1; i < tble->numOfColumns; i++)
   {
      int index;

      index = getTbleIndex(tble, 0, i-1);

      status = IBISColumnWrite(ibis, ((char*)tble->data)+index, i+1, 1, tble->numOfEntities);
      if(status != 1) IBISSignal(ibis, status, 1);
   }

   free(tagCol);
}

/*************************************************************/
int openIBISOutDLP(tble, outParm)
   int outParm;
   LPTble *tble;
{
   int unit, ibis, status, i;
   int parmct, parmdf;
   char fileNames[20][99];

   for(i = tble->numOfColumns; i > 0; i--)
      strcpy(tble->fmt[i], tble->fmt[i-1]);
   strcpy(tble->fmt[0], "FULL");
   tble->numOfColumns++;

   getFileName(IBIS_OUT, outParm, fileNames);

   /*
   for(int i = 0; i < nCol; i++)
      printf("format: %s\n", fmt[i]);
   */

   zvselpi(0);
   zvunit(&unit, "out", outParm, 0);
   status = IBISFileUnit(unit, &ibis, "write", tble->numOfColumns, tble->numOfEntities, 0, "column");
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("\tOutputting LPTble to %s - nr: %d nc: %d ibis: %d\n", fileNames[outParm-1], tble->numOfEntities, tble->numOfColumns, ibis);

   status = IBISFileSet(ibis, "FORMATS", tble->fmt, 6);
   if(status != 1) IBISSignal(ibis, status, 1);
   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

/*************************************************************/
LPList* getEmptyList(fmt)
   int fmt;
{
   LPList *list;

   printf("\tCreating an empty LPList.\n");
   list = (LPList*)calloc(1, sizeof(LPList));
   list->numOfEntities = 0;
   list->totDataSize = 0;
   list->fmt = fmt;

   allocLPList(list);

   return list;
}

/*************************************************************/
LPTble* getEmptyTble(fmt, colLoc, recordSize, nColumns)
   char fmt[99][6];
   int *colLoc, recordSize, nColumns;
{
   int i;
   LPTble *tble;

   printf("\tCreating an empty LPTble.\n");
   tble = (LPTble*)calloc(1, sizeof(LPTble));
   tble->recordSize = recordSize;
   tble->numOfEntities = 0;
   tble->numOfColumns = nColumns;

   mz_alloc1((unsigned char**)&tble->data, 0, 0);
   for(i = 0; i < nColumns+1; i++)
      strcpy(tble->fmt[i], fmt[i]);
   mz_alloc1((unsigned char**)&tble->colLoc, nColumns+1, sizeof(int));
   memcpy(tble->colLoc, colLoc, (nColumns+1)*sizeof(int));

   return tble;
}

/*************************************************************/
void getTbleRecord(tble, i, recordBuf)
   LPTble *tble;
   void *recordBuf;
   int i;
{
   int j;

   for(j = 0; j < tble->numOfColumns-1; j++)
      memcpy((char*)recordBuf+tble->colLoc[j], ((char*)tble->data)+getTbleIndex(tble, i, j), tble->colLoc[j+1]-tble->colLoc[j]);

   memcpy((char*)recordBuf+tble->colLoc[j], ((char*)tble->data)+getTbleIndex(tble, i, j), tble->recordSize-tble->colLoc[tble->numOfColumns-1]);
}

/*************************************************************/
void insertTbleRecord(tble, recordBuf)
   LPTble *tble;
   void *recordBuf;
{
   int i, nCpyBytes, tmpIndex;
   void *tmpData;

   mz_alloc1((unsigned char**)&tmpData, tble->numOfEntities*tble->recordSize+tble->recordSize, 1);

   tmpIndex = 0;
   for(i = 0; i < tble->numOfColumns; i++)
   {
      int index;

      index = getTbleIndex(tble, 0, i);

      if(i+1 < tble->numOfColumns)
         nCpyBytes = tble->numOfEntities*(tble->colLoc[i+1] - tble->colLoc[i]);
      else
     	 nCpyBytes = tble->numOfEntities*(tble->recordSize-tble->colLoc[i]);
      memcpy((char*)tmpData+tmpIndex, ((char*)(tble->data))+index, nCpyBytes);
      tmpIndex += nCpyBytes;

      if(i+1 < tble->numOfColumns)
         nCpyBytes = tble->colLoc[i+1]-tble->colLoc[i];
      else
         nCpyBytes = tble->recordSize-tble->colLoc[i];
      memcpy((char*)tmpData+tmpIndex, (char*)recordBuf+tble->colLoc[i], nCpyBytes);
      tmpIndex += nCpyBytes;
   }

   free(tble->data);
   tble->data = tmpData;
   ++tble->numOfEntities;
}

/*************************************************************/
void insertListEntity(list, nVertices, buf, sizeOfBuf)
   LPList* list;
   int nVertices, sizeOfBuf;
   void *buf;
{
   int lastPosition, index, sizeOfData;

   ++list->numOfEntities;
   lastPosition = list->totDataSize;

   sizeOfData = sizeof(double);
   /*
   if(list->fmt == LP_FULL || list->fmt == LP_FLOAT)
      sizeOfData = 4;
   else
      sizeOfData = 8;
   */

   index = list->totDataSize*sizeOfData;
   list->totDataSize += sizeOfBuf/sizeOfData;

   int i;

   list->data = realloc(list->data, list->totDataSize*sizeOfData+sizeOfBuf);
   list->vertCnt = realloc(list->vertCnt, list->numOfEntities*sizeof(int));
   list->map = realloc(list->map, list->numOfEntities*sizeof(int));

   memcpy((char*)(list->data)+index, buf, sizeOfBuf);
   //   printLPData(list);

   list->vertCnt[list->numOfEntities-1] = nVertices;
   list->map[list->numOfEntities-1] = lastPosition;

   retagList(list);
}


























