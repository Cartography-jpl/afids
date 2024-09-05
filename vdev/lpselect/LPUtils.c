#include "defines.h"
#include "shapefil.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "LPUtils.h"

/*************************************************************/
void hashLP(list, nr)
   LPList *list;
   int nr;
{
   int type, length, *tmpMap, i, entityCount, verticesCount;
   void *tmpData;

   type = getType(list->data);
   if(type == LP_DOUBLE)
      mz_alloc1((unsigned char**)&tmpMap, nr/2, sizeof(double));
   else if(type == LP_FLOAT)
      mz_alloc1((unsigned char**)&tmpMap, nr/2, sizeof(float));
   else if(type == LP_FULL)
      mz_alloc1((unsigned char**)&tmpMap, nr/2, sizeof(int));

   tmpData = list->data;
   i = entityCount = verticesCount = 0;
   while(i < nr)
   {
      tmpMap[entityCount++] = i;
      length = getLength(tmpData);
      verticesCount += length/getCycle(tmpData);
      if(type == LP_DOUBLE)
      {
         length += 2;
         (double *)(tmpData) = (double*)(tmpData)+length;
      }
      else if(type == LP_FLOAT || LP_FULL)
      {
         length += 4;
         (float *)(tmpData) = (float*)(tmpData)+length;
      }
      i += length;
   }

   mz_alloc1((unsigned char**)&(list->map), entityCount, sizeof(int));
   for(i = 0; i < entityCount; i++)
      (list->map)[i] = tmpMap[i];
   list->numOfEntities = entityCount;
   list->numOfVertices = verticesCount;

   free(tmpMap);
}

/*************************************************************/
LPList* getLPList(ibis)
   int ibis;
{
   LPList *list;
   int nr, type;
   char format[99];

   list = (LPList*)calloc(1, sizeof(LPList));
   IBISFileGet(ibis, "nr", &nr, 0, 0, 0);
   IBISColumnGet(ibis, "format", format, 1);
   if(strcmp(format, "DOUB") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(double));
   else if(strcmp(format, "REAL") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(float));
   else if(strcmp(format, "FULL") == 0)
      mz_alloc1((unsigned char **)&list->data, nr, sizeof(int));
   IBISColumnRead(ibis, (double *)(list->data), 1, 1, nr);

   hashLP(list, nr);
   
   return list;
}

/*************************************************************/
void destroyLPList(list)
   LPList* list;
{
   free(list->data);
   free(list->map);
   free(list);
}

/*************************************************************/
double getDoubleXYorZ(list, entity, vertex, xyOrz)
   LPList *list;
   int entity, vertex, xyOrz;
{
   int cycle = getCycle(list->data);

   if(xyOrz == LP_X) 
      return ((double *)(list->data))[(list->map)[entity]+vertex*cycle+2];
   if(xyOrz == LP_Y)
      return ((double *)(list->data))[(list->map)[entity]+vertex*cycle+3];
   if(xyOrz == LP_Z && cycle == 3)
      return ((double *)(list->data))[(list->map)[entity]+vertex*cycle+4];

   zmabend("requested entity-vertex-xyOrz value is not valid.");
}

/*************************************************************/
void setDoubleXYorZ(list, entity, vertex, xyOrz, val)
   LPList *list;
   int entity, vertex, xyOrz;
   double val;
{
   int cycle = getCycle(list->data);

   if(xyOrz == LP_X)
      ((double *)(list->data))[(list->map)[entity]+vertex*cycle+2] = val;
   else if(xyOrz == LP_Y)
      ((double *)(list->data))[(list->map)[entity]+vertex*cycle+3] = val;
   else if(xyOrz == LP_Z && cycle == 3)
      ((double *)(list->data))[(list->map)[entity]+vertex*cycle+4] = val;
   else
      zmabend("requested entity-vertex-xyOrz value is not valid.");
}

/*************************************************************/
int getEntityLength(list, entity)
   LPList *list;
   int entity;
{
   if(getType(list->data) == LP_DOUBLE)
      return getLength((double *)(list->data) + (list->map)[entity]);
   if(getType(list->data) == LP_FLOAT)
      return getLength((float *)(list->data) + (list->map)[entity]);
   if(getType(list->data) == LP_FULL)
      return getLength((int *)(list->data) + (list->map)[entity]);
}

/*************************************************************/
int getEntityTag(list, entity)
   LPList *list;
   int entity;
{
   int type = getType(list->data);

   if(type == LP_DOUBLE)
      return getTag((double *)(list->data) + (list->map)[entity]);
   if(type == LP_FLOAT || type == LP_FULL)
      return getTag((float *)(list->data) + (list->map)[entity]);
}

/*************************************************************/
int getEntityType(list, entity)
   LPList *list;
   int entity;
{
   int type = getType(list->data);

   if(type == LP_DOUBLE)
      return getType((double *)(list->data) + (list->map)[entity]);
   if(type == LP_FLOAT || type == LP_FULL)
      return getType((float *)(list->data) + (list->map)[entity]);
}

/*************************************************************/
int getEntityCycle(list, entity)
   LPList *list;
   int entity;
{
   int type = getType(list->data);

   if(type == LP_DOUBLE)
      return getCycle((double *)(list->data) + (list->map)[entity]);
   if(type == LP_FLOAT || type == LP_FULL)
      return getCycle((float *)(list->data) + (list->map)[entity]);
}

/*************************************************************/
int getEntityNumOfVertices(list, entity)
   LPList *list;
   int entity;
{
   if(getType(list->data) == LP_DOUBLE)
      return getLength((double *)(list->data) + (list->map)[entity])/getCycle((double *)(list->data) + (list->map)[entity]);
   if(getType(list->data) == LP_FLOAT)
      return getLength((float *)(list->data) + (list->map)[entity])/getCycle((double *)(list->data) + (list->map)[entity]);
   if(getType(list->data) == LP_FULL)
      return getLength((int *)(list->data) + (list->map)[entity])/getCycle((double *)(list->data) + (list->map)[entity]);
}

/*************************************************************/
void getHdr(ibis, type, tag, cycle, length, startPos, fmt)
   int ibis, startPos, *type, *tag, *cycle, *length, fmt;
{
   void *hdr;

   mz_alloc1((unsigned char**)&hdr, 16, 1);

   if(fmt == 4)
      IBISColumnRead(ibis, (unsigned int*)hdr, 1, startPos, 4);
   else if(fmt == 7)
      IBISColumnRead(ibis, (float*)hdr, 1, startPos, 4);
   else if(fmt == 8)
      IBISColumnRead(ibis, (double*)hdr, 1, startPos, 2);

   *type = getType(hdr);
   *tag = getTag(hdr);
   *cycle= getCycle(hdr);
   *length = getLength(hdr);

   free(hdr);
}

/*************************************************************/
void setTag(data, tag)
   int tag;
   void* data;
{
   ((unsigned int*)data)[1] = tag;
}

/*************************************************************/
int getType(data)
   void* data;
{
   return (int)(((unsigned char*)data)[0]);
}

/*************************************************************/
int getTag(data)
   void *data;
{
   return ((unsigned int*)data)[1];
}

/*************************************************************/
int getCycle(data)
   void* data;
{
   return (int)(((unsigned char*)data)[8]);
}

/*************************************************************/
int getLength(data)
   void *data;
{
   return ((unsigned int*)data)[3];
}

/*************************************************************/
int nextEntityIndex(data)
   void *data;
{
   int length, type, tag;

   length = getLength(data);
   type = getType(data);
   tag = getTag(data);
   //printf("**tag: %d length: %d type: %d\n", tag, length, type);
   if(type == 8)
      return length + 2;
   else if(type == 4 || type == 7)
      return length + 4;

   return -1;
}

/*************************************************************/
int writeToIBIS(ibis, buff, startIndex, numOfRows, fmt)
   void *buff;
   int ibis, startIndex, numOfRows, fmt;
{
   if(fmt == 4)
      IBISColumnWrite(ibis, (unsigned int*)buff, 1, startIndex, numOfRows);
   else if(fmt == 7)
      IBISColumnWrite(ibis, (float*)buff , 1, startIndex, numOfRows);
   else if(fmt == 8)
      IBISColumnWrite(ibis, (double*)buff, 1, startIndex, numOfRows);

   if(fmt == 8) return startIndex+numOfRows;
   else return startIndex+numOfRows;
}

/*************************************************************/
int writeHdr(ibis, fmt, tag, cycle, length, index)
   int ibis, fmt, tag, cycle, length, index;
{
   int i;
   void *hdr;

   mz_alloc1((unsigned char**)&hdr, 16, 1);
   for(i = 0; i < 16; i++) ((unsigned char*)hdr)[i] = 0;

   ((unsigned char*)hdr)[0] = (unsigned char)fmt;
   ((unsigned int*)hdr)[1] = tag;
   ((unsigned char*)hdr)[8] = (unsigned char)cycle;
   ((unsigned int*)hdr)[3] = length;

   if(fmt == 8) 
   {
      writeToIBIS(ibis, hdr, index, 2, fmt);
      free(hdr);
      return index + 2;
   }
   else if(fmt == 4 || fmt == 7)
   {
      writeToIBIS(ibis, hdr, index, 4, fmt);
      free(hdr);
      return index + 4;
   }

   return -1;
}

/*************************************************************/
void printEntity(ibis, row, fmt, nVertices)
   int ibis, row, fmt, nVertices;
{
   int nr, type, tag, cycle, len, i, j;
   double *buff;

   IBISFileGet(ibis, "nr", &nr, 0, 0, 0);
   if(nr < row) return;

   getHdr(ibis, &type, &tag, &cycle, &len, row, fmt);
   if(fmt == 4 || fmt == 7)
      row += 4;
   else if(fmt == 8)
      row += 2;

   printf("tag: %d type: %d cycle: %d length: %d row: %d\n", tag, type, cycle, len, row);

   if(nVertices < len && nVertices >= 0) len = nVertices;
   mz_alloc1((unsigned char**)&buff, len, sizeof(double));
   if(len > 0) IBISColumnRead(ibis, buff, 1, row, len);

   for(i = 0; i < len; i+=cycle)
   {
      for(j = 0; j < cycle; j++)
         printf("\t%f", buff[i+j]);
      printf("\n");
   }

   free(buff);
}

/*************************************************************/
int openIBISOutSLP(nCol, nRow, fmt)
   int nCol, nRow, fmt;
{
   int unit, ibis, status, i;
   int parmct, parmdf;
   char outFile[99];

   zvselpi(0);
   zvparm("out", outFile, &parmct, &parmdf, 1, 99);
   strcat(outFile, ".slp");

   zvunit(&unit, "U_NAME", 1, "U_NAME", outFile, 0);
   status = IBISFileUnit(unit, &ibis, "write", nCol, nRow, 0, 0);
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("IBIS opening name: %s row: %d col: %d ibis: %d\n", outFile, nRow, nCol, ibis);

   if(fmt == 8)
      status = IBISFileSet(ibis, "fmt_default", "doub", 1);
   else if(fmt == 4)
      status = IBISFileSet(ibis, "fmt_default", "full", 1);
   else if(fmt == 7)
      status = IBISFileSet(ibis, "fmt_default", "real", 1);
   if(status != 1) IBISSignal(ibis, status, 1);

   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

/*************************************************************/
int openIBISOutDLP(nCol, nRow, fmt)
   int nCol, nRow;
   char fmt[][6];
{
   int unit, ibis, status, i;
   int parmct, parmdf;
   char outFile[99];

   zvselpi(0);
   zvparm("out", outFile, &parmct, &parmdf, 1, 99);
   strcat(outFile, ".dlp");

/*
   printf("=====\n");
   for(i = 0; i < nCol; i++)
      printf("%s\n", fmt[i]);
*/

   zvunit(&unit, "U_NAME", 1, "U_NAME", outFile, 0);
   status = IBISFileUnit(unit, &ibis, "write", nCol, nRow, fmt, "column");
   if(status != 1) IBISSignalU(unit, status, 1);
   printf("IBIS opening name: %s row: %d col: %d ibis: %d\n", outFile, nRow, nCol, ibis);

   status = IBISFileUnitOpen(ibis);
   if(status != 1) IBISSignal(ibis, status, 1);

   return ibis;
}

/*************************************************************/
int getLastTag(data, nr)
   int nr;
   void *data;
{
   int tag, index, totIndex, type;
   void *iterate;

   iterate = data;

   totIndex = 0;
   while(totIndex < nr)
   {
      index = nextEntityIndex(iterate);
      totIndex += index;
      tag = getTag(iterate);

      type = getType(iterate);
      if(type == 8) (double*)iterate += index;
      else if(type == 4 || type == 7) (unsigned int*)iterate += index;
   }

   return tag;
}
/*************************************************************/
