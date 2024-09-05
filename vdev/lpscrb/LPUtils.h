#ifndef LPUTILS_H
#define LPUTILS_H

#define LP_DOUBLE 8
#define LP_FLOAT 7
#define LP_FULL 4
#define LP_X 1
#define LP_Y 2
#define LP_Z 3

typedef struct lplist
{
   void *data;
   int *map, numOfEntities, numOfVertices;
} LPList;

LPList* getLPList(int ibis);

void destroyLPList(LPList *list);

int getEntityLength(LPList *list, int entity);

int getEntityTag(LPList *list, int entity);

int getEntityNumOfVertices(LPList *list, int entity);

int getEntityCycle(LPList *list, int entity);

double getDoubleXYorZ(LPList *list, int entity, int vertex, int xyOrz);

void setDoubleXYorZ(LPList *list, int entity, int vertex, int xyOrz, double val);

LPList* getLPList(int ibis);

void getHdr(int ibis, int *type, int *tag, int *cycle, int *length, int startPos, int fmt);

void setTag(void *data, int tag);

int getType(void *data);

int getTag(void *data);

int getCycle(void *data);

int getLength(void *data);

int nextEntityIndex(void *data);

int writeToIBIS(int ibis, void *buff, int startIndex, int numOfRows, int fmt);

int writeHdr(int ibis, int fmt, int tag, int cycle, int length, int index);

void printEntity(int ibis, int row, int fmt, int nVertices);

int openIBISOutSLP(int nCol, int nRow, int fmt);

int openIBISOutDLP(int nCol, int nRow, char fmt[][6]);

int getLastTag(void *data, int nr);

#endif
