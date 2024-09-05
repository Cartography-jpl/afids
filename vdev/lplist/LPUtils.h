#ifndef LPUTILS_H
#define LPUTILS_H

#define LP_DOUBLE 8
#define LP_FLOAT 7
#define LP_FULL 4

typedef struct lplist
{
   void *data;
   int *map, numOfEntities;
} LPList;

LPList* getLPList(int ibis);

int getEntityXYorZPayload(void *data, char xyOrz, unsigned char **xyOrzData);

int getEntityPayload(void *data, void **payload);

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
