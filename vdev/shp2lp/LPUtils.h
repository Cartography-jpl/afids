#ifndef LPUTILS_H
#define LPUTILS_H

#include "shapefil.h"
#define LP_STRING 17
#define LP_DOUBLE 8
#define LP_FLOAT 7
#define LP_FULL 4
#define LP_X 1
#define LP_Y 2
#define LP_Z 3
#define LP_SLP 10
#define LP_DLP 11

/***********************************************
LPUtils.h

Written by Peter Kim

Aug 02 2007: Documented for testing and finalizing. pk
***********************************************/

/***********************************************
This structure holds LP shape data.

data         : holds raw data that can be output
               into a .slp file without any
               additional formatting
map          : holds a mapping of all the entity
               locations in the data so that searching
               for an entity will have complexity O(1).
vertCnt      : holds number of vertices of each entity
numOfEntities: holds number of entities in LPList
totDataSize  : holds number of rows in IBIS .slp file.
***********************************************/
typedef struct lpshp
{
   double *data;
   int *map, *vertCnt, numOfEntities, totDataSize, fmt;
} LPList;

/***********************************************
This structure holds LP tabular data.
Note: Tags in lpTble is virtual, in that they
      are calculated during runtime according
      to a record's position in the table.  It
      does not read in the tag number from the
      .dlp IBIS file but rather is assumed to
      be ordered and increasing by 1.

data         : holds raw data that can be output
               into a .dlp file without any
               additional fomatting.  Although the
               data is held in a 1-D array, users
               can access it in (record, field)
               manner through functions.
fmt          : holds column formats in IBIS format
               "A16" - alphnumeric column
               "FULL" - integer column
               "DOUB" - double column
recordSize   : holds the size of a single record
               in bytes
numOfEntities: holds number of entities in LPTble
numOfColumns : holds number of columns in LPTble
colLoc       : holds location of columns in
               bytes.  Used for quick field
               lookup.
               (ex. if columns of LPTble are
               "A16", "FULL", "DOUB" then
               colLoc would have 3 entities with
               colLoc[0] = 0 (start of record)
               colLoc[1] = 17 (16 char string +
                               1 for null)
               colLoc[2] = 21 (17 + 4 from "FULL")
***********************************************/
typedef struct lpdata
{
   void *data;
   char fmt[99][6];
   int recordSize, numOfEntities, numOfColumns, *colLoc;
} LPTble;

/***********************************************
This structure is for future work and not yet
implemented.  But it is evident that if LP
is used frequently in the future, then consistency
checking will be required between the shape
and tabular data.  This structure will help
with that process by connecting both
shape and tabular data together.

shp          : holds LPList
data         : holds LPTble
***********************************************/
typedef struct lp
{
   LPList *shp;
   LPTble *data;
} LP;

/***********************************************
This function prints a single tabular record.
***********************************************/
void printTbleEntity(LPTble *tble, int entityNum);

/***********************************************
This function returns a LPList from a given
.slp file.
***********************************************/
LPList* slp2LPList(int ibis);

/***********************************************
This function returns a LPList from a given
.shp file handle.
***********************************************/
LPList* shp2LPList(SHPHandle handle);

/***********************************************
This function returns the index of the tabular
data (stored in a 1-D array) that is inside the
given LPTble by record and field.  Index is
the offset from the beginning of the data
in byte (char*) elements.
***********************************************/
int getTbleIndex(LPTble *tble, int record, int field);

/***********************************************
This function writes the given string into
the tabular data in the given field and record.
PRECONDITIONS: the given field must be a string
field and the given record number must be 
within the current data size (it does not 
GROW the table).
***********************************************/
void writeTbleString(LPTble *tble, char *string, int field, int record);

/***********************************************
This function writes the given double value into
the tabular data in the given field and record.
PRECONDITIONS: the given field must be a double
field and the given record number must be 
within the current data size (it does not 
GROW the table).
***********************************************/
void writeTbleDouble(LPTble *tble, double value, int field, int record);

/***********************************************
This function writes the given integer value into
the tabular data in the given field and record.
PRECONDITIONS: the given field must be a integer
field and the given record number must be 
within the current data size (it does not 
GROW the table).
***********************************************/
void writeTbleInt(LPTble *tble, int value, int field, int record);

/***********************************************
This function returns a LPTble from the given
.dlp tabular file.
***********************************************/
LPTble* dlp2LPTble(int ibis);

/***********************************************
This function returns a LPTble from the given
.dbf tabular file.
***********************************************/
LPTble* dbf2LPTble(DBFHandle handle);

/***********************************************
This function destroys the LPList and frees
up memory.  Must be called after creating
a LPList to prevent memory leak.
***********************************************/
void destroyLPList(LPList *list);

/***********************************************
This function destroys the LPTble and frees
up memory.  Must be called after creating
a LPTble to prevent memory leak.
***********************************************/
void destroyLPTble(LPTble *table);

/***********************************************
This function returns a double X, Y, or Z value
from the given LPList.
***********************************************/
double getDoubleXYorZ(LPList *list, int entity, int vertex, int xyOrz);

/***********************************************
This function sets a double X, Y, or Z value
on the given LPList.
PRECONDITION: The given entity must be within
the number of existing entities in the list.
The vertex must be within the existing vertex
types (i.e. do not attempt to set Z when the
vertex consists of only X and Y values).  This
function does not GROW the list.
***********************************************/
void setDoubleXYorZ(LPList *list, int entity, int vertex, int xyOrz, double val);

/***********************************************
This function returns the length of an entity
from the given LPList.  It returns the length
in integer form no matter what format the
LPList is in.
***********************************************/
int getLength(LPList *list, int entity);

/***********************************************
This function returns the tag of an entity
from the given LPList.  It returns the tag
in integer form no matter what format the
LPList is in.
***********************************************/
int getTag(LPList *list, int entity);

/***********************************************
This function returns the cycle of an entity
from the given LPList.  It returns the cycle
in integer form no matter what format the
LPList is in.
***********************************************/
int getCycle(LPList *list, int entity);

/***********************************************
This function prints a single entity from
the given LPList specified by the entity number.  
It also prints only the specified amount of 
vertices.  If the nVertices == -1, it will
print all the vertices.
***********************************************/
void printListEntity(LPList *list, int entityNum, int nVertices);

/***********************************************
This function adds 2 given LPLists and returns
a third LPList that is a concatenation of
the 2.  The tag numbers are reset in ascending
order starting from 1.
PRECONDITION: The 2 LPLists must be in the
same format.
***********************************************/
LPList* addLists(LPList *list1, LPList *list2);

/***********************************************
This function opens a .slp file for outputting
a LPList with specified row, format, and
"out" parameter in the proc call.  It returns
the IBIS number to the .slp file.
***********************************************/
int openIBISOutSLP(int nRow, int fmt, int outParm);

/***********************************************
This function adds 2 given LPTbles and returns
a third LPTble that is a concatenation of
the 2.  Please remember the LPTble tags are
virtual in that they are simply determined by
the record's position in the table.
PRECONDITION: The 2 LPTbles must have the
same column formats.
***********************************************/
LPTble* addTbles(LPTble *tble1, LPTble *tble2);

/***********************************************
This function outputs the given LPList into
the given .slp IBIS file.
***********************************************/
void printSLP(int ibis, LPList *list);

/***********************************************
This function outputs the given LPTble into
the given .dlp IBIS file.
***********************************************/
void printDLP(int ibis, LPTble *tble);

/***********************************************
This function opens a .dlp file for outputting
a LPTble with specified row, format, column, and
"out" parameter in the proc call.  It returns
an number to an IBIS file.
***********************************************/
int openIBISOutDLP(LPTble *tble, int outParm);

/***********************************************
This function returns an empty LPList.  The
returned list will have 0 entities.  All
dynamic arrays inside the LPList structure
will have been allocated but with 0 memory
length.  User can use insertListEntity function
to add entities into this empty list.  A
desired format must be entered (4 - full
7 - real, 8 - double).
***********************************************/
LPList* getEmptyList(int fmt);

/***********************************************
This function returns an empty LPTble.  The
returned table will have 0 records.  All
dynamic arrays inside the LPTble structure
will have been allocated but with 0 memory
length.  User can use insertTbleRecord function
to add records into this empty table.  A
desired format must be entered (4 - full
7 - real, 8 - double) along with the size
and format of the columns and rows.  Please
look at LPTble struct above for detailed
description of the function parameters.
***********************************************/
LPTble* getEmptyTble(char fmt[99][6], int *colLoc, int recordSize, int nColumns);

/***********************************************
This function will return a table record
through the recordBuf parameter.  The LPTble
of the record, record number, and the pre-
allocated recordBuf must be passed through the
parameter.
***********************************************/
void getTbleRecord(LPTble *tble, int i, void *recordBuf);

/***********************************************
This function will allows the user to insert
a record to the end of the table.  The
record is passed through the recordBuf
parameter and is inserted into the given LPTble.
The LPTble, record number, and the record
buffer must be passed in through the parameter.
***********************************************/
void insertTbleRecord(LPTble *tble, void *recordBuf);

/***********************************************
This function will allows the user to insert
an entity into the given LPList.  The entity
is passed through the buf and is inserted
at the endo of the list.  The LPList, number
of vertices, entity buffer, and the size of the
buffer must all be passed through the parameter.
***********************************************/
void insertListEntity(LPList *list, int nVertices, void *buf, int sizeOfBuf);

#endif

















