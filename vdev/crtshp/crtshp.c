#include <stdio.h>
#include <stdlib.h>
#include "vicmain_c.h"

#include "shapefil.h"
#include "carto/cartoMemUtils.h"
#include <string.h>

/* prototype */
void createDBF(const char *fileName, int nEntities);
void createTxt(const char *fileName, int minX, int maxX, int minY, int maxY, int inc);



/****************************************************************/
void createDBF(fileName, nEntities)
     const char* fileName;
     int nEntities;
{
  DBFHandle dbfHandle;
    char message[100];

  dbfHandle = DBFCreate(fileName);
  

  DBFAddField(dbfHandle, "ID", FTInteger, 9, 0);
  DBFAddField(dbfHandle, "HWY NAME", FTString, 10, 0);
  int i;
  for(i = 0; i < nEntities; i++)
    {
      DBFWriteDoubleAttribute(dbfHandle, i, 0, i);
      char hwyName[10];
      sprintf(hwyName, "HWY %d", i+1);
      DBFWriteStringAttribute(dbfHandle, i, 1, hwyName);
    }
  DBFClose(dbfHandle);

  DBFOpen(fileName, "rb");
  for(i = 0; i < DBFGetRecordCount(dbfHandle); i++)
    {
      sprintf(message,"id: %d ", DBFReadIntegerAttribute(dbfHandle, i, 0));
      zvmessage(message," ");
      sprintf(message,"name: %s\n", DBFReadStringAttribute(dbfHandle, i, 1));
      zvmessage(message," ");
    }
  DBFClose(dbfHandle);
}
/*****************************************************************/
void createTxt(fileName, minX, maxX, minY, maxY, inc)
     const char*fileName;
     int minX, maxX, minY, maxY, inc;
{
//  int nEntities = maxY/inc;

  FILE *file = fopen(fileName, "w");
  int j;
  for(j = 0; j < maxY; j+=inc)
    fprintf(file, "2\n2\n%d\n%d\n%d\n%d\n",minX, maxX, minY + j, minY + j);

  fclose(file);
}

/******************************************************************/
void main44()
{
   SHPHandle shpHandle;
   int cycle, nEntities=0;
    int ct, def;
    char inFileName[99], outFileName[99];
    FILE *infile;
    char message[100];
     int nVertices;
     SHPObject *shpObject;
     double *xBuf, *yBuf, *zBuf;

    zifmessage ("crtshp - 11-11-2019 - rjb - 64-bit");
    
   createTxt("inPolys.txt", 0, 5295, 0, 3674, 10);

   zvparm("inp",inFileName,&ct,&def,1,0); 
   infile = fopen(inFileName, "r");

   zvparm("out",outFileName,&ct,&def,1,0);

   shpHandle = SHPCreate(outFileName, SHPT_POLYGON);
   while(fscanf(infile, "%d", &cycle) != -1)
   {

     sprintf(message,"cycle: %d\n", cycle);
     zvmessage(message," ");

     fscanf(infile, "%d", &nVertices);
     sprintf(message,"nVertices: %d\n", nVertices);
     zvmessage(message," ");

     mz_alloc1((unsigned char**)&xBuf, nVertices, sizeof(double));
     mz_alloc1((unsigned char**)&yBuf, nVertices, sizeof(double));
     if(cycle == 3)
       mz_alloc1((unsigned char**)&zBuf, nVertices, sizeof(double));

     int i;
     for(i = 0; i < nVertices; i++)
       fscanf(infile, "%lf", &xBuf[i]);
     for(i = 0; i < nVertices; i++)
       fscanf(infile, "%lf", &yBuf[i]);
     if(cycle == 3)
       for(i = 0; i < nVertices; i++)
	 fscanf(infile, "%lf", &zBuf[i]);

     for(i = 0; i < nVertices; i++) {
       sprintf(message,"x = %f y = %f\n", xBuf[i], yBuf[i]);
       zvmessage(message," ");  
    }
     if(cycle == 2)
       shpObject = SHPCreateSimpleObject(SHPT_POLYGON, nVertices, xBuf, yBuf, NULL);
     else if(cycle == 3)
       shpObject = SHPCreateSimpleObject(SHPT_POLYGON, nVertices, xBuf, yBuf, zBuf);

     SHPWriteObject(shpHandle, -1, shpObject);
     SHPDestroyObject(shpObject);
     free(xBuf);
     free(yBuf);
     if(cycle == 3) free(zBuf);

     ++nEntities;
   }
    sprintf (message,"nEntities = %d\n",nEntities);
    zvmessage(message," ");
   createDBF("testShapes", nEntities);

   SHPClose(shpHandle);
   fclose(infile);
}
