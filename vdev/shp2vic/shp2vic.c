#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"

#include "carto/shapefil.h"

void loadFeatureNames (char * path);
char * featureName (int code);

void main44(void)
{
   char inpfilename [99];
   char outfilename [99];
   int parmct, parmdf, swap, docodes, doz;
   SHPHandle shapeHandle;
   SHPObject * shapeObject;
   DBFHandle dbfHandle;
   int dbfFieldCount, dbfRecordCount=0;
   int nEntities, nShapetype;
   int iEntity;
   double xyzmMin [4], xyzmMax [4];
   double * xColumn, * yColumn, * zColumn;
   int numRows, row;
   int status, vunit, iunit=0;
   char ** codeFields, ** ctdbFields;
   int index;
   int codeIndex = -1, ctdbIndex = -1;
           
   zifmessage("shp2vic version Thu Jan  3 2008");

   zvparm ("shpinp", inpfilename, &parmct, &parmdf, 1, 99);
   zvparm ("ibisout", outfilename, &parmct, &parmdf, 1, 99);
   swap = zvptst("swap");
   docodes = zvptst("docodes");
   doz = zvptst("doz");

   if (docodes) {
     /* get field names from dbf */
     dbfHandle = DBFOpen (inpfilename, "rb");
     if (! dbfHandle)
       zmabend ("failed to open shpinp parm with dbf suffix");

     dbfFieldCount = DBFGetFieldCount (dbfHandle);
     dbfRecordCount = DBFGetRecordCount (dbfHandle);
     
     codeFields = (char **) malloc (sizeof (char *) * dbfRecordCount);
     ctdbFields = (char **) malloc (sizeof (char *) * dbfRecordCount);

     if ((codeIndex = DBFGetFieldIndex (dbfHandle, "CODE")) < 0)
       zmabend ("didn't find CODE field in dbf");
     if ((ctdbIndex = DBFGetFieldIndex (dbfHandle, "CTDB_TYPE")) < 0)
       zmabend ("didn't find CTDB_TYPE field dbf");

     printf ("type of CODE is %c\n", DBFGetNativeFieldType (dbfHandle, codeIndex));
     printf ("type of CTDB_TYPE is %c\n", DBFGetNativeFieldType (dbfHandle, ctdbIndex));
     
     for (index = 0; index < dbfRecordCount; index ++) {
       codeFields [index] = strdup (DBFReadStringAttribute (dbfHandle, index, codeIndex));
       ctdbFields [index] = strdup (DBFReadStringAttribute (dbfHandle, index, ctdbIndex));
     }

     DBFClose (dbfHandle);
   }

   /* open shape input */
   shapeHandle = SHPOpen (inpfilename, "rb");
   if (! shapeHandle)
     zmabend ("failed to open shpinp with shp or shx suffix");

   SHPGetInfo (shapeHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

   if (docodes)
     if (nEntities != dbfRecordCount)
       zmabend ("shape and dbf files have different numbers of shapes");

   numRows = 0;
   for (iEntity = 0; iEntity < nEntities; iEntity ++) {
     int part, vertex;
     shapeObject = SHPReadObject (shapeHandle, iEntity);

     for (part = 0, vertex = 0; vertex < shapeObject->nVertices; vertex ++) {
       if (part < shapeObject->nParts && shapeObject->panPartStart [part] == vertex) {
	 part ++;
  	 if (part > 1)
	   numRows ++;		/* Insert null at part boundaries*/
       }
       numRows ++;		/* This is a row */
     }

     numRows ++;		/* Insert null at entity boundaries */

     SHPDestroyObject (shapeObject);
   }

   xColumn = (double *) malloc (sizeof (double) * numRows);
   yColumn = (double *) malloc (sizeof (double) * numRows);
   if (doz)
     zColumn = (double *) malloc (sizeof (double) * numRows);
   else
     zColumn = 0;

   for (row = 0, iEntity = 0; iEntity < nEntities; iEntity ++) {
     int part, vertex;
     shapeObject = SHPReadObject (shapeHandle, iEntity);

     for (part = 0, vertex = 0; vertex < shapeObject->nVertices; vertex ++) {
       if (part < shapeObject->nParts && shapeObject->panPartStart [part] == vertex) {
	 part ++;
	 if (part > 1) {
	   xColumn [row] = 0.0;
	   yColumn [row] = 0.0;
	   if (doz)
	     zColumn [row] = 0.0;
	   row ++;
	 }
       }
       xColumn [row] = shapeObject->padfX [vertex];
       yColumn [row] = shapeObject->padfY [vertex];
       if (doz)
	 zColumn [row] = shapeObject->padfZ [vertex];
       row ++;
     }
     xColumn [row] = 0.0;
     yColumn [row] = 0.0;
     if (doz)
       zColumn [row] = 0.0;
     row ++;

     SHPDestroyObject (shapeObject);
   }

   /* open/setup IBIS output for x, y, possibly z */
   if ((status = zvunit (& vunit, "U_NAME", 1, "U_NAME", outfilename, NULL)) != 1)
     zmabend ("zvunit failed on U_NAME");
   if ((status = IBISFileUnit (vunit, &iunit, "write", doz?3:2, numRows,0, 0)) != 1)
     IBISSignal (iunit, status, 1);

   if ((status = IBISFileSet( iunit, "fmt_default", "doub", 1 )) != 1)
     IBISSignal (iunit, status, 1);
   if ((status = IBISFileUnitOpen (iunit)) != 1)
     IBISSignal (iunit, status, 1);

   /* write IBIS */
   if ((status = IBISColumnWrite (iunit, (char*)xColumn, swap?2:1, 1, numRows)) != 1)
     IBISSignal (iunit, status, 1);
   if ((status = IBISColumnWrite (iunit, (char*)yColumn, swap?1:2, 1, numRows)) != 1)
     IBISSignal (iunit, status, 1); 
   if (doz)
     if ((status = IBISColumnWrite (iunit, (char*)zColumn, 3, 1, numRows)) != 1)
       IBISSignal (iunit, status, 1); 

   /* close IBIS */
   if ((status = IBISFileClose (iunit, 0)) != 1)
     IBISSignal (iunit, status, 1);

   SHPClose (shapeHandle);
}


