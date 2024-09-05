#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "shapefil.h"

int main (int argc, char * argv []) {
   SHPHandle shapeHandle;
   SHPObject * shapeObject;
   int nEntities, nShapetype;
   int iEntity;
   double xyzmMin [4], xyzmMax [4];
   FILE * cbsout;
   int cbsVecId, part;
   int vectorTypeId;
   int firstVectorId;

   if (argc == 5) {
     if (! (cbsout = fopen (argv [2], "a"))) {
       fprintf (stderr, "error appending %s\n", argv [2]);
       exit (1);
     }

     if (sscanf (argv [3], "%d", & vectorTypeId) != 1) {
       fprintf (stderr, "error parsing vectorTypeId (an integer) from \"%s\"\n", argv [3]);
       exit (1);
     }

     if (sscanf (argv [4], "%d", & firstVectorId) != 1) {
       fprintf (stderr, "error parsing firstVectorId (an integer) from \"%s\"\n", argv [4]);
       exit (1);
     }
   } else {
     fprintf (stderr, "usage: %s shpin cbsout vectorTypeId firstVectorId\n", argv [0]);
     exit (1);
   }

   /* open shape input */
   if (! (shapeHandle = SHPOpen (argv [1], "rb"))) {
     fprintf (stderr, "error opening %s for input\n", argv [1]);
     exit (1);
   }   

   SHPGetInfo (shapeHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

   /* check shape type */
   switch (nShapetype) {
   case SHPT_NULL:
     printf ("%s contains an unsupported shape entity type (NULL)\n", argv [1]); exit (1);
   case SHPT_POINT:
     printf ("%s contains an unsupported shape entity type (POINT)\n", argv [1]); exit (1);
   case SHPT_ARC:
   case SHPT_POLYGON:
     break;
   case SHPT_MULTIPOINT:
     printf ("%s contains an unsupported shape entity type (MULTIPOINT)\n", argv [1]); exit (1);
   case SHPT_POINTZ:
     printf ("%s contains an unsupported shape entity type (POINTZ)\n", argv [1]); exit (1);
   case SHPT_ARCZ:
     printf ("%s contains an unsupported shape entity type (ARCZ)\n", argv [1]); exit (1);
   case SHPT_POLYGONZ:
     printf ("%s contains an unsupported shape entity type (POLYGONZ)\n", argv [1]); exit (1);
   case SHPT_MULTIPOINTZ:
     printf ("%s contains an unsupported shape entity type (MULTIPOINTZ)\n", argv [1]); exit (1);
   case SHPT_POINTM:
     printf ("%s contains an unsupported shape entity type (POINTM)\n", argv [1]); exit (1);
   case SHPT_ARCM:
     printf ("%s contains an unsupported shape entity type (ARCM)\n", argv [1]); exit (1);
   case SHPT_POLYGONM:
     printf ("%s contains an unsupported shape entity type (POLYGONM)\n", argv [1]); exit (1);
   case SHPT_MULTIPOINTM:
     printf ("%s contains an unsupported shape entity type (MULTIPOINTM)\n", argv [1]); exit (1);
   case SHPT_MULTIPATCH:
     printf ("%s contains an unsupported shape entity type (MULTIPATCH)\n", argv [1]); exit (1);
   default:
     printf ("%s contains an unsupported shape entity type (UNKNOWN)\n", argv [1]); exit (1);
   }

   for (iEntity = 0, cbsVecId = firstVectorId; iEntity < nEntities; iEntity ++) {
     int vertex;
     shapeObject = SHPReadObject (shapeHandle, iEntity);

     part = 0;
     for (vertex = 0; vertex < shapeObject->nVertices; vertex ++) {
       if (vertex == shapeObject -> panPartStart [part]) { /* first vertex of part */
	 if (part != 0)
	   fprintf (cbsout, "END\n");

	 {
	   int partLength;

	   if (shapeObject -> nParts == 1)
	     partLength = shapeObject->nVertices;
	   else if (part < shapeObject -> nParts - 1)
	     partLength = shapeObject -> panPartStart [part + 1] - vertex;
	   else
	     partLength = shapeObject->nVertices - vertex;

	   /* cbs vector header */
	   fprintf (cbsout, " %d %d 2 %d 0 0 0 0\n", vectorTypeId, partLength, cbsVecId ++);
	   fprintf (cbsout, "0.0 0.0 0.0 0.0 0.0\n");
	 }

	 if (part < shapeObject -> nParts - 1)
	   part ++;
       }

       fprintf (cbsout, "%11.6f %10.6f           0.0\n", shapeObject->padfX [vertex], shapeObject->padfY [vertex]);

     }

     fprintf (cbsout, "END\n");

     SHPDestroyObject (shapeObject);
   }

   SHPClose (shapeHandle);

   fclose (cbsout);

   return 0;
}
