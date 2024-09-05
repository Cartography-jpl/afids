#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#include "shapefil.h"

typedef struct point {
  double x;
  double y;
} point;

typedef struct poly {
  point * points;
  int length;
} poly;

int pointInForeground (double x, double y, int polyCount, poly * polys);

int main (int argc, char * argv []) {
   SHPHandle shapeHandle;
   SHPObject * shapeObject;
   int nEntities, nShapetype;
   int iEntity;
   double xyzmMin [4], xyzmMax [4];
   int i;
   int polyCount = 0;
   poly * polys = 0;
   int fieldCount, field;
   DBFFieldType fieldType;
   char fieldName [13];
   int fieldWidth;
   int decimals;
   char * value;

   if (argc < 4) {
     fprintf (stderr, "usage: %s in out other1 other2 ... othern\n", argv [0]);
     exit (1);
   }

   /* load all other polygons into memory */
   for (i = 3; i < argc; i ++) {
     /* open shape input */
     if (! (shapeHandle = SHPOpen (argv[i], "rb"))) {
       fprintf (stderr, "error opening %s for input\n", argv[i]);
       exit (1);
     }   

     SHPGetInfo (shapeHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);

     if (nShapetype != SHPT_POLYGON) {
       fprintf (stderr, "error: %s does not contain SHPT_POLYGON shapes\n", argv[i]);
       exit (1);
     }

     polyCount += nEntities;
     polys = realloc (polys, polyCount * sizeof(poly));

     for (iEntity = 0; iEntity < nEntities; iEntity ++) {
       int vertex;

       shapeObject = SHPReadObject (shapeHandle, iEntity);

       polys[polyCount - nEntities + iEntity] . length = shapeObject->nVertices;
       polys[polyCount - nEntities + iEntity] . points = (point *) malloc( shapeObject->nVertices * sizeof(point) );

       for (vertex = 0; vertex < shapeObject->nVertices; vertex ++) {
	 polys[polyCount - nEntities + iEntity] . points [vertex] . x = shapeObject->padfX [vertex];
	 polys[polyCount - nEntities + iEntity] . points [vertex] . y = shapeObject->padfY [vertex];
       }

       SHPDestroyObject (shapeObject);
     }

     SHPClose (shapeHandle);
   }

   {
     SHPHandle shapeIn, shapeOut;
     DBFHandle dbfIn, dbfOut;
     int outShapeIndex;

     if (! (shapeOut = SHPCreate (argv[2], SHPT_POLYGON))) {
       fprintf (stderr, "error opening %s shape for output\n", argv[2]);
       exit (1);
     }   

     if (! (dbfOut = DBFCreate (argv[2]))) {
       fprintf (stderr, "error opening %s dbf for output\n", argv[2]);
       exit (1);
     }   

     if (! (shapeIn = SHPOpen (argv[1], "rb"))) {
       fprintf (stderr, "error opening %s shape for input\n", argv[1]);
       exit (1);
     }   

     if (! (dbfIn = DBFOpen (argv[1], "rb"))) {
       fprintf (stderr, "error opening %s dbf for input\n", argv[1]);
       exit (1);
     }   

     /* Copy DBF Schema */
     fieldCount = DBFGetFieldCount (dbfIn);

     for (field = 0; field < fieldCount; field ++) {
       fieldType = DBFGetFieldInfo (dbfIn, field, fieldName, & fieldWidth, & decimals);
       if (fieldType == FTString) {
	 if (DBFAddField (dbfOut, fieldName, FTString, fieldWidth, decimals) < 0 ) {
	   fprintf (stderr, "error adding dbf field to %s\n", argv[2]);
	   exit (1);
	 }
       } else {
	 fprintf (stderr, "non string dbf field in %s\n", argv[1]);
	 exit (1);
       }
     }

     /* get shape count */
     SHPGetInfo (shapeIn, & nEntities, & nShapetype, xyzmMin, xyzmMax);

     if (nShapetype != SHPT_POLYGON) {
       fprintf (stderr, "error: %s does not contain SHPT_POLYGON shapes\n", argv[1]);
       exit (1);
     }

     for (iEntity = 0; iEntity < nEntities; iEntity ++) {
       shapeObject = SHPReadObject (shapeIn, iEntity);

       if (pointInForeground (shapeObject->padfX [0], shapeObject->padfY [0], polyCount, polys)) {
	 outShapeIndex = SHPWriteObject (shapeOut, -1, shapeObject);

	 /* copy dbf fields */
	 for (field = 0; field < fieldCount; field ++) {
	   value = (char *) DBFReadStringAttribute (dbfIn, iEntity, field);

	   if (value) {
	     value = strdup (value);
	   } else {
	     value = strdup ("");
	   }

	   if (! DBFWriteStringAttribute (dbfOut, outShapeIndex, field, value)) {
	     fprintf (stderr, "DBFWriteStringAttribute failed\n");
	     exit (1);
	   }

	   free (value);
	 }
       }

       SHPDestroyObject (shapeObject);
     }

     SHPClose (shapeIn);
     SHPClose (shapeOut);
     DBFClose (dbfIn);
     DBFClose (dbfOut);
   }

   return 0;
}

/*
 * C code from the article
 * "An Incremental Angle Point in Polygon Test"
 * by Kevin Weiler, kjw@autodesk.com
 * in "Graphics Gems IV", Academic Press, 1994
 */

	/* quadrant id's, incremental angles, accumulated angle values */
typedef short quadrant_type;

	/* result value from point in polygon test */
typedef enum pt_poly_relation {OUTSIDE, INSIDE} pt_poly_relation;

	/* determine the quadrant of a polygon point
	   relative to the test point */
#define quadrant(vertex, x, y) \
  ( (vertex->x > x) ? ((vertex->y > y) ? 0 : 3) : ( (vertex->y > y) ? 1 : 2) )

	/* determine x intercept of a polygon edge
	   with a horizontal line at the y value of the test point */
#define x_intercept(pt1, pt2,  yy) \
  (pt2->x - ( (pt2->y - yy) * ((pt1->x - pt2->x) / (pt1->y - pt2->y)) ) )

	/* adjust delta */
#define adjust_delta(delta, vertex, next_vertex, xx, yy)		\
  switch (delta) {							\
	    /* make quadrant deltas wrap around */			\
    case  3:	delta = -1; break;					\
    case -3:	delta =	 1; break;					\
	    /* check if went around point cw or ccw */			\
    case  2: case -2: if (x_intercept(vertex, next_vertex, yy) > xx)	\
		    delta =  - (delta);					\
		break;							\
    }

int pointInPoly (double x, double y, poly * polyp)
{
  int i;
  point *  vertex, * first_vertex, * next_vertex;
  quadrant_type quad, next_quad, delta, angle;

  /* initialize */
  i = 0;
  vertex = first_vertex = polyp -> points;
  quad = quadrant(vertex, x, y);
  angle = 0;
  /* loop on all vertices of polygon */
  do {
    i ++;
    if (i == polyp -> length)
      i = 0;
    next_vertex = polyp -> points + i;
    /* calculate quadrant and delta from last quadrant */
    next_quad = quadrant(next_vertex, x, y);
    delta = next_quad - quad;
    adjust_delta(delta,vertex,next_vertex,x,y);
    /* add delta to total angle sum */
    angle = angle + delta;
    /* increment for next step */
    quad = next_quad;
    vertex = next_vertex;
  } while (vertex != first_vertex);

  /* complete 360 degrees (angle of + 4 or -4 ) means inside */
  if ((angle == +4) || (angle == -4))
    return INSIDE;
  else
    return OUTSIDE;
}

int pointInForeground (double x, double y, int polyCount, poly * polyp) {
  int i;

  for (i = 0; i < polyCount; i ++)
    if (pointInPoly (x, y, polyp + i)) {
      return 1;
    }

  return 0;
}

