#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gems.h"
#include "shiftNscale.h"
#include "loadlines.h"

#define lineBufSize 100

/*
 * getVectorType expects fSource to be looking at a vector id in an
 * FCF file. It skips the id and gets the next line.  If the next line
 * is "END", it returns happy with END stuffed in the
 * lineBuf. Otherwise, it tries to parse a vector ID out of the
 * lineBuf. If successful, it tries to read another line, expecting a
 * vector type. If successful, it returns the vector type in the
 * lineBuf, with lineNumP pointing to the next line. The arg fcfName
 * should be the name of the source FCF file. It is used for error
 * messages. */

static int getVectorType (char * lineBuf, FILE * fSource, char * fcfName, int * lineNumP) {
  /* get vector type */
  if (!fgets (lineBuf, lineBufSize, fSource))
    strcpy (lineBuf, "END"); /* end of file */

  (*lineNumP) ++;

  return 0;
}

/* returns 0 when happy */
static int readPoints (char * path,
		       char * inBuf,
		       int * pointCountP,
		       int * lineNumP,
		       char * fcfName,
		       Point2 ** pointsP,
		       FILE * fSource, 
		       int * featCodeP,
		       int shift) {
  int i;
  double x, y;
  char lineBuf [lineBufSize];

  /* get the vector point count */
  if (sscanf (inBuf + 4, "%d %d", featCodeP, pointCountP) != 2) 
    {printf ("Error reading feature code or point count from %s on line %d of %s\n",
	     inBuf, *lineNumP, path); return 1;}
  
  /* create space for the points */
  if (*pointsP) free (*pointsP);
  if (! (*pointsP = (Point2 *) malloc (*pointCountP * sizeof (Point2))))
    {printf ("Error allocating memory for a vector of %d points\n", *pointCountP); return 1;}

  
  /* read the points */
  for (i = 0; i < *pointCountP; i ++) {
    if (!fgets (lineBuf, lineBufSize, fSource)) {
      printf ("Error reading line vector data on line %d of %s\n", *lineNumP, fcfName);
      return 1;
    }
    (*lineNumP) ++;
    
    if (sscanf (lineBuf, "%lf %lf", &x, &y) != 2) {
      printf ("Error reading point from %s on line %d of %s\n", lineBuf, *lineNumP, fcfName);
      return 1;
    }
    
    if (shift) {
      (*pointsP) [i] . x = shiftX (x);
      (*pointsP) [i] . y = shiftY (y);
    }
  }
  
  /* skip "END" */
  if (!fgets (lineBuf, lineBufSize, fSource))
    {printf ("Error reading line vector data on line %d of %s\n", *lineNumP, fcfName); return 1;}
  (*lineNumP) ++;
  if (strncmp (lineBuf, "END", 3))
    {printf ("Expected 'END' but read %s on line %d of %s\n", lineBuf, *lineNumP, fcfName); return 1;}

  return 0;
}

/* returns 0 when happy */
/* lc is a pointer to the line count */
/* lv is a pointer to the line vector */
/* shift is a flag for coordinates processed as positive numbers */
int loadLines (char * path, int * lc, line **lv, int shift) {
  int lineNum;
  char lineBuf [lineBufSize];
  int featCode;
  FILE * fSource;
  int pointCount;
  Point2 * points = 0;

  lineNum = 0;

  if (! (fSource = fopen (path, "r"))) {
    printf ("Error opening %s for reading\n", path); return 5;
  }

  if (getVectorType (lineBuf, fSource, path, & lineNum)) return 6;

  while (strncmp (lineBuf, "END", 3)) {
    /* allow LINE or AREA */
    if (strncmp (lineBuf, "LINE", 4) &&
	strncmp (lineBuf, "AREA", 4)) 
      {printf ("Found vector type %s when expecting LINE or AREA on line %d of %s\n",
	       lineBuf, lineNum, path); return 7;}
    
    if (readPoints (path, lineBuf, &pointCount, &lineNum, path, &points, fSource, &featCode, shift)) return 8;

    /* process vector */
    if (! strncmp (lineBuf, "LINE", 4)) {
      /* at this point, pointCount and points hold the line data */
      (*lc) ++;
      * lv = (line *) realloc (* lv, (*lc) * sizeof(line));
      
      (* lv) [(*lc) - 1] . featureCode = featCode;
      (* lv) [(*lc) - 1] . length = pointCount;
      (* lv) [(*lc) - 1] . points = points;
      points = 0;		/* so readPoints doesn't free it */
	  
      if (getVectorType (lineBuf, fSource, path, & lineNum)) return 9;
    } else { /* AREA */
      /* skip area (there shouldn't be line areas, but who knows) */
      if (getVectorType (lineBuf, fSource, path, & lineNum)) return 10;
      while (!strncmp (lineBuf, "HOLE", 4)) {
	if (readPoints (path, lineBuf, &pointCount, &lineNum, path, &points, fSource, &featCode, shift)) return 11;
	if (getVectorType (lineBuf, fSource, path, & lineNum)) return 12;
      }
    }
  }
  fclose (fSource);  

  return 0;
}

#if 0
int addLinePoint (line * aLine, double x, double y) {
  aLine -> length ++;
  aLine -> points = (Point2 *) realloc (aLine -> points, aLine -> length * sizeof (Point2));
  aLine -> points [aLine -> length - 1] . x = x;
  aLine -> points [aLine -> length - 1] . y = y;
}
#endif

