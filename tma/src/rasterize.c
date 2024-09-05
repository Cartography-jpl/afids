#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include "gems.h"
#include "plotline.h"
#include "shiftNscale.h"
#include "concave.h"

/*
 * This program reads a modified arc ungenerate format file
 * representing lines and filled areas and writes a binary pgm raster
 * equivalent.  
 * Parameters:
 *   char * argPattern = "source dest x0 y0 x1 y1 width height region bg [gc gw gh]";
 * source // The path to the source vector file
 * dest // The path for the computed pgm raster file
 * x0, y0, x1, y1 // The bounding box in degrees of the source vectors
 * width, height // The shape of the resulting pgm raster
 * region // The region ID
 * bg // The background color
 * dataSet
 * applicationId
 * gc, gw, gh // Optional grid color, width and height of grid cells
*/

static char * image, * scratchImage;
static int width, height, segmentFirstRow, segmentHeight, segmentCount;

static float x0, y0, x1, y1, xrastovervect, yrastovervect;

int convertX (int x) {
  return MAX (0, MIN (width - 1, (x - x0) * xrastovervect));
}

int convertXd (double x) {
  return (x - x0) * xrastovervect;
}

int convertY (int y) {
  return MAX (0, MIN (height - 1, height - 1 - ((y - y0) * yrastovervect)));
}

int convertYd (double y) {
  return height - 1 - ((y - y0) * yrastovervect);
}

static int fillval;

void fillProc (int y, int xl, int xr) {
  int x;

  y = convertY (y);

  if (y >= segmentFirstRow && y < segmentFirstRow + segmentHeight) { /* in current mem segment */
    y %= segmentHeight;

    xl = convertX (xl);
    xr = convertX (xr);
      
    for (x = xl; x <= xr; x ++)
      scratchImage [x + y * width] = fillval;
  }
}

void checkTclShell (Tcl_Interp *interp, int expression, char * message) {
  if (expression) {
    fprintf (stderr, message);
    if (* (interp -> result) != 0)
      fprintf (stderr, "%s\n", interp -> result);
    exit (1);
  }
}

typedef struct {
  int feature;
  int pgm;
  char * terrainId;
  int priority;
} featurePgmMapRec;

static featurePgmMapRec * featurePgmMap = 0;
static int featurePgmMapLength = 0;
static int pgmValuePriorities [256];

static void loadPGMMap (char * region, double * xFudge, double * yFudge, char * dataSet, char * applicationId) {
  int i;
  Tcl_Interp *interp;
  char script[1000];
  int bubbled;
  featurePgmMapRec tmpRec;
  int mapping;

  for (i = 0; i < 256; i ++)
    pgmValuePriorities [i] = 0;

  interp = Tcl_CreateInterp ();
  checkTclShell (interp, Tcl_Init (interp) == TCL_ERROR, "Could not perform Tcl_AppInit.\n");
  checkTclShell (interp, Tcl_Eval (interp, "package require dbtcl\n") != TCL_OK, "Could not require dbtcl\n");

#if DO_FUDGE
  /* get registration terms */
  sprintf (script,
	   "set result [Data_Select tma_regionThemes {REGISTRATION_LAT REGISTRATION_LON} {{THEME_TYPE Terrain} {REGION_NAME %s}}]\n",
	   region);
  checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK, "Could not query tma_regionThemes\n");
  checkTclShell (interp, Tcl_Eval (interp, "lindex [lindex $result 0] 0") != TCL_OK ||
		 sscanf (interp -> result, "%lf", yFudge) != 1,
		 "Could not index feature value\n");
  checkTclShell (interp, Tcl_Eval (interp, "lindex [lindex $result 0] 1") != TCL_OK ||
		 sscanf (interp -> result, "%lf", xFudge) != 1,
		 "Could not index pgm value\n");
#else
  * xFudge = * yFudge = 0.0;
#endif
  
  /* get feature value to pgm and terrain id mapping */
  sprintf (script,
	   "set result [Data_Select tma_terrainMapping {FEATURE_VALUE PGM_VALUE TERRAIN_ID} {{APPLICATION_ID %s} {DATA_SET %s}}]\n",
	   applicationId, dataSet);
  checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK, "Could not query tma_terrainMapping\n");
  checkTclShell (interp, Tcl_Eval (interp, "llength $result") != TCL_OK || sscanf (interp -> result, "%i", & featurePgmMapLength) != 1,
		 "Could not query result length\n");

  featurePgmMap = (featurePgmMapRec *) malloc (sizeof (featurePgmMapRec) * featurePgmMapLength);

  /* get pgm value priority based on terrain id */
  for (mapping = 0; mapping < featurePgmMapLength; mapping ++) {
    sprintf (script, "lindex [lindex $result %i] 0", mapping);
    checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK || sscanf (interp -> result, "%i", & (featurePgmMap [mapping] . feature)) != 1,
		     "Could not index feature value\n");
    sprintf (script, "lindex [lindex $result %i] 1", mapping);
    checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK || sscanf (interp -> result, "%i", & (featurePgmMap [mapping] . pgm)) != 1,
		   "Could not index pgm value\n");
    if (featurePgmMap [mapping] . pgm < 1 || featurePgmMap [mapping] . pgm > 255) {
      printf ("PGM value %d mapped to 255\n", featurePgmMap [mapping] . pgm);
      featurePgmMap [mapping] . pgm = 255;
    }
    sprintf (script, "lindex [lindex $result %i] 2", mapping);
    checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK || ! * interp -> result, "Could not index terrainId\n");

    featurePgmMap [mapping] . terrainId = strdup (interp -> result);

    sprintf (script,
	     "lindex [lindex  [Data_Select tma_terrain {RASTER_PRIORITY} {{APPLICATION_ID %s} {TERRAIN_ID %s}}] 0] 0\n",
	     applicationId, featurePgmMap [mapping] . terrainId);
    featurePgmMap [mapping] . priority = 0;
    checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK, "Could not query tma_terrainMapping for terrain priority\n");

    sscanf (interp -> result, "%i", & (featurePgmMap [mapping] . priority));
  }

  /* we're done with the database */
  Tcl_DeleteInterp (interp);

  /* sort into increasing priority order */
  bubbled = 1;
  while (bubbled) {
    bubbled = 0;
    for (mapping = 1; mapping < featurePgmMapLength; mapping ++)
      if (featurePgmMap [mapping - 1] . priority > featurePgmMap [mapping] . priority) {
	tmpRec = featurePgmMap [mapping - 1];
	featurePgmMap [mapping - 1] = featurePgmMap [mapping];
	featurePgmMap [mapping] = tmpRec;
	bubbled = 1;
      }
  }

  /* load PGM priorities */
  for (mapping = 0; mapping < featurePgmMapLength; mapping ++)
    pgmValuePriorities [featurePgmMap [mapping] . pgm] = featurePgmMap [mapping] . priority;
}

static int priority (int pgmvalue) {
  if (pgmvalue < 1 || pgmvalue > 255)
    return 0;

  return pgmValuePriorities [pgmvalue];
}

static int preferred (int old, int new) {
  if (priority (old) > priority (new))
    return old;
  else
    return new;
}

static void plotAPoint (int x, int y) {
  if (x >= 0 && y >= 0 && x < width && y < height) /* in overall image */
    if (y >= segmentFirstRow && y < segmentFirstRow + segmentHeight) { /* in current mem segment */
      y %= segmentHeight;
      image [x + y * width] = preferred (image [x + y * width], fillval);
    }
}

static int lineBufSize = 100;

int getVectorType (char * lineBuf, FILE * fSource, char * argv [], int * lineNumP) {
  /* get vector type */
  if (!fgets (lineBuf, lineBufSize, fSource))
    strcpy (lineBuf, "END"); /* end of file */

  (*lineNumP) ++;

  return 0;
}

int readPoints (char * inBuf, int * pointCountP, int * lineNumP, char * argv [],
		 Point2 ** pointsP, FILE * fSource, int * featCodeP, double xFudge, double yFudge) {
  int i;
  float x, y;
  char lineBuf [lineBufSize];

  /* get the vector point count */
  if (sscanf (inBuf + 4, "%d %d", featCodeP, pointCountP) != 2) 
    {printf ("Error reading feature code or point count from %s on line %d of %s\n",
	     inBuf, *lineNumP, argv[1]); return 1;}
  
  /* map it */
  for (i = 0; i < featurePgmMapLength; i ++)
    if (featurePgmMap [i] . feature == * featCodeP) {
      * featCodeP = featurePgmMap [i] . pgm;
      break;
    }
  if (i == featurePgmMapLength) {
    printf ("Terrain feature code %d not found in feature PGM map; ignoring...\n", * featCodeP);
    * featCodeP = 0;
  }

  /* create space for the points */
  if (*pointsP) free (*pointsP);
  if (! (*pointsP = (Point2 *) malloc (*pointCountP * sizeof (Point2))))
    {printf ("Error allocating memory for a vector of %d points\n", *pointCountP); return 1;}
  
  /* read the points */
  for (i = 0; i < *pointCountP; i ++) {
    if (!fgets (lineBuf, lineBufSize, fSource)) {
      printf ("Error reading line vector data on line %d of %s\n", *lineNumP, argv[1]);
      return 1;
    }
    (*lineNumP) ++;
    
    if (sscanf (lineBuf, "%f %f", &x, &y) != 2) {
      printf ("Error reading point from %s on line %d of %s\n", lineBuf, *lineNumP, argv[1]);
      return 1;
    }
    
    x += xFudge;
    y += yFudge;

    (*pointsP) [i] . x = shiftNscaleX (x);
    (*pointsP) [i] . y = shiftNscaleY (y);
  }
  
  /* skip "END" */
  if (!fgets (lineBuf, lineBufSize, fSource))
    {printf ("Error reading line vector data on line %d of %s\n", *lineNumP, argv[1]); return 1;}
  (*lineNumP) ++;
  if (strncmp (lineBuf, "END", 3))
    {printf ("Expected 'END' but read %s on line %d of %s\n", lineBuf, *lineNumP, argv[1]);
    return 1;}
  return 0;
}

void mergeScratch (int polyMinXi, int polyMaxXi, int polyMinYi, int polyMaxYi) {
  int i, j, jm;

  for (j = polyMinYi; j <= polyMaxYi; j ++)
    if (j >= segmentFirstRow && j < segmentFirstRow + segmentHeight) { /* in current mem segment */
      jm = j % segmentHeight;
      
      for (i = polyMinXi; i <= polyMaxXi; i ++) 
	if (scratchImage [i + jm * width])
	  image [i + jm * width] = scratchImage [i + jm * width];
    }
}

int main (int argc, char * argv []) {
  char * argPattern = "source dest x0 y0 x1 y1 width height region bg dataSet applicationId [gc gw gh]";
  int status;
  int i, j, jm, lineNum;
  int pointCount;
  Point2 * points = (Point2 *) 0;
  FILE * fSource, * fDest;
  char header [1000];
  char lineBuf [lineBufSize];
  GemsWindow window;
  int bgVal;
  int featCode;
  double polyMinX, polyMaxX, polyMinY, polyMaxY;
  int polyMinXi, polyMaxXi, polyMinYi, polyMaxYi;
  int memSegment, memDivider, touchesSegment;
  int gridColor, gridWidth, gridHeight;

#define PDCXOFFSET  0.0011111111111111111
#define PDCYOFFSET -0.0102777777777777777

  double xFudge = PDCXOFFSET;
  double yFudge = PDCYOFFSET;

  /* check args */
  if (! (argc == 13 || argc == 16)) {
    printf ("usage: %s %s\n", argv [0], argPattern);
    return 1;
  }

  if ((status = sscanf (argv[10], "%d", &bgVal)) != 1 ||
      bgVal < 0 || bgVal > 255) {
    printf ("Background value (%s) must be in range 0 .. 255\n", argv[10]); return 2;
  }

  if (argc == 16) {
    if ((status = sscanf (argv[13], "%d", &gridColor)) != 1 ||
	gridColor < 0 || gridColor > 255) {
      printf ("Grid color value (%s) must be in range 0 .. 255\n", argv[13]); return 21;
    }
    
    if ((status = sscanf (argv[14], "%d", &gridWidth)) != 1 ||
	gridWidth < 1)
      {printf ("Error parsing positive grid width from (%s)\n", argv[14]); return 22;}
    
    if ((status = sscanf (argv[15], "%d", &gridHeight)) != 1 ||
	gridHeight < 1)
      {printf ("Error parsing positive grid height from (%s)\n", argv[15]); return 23;}
  } else
    gridColor = 0;
  
  if (! (fSource = fopen (argv[1], "r"))) {
    printf ("Error opening %s for reading\n", argv[1]); return 4;
  }

  if (! (fDest = fopen (argv[2], "w"))) {
    printf ("Error opening %s for writing\n", argv[2]); return 5;
  }

  if ((status = sscanf (argv[3], "%f", &x0)) != 1 ||
      (status = sscanf (argv[4], "%f", &y0)) != 1 ||
      (status = sscanf (argv[5], "%f", &x1)) != 1 ||
      (status = sscanf (argv[6], "%f", &y1)) != 1) {
    printf ("Error parsing bouding box from %s, %s, %s, %s\n",
	    argv[3], argv[4], argv[5], argv[6]); return 6;
  }

  x0 = shiftNscaleX (x0);
  x1 = shiftNscaleX (x1);
  y0 = shiftNscaleY (y0);
  y1 = shiftNscaleY (y1);

  if ((status = sscanf (argv[7], "%i", &width)) != 1 ||
      (status = sscanf (argv[8], "%i", &height)) != 1) {
    printf ("Error reading raster shape from %s, %s\n", argv[7], argv[8]); return 7;
  }

  printf ("loading pgm map\n");

  /* load feature id/pgm value mapping; rasterization priorities; registration terms */
  loadPGMMap (argv [9], & xFudge, & yFudge, argv [11], argv [12]);

  printf ("writing image header\n");

  /* write image header */
  sprintf (header, "P5\n%d %d\n255\n", width, height);
  fwrite (header, strlen(header), 1, fDest);

  xrastovervect = (width - 1.0) / (x1 - x0);
  yrastovervect = (height - 1.0) / (y1 - y0);

  window.x0 = x0;
  window.y0 = y0;
  window.x1 = x1;
  window.y1 = y1;

  printf ("determining segment size\n");

  /* allocate image space */
  image = scratchImage = 0;
  memDivider = 1;
  while (image == 0 || scratchImage == 0) {
    free (image);
    free (scratchImage);
    image = scratchImage = 0;
    segmentHeight = height / memDivider;
    image = (char *) malloc (width * segmentHeight);
    scratchImage = (char *) malloc (width * segmentHeight);
    memDivider *= 2;
  }
  memDivider /= 2;

  segmentCount = memDivider;
  if (height % segmentHeight)
    segmentCount ++;

  printf ("using %d segments\n", segmentCount);

  for (memSegment = 0; memSegment < segmentCount; memSegment ++) {
    segmentFirstRow = memSegment * segmentHeight;

    printf ("clearing segment %d of image\n", memSegment);

    /* clear image */
    memset (image, 0, width * segmentHeight);
  
    lineNum = 0;

    rewind (fSource);

    printf ("rasterizing segment\n");

    if (getVectorType (lineBuf, fSource, argv, & lineNum)) return 8;

    while (strncmp (lineBuf, "END", 3)) {
      /*    printf ("reading %s at line %d\n", argv [1], lineNum);*/
      /* allow LINE or AREA */
      if (strncmp (lineBuf, "LINE", 4) &&
	  strncmp (lineBuf, "AREA", 4) &&
	  strncmp (lineBuf, "HOLE", 4)) {
	printf ("Found vector type %s when expecting LINE or AREA on line %d of %s\n", lineBuf, lineNum, argv[1]); return 9;
      }
    
      if (readPoints (lineBuf, &pointCount, &lineNum, argv, &points, fSource, & featCode, xFudge, yFudge)) return 10;

      touchesSegment = 0;
      for (i = 0; i < pointCount; i ++) {
	j = (convertYd (points [i].y));
	if (j >= segmentFirstRow && j < segmentFirstRow + segmentHeight) { /* in current mem segment */
	  touchesSegment = 1;
	  break;
	}
      }
	
      /* process vector */
      if (! strncmp (lineBuf, "LINE", 4)) {
	/*      printf ("rasterizing %d point line vector %d\n", pointCount, lineCount ++);*/
	fillval = featCode;
	
	if (touchesSegment)
	  for (i = 1; i < pointCount; i ++)
	    if (plotLine (convertXd (points [i - 1].x),
			  convertYd (points [i - 1].y),
			  convertXd (points [i].x),
			  convertYd (points [i].y),
			  plotAPoint))
	      return 11;
	
	if (getVectorType (lineBuf, fSource, argv, & lineNum)) return 12;
      } else { /* AREA or HOLE */
	/*      printf ("rasterizing %d point area vector %d\n", pointCount, areaCount ++);*/

	if (! strncmp (lineBuf, "HOLE", 4))
	  printf ("found HOLE without AREA, assuming it's an AREA\n");

	if (touchesSegment) {
	  fillval = featCode;
	  
	  /* clear scratchImage */
	  polyMinX = 1E12;
	  polyMaxX = 0;
	  polyMinY = 1E12;
	  polyMaxY = 0;
	  for (i = 0; i < pointCount; i ++) {
	    polyMinX = MIN (polyMinX, points [i] .x);
	    polyMaxX = MAX (polyMaxX, points [i] .x);
	    polyMinY = MIN (polyMinY, points [i] .y);
	    polyMaxY = MAX (polyMaxY, points [i] .y);
	  }
	  polyMinXi = convertX (polyMinX);
	  polyMaxXi = convertX (polyMaxX);
	  polyMinYi = convertY (polyMaxY);
	  polyMaxYi = convertY (polyMinY);
	  
	  for (j = polyMinYi; j <= polyMaxYi; j ++)
	    if (j >= segmentFirstRow && j < segmentFirstRow + segmentHeight) { /* in current mem segment */
	      jm = j % segmentHeight;

	      for (i = polyMinXi; i < polyMaxXi; i ++) 
		scratchImage [i + jm * width] = 0; 
	    }
	  
	  /*      printf ("computing fill... "); fflush (stdout);*/
	  concave (pointCount, points, & window, fillProc);
	  /*      printf ("done.\n");*/
	}
	
	if (getVectorType (lineBuf, fSource, argv, & lineNum)) return 13;
	
	while (!strncmp (lineBuf, "HOLE", 4)) {
	  if (readPoints (lineBuf, &pointCount, &lineNum, argv, &points, fSource, & featCode, xFudge, yFudge)) return 14;

	  fillval = 0;
	  if (touchesSegment)
	    concave (pointCount, points, & window, fillProc);

	  if (getVectorType (lineBuf, fSource, argv, & lineNum)) return 15;
	}
	
	if (touchesSegment)
	  mergeScratch (polyMinXi, polyMaxXi, polyMinYi, polyMaxYi);
      }
    }

    printf ("filling background\n");

    /* fill background */
    for (j = 0; j < segmentHeight; j ++)
      for (i = 0; i < width; i ++)
	if (image [i + j * width] == 0)
	  image [i + j * width] = bgVal;
    
    if (gridColor > 0) {
      /* for each longitude line */
      for (i = gridWidth; i < width; i += gridWidth)
	for (j = 0; j < height; j ++)
	  image [i + j * width] = gridColor;
      /* for each latitude line */
      for (j = gridHeight; j < height; j += gridHeight)
	for (i = 0; i < width; i ++)
	  image [i + j * width] = gridColor;
    }

    /* write image */
    if (memSegment < segmentCount - 1 || ! (height % segmentHeight)) {
      printf ("writing by segment\n");
      fwrite (image, width * segmentHeight, 1, fDest);
    /*for (delay = 0; delay < width * segmentHeight; delay ++)
      fwrite (image + delay, 1, 1, fDest);*/
    } else {
      printf ("not writing by segment\n");
      fwrite (image, width * (height % segmentHeight), 1, fDest);
      /*for (delay = 0; delay < width * (height % segmentHeight); delay ++)
	fwrite (image + delay, 1, 1, fDest);*/
    }
  }

  fclose (fSource);  
  fclose (fDest);
  return 0;
}

