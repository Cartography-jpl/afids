#include <stdio.h>
#include <stdlib.h>
#include <tcl.h>
#include "rnf.h"
#include "loadlines.h"
#include "intersect.h"
#include "pointToSegment.h"
#include "xlines.h"
#include "shiftNscale.h"
#include "plotline.h"

void checkTclShell (Tcl_Interp *interp, int expression, char * message) {
  if (expression) {
    fprintf (stderr, message);
    if (* (interp -> result) != 0)
      fprintf (stderr, "%s\n", interp -> result);
    exit (1);
  }
}

void dumpRoad (line * road) {
  int i;

  printf ("featureCode: %d length: %d\n", road -> featureCode, road -> length);
  for (i = 0; i < road -> length; i ++)
    printf ("point %03d x: %25.12f y: %25.12f\n", i, road -> points [i] . x, road -> points [i] . y);
}

typedef struct {
  int lineID;
  int segmentID;
} BinCell;

typedef BinCell * Bin;

typedef struct {
  int binSize;
  Bin bin;
} MatrixCell;

static void initMatrix (MatrixCell * matrix, int width, int height) {
  int i, j;

  for (i = 0; i < width; i ++)
    for (j = 0; j < height; j ++) {
      matrix [IJ2IDX (i, j)] .binSize = 0;
      matrix [IJ2IDX (i, j)] .bin = 0;
    }
}

static MatrixCell * roadMatrix;
static MatrixCell * riverMatrix;
static void addToBin (MatrixCell * matrix, int line, int seg, int i, int j, int width, int height) {
  matrix [IJ2IDX (i, j)] .binSize ++;
  if (! (matrix [IJ2IDX (i, j)] .bin = (BinCell *) realloc (matrix [IJ2IDX (i, j)] .bin, matrix [IJ2IDX (i, j)] .binSize * sizeof (BinCell)))) {
    printf ("addToBin failed to realloc at l %d, s %d, i %d, j %d\n", line, seg, i, j);
    return;
  }
  matrix [IJ2IDX (i, j)] .bin [matrix [IJ2IDX (i, j)] .binSize - 1] .lineID = line;
  matrix [IJ2IDX (i, j)] .bin [matrix [IJ2IDX (i, j)] .binSize - 1] .segmentID = seg;
}

static roadNode * addAuxNode (int lineID,
			      int segID,
			      structuralRoad * sroads,
			      double xp, double yp) {

  int newCount = ++ sroads [lineID] .snodes [segID] . auxNodeCount;

  if (! (sroads [lineID] .snodes [segID] . auxNodes =
	 (auxNode *) realloc (sroads [lineID] .snodes [segID] . auxNodes, sizeof (auxNode) * newCount))) {
    printf ("addAuxNode failed to alloc at l %d, s %d\n", lineID, segID);
    return 0;
  }
  sroads [lineID] .snodes [segID] . auxNodes [newCount - 1] .x = xp;
  sroads [lineID] .snodes [segID] . auxNodes [newCount - 1] .y = yp;

  return & (sroads [lineID] .snodes [segID] . auxNodes [newCount - 1] .rn);
}

static void addRPAuxNode (int lineID,
			  int segID,
			  structuralRoad * sroads,
			  int i, int j,
			  double bestDist,
			  double xp, double yp, int width, int height) {
  roadNode * rn = addAuxNode (lineID, segID, sroads, xp, yp);

  rn -> gpIndex = IJ2IDX (i, j);
  rn -> u.roadPointDetails .distToGP = bestDist;
  rn -> u.roadPointDetails .riverCodeCount = 0;
  rn -> u.roadPointDetails .riverCodes = 0;
  rn -> distToNextNode = 0;
}

static void addRIAuxNode (int lineID,
			  int segID,
			  structuralRoad * sroads,
			  double xp, double yp,
			  int otherRoadIndex,
			  int otherRoadNode,
			  int gpIndex) {
  roadNode * rn = addAuxNode (lineID, segID, sroads, xp, yp);

  rn -> gpIndex = -1;
  rn -> u.intersectionDetails .otherRoadIndex = otherRoadIndex;
  rn -> u.intersectionDetails .otherRoadNode = otherRoadNode;
  rn -> u.intersectionDetails .gpIndex = gpIndex;
  rn -> distToNextNode = 0;
}

/* k in range 0 .. 7 */
/* 0 1 2 */
/* 3   4 */
/* 5 6 7 */
static double x0, winy0, x1, winy1;
static double binWidth, binHeight;
static int neighborExists (int i, int j, int k, int width, int height, double * xc, double * yc) {
  switch (k) {
  case 0:
    * xc = (i - 0.5) * binWidth;
    * yc = (j - 0.5) * binHeight;
    return i > 0 && j > 0;
  case 1:
    * xc = (i + 0.5) * binWidth;
    * yc = (j - 0.5) * binHeight;
    return j > 0;
  case 2:
    * xc = (i + 1.5) * binWidth;
    * yc = (j - 0.5) * binHeight;
    return i < width - 1 && j > 0;
  case 3:
    * xc = (i - 1.5) * binWidth;
    * yc = (j + 0.5) * binHeight;
    return i > 0;
  case 4:
    * xc = (i + 1.5) * binWidth;
    * yc = (j + 0.5) * binHeight;
    return i < width - 1;
  case 5:
    * xc = (i - 0.5) * binWidth;
    * yc = (j + 1.5) * binHeight;
    return i > 0 && j < height - 1;
  case 6:
    * xc = (i + 0.5) * binWidth;
    * yc = (j + 1.5) * binHeight;
    return j < height - 1;
  case 7:
    * xc = (i + 1.5) * binWidth;
    * yc = (j + 1.5) * binHeight;
    return i < width - 1 && j < height - 1;
  default:
    printf ("illegal neighbor %d\n", k);
    return 0;
  }
}

static MatrixCell * dropInBinMatrix;
static int dropInBinWidth;
static int dropInBinHeight;
static int dropInBinLine;
static int dropInBinSeg;

void  dropInBin (int x, int y) {
  if (x > -1 && x < dropInBinWidth &&
      y > -1 && y < dropInBinHeight)
    addToBin (dropInBinMatrix, dropInBinLine, dropInBinSeg, x, y, dropInBinWidth, dropInBinHeight);
}

int main (int argc, char * argv []) {
  char * argPattern = "TVFroads TVFrivers RIF RNF x0 y0 x1 y1 width height regionName";
  int status;
  int width, height;
  roadDescription * roadNetwork = 0;
  FILE * fRIF, * fRNF;
  int rifRecCount;
  RIFrec * rifRecs;
  int roadCount = 0;
  line * roads = 0;
  int riverCount = 0;
  line * rivers = 0;
  int i, j, k, road, river, seg, otherSeg;
  structuralRoad * sroads = 0;
  int multiCellSegmentCount = 0;
  int longSegCount = 0;
  gridCell * grid = 0;
  double xc, yc, xIgnore, yIgnore;
  int lineID, segID;
  double bestDist;
  int bestSeg;
  double * doubles = 0;
  double xp, yp;
  int bubbled;
  int nodeCount;
  double dTemp;
  auxNode an;
  int auxNodeIndex;
  int saveSeg, thisSeg, thisAuxIndex;
  double thisX, thisY;
  int gpIndex;
  int riverSeg;
  roadNode * rnp;
  int * ints;
  int intCount;
  double neighborXc, neighborYc;
  int xLeft, xRight, yLeft, yRight;
  int otherRoadIndex, otherRoadNode;
  int oldSegIndex;	
  int binI, binJ;
  Tcl_Interp *interp;
#ifdef FUDGE_IT
  double regisXoffset, regisYoffset;
  char script[1000];
#else
#define regisXoffset 0.0
#define regisYoffset 0.0
#endif

  /* check args */
  if (argc != 12) {printf ("usage: %s %s\n", argv [0], argPattern); return 1;}

  /* spin up Tcl shell */
  interp = Tcl_CreateInterp ();
  checkTclShell (interp, Tcl_Init (interp) == TCL_ERROR, "Could not perform Tcl_AppInit.\n");
  checkTclShell (interp, Tcl_Eval (interp, "package require dbtcl\n") != TCL_OK, "Could not require dbtcl\n");

#ifdef FUDGE_IT
  /* get registration terms */
  sprintf (script,
	   "set result [Data_Select tma_regionThemes {REGISTRATION_LAT REGISTRATION_LON} {{THEME_TYPE Terrain} {REGION_NAME %s}}]\n",
	   argv [11]);
  checkTclShell (interp, Tcl_Eval (interp, script) != TCL_OK, "Could not query tma_regionThemes\n");
  checkTclShell (interp, Tcl_Eval (interp, "lindex [lindex $result 0] 0") != TCL_OK, "Could not index feature value\n");
  if (sscanf (interp -> result, "%lf", & regisYoffset) != 1)
    regisYoffset = 0.0;
  checkTclShell (interp, Tcl_Eval (interp, "lindex [lindex $result 0] 1") != TCL_OK, "Could not index pgm value\n");
  if (sscanf (interp -> result, "%lf", & regisXoffset) != 1)
    regisXoffset = 0.0;
#endif

  /* we're done with the database */
  Tcl_DeleteInterp (interp);

  /* load TVF road file */
  if ((status = loadLines (argv [1], & roadCount, & roads, 1)))
    {printf ("%s: loadLines returned %d while attempting to load roads from %s\n", argv [0], status, argv [1]); return 2;}

  for (i = 0; i < roadCount; i ++)
    for (j = 0; j < roads [i] . length; j ++) {
      roads [i] . points [j] . x += regisXoffset;
      roads [i] . points [j] . y += regisYoffset;
    }

  /* load TVF river file */
  if ((status = loadLines (argv [2], & riverCount, & rivers, 1)))
    {printf ("%s: loadLines returned %d while attempting to load rivers from %s\n", argv [0], status, argv [2]); return 3;}

  /* load RIF recs */
  if (! (fRIF = fopen (argv[3], "r"))) {printf ("%s: error opening %s for writing\n", argv [0], argv[3]); return 4;}
  if (fread (& rifRecCount, sizeof (int) , 1, fRIF) != 1)
    {printf ("%s: error reading rifRecCount from %s\n", argv [0], argv[3]); return 5;}
  if (! (rifRecs = (RIFrec *) malloc (sizeof (RIFrec) * rifRecCount)))
    {printf ("%s: error allocating %d RIFrecs\n", argv [0], rifRecCount); return 6;}
  if (fread (rifRecs, sizeof (RIFrec) , rifRecCount, fRIF) != rifRecCount)
    {printf ("%s: error reading rifRecs from %s\n", argv [0], argv[3]); return 7;}
  fclose (fRIF);

  for (i = 0; i < rifRecCount; i ++) {
    rifRecs [i] . x += regisXoffset;
    rifRecs [i] . y += regisYoffset;
  }

  /* open RNF file for writing */
  if (! (fRNF = fopen (argv[4], "w"))) {printf ("%s: error opening %s for writing\n", argv [0], argv[4]); return 8;}

  if ((status = sscanf (argv[5], "%lf", &x0)) != 1 ||
      (status = sscanf (argv[6], "%lf", &winy0)) != 1 ||
      (status = sscanf (argv[7], "%lf", &x1)) != 1 ||
      (status = sscanf (argv[8], "%lf", &winy1)) != 1) {
    printf ("%s: Error parsing bouding box from %s, %s, %s, %s\n", argv [0], argv[5], argv[6], argv[7], argv[8]);
    return 9;
  }
  x0 = shiftX (x0);
  x1 = shiftX (x1);
  winy0 = shiftY (winy0);
  winy1 = shiftY (winy1);

  if ((status = sscanf (argv[9], "%i", &width)) != 1 ||
      (status = sscanf (argv[10], "%i", &height)) != 1) {
    printf ("Error reading raster shape from %s, %s\n", argv[9], argv[10]);
    return 10;
  }

  /* build structural roads from lines */
  if (! (sroads = (structuralRoad *) malloc (roadCount * sizeof (structuralRoad)))) {
    printf ("%s: failed mallocing %d roads\n", argv [0], roadCount);
    return 101;
  }

  for (i = 0; i < roadCount; i ++) {
    sroads [i] . featureCode = roads [i] . featureCode;
    sroads [i] . snodeCount = roads [i] . length;
    if (! (sroads [i] . snodes = (structuralNode *) malloc (sizeof (structuralNode) * sroads [i] . snodeCount))) {
      printf ("%s: failed mallocing %d snoads\n", argv [0], sroads [i] . snodeCount);
      return 102;
    }
    for (j = 0; j < roads [i] . length; j ++) {
      sroads [i] . snodes [j] . x = roads [i] . points [j] . x;
      sroads [i] . snodes [j] . y = roads [i] . points [j] . y;
      sroads [i] . snodes [j] . auxNodeCount = 0;
      sroads [i] . snodes [j] . auxNodes = 0;
    }
  }

  binWidth = (x1 - x0) / width;
  binHeight = (winy1 - winy0) / height;
#define BIN_I(x) ((int) (((x) - x0)/binWidth))
#define BIN_J(y) ((int) (((y) - winy0)/binHeight))

  /* create road matrix */
  if (! (roadMatrix = (MatrixCell *) malloc (sizeof (MatrixCell) * width * height))) {
      printf ("%s: failed mallocing %d road MatrixCells\n", argv [0], width * height);
      return 104;
    }
  initMatrix (roadMatrix, width, height);

  /* load road matrix */
  multiCellSegmentCount = 0;
  longSegCount = 0;
  dropInBinMatrix = roadMatrix;
  dropInBinWidth = width;
  dropInBinHeight = height;
  for (road = 0; road < roadCount; road ++) {
    dropInBinLine = road;
    for (seg = 0; seg < roads [road] .length - 1; seg ++) {
      dropInBinSeg = seg;
      /* calc the matrix bin for the left side of the segment */
      xLeft = BIN_I (roads [road] .points [seg] .x);
      yLeft = BIN_J (roads [road] .points [seg] .y);
      xRight = BIN_I (roads [road] .points [seg + 1] .x);
      yRight = BIN_J (roads [road] .points [seg + 1] .y);

      /* if left and right are in the same bin and the bin is within the allocated matrix */
      if (xLeft == xRight &&
	  yLeft == yRight &&
	  xLeft > -1 && xLeft < width &&
	  yLeft > -1 && yLeft < height)
	/* drop the segment in the bin */
	addToBin (roadMatrix, road, seg, xLeft, yLeft, width, height);
      else {
	/* dropInBin will decide whether a given point falls in a matrix bin as it rasterizes along between the end points */
	plotLine (xLeft, yLeft, xRight, yRight, dropInBin);
	multiCellSegmentCount ++;
      }
    }
    free (roads [road] .points);
  }
  free (roads);
  printf ("%s: counted %d multi cell road segments\n", argv [0], multiCellSegmentCount);

  /* create river matrix */
  if (! (riverMatrix = (MatrixCell *) malloc (sizeof (MatrixCell) * width * height))) {
      printf ("%s: failed mallocing %d river MatrixCells\n", argv [0], width * height);
      return 103;
    }
  initMatrix (riverMatrix, width, height);

  /* load river matrix */
  multiCellSegmentCount = 0;
  longSegCount = 0;
  dropInBinMatrix = riverMatrix;
  dropInBinWidth = width;
  dropInBinHeight = height;
  for (river = 0; river < riverCount; river ++) {
    dropInBinLine = river;
    for (seg = 0; seg < rivers [river] .length - 1; seg ++) {
      dropInBinSeg = seg;
      /* calc the matrix bin for the left side of the segment */
      xLeft = BIN_I (rivers [river] .points [seg] .x);
      yLeft = BIN_J (rivers [river] .points [seg] .y);
      xRight = BIN_I (rivers [river] .points [seg + 1] .x);
      yRight = BIN_J (rivers [river] .points [seg + 1] .y);

      /* if left and right are in the same bin and the bin is within the allocated matrix */
      if (xLeft == xRight &&
	  yLeft == yRight &&
	  xLeft > -1 && xLeft < width &&
	  yLeft > -1 && yLeft < height)
	/* drop the segment in the bin */
	addToBin (riverMatrix, river, seg, xLeft, yLeft, width, height);
      else {
	/* dropInBin will decide whether a given point falls in a matrix bin as it rasterizes along between the end points */
	plotLine (xLeft, yLeft, xRight, yRight, dropInBin);
	multiCellSegmentCount ++;
      }
    }
  }
  printf ("%s: counted %d multi cell river segments\n", argv [0], multiCellSegmentCount);

  /* add road points to road segments as aux nodes */
  for (i = 0; i < width; i ++) {
    for (j = 0; j < height; j ++) {
      xc = x0 + (i + 0.5) * binWidth; /* grid cell center */
      yc = winy0 + (j + 0.5) * binHeight;
      doubles = (double *) malloc (sizeof (double) * roadMatrix [IJ2IDX (i, j)] .binSize); /*  shortest dist from each segment to GP */
      for (seg = 0; seg < roadMatrix [IJ2IDX (i, j)] .binSize; seg ++) {
	lineID = roadMatrix [IJ2IDX (i, j)] .bin [seg] .lineID;
	segID = roadMatrix [IJ2IDX (i, j)] .bin [seg] .segmentID;
	doubles [seg] = pointToSegment (sroads [lineID] .snodes [segID] .x, sroads [lineID] .snodes [segID] .y,
					sroads [lineID] .snodes [segID + 1] .x, sroads [lineID] .snodes [segID + 1] .y,
					xc, yc,
					& xp, & yp);
      }

      for (seg = 0; seg < roadMatrix [IJ2IDX (i, j)] .binSize; seg ++) {
	while (seg < roadMatrix [IJ2IDX (i, j)] .binSize && doubles [seg] < 0) seg ++;
	if (seg < roadMatrix [IJ2IDX (i, j)] .binSize) {
	  bestSeg = seg;
	  bestDist = doubles [seg];
	  for (otherSeg = seg + 1; otherSeg < roadMatrix [IJ2IDX (i, j)] .binSize; otherSeg ++)
	    if (doubles [otherSeg] >= 0 &&
		roadMatrix [IJ2IDX (i, j)] .bin [seg] .lineID == roadMatrix [IJ2IDX (i, j)] .bin [otherSeg] .lineID) {
	      if (doubles [otherSeg] < bestDist) {
		doubles [bestSeg] = -1;
		bestDist = doubles [otherSeg];
		bestSeg = otherSeg;
	      } else {
		doubles [otherSeg] = -1;
	      }
	    }

	  doubles [bestSeg] = -1;
	  /* this is the shortest distance between grid point [IJ2IDX (i, j)] and the road lineID */
	  lineID = roadMatrix [IJ2IDX (i, j)] .bin [bestSeg] .lineID;
	  segID = roadMatrix [IJ2IDX (i, j)] .bin [bestSeg] .segmentID;
	  bestDist = pointToSegment (sroads [lineID] .snodes [segID] .x, sroads [lineID] .snodes [segID] .y,
				     sroads [lineID] .snodes [segID + 1] .x, sroads [lineID] .snodes [segID + 1] .y,
				     xc, yc,
				     & xp, & yp);

	  addRPAuxNode (lineID, segID, sroads, i, j, bestDist, xp, yp, width, height);
	}
      }
      free (doubles);
      doubles = 0;
    }
  }

  /* add intersection points to road segments as aux nodes */
  for (i = 0; i < rifRecCount; i ++) {
    binI = BIN_I (rifRecs [i] .x);
    binJ = BIN_J (rifRecs [i] .y);

    if (binI >= 0 && binI < width &&
	binJ >= 0 && binJ < height) {

      gpIndex = IJ2IDX (binI, binJ);

      addRIAuxNode (rifRecs [i] .road1ID, rifRecs [i] .segment1ID, sroads,
		    rifRecs [i] .x, rifRecs [i] .y,
		    rifRecs [i] .road2ID, rifRecs [i] .segment2ID,
		    gpIndex);
      addRIAuxNode (rifRecs [i] .road2ID, rifRecs [i] .segment2ID, sroads,
		    rifRecs [i] .x, rifRecs [i] .y,
		    rifRecs [i] .road1ID, rifRecs [i] .segment1ID,
		    gpIndex);
    }
  }

  /* sort aux nodes per segment */
  for (road = 0; road < roadCount; road ++)
    for (seg = 0; seg < sroads [road] .snodeCount; seg ++) {

      nodeCount = sroads [road] .snodes [seg] .auxNodeCount;
      if (nodeCount > 1) {
	doubles = (double *) malloc (sizeof (double) * nodeCount);
	for (i = 0; i < nodeCount; i ++)
	  doubles [i] = DIST (sroads [road] .snodes [seg] .x,
			      sroads [road] .snodes [seg] .y,
			      sroads [road] .snodes [seg] .auxNodes [i] .x,
			      sroads [road] .snodes [seg] .auxNodes [i] .y);

	/* bubble sort */
	bubbled = 1;
	while (bubbled) {
	  bubbled = 0;
	  for (i = 0; i < nodeCount - 1; i ++) {
	    if (doubles [i] > doubles [i + 1]) {
	      bubbled = 1;

	      dTemp = doubles [i];
	      doubles [i] = doubles [i + 1];
	      doubles [i + 1] = dTemp;

	      an = sroads [road] .snodes [seg] .auxNodes [i];
	      sroads [road] .snodes [seg] .auxNodes [i] = sroads [road] .snodes [seg] .auxNodes [i + 1];
	      sroads [road] .snodes [seg] .auxNodes [i + 1] = an;
	    }
	  }
	}
	free (doubles);
      }
    }

  /* compute node to next node distances for each road, excluding structural nodes */
  for (road = 0; road < roadCount; road ++) {
    thisSeg = -1;
    for (seg = 0; seg < sroads [road] .snodeCount; seg ++) {
      for (auxNodeIndex = 0; auxNodeIndex < sroads [road] .snodes [seg] .auxNodeCount; auxNodeIndex ++) {
	if (thisSeg < 0) {	/* save first aux node of road */
	  thisSeg = seg;
	  thisAuxIndex = auxNodeIndex;
	  thisX = sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .x;
	  thisY = sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .y;
	} else {		/* compute dist between this node and last node; store in last node */
	  if (seg == thisSeg) {
	    /* distance between nodes in the same segment is the simple distance */
	    sroads [road] .snodes [thisSeg] .auxNodes [thisAuxIndex] .rn .distToNextNode =
	      DIST (thisX, thisY,
		    sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .x,
		    sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .y);
	  } else {
	    /* if the nodes are in different segments, the distance follows the road through structural nodes */

	    /* first the distance from the last aux node to the next structural node */
	    sroads [road] .snodes [thisSeg] .auxNodes [thisAuxIndex] .rn .distToNextNode =
	      DIST (thisX, thisY,
		    sroads [road] .snodes [thisSeg + 1] .x,
		    sroads [road] .snodes [thisSeg + 1] .y);
	    /* next add the distance along the road to the current segment head */
	    saveSeg = thisSeg;
	    for (thisSeg ++; thisSeg < seg; thisSeg ++)
	      sroads [road] .snodes [saveSeg] .auxNodes [thisAuxIndex] .rn .distToNextNode +=
		DIST (sroads [road] .snodes [thisSeg] .x,
		      sroads [road] .snodes [thisSeg] .y,
		      sroads [road] .snodes [thisSeg + 1] .x,
		      sroads [road] .snodes [thisSeg + 1] .y);
	    /* finally add the distance from the last structural node to the current aux node */
	    sroads [road] .snodes [saveSeg] .auxNodes [thisAuxIndex] .rn .distToNextNode +=
	      DIST (sroads [road] .snodes [thisSeg] .x,
		    sroads [road] .snodes [thisSeg] .y,
		    sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .x,
		    sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .y);
	  }

	  thisSeg = seg;
	  thisAuxIndex = auxNodeIndex;
	  thisX = sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .x;
	  thisY = sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .y;
	}
      }
    }
  }

  /* compute river codes for each road point */
  for (road = 0; road < roadCount; road ++)
    for (seg = 0; seg < sroads [road] .snodeCount; seg ++)
      for (auxNodeIndex = 0; auxNodeIndex < sroads [road] .snodes [seg] .auxNodeCount; auxNodeIndex ++) {
	gpIndex = sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .rn .gpIndex;
	if (gpIndex >= 0) {	/* i.e. if it''s a road point, not an intersection point */
	  i = IDX2I (gpIndex);	/* grid point index */
	  j = IDX2J (gpIndex);
	  
	  xc = x0 + (i + 0.5) * binWidth; /* grid point center */
	  yc = winy0 + (j + 0.5) * binHeight;
	  intCount = 0;		/* will hold river ids */
	  ints = 0;
	  
	  for (riverSeg = 0; riverSeg < riverMatrix [IJ2IDX (i, j)] .binSize; riverSeg ++) {
	    lineID = riverMatrix [IJ2IDX (i, j)] .bin [riverSeg] .lineID;
	    segID = riverMatrix [IJ2IDX (i, j)] .bin [riverSeg] .segmentID;
	    for (river = 0; river < intCount && ints [river] != lineID; river ++)
	      ;
	    if (river == intCount) /* have not seen it yet */
	      if (lines_intersect (rivers [lineID] .points [segID] .x,
				   rivers [lineID] .points [segID] .y,
				   rivers [lineID] .points [segID + 1] .x,
				   rivers [lineID] .points [segID + 1] .y,
				   xc, yc,
				   sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .x,
				   sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .y,
				   & xIgnore, & yIgnore) != DONT_INTERSECT) {
		rnp = & (sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .rn);
		rnp -> u.roadPointDetails .riverCodeCount ++;
		rnp -> u.roadPointDetails .riverCodes =
		  (int *) realloc (rnp -> u.roadPointDetails .riverCodes, sizeof (int) * rnp -> u.roadPointDetails .riverCodeCount);
		rnp -> u.roadPointDetails .riverCodes [rnp -> u.roadPointDetails .riverCodeCount - 1] =
		  rivers [lineID] .featureCode;
		
		intCount ++;
		ints = (int *) realloc (ints, sizeof (int) * intCount);
		ints [intCount - 1] = lineID;
	      }
	  }
	  free (ints);
	}
      }
  
  /* create new road network */
  roadNetwork = (roadDescription *) malloc (sizeof (roadDescription) * roadCount);
  for (i = 0; i < roadCount; i ++) {
    roadNetwork [i] .featureCode = sroads [i] .featureCode;
    roadNetwork [i] .nodeCount = 0;
    roadNetwork [i] .sRoadNodes = 0;
  }
    
  for (road = 0; road < roadCount; road ++) {
    for (seg = 0; seg < sroads [road] .snodeCount; seg ++) {
      for (auxNodeIndex = 0; auxNodeIndex < sroads [road] .snodes [seg] .auxNodeCount; auxNodeIndex ++) {
	roadNetwork [road] .nodeCount ++;
	roadNetwork [road] .sRoadNodes = (sRoadNode *) realloc (roadNetwork [road] .sRoadNodes, sizeof (sRoadNode) * roadNetwork [road] .nodeCount);
	roadNetwork [road] .sRoadNodes [roadNetwork [road] .nodeCount - 1] .rn =
	  sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .rn;

	/* if this is an intersection, save its new segid with its old aux node for later */
	/* also save the old seg id with the new segment for later */
	if (sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .rn .gpIndex < 0) {
	  sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .newSegID = roadNetwork [road] .nodeCount - 1;
	  roadNetwork [road] .sRoadNodes [roadNetwork [road] .nodeCount - 1] .oldSegIndex = seg;
	} else {		/* otherwise, set those values to -1 */
	  sroads [road] .snodes [seg] .auxNodes [auxNodeIndex] .newSegID = -1;
	  roadNetwork [road] .sRoadNodes [roadNetwork [road] .nodeCount - 1] .oldSegIndex = -1;
	}
      }
    }
  }

  /* update intersection segment ids, since they changed when roadNetwork was made from sroads; */
  /* invert road point y dimension of gpIndex */
  for (road = 0; road < roadCount; road ++) {
    for (seg = 0; seg < roadNetwork [road] .nodeCount; seg ++) {
      gpIndex = roadNetwork [road] .sRoadNodes [seg] .rn.gpIndex;

      /* if this is an intersection, update its segment id */
      if (gpIndex < 0) {
	otherRoadIndex = roadNetwork [road] .sRoadNodes [seg] .rn.u.intersectionDetails .otherRoadIndex;
	otherRoadNode = roadNetwork [road] .sRoadNodes [seg] .rn.u.intersectionDetails .otherRoadNode; /* need to update this */

	/* get seg index for this segment in the old road */
	oldSegIndex = roadNetwork [road] .sRoadNodes [seg] .oldSegIndex;

	/* find the intersection */
	for (i = 0; i < sroads [otherRoadIndex] .snodes [otherRoadNode] .auxNodeCount; i ++)
	  if (sroads [otherRoadIndex] .snodes [otherRoadNode] .auxNodes [i] .rn.u.intersectionDetails .otherRoadIndex == road &&
	      sroads [otherRoadIndex] .snodes [otherRoadNode] .auxNodes [i] .rn.u.intersectionDetails .otherRoadNode == oldSegIndex)
	    break;
	if (i >= sroads [otherRoadIndex] .snodes [otherRoadNode] .auxNodeCount) {
	  printf ("internal error\n");
	  return 911;
	}

	roadNetwork [road] .sRoadNodes [seg] .rn.u.intersectionDetails .otherRoadNode =
	  sroads [otherRoadIndex] .snodes [otherRoadNode] .auxNodes [i] .newSegID;

	/* invert its y dimension */
	gpIndex = roadNetwork [road] .sRoadNodes [seg] .rn.u.intersectionDetails .gpIndex;
  	i = IDX2I (gpIndex);
  	j = IDX2J (gpIndex);

  	j = height - j - 1;

	roadNetwork [road] .sRoadNodes [seg] .rn.u.intersectionDetails .gpIndex = IJ2IDX (i, j);

      } else {			/* this is a road point; invert its y dimension */
  	i = IDX2I (gpIndex);
  	j = IDX2J (gpIndex);

  	j = height - j - 1;
	
  	roadNetwork [road] .sRoadNodes [seg] .rn.gpIndex = IJ2IDX (i, j);
      }
    }
  }

  /* return sroads structure to malloc */
  for (road = 0; road < roadCount; road ++) {
    for (seg = 0; seg < sroads [road] .snodeCount; seg ++)
      free (sroads [road] .snodes [seg] .auxNodes);
      
    free (sroads [road] .snodes);
  }
  free (sroads);

  /* create grid */
  if (! (grid = (gridCell *) malloc (sizeof (gridCell) * width * height))) {
    printf ("grid allocation failure\n");
    return 911;
  }
  for (i = 0; i < width; i ++)
    for (j = 0; j < height; j ++) {
      grid [IJ2IDX (i, j)] .roadPointCount = 0;
      grid [IJ2IDX (i, j)] .roadPoints = 0;
      for (k = 0; k < 8; k ++) {
	grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodeCount = 0;
	grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodes = 0;
      }
    }

  /* add road points to grid */
  for (road = 0; road < roadCount; road ++)
    for (seg = 0; seg < roadNetwork [road] .nodeCount; seg ++) {
      gpIndex = roadNetwork [road] .sRoadNodes [seg] .rn.gpIndex;
      if (gpIndex >= 0) {	/* i.e. if it's a road point, not an intersection point */
	grid [gpIndex] .roadPointCount ++;
	grid [gpIndex] .roadPoints = (gridRoadPoint *) realloc (grid [gpIndex] .roadPoints, sizeof (gridRoadPoint) * grid [gpIndex] .roadPointCount);
	grid [gpIndex] .roadPoints [grid [gpIndex] .roadPointCount - 1] .roadID = road;
	grid [gpIndex] .roadPoints [grid [gpIndex] .roadPointCount - 1] .pointID = seg;
      }
    }

  /* compute river codes for each grid point; this takes twice as long as it should */
  for (i = 0; i < width; i ++)
    for (j = 0; j < height; j ++) {
      xc = x0 + (i + 0.5) * binWidth; /* grid point center */
      yc = winy0 + (j + 0.5) * binHeight;
      for (k = 0; k < 8; k ++)
	if (neighborExists (i, j, k, width, height, & neighborXc, & neighborYc)) {
	  intCount = 0;		/* will hold river ids */
	  ints = 0;
	  for (riverSeg = 0; riverSeg < riverMatrix [IJ2IDX (i, j)] .binSize; riverSeg ++) {
	    lineID = riverMatrix [IJ2IDX (i, j)] .bin [riverSeg] .lineID;
	    segID = riverMatrix [IJ2IDX (i, j)] .bin [riverSeg] .segmentID;
	    for (river = 0; river < intCount && ints [river] != lineID; river ++)
	      ;
	    if (river == intCount)	/* have not seen it yet */
	      if (lines_intersect (rivers [lineID] .points [segID] .x,
				   rivers [lineID] .points [segID] .y,
				   rivers [lineID] .points [segID + 1] .x,
				   rivers [lineID] .points [segID + 1] .y,
				   xc, yc,
				   neighborXc, neighborYc,
				   & xIgnore, & yIgnore) != DONT_INTERSECT) {
		grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodeCount ++;
		grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodes =
		  (int *) realloc (grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodes, sizeof (int) * grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodeCount);
		grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodes [grid [IJ2IDX (i, j)] .riverCodes [k] .riverCodeCount - 1] =
		  rivers [lineID] .featureCode;
		
		intCount ++;
		ints = (int *) realloc (ints, sizeof (int) * intCount);
		ints [intCount - 1] = lineID;
	      }
	  }
	  free (ints);
	}
    }
  
  /* write road network */
  fwrite (& roadCount, sizeof (int), 1, fRNF);
  for (road = 0; road < roadCount; road ++) {
    fwrite (& (roadNetwork [road] .featureCode), sizeof (int), 1, fRNF);
    fwrite (& (roadNetwork [road] .nodeCount), sizeof (int), 1, fRNF);
    for (seg = 0; seg < roadNetwork [road] .nodeCount; seg ++) {
      fwrite (& (roadNetwork [road] .sRoadNodes [seg] .rn), sizeof (roadNode), 1, fRNF);
      if (roadNetwork [road] .sRoadNodes [seg] .rn.gpIndex >= 0 && /* it''s a road point */
	  roadNetwork [road] .sRoadNodes [seg] .rn.u.roadPointDetails .riverCodeCount) /* it has river codes */
	fwrite (roadNetwork [road] .sRoadNodes [seg] .rn.u.roadPointDetails .riverCodes, sizeof (int), 
		roadNetwork [road] .sRoadNodes [seg] .rn.u.roadPointDetails .riverCodeCount, fRNF);
    }
  }
  
  /* write grid */
  fwrite (& width, sizeof (int), 1, fRNF);
  fwrite (& height, sizeof (int), 1, fRNF);
  for (i = 0; i < width * height; i ++) {
    fwrite (& (grid [i] .roadPointCount), sizeof (int), 1, fRNF);
    if (grid [i] .roadPointCount)
      fwrite (grid [i] .roadPoints, sizeof (gridRoadPoint), grid [i] .roadPointCount, fRNF);
    for (k = 0; k < 8; k ++) {
      fwrite (& (grid [i] .riverCodes [k] .riverCodeCount), sizeof (int), 1, fRNF);
      if (grid [i] .riverCodes [k] .riverCodeCount)
	fwrite (grid [i] .riverCodes [k] .riverCodes, sizeof (int), grid [i] .riverCodes [k] .riverCodeCount, fRNF);
    }
  }

  fclose (fRNF);
  return 0;
}  
