#include <stdio.h>
#include <stdlib.h>
#include "intersect.h"
#include "loadlines.h"
#include "xlines.h"

typedef struct {
  int roadID;
  int segmentID;
} BinCell;

typedef BinCell * Bin;

typedef struct {
  int binSize;
  Bin bin;
} MatrixCell;

#define matrixRes 256
static MatrixCell matrix [matrixRes][matrixRes];

static void initMatrix () {
  int i, j;

  for (i = 0; i < matrixRes; i ++)
    for (j = 0; j < matrixRes; j ++) {
      matrix [i][j] .binSize = 0;
      matrix [i][j] .bin = 0;
    }
}

static void addToBin (int road, int seg, int i, int j) {
  i = MIN (i, matrixRes - 1);
  j = MIN (j, matrixRes - 1);

  // make sure it's not already binned here due to edge effects
  int k;
  for (k = 0; k <  matrix [i][j] .binSize; ++k)
    if ( matrix [i][j].bin[k].roadID == road && matrix [i][j].bin[k].segmentID == seg) {
      printf("skipping addToBin(%d, %d, %d, %d)\n", road, seg, i, j);
      return;
    }

  // road 69693 segment 3 intersects road 8898 segment 0
    
  matrix [i][j] .binSize ++;
  matrix [i][j] .bin = (BinCell *) realloc (matrix [i][j] .bin, matrix [i][j] .binSize * sizeof (BinCell));
  matrix [i][j] .bin [matrix [i][j] .binSize - 1] .roadID = road;
  matrix [i][j] .bin [matrix [i][j] .binSize - 1] .segmentID = seg;
}

static double xmin = 1000;
static double ymin = 1000;
static double xmax = -1;
static double ymax = -1;
static double binWidth;
static double binHeight;

#define BIN_I(x) ((int) (((x) - xmin)/binWidth))
#define BIN_J(y) ((int) (((y) - ymin)/binHeight))

static void loadMatrix (line * roads, int roadCount) {
  int road, seg;
  int multiCellSegmentCount = 0;
  int longSegCount = 0;

  initMatrix ();

  for (road = 0; road < roadCount; road ++)
    for (seg = 0; seg < roads [road] .length - 1; seg ++) {
      addToBin (road, seg, BIN_I (roads [road] .points [seg] .x), BIN_J (roads [road] .points [seg] .y));

      if (BIN_I (roads [road] .points [seg] .x) != BIN_I (roads [road] .points [seg + 1] .x) ||
	  BIN_J (roads [road] .points [seg] .y) != BIN_J (roads [road] .points [seg + 1] .y)) {
	addToBin (road, seg, BIN_I (roads [road] .points [seg + 1] .x), BIN_J (roads [road] .points [seg + 1] .y));
	multiCellSegmentCount ++;

	if (abs (BIN_I (roads [road] .points [seg] .x) - BIN_I (roads [road] .points [seg + 1] .x)) > 1 ||
	    abs (BIN_J (roads [road] .points [seg] .y) - BIN_J (roads [road] .points [seg + 1] .y)) > 1)
	  longSegCount ++;
      }
    }
  printf ("loadMatrix counted %d multi cell segments and %d long segments\n", multiCellSegmentCount, longSegCount);
}

int main (int argc, char * argv []) {
  char * argPattern = "TVFname RIFname";
  int status;
  int roadCount = 0;
  line * roads = 0;
  FILE * fDest;
  int RIFrecCount = 0;
  int roadID;
  int segmentID;
  int otherSegment, otherRoad;
  double doubleX, doubleY;
  RIFrec aRIFrec;
  int segCount = 0;
  int matI, matJ, binI, binJ;

  /* check args */
  if (argc != 3) {printf ("usage: %s %s\n", argv [0], argPattern); return 1;}

  printf ("loading roads ... "); fflush (stdout);

  /* load roads */
  if ((status = loadLines (argv [1], & roadCount, & roads, 1)))
    {printf ("%s: loadLines returned %d while attempting to load roads from %s\n", argv [0], status, argv [1]); return 2;}

  printf ("done.\nloaded %d roads\n", roadCount);

  /* get road stats */
  for (roadID = 0; roadID < roadCount; roadID ++) {
    segCount += roads [roadID] .length -1;
    for (segmentID = 0; segmentID < roads [roadID] . length; segmentID ++) {
      xmin = MIN (xmin, roads [roadID] . points [segmentID] . x);
      xmax = MAX (xmax, roads [roadID] . points [segmentID] . x);
      ymin = MIN (ymin, roads [roadID] . points [segmentID] . y);
      ymax = MAX (ymax, roads [roadID] . points [segmentID] . y);
    }
  }

  binWidth = (xmax - xmin) / matrixRes;
  binHeight = (ymax - ymin) / matrixRes;

  printf ("total %d segments, averaging %f segments/road\n", segCount, (segCount + 1.0) / roadCount);
  printf ("x range %.10f to %.10f, y range %.10f to %.10f\n", xmin, xmax, ymin, ymax);

  /* sort into bins */
  printf ("loading matrix... "); fflush (stdout);
  loadMatrix (roads, roadCount);
  printf ("done.\n");

  /* get matrix stats */
  segCount = 0;
  for (matI = 0; matI < matrixRes; matI ++)
    for (matJ = 0; matJ < matrixRes; matJ ++)
      segCount += matrix [matI][matJ] .binSize;

  printf ("matrix holds %d segments\n", segCount);

  /* open RIF file for writing */
  if (! (fDest = fopen (argv[2], "w"))) {printf ("%s: error opening %s for writing\n", argv [0], argv[2]); return 3;}

  fwrite (& RIFrecCount, sizeof (int), 1, fDest); /* update when done */

  for (matI = 0; matI < matrixRes; matI ++) {
/*    printf ("scanning column %d\n", matI);*/
    for (matJ = 0; matJ < matrixRes; matJ ++) {
/*      printf ("scanning column %d, row %d\n", matI, matJ);*/
      for (binI = 0; binI < matrix [matI][matJ] .binSize - 1; binI ++)
	for (binJ = binI + 1; binJ < matrix [matI][matJ] .binSize; binJ ++) {
	  roadID = matrix [matI][matJ] .bin [binI] .roadID;
	  segmentID = matrix [matI][matJ] .bin [binI] .segmentID;
	  otherRoad = matrix [matI][matJ] .bin [binJ] .roadID;
	  otherSegment = matrix [matI][matJ] .bin [binJ] .segmentID;

	  /* road 8898 seg 0 added to bin 9, 1 with binSize now 739 */
	  /* road 69693 seg 3 added to bin 9, 1 with binSize now 3097 */

#if 0
	  if (matI == 9 && matJ == 1 && binI == 738) /* && binJ == 3097*/
	    printf ("comparing crossed roads\n");
#endif

	  if (lines_intersect (roads [roadID] . points [segmentID] . x,
			       roads [roadID] . points [segmentID] . y,
			       roads [roadID] . points [segmentID + 1] . x,
			       roads [roadID] . points [segmentID + 1] . y,
			       roads [otherRoad] . points [otherSegment] . x,
			       roads [otherRoad] . points [otherSegment] . y,
			       roads [otherRoad] . points [otherSegment + 1] . x,
			       roads [otherRoad] . points [otherSegment + 1] . y,
			       & doubleX, & doubleY) == DO_INTERSECT) {

	    if ((roadID != otherRoad || abs (segmentID - otherSegment) > 1) &&
		BIN_I (doubleX) == matI && BIN_J (doubleY) == matJ) {
	      aRIFrec . road1ID = roadID;
	      aRIFrec . segment1ID = segmentID;
	      aRIFrec . road2ID = otherRoad;
	      aRIFrec . segment2ID = otherSegment;
	      aRIFrec . x = doubleX;
	      aRIFrec . y = doubleY;
	      fwrite (& aRIFrec, sizeof (RIFrec), 1, fDest);
	      RIFrecCount ++;
	    }
	  }
	}
    }
  }

#if 0
  for (roadID = 0; roadID < roadCount; roadID ++) {
    printf ("examining road %d (%d segments)\n", roadID, roads [roadID] . length);

    /* limit segmentID to roads [roadID] . length - 1, since the last point does not begin a segment */
    for (segmentID = 0; segmentID < roads [roadID] . length - 1; segmentID ++) {
      /* check the remaining segments in this road */
      /* otherSegment starts at segmentID + 2, since we already know that adjacent segments connect */
      for (otherSegment = segmentID + 2; otherSegment < roads [roadID] . length - 1; otherSegment ++)
	if (lines_intersect (roads [roadID] . points [segmentID] . x,
			     roads [roadID] . points [segmentID] . y,
			     roads [roadID] . points [segmentID + 1] . x,
			     roads [roadID] . points [segmentID + 1] . y,
			     roads [roadID] . points [otherSegment] . x,
			     roads [roadID] . points [otherSegment] . y,
			     roads [roadID] . points [otherSegment + 1] . x,
			     roads [roadID] . points [otherSegment + 1] . y,
			     & doubleX, & doubleY) == DO_INTERSECT) {

	  printf ("road %d segment %d intersects road %d segment %d\n", roadID, segmentID, roadID, otherSegment);

	  aRIFrec . road1ID = roadID;
	  aRIFrec . segment1ID = segmentID;
	  aRIFrec . road2ID = roadID;
	  aRIFrec . segment2ID = otherSegment;
	  aRIFrec . x = doubleX;
	  aRIFrec . y = doubleY;
	  fwrite (& aRIFrec, sizeof (RIFrec), 1, fDest);
	  RIFrecCount ++;
	}

      /* check the remaining roads */
      for (otherRoad = roadID + 1; otherRoad < roadCount; otherRoad ++) {
	for (otherSegment = 0; otherSegment < roads [otherRoad] . length - 1; otherSegment ++) {
	  if (lines_intersect (roads [roadID] . points [segmentID] . x,
			       roads [roadID] . points [segmentID] . y,
			       roads [roadID] . points [segmentID + 1] . x,
			       roads [roadID] . points [segmentID + 1] . y,
			       roads [otherRoad] . points [otherSegment] . x,
			       roads [otherRoad] . points [otherSegment] . y,
			       roads [otherRoad] . points [otherSegment + 1] . x,
			       roads [otherRoad] . points [otherSegment + 1] . y,
			       & doubleX, & doubleY) == DO_INTERSECT) {

	    printf ("Road %d segment %d intersects road %d segment %d\n", roadID, segmentID, otherRoad, otherSegment);
#if 0
	    printf ("road1 (%.10f, %.10f)-(%.10f, %.10f) road2 (%.10f, %.10f)-(%.10f, %.10f)\n",
		    roads [roadID] . points [segmentID] . x,
		    roads [roadID] . points [segmentID] . y,
		    roads [roadID] . points [segmentID + 1] . x,
		    roads [roadID] . points [segmentID + 1] . y,
		    roads [otherRoad] . points [otherSegment] . x,
		    roads [otherRoad] . points [otherSegment] . y,
		    roads [otherRoad] . points [otherSegment + 1] . x,
		    roads [otherRoad] . points [otherSegment + 1] . y);
	    printf ("intersection at (%.10f, %.10f)\n", doubleX, doubleY);
#endif
	    
	    aRIFrec . road1ID = roadID;
	    aRIFrec . segment1ID = segmentID;
	    aRIFrec . road2ID = otherRoad;
	    aRIFrec . segment2ID = otherSegment;
	    aRIFrec . x = doubleX;
	    aRIFrec . y = doubleY;
	    fwrite (& aRIFrec, sizeof (RIFrec), 1, fDest);
	    RIFrecCount ++;
	  }
	}
      }
    }
  }
#endif

  printf ("rec count %d\n", RIFrecCount);
  printf ("writing rec count\n");

  rewind (fDest);

  fwrite (& RIFrecCount, sizeof (int), 1, fDest); /* update when done */

  fclose (fDest);

  printf ("done");
  return 0;
}
