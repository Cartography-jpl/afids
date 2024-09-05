#include <stdlib.h>
#include <stdio.h>
#include "rnf.h"

/* returns 0 when happy */
int loadRNF (char * path, dRoadDescription ** roadNetworkP, int * roadCount, gridCell ** gridP, int * width, int * height) {
  FILE * f;
  int road, seg, i, k;
  dRoadDescription * roadNetwork;
  gridCell * grid;

  if (! (f = fopen (path, "r"))) {
    printf ("loadRNF: error opening %s for reading\n", path);
    return LOADRNF_BAD_PATH;
  }

  /*** read road network ***/

  /* get roadCount */
  if (fread (roadCount, sizeof (int), 1, f) != 1)
    return LOADRNF_READ_FAILED;

  /* allocate space for road descriptions */
  if (! (roadNetwork = * roadNetworkP = (dRoadDescription *) malloc (sizeof (dRoadDescription) * (* roadCount))))
    return LOADRNF_MALLOC_FAILED;

  /* foreach road description ... */
  for (road = 0; road < * roadCount; road ++) {
    /* get the feature code */
    if (fread (& (roadNetwork [road] .featureCode), sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;
    /* get the node count */
    if (fread (& (roadNetwork [road] .nodeCount), sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;
    /* if there are nodes, allocate space for them */
    if (roadNetwork [road] .nodeCount) {
      if (! (roadNetwork [road] .roadNodes = (dRoadNode *) malloc (sizeof (dRoadNode) * (roadNetwork [road] .nodeCount)))) return LOADRNF_MALLOC_FAILED;
    } else
      roadNetwork [road] .roadNodes = 0;
    /* foreach node in the road ... */
    for (seg = 0; seg < roadNetwork [road] .nodeCount; seg ++) {
      /* read the node */
      if (fread (& (roadNetwork [road] .roadNodes [seg] .rn), sizeof (roadNode), 1, f) != 1) return LOADRNF_READ_FAILED;
      /* initialize its Dijkstra bookkeeping data */
      roadNetwork [road] .roadNodes [seg] .costToNextNode = 0.0;
      roadNetwork [road] .roadNodes [seg] .costToGP = 0.0;
      /* if it has river codes, allocate space for them */
      if (roadNetwork [road] .roadNodes [seg] .rn.gpIndex >= 0 && /* it''s a road point */
	  roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodeCount) { /* it has river codes */
	if (! (roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodes =
	       (int *) malloc (sizeof (int) * roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodeCount))) return LOADRNF_MALLOC_FAILED;
	/* get the river codes */
	if (fread (roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodes, sizeof (int), 
		roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodeCount, f) !=
	    roadNetwork [road] .roadNodes [seg] .rn.u.roadPointDetails .riverCodeCount)
	  return LOADRNF_READ_FAILED;
      }
    }
  }
  
  /*** read grid ***/

  /* get grid width */
  if (fread (width, sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;

  /* get grid height */
  if (fread (height, sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;

  /* allocate grid space */
  if (! (grid = * gridP = (gridCell *) malloc (sizeof (gridCell) * (* width) * (* height)))) return LOADRNF_MALLOC_FAILED;

  /* foreach grid cell ... */
  for (i = 0; i < (* width) * (* height); i ++) {
    /* get the road point count */
    if (fread (& (grid [i] .roadPointCount), sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;
    /* if the cell has road points, allocate space for them */
    if (grid [i] .roadPointCount) {
      if (! (grid [i] .roadPoints = (gridRoadPoint *) malloc (sizeof (gridRoadPoint) * grid [i] .roadPointCount))) return LOADRNF_MALLOC_FAILED;
      /* get the road points */
      if (fread (grid [i] .roadPoints, sizeof (gridRoadPoint), grid [i] .roadPointCount, f) != grid [i] .roadPointCount) return LOADRNF_READ_FAILED;
    }
    /* foreach grid neighbor ... */
    for (k = 0; k < 8; k ++) {
      /* get the number of rivers to cross to get to the neighbor */
      if (fread (& (grid [i] .riverCodes [k] .riverCodeCount), sizeof (int), 1, f) != 1) return LOADRNF_READ_FAILED;
      /* there are such rivers */
      if (grid [i] .riverCodes [k] .riverCodeCount) {
	/* allocate space for the river codes */
	if (! (grid [i] .riverCodes [k] .riverCodes = (int *) malloc (sizeof (int) * grid [i] .riverCodes [k] .riverCodeCount))) return LOADRNF_MALLOC_FAILED;
	/* get the river codes */
	if (fread (grid [i] .riverCodes [k] .riverCodes, sizeof (int), grid [i] .riverCodes [k] .riverCodeCount, f) != grid [i] .riverCodes [k] .riverCodeCount)
	  return LOADRNF_READ_FAILED;
      }
    }
  }

  fclose (f);

  return LOADRNF_HAPPY;
}

int checkRNF (dRoadDescription * roadNetworkP, int roadCount, gridCell * gridP, int width, int height, int verbose) {
  int road, node, otherRoadIndex, otherRoadNode;
  int roadLengthSum = 0;
  int minRoadLength = 10000;
  int maxRoadLength = -1;
  int zeroLengthRoadCount = 0;
  int * zeroLengthRoadIDs = 0;
  int i, j, k, gpIndex;
  
  int roadPointSum = 0;
  int minRoadPointCount = 10000;
  int maxRoadPointCount = -1;
  int riverCrossingSum = 0;
  int minRiverCrossingCount = 10000;
  int maxRiverCrossingCount = -1;
  int cellsWithARoad = 0;
  int cellsWithARiver = 0;
  int cellsWithEither = 0;
  int thisCellHasARiver;

  int status = 0;

  for (road = 0; road < roadCount; road ++) {
    roadLengthSum += roadNetworkP [road] .nodeCount;
#define MIN(x, y) ((x) > (y) ? (y) : (x))
#define MAX(x, y) ((x) < (y) ? (y) : (x))
    minRoadLength = MIN (minRoadLength, roadNetworkP [road] .nodeCount);
    maxRoadLength = MAX (maxRoadLength, roadNetworkP [road] .nodeCount);
    if (! roadNetworkP [road] .nodeCount) {
      zeroLengthRoadCount ++;
      zeroLengthRoadIDs = (int *) realloc (zeroLengthRoadIDs, sizeof (int) * zeroLengthRoadCount);
      zeroLengthRoadIDs [zeroLengthRoadCount - 1] = road;
    }
    for (node = 0; node < roadNetworkP [road] .nodeCount; node ++)
      if (roadNetworkP [road] .roadNodes [node] .rn.gpIndex < 0) {
	otherRoadIndex = roadNetworkP [road] .roadNodes [node] .rn.u.intersectionDetails .otherRoadIndex;
	otherRoadNode = roadNetworkP [road] .roadNodes [node] .rn.u.intersectionDetails .otherRoadNode;

	if (otherRoadIndex >= roadCount) {
	  printf ("Road %d, node %d intersects with node %d of road %d, but there are only %d roads\n",
		  road, node, otherRoadNode, otherRoadIndex, roadCount);
	  status = 1;
	} else {
	  if (otherRoadNode >= roadNetworkP [otherRoadIndex] .nodeCount) {
	    printf ("Road %d, node %d intersects with node %d of road %d, a road with only %d nodes",
		    road, node, otherRoadNode, otherRoadIndex, roadNetworkP [otherRoadIndex] .nodeCount);
	  status = 1;
	  }
	  if (roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.gpIndex >= 0) {
	    printf ("Road %d, node %d intersects with node %d of road %d, a road point with gpIndex %d",
		    road, node, otherRoadNode, otherRoadIndex, roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.gpIndex);
	    status = 1;
	  }
	  if (roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.u.intersectionDetails .otherRoadIndex != road ||
	      roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.u.intersectionDetails .otherRoadNode != node) {
	    printf ("Road %d, node %d intersects with node %d of road %d, but that node intersects with road %d, node %d\n",
		    road, node, otherRoadNode, otherRoadIndex,
		    roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.u.intersectionDetails .otherRoadIndex,
		    roadNetworkP [otherRoadIndex] .roadNodes [otherRoadNode] .rn.u.intersectionDetails .otherRoadNode);
	    status = 1;
	  }
	  if (otherRoadIndex == road && otherRoadNode == node) {
	    printf ("Road %d, node %d intersects with itself\n", road, node);
	    status = 1;
	  }
	}
      }
  }

  if (verbose) {
    printf ("%d roads, averaging %f nodes each\n", roadCount, ((float) roadLengthSum) / roadCount);
    printf ("%d minRoadLength, %d maxRoadLength\n", minRoadLength, maxRoadLength);
    printf ("%d zeroLengthRoads\n", zeroLengthRoadCount);
    
    for (j = 0; j < height; j ++)
      for (i = 0; i < width; i ++) {
	gpIndex = i + j * width;
	roadPointSum += gridP [gpIndex] .roadPointCount;
	minRoadPointCount = MIN (minRoadPointCount, gridP [gpIndex] .roadPointCount);
	maxRoadPointCount = MAX (maxRoadPointCount, gridP [gpIndex] .roadPointCount);
	if (gridP [gpIndex] .roadPointCount)
	  cellsWithARoad ++;

	thisCellHasARiver = 0;
	for (k = 0; k < 8; k ++) {
	  riverCrossingSum += gridP [gpIndex] .riverCodes [k] .riverCodeCount;
	  minRiverCrossingCount = MIN (minRiverCrossingCount, gridP [gpIndex] .riverCodes [k] .riverCodeCount);
	  maxRiverCrossingCount = MAX (maxRiverCrossingCount, gridP [gpIndex] .riverCodes [k] .riverCodeCount);
	  if (gridP [gpIndex] .riverCodes [k] .riverCodeCount)
	    thisCellHasARiver = 1;
	}
	if (thisCellHasARiver)
	  cellsWithARiver ++;

	if (gridP [gpIndex] .roadPointCount || thisCellHasARiver)
	  cellsWithEither ++;
      }
    
    printf ("grid width %d, height %d, %d total cells\n", width, height, width * height);
    printf ("%d minRoadPointCount, %d maxRoadPointCount, avg %f\n", minRoadPointCount, maxRoadPointCount, ((float) roadPointSum) / (width * height));
    printf ("%d minRiverCrossings, %d maxRiverCrossings, avg %f\n", minRiverCrossingCount, maxRiverCrossingCount, ((float) riverCrossingSum) / (width * height));
    printf ("%f%% of grid cells have at least one road\n", 100.0 * ((float) cellsWithARoad) / (width * height));
    printf ("%f%% of grid cells have at least one river\n", 100.0 * ((float) cellsWithARiver) / (width * height));
    printf ("%f%% of grid cells have at least one road or river\n", 100.0 * ((float) cellsWithEither) / (width * height));
  }

  return status;
}


void freeRNF (dRoadDescription * roadNetworkP, int roadCount, gridCell * gridP, int width, int height) {
  int road, i, k;

  for (road = 0; road < roadCount; road ++)
    free (roadNetworkP [road] .roadNodes);
  free (roadNetworkP);

  for (i = 0; i < width * height; i ++) {
    if (gridP [i] .roadPointCount)
      free (gridP [i] .roadPoints);
    for (k = 0; k < 8; k ++)
      if (gridP [i] .riverCodes [k] .riverCodeCount)
	free (gridP [i] .riverCodes [k] .riverCodes);
  }

  free (gridP);
}
