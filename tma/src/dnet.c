#include <stdlib.h>
#include <stdio.h>

#include "dnet.h"

static int neighborExists (int i, int j, int k, int width, int height, int * nbi, int * nbj) {
  switch (k) {
  case 0:
    * nbi = i - 1;
    * nbj = j - 1;
    return i > 0 && j > 0;
  case 1:
    * nbi = i;
    * nbj = j - 1;
    return j > 0;
  case 2:
    * nbi = i + 1;
    * nbj = j - 1;
    return i < width - 1 && j > 0;
  case 3:
    * nbi = i - 1;
    * nbj = j;
    return i > 0;
  case 4:
    * nbi = i + 1;
    * nbj = j;
    return i < width - 1;
  case 5:
    * nbi = i - 1;
    * nbj = j + 1;
    return i > 0 && j < height - 1;
  case 6:
    * nbi = i;
    * nbj = j + 1;
    return j < height - 1;
  case 7:
    * nbi = i + 1;
    * nbj = j + 1;
    return i < width - 1 && j < height - 1;
  default:
    printf ("illegal neightbor %d\n", k);
    return 0;
  }
}

int buildDnet (double *** gridCosts, dRoadDescription * roadNetworkP, int roadCount, int width, int height, dnet ** dnetP, gridCell * gridP) {
  dnet * dn = 0;
  dnetNode * nv = 0;
  dnetNode * nodeP = 0;
  int road, node, line, sample;
  int roadsOffset = width * height;
  int k;
  int nbLine, nbSample;
  int roadNodeCount = 0;
  int neighborRoad, neighborNode;
  int gpIndex;
  int dnetIndex;
  double costToGP, costToNextNode;
  int riverCodeCount, riverIndex;

  for (road = 0; road < roadCount; road ++)
    for (node = 0; node < roadNetworkP [road] .nodeCount; node ++) {
      roadNetworkP [road] .roadNodes [node] .rn.dnetIndex = roadNodeCount;
      roadNodeCount ++;
    }

  if (! (* dnetP = dn = (dnet *) malloc (sizeof (dnet)))) return DNET_MALLOC_FAILED;
  dn -> width = width;
  dn -> height = height;
  dn -> nc = roadsOffset + roadNodeCount;
  if (! (dn -> nv = nv = (dnetNode *) malloc (sizeof (dnetNode) * dn -> nc))) return DNET_MALLOC_FAILED;

  for (node = 0; node < dn -> nc; node ++) {
    nv [node] .bestTime = TOO_HUGE;
    nv [node] .alreadyWet = 0;
    nv [node] .withinBorder = 0;
    nv [node] .flowFrom = -1;
    nv [node] .gpIndex = -1;
    nv [node] .nbc = 0;
    nv [node] .nbv = 0;
    nv [node] .nbcv = 0;
    nv [node] .ncnd = 0;
  }

  fprintf (stderr, "building grid point derived nodes\n");

  /* build grid point derived nodes */
  for (line = 0; line < height; line ++)
    for (sample = 0; sample < width; sample ++) {
      gpIndex = sample + line * width;
      nodeP = nv + gpIndex;
      nodeP -> gpIndex = gpIndex;
      for (k = 0; k < 8; k ++)
	/* add grid point to grid point neighbors */
	if (neighborExists (sample, line, k, width, height, & nbSample, & nbLine)) {
	  nodeP -> nbc ++;

	  /* extend the neighbor index vector */
	  if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	  nodeP -> nbv [nodeP -> nbc - 1] = nbSample + nbLine * width; /* neighbor index */

	  /* extend the neighbor cost vector */
	  if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	  if (gridCosts)	/* dnettest doesn't use gridCosts */
	    nodeP -> nbcv [nodeP -> nbc - 1] = gridCosts [line] [sample] [k];	/* cost to neighbor */

	  /* extend the neighbor non cost data vector; in this case it's feature codes of rivers crossed while travelling to grid neighbors */
	  if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	  riverCodeCount = gridP [gpIndex] . riverCodes [k] . riverCodeCount;
	  nodeP -> ncnd [nodeP -> nbc - 1] . roadType = 0;
	  nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = riverCodeCount;
	  if (! (nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = (int *) malloc (sizeof (int) * riverCodeCount))) return DNET_MALLOC_FAILED;
	  for (riverIndex = 0; riverIndex < riverCodeCount; riverIndex ++)
	    nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes [riverIndex] = gridP [IJ2IDX (sample, line)] . riverCodes [k] . riverCodes [riverIndex];
	}
    }      

  fprintf (stderr, "building road node derived nodes\n");

  /* build road node derived nodes */
  roadNodeCount = 0;
  for (road = 0; road < roadCount; road ++) {

    for (node = 0; node < roadNetworkP [road] .nodeCount; node ++) {

      gpIndex = roadNetworkP [road] .roadNodes [node] .rn.gpIndex;


      if (gpIndex < 0) {	/* intersection point */
	/* get the intersection point */
	nodeP = nv + roadsOffset + roadNetworkP [road] .roadNodes [node] .rn.dnetIndex;

	/* set its gpIndex */
	nodeP -> gpIndex = roadNetworkP [road] .roadNodes [node] .rn.u.intersectionDetails .gpIndex;

	/* add intersection neighbor */
	neighborRoad = roadNetworkP [road] .roadNodes [node] .rn.u.intersectionDetails .otherRoadIndex;
	neighborNode = roadNetworkP [road] .roadNodes [node] .rn.u.intersectionDetails .otherRoadNode;
	dnetIndex = roadNetworkP [neighborRoad] .roadNodes [neighborNode] .rn.dnetIndex;

	nodeP -> nbc ++;

	/* extend the neighbor index vector */
	if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbv [nodeP -> nbc - 1] = roadsOffset + dnetIndex; /* neighbor index */

	/* extend the neighbor cost vector */
	if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbcv [nodeP -> nbc - 1] = 0; /* cost to neighbor */

	/* extend the neighbor non cost data vector; in this case it's feature code of a road intersection */
	if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> ncnd [nodeP -> nbc - 1] . roadType = roadNetworkP [road] . featureCode;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = 0;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = 0;

      } else {			/* road point (a connection point to the grid) */
	/* get the grid point */
	nodeP = nv + gpIndex;	/* the grid point */
	costToGP = roadNetworkP [road] .roadNodes [node] .costToGP;

	/* add grid point's neighbor */
	nodeP -> nbc ++;

	/* extend the neighbor index vector */
	if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbv [nodeP -> nbc - 1] = roadsOffset + roadNetworkP [road] .roadNodes [node] .rn.dnetIndex; /* neighbor index */

	/* extend the neighbor cost vector */
	if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbcv [nodeP -> nbc - 1] = costToGP; /* cost to neighbor */

	/* extend the neighbor non cost data vector; in this case it's feature codes of rivers crossed while travelling from grid to road */
	if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	riverCodeCount = roadNetworkP [road] .roadNodes [node] .rn.u.roadPointDetails . riverCodeCount;
	nodeP -> ncnd [nodeP -> nbc - 1] . roadType = roadNetworkP [road] . featureCode;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = riverCodeCount;
	if (! (nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = (int *) malloc (sizeof (int) * riverCodeCount))) return DNET_MALLOC_FAILED;
	for (riverIndex = 0; riverIndex < riverCodeCount; riverIndex ++)
	  nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes [riverIndex] = roadNetworkP [road] .roadNodes [node] .rn.u.roadPointDetails . riverCodes [riverIndex];

	/* get the road point */
	nodeP = nv + roadsOffset + roadNetworkP [road] .roadNodes [node] .rn.dnetIndex;
	/* set its gpIndex */
	nodeP -> gpIndex = gpIndex;

	/* now add road point's neighbor */
	nodeP -> nbc ++;

	/* extend the neighbor index vector */
	if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbv [nodeP -> nbc - 1] = gpIndex; /* neighbor index */

	/* extend the neighbor cost vector */
	if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbcv [nodeP -> nbc - 1] = costToGP; /* cost to neighbor */

	/* extend the neighbor non cost data vector; in this case it's feature codes of rivers crossed while travelling from road to grid */
	if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	riverCodeCount = roadNetworkP [road] .roadNodes [node] .rn.u.roadPointDetails . riverCodeCount;
	nodeP -> ncnd [nodeP -> nbc - 1] . roadType = roadNetworkP [road] . featureCode;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = riverCodeCount;
	if (! (nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = (int *) malloc (sizeof (int) * riverCodeCount))) return DNET_MALLOC_FAILED;
	for (riverIndex = 0; riverIndex < riverCodeCount; riverIndex ++)
	  nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes [riverIndex] = roadNetworkP [road] .roadNodes [node] .rn.u.roadPointDetails . riverCodes [riverIndex];
      }

      /* get the road node */
      nodeP = nv + roadsOffset + roadNetworkP [road] .roadNodes [node] .rn.dnetIndex;

      /* add road to road neighbors */

      /* looking backward */
      if (node > 0) {
	costToNextNode = roadNetworkP [road] .roadNodes [node - 1] .costToNextNode;

	nodeP -> nbc ++;

	/* extend the neighbor index vector */
	if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbv [nodeP -> nbc - 1] = roadsOffset + roadNetworkP [road] .roadNodes [node - 1] .rn.dnetIndex; /* neighbor index */

	/* extend the neighbor cost vector */
	if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbcv [nodeP -> nbc - 1] = costToNextNode; /* cost to neighbor */

	/* extend the neighbor non cost data vector; in this case it's the feature code of a road */
	if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> ncnd [nodeP -> nbc - 1] . roadType = roadNetworkP [road] . featureCode;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = 0;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = 0;
      }

      /* looking forward */
      if (node < roadNetworkP [road] .nodeCount - 1) { 
	costToNextNode = roadNetworkP [road] .roadNodes [node] .costToNextNode;

	nodeP -> nbc ++;

	/* extend the neighbor index vector */
	if (! (nodeP -> nbv = (int *) realloc (nodeP -> nbv, sizeof (int) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbv [nodeP -> nbc - 1] = roadsOffset + roadNetworkP [road] .roadNodes [node + 1] .rn.dnetIndex; /* neighbor index */

	/* extend the neighbor cost vector */
	if (! (nodeP -> nbcv = (double *) realloc (nodeP -> nbcv, sizeof (double) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> nbcv [nodeP -> nbc - 1] = costToNextNode; /* cost to neighbor */

	/* extend the neighbor non cost data vector; in this case it's the feature code of a road */
	if (! (nodeP -> ncnd = (nonCostNeighborData *) realloc (nodeP -> ncnd, sizeof (nonCostNeighborData) * nodeP -> nbc))) return DNET_MALLOC_FAILED;
	nodeP -> ncnd [nodeP -> nbc - 1] . roadType = roadNetworkP [road] . featureCode;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCount = 0;
	nodeP -> ncnd [nodeP -> nbc - 1] . riverCodes = 0;
      }
    }
  }

  fprintf (stderr, "DNET_HAPPY\n");

  return DNET_HAPPY;
}

/* returns 0 when happy */
int checkDnet (dnet * dnetP, int verbose) {
  int status = 0;
  int node, neighbor, otherNeighbor, neighborIndex;
  int gridSize = dnetP -> width * dnetP -> height;
  int totalNeighborCount = 0, nonGridNeighborCount = 0;
  int minNeighborCount = 10000, maxNeighborCount = -1;
  int thisI, thisJ, neighborI, neighborJ, neighborDistI, neighborDistJ;
  int width, height;

  width = dnetP -> width;
  height = dnetP -> height;

  if (verbose) {
    printf ("width %d and height %d give %d grid cells\n", width, height, gridSize);
    printf ("total %d dnet nodes\n", dnetP -> nc);
  }

  if (dnetP -> nc < gridSize) {
    printf ("Node count %d < width * height (%d * %d == %d)\n",
	    dnetP -> nc, width, height, gridSize);
    status = 1;
  }

  for (node = 0; node < gridSize; node ++) {
    if (dnetP -> nv [node] . gpIndex != node) {
      printf ("Grid cell %d doesn't know its own index\n", node);
      status = 1;
    }
  }

  /* for each grid node */
  for (node = 0; node < dnetP -> nc; node ++) {
    /* check gpIndex */
    if (dnetP -> nv [node] . gpIndex < 0 || dnetP -> nv [node] . gpIndex >= width * height) {
       printf ("Node %d has gpIndex == %d <= || >= %d\n", node, dnetP -> nv [node] . gpIndex, width * height);
    }

    if (verbose) {
      totalNeighborCount += dnetP -> nv [node] . nbc;
      if (node >= gridSize)
	nonGridNeighborCount += dnetP -> nv [node] . nbc;
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#define MAX(a, b) ((a) < (b) ? (b) : (a))
      minNeighborCount = MIN (minNeighborCount, dnetP -> nv [node] . nbc);
      maxNeighborCount = MAX (maxNeighborCount, dnetP -> nv [node] . nbc);
    }

    thisI = IDX2I (dnetP -> nv [node] . gpIndex);
    thisJ = IDX2J (dnetP -> nv [node] . gpIndex);
    /* for each of its neighbors */
    for (neighbor = 0; neighbor < dnetP -> nv [node] . nbc; neighbor ++) {
      /* verify that neighbor index is in range */
      neighborIndex = dnetP -> nv [node] . nbv [neighbor];
      if (neighborIndex >= dnetP -> nc || neighborIndex < 0) {
	printf ("Node %d, neighbor %d, has index %d out of node count range (0 .. %d - 1)\n",
		node, neighbor, neighborIndex, dnetP -> nc);
	status = 1;
      } else {
	/* verify that neighbor is close */
	neighborI = IDX2I (dnetP -> nv [neighborIndex] . gpIndex);
	neighborJ = IDX2J (dnetP -> nv [neighborIndex] . gpIndex);
	neighborDistI = thisI - neighborI;
	neighborDistJ = thisJ - neighborJ;
	if (neighborDistI * neighborDistI > 1 || neighborDistJ * neighborDistJ > 1)
	  printf ("Node %d at gpIndex %d (%d, %d) has far neighbor %d at gpIndex %d (%d, %d)\n",
		  node, dnetP -> nv [node] . gpIndex, thisI, thisJ,
		  neighborIndex, dnetP -> nv [neighborIndex] . gpIndex, neighborI, neighborJ);

	/* verify that neighbors know each other */
	for (otherNeighbor = 0; otherNeighbor < dnetP -> nv [neighborIndex] . nbc; otherNeighbor ++)
	  if (dnetP -> nv [neighborIndex] . nbv [otherNeighbor] == node)
	    break;
	if (otherNeighbor >= dnetP -> nv [neighborIndex] . nbc) {
	  printf ("Node %d neighbor %d doesn't know it's a neighbor\n", node, neighborIndex);
	  status = 1;
	}
      }
    }
  }

  if (verbose) {
    printf ("average neighbor count: %f\n", (float) totalNeighborCount / dnetP -> nc);
    printf ("non grid average neighbor count: %f\n", (float) nonGridNeighborCount / dnetP -> nc);
    printf ("minNeighborCount  %d\n", minNeighborCount);
    printf ("maxNeighborCount  %d\n", maxNeighborCount);
  }

  return status;
}

void freeDnet (dnet * dnetP) {
  int node;

  if (! dnetP)
    return;

  for (node = 0; node < dnetP -> nc; node ++) {
    free (dnetP -> nv [node] .nbv);
    free (dnetP -> nv [node] .nbcv);
    free (dnetP -> nv [node] .ncnd);
  }
  free (dnetP -> nv);
  free (dnetP);
}

int writeDnet (char * path, dnet * dnetP) {
  FILE * f;
  int node, neighbor;

  if (! (f = fopen (path, "wb"))) return DNET_BAD_PATH;

  if (fwrite (& dnetP -> nc, sizeof (int), 1, f) != 1 ||
      fwrite (& dnetP -> width, sizeof (int), 1, f) != 1 ||
      fwrite (& dnetP -> height, sizeof (int), 1, f) != 1) {
    fclose (f);
    return DNET_WRITE_FAILED;
  }

  for (node = 0; node < dnetP -> nc; node ++) {
    if (fwrite (& dnetP -> nv [node] . bestTime, sizeof (double), 1, f) != 1 ||
	fwrite (& dnetP -> nv [node] . alreadyWet, sizeof (char), 1, f) != 1 ||
	fwrite (& dnetP -> nv [node] . withinBorder, sizeof (char), 1, f) != 1 ||
	fwrite (& dnetP -> nv [node] . flowFrom, sizeof (int), 1, f) != 1 ||
	fwrite (& dnetP -> nv [node] . gpIndex, sizeof (int), 1, f) != 1 ||
	fwrite (& dnetP -> nv [node] . nbc, sizeof (int), 1, f) != 1) {
      fclose (f);
      return DNET_WRITE_FAILED;
    }
    
    for (neighbor = 0; neighbor < dnetP -> nv [node] . nbc; neighbor ++) {
      if (fwrite (& dnetP -> nv [node] . nbv [neighbor], sizeof (int), 1, f) != 1 ||
	  fwrite (& dnetP -> nv [node] . nbcv [neighbor], sizeof (double), 1, f) != 1) {
	fclose (f);
	return DNET_WRITE_FAILED;
      }
    }
  }

  fclose (f);
  return DNET_HAPPY;
}

static void lineSample2latLon(double *lat, double *lon, int line, int sample,
			      int lines, int samples,
			      double nwlat, double selat, double nwlon, double selon) {
  double proportion;

  proportion = (double) line/(double) lines;
  *lat = nwlat + proportion * (selat - nwlat);

  proportion = (double) sample/(double) samples;
  *lon = nwlon + proportion * (selon - nwlon);
}

void writeDnetAscii (char * filename, dnet * dnetP, double nwlat, double selat, double nwlon, double selon) {
  FILE * f;
  int node, neighbor;
  char path[512];
  int firstRoadIndex = dnetP -> width * dnetP -> height;
  sprintf(path, "%s/%s", getenv("TMA_DATA"), filename);

  if (! (f = fopen (path, "w"))) {
    fprintf(stderr, "writeDnetAscii: bad path: %s\n", path);
    return;
  }

  fprintf(f, "nc %d\nwidth %d\nheight%d\ngrid cell count %d\n", dnetP -> nc, dnetP -> width, dnetP -> height, dnetP -> width * dnetP -> height);

  for (node = 0; node < dnetP -> nc; node ++) {
    fprintf(f, "node %d\n", node);
    if (node < firstRoadIndex) {
      double lat, lon;
      lineSample2latLon(&lat, &lon, IDX2LINE(node, dnetP), IDX2SAMPLE(node, dnetP),
			dnetP -> height, dnetP -> width,
			nwlat, selat, nwlon, selon);
      fprintf(f, "\tlat %lf lon %lf\n", lat, lon);
    }
    fprintf(f, "\tbestTime       %lf\n", dnetP -> nv [node] . bestTime);
    fprintf(f, "\talreadyWet     %d\n", dnetP -> nv [node] . alreadyWet);
    fprintf(f, "\twithinBorder   %d\n", dnetP -> nv [node] . withinBorder);
    fprintf(f, "\tflowFrom       %d\n", dnetP -> nv [node] . flowFrom);
    fprintf(f, "\tgpIndex        %d\n", dnetP -> nv [node] . gpIndex);
    fprintf(f, "\tneighbor count %d\n", dnetP -> nv [node] . nbc);
    
    for (neighbor = 0; neighbor < dnetP -> nv [node] . nbc; neighbor ++) {
      fprintf(f, "\tneighbor %d\n", neighbor);
      fprintf(f, "\t\tindex  %d\n", dnetP -> nv [node] . nbv [neighbor]);
      fprintf(f, "\t\tcost %lf\n", dnetP -> nv [node] . nbcv [neighbor]);
    }

    fprintf(f, "\n");
  }

  fclose (f);
}

/* returns 0 when happy */
int readDnet (char * path, dnet ** dnetPP) {
  dnet * dnetP;
  FILE * f;
  int node, neighbor;

  if (! (dnetP = * dnetPP = (dnet *) malloc (sizeof (dnet)))) return DNET_MALLOC_FAILED;

  if (! (f = fopen (path, "rb"))) return DNET_BAD_PATH;

  if (fread (& dnetP -> nc, sizeof (int), 1, f) != 1 ||
      fread (& dnetP -> width, sizeof (int), 1, f) != 1 ||
      fread (& dnetP -> height, sizeof (int), 1, f) != 1) {
    fclose (f);
    return DNET_READ_FAILED;
  }

  if (! (dnetP -> nv = (dnetNode *) malloc (sizeof (dnetNode) * dnetP -> nc))) return DNET_MALLOC_FAILED;

  for (node = 0; node < dnetP -> nc; node ++) {
    if (fread (& dnetP -> nv [node] . bestTime, sizeof (double), 1, f) != 1 ||
	fread (& dnetP -> nv [node] . alreadyWet, sizeof (char), 1, f) != 1 ||
	fread (& dnetP -> nv [node] . withinBorder, sizeof (char), 1, f) != 1 ||
	fread (& dnetP -> nv [node] . flowFrom, sizeof (int), 1, f) != 1 ||
	fread (& dnetP -> nv [node] . gpIndex, sizeof (int), 1, f) != 1 ||
	fread (& dnetP -> nv [node] . nbc, sizeof (int), 1, f) != 1) {
      fclose (f);
      return DNET_READ_FAILED;
    }
    
    if (! (dnetP -> nv [node] . nbv = (int *) malloc (sizeof (int) * dnetP -> nv [node] . nbc))) return DNET_MALLOC_FAILED;
    if (! (dnetP -> nv [node] . nbcv = (double *) malloc (sizeof (double) * dnetP -> nv [node] . nbc))) return DNET_MALLOC_FAILED;

    for (neighbor = 0; neighbor < dnetP -> nv [node] . nbc; neighbor ++) {
      if (fread (& dnetP -> nv [node] . nbv [neighbor], sizeof (int), 1, f) != 1 ||
	  fread (& dnetP -> nv [node] . nbcv [neighbor], sizeof (double), 1, f) != 1) {
	fclose (f);
	return DNET_READ_FAILED;
      }
    }
  }

  fclose (f);
  return DNET_HAPPY;
}

/* returns 0 when happy */
int preferRoad (dnet * dnetP, int * dnetIndex) {
  dnetNode * nodeP;
  int neighbor, neighborIndex;

  /* this function queries terrain type of a grid cell node only */
  if (* dnetIndex < 0 || * dnetIndex >= dnetP -> width * dnetP -> height) return DNET_BAD_TERRAIN_QUERY;

  nodeP = & (dnetP -> nv [* dnetIndex]);

  /* if there is a road neighbor, use its index instead */
  for (neighbor = 0; neighbor < nodeP -> nbc; neighbor ++) {
    neighborIndex = nodeP -> nbv [neighbor];

    if (neighborIndex >= dnetP -> width * dnetP -> height) {
      * dnetIndex = neighborIndex;
      return DNET_HAPPY;
    }
  }

  return DNET_HAPPY;
}

/* returns 0 when happy */
int queryTerrainRoadsAndRivers (dnet * dnetP, int line, int sample, int * roadCount, int ** roadFeatureCodes, int * riverCount, int ** riverFeatureCodes) {
  static int * roadCodes = 0;
  static int * riverCodes = 0;

  int dnetIndex = sample + dnetP -> width * line;
  dnetNode * nodeP;
  int neighbor, river, neighborIndex;

  /* this function queries terrain type of a grid cell node only */
  if (dnetIndex < 0 || dnetIndex >= dnetP -> width * dnetP -> height) {
    fprintf (stderr, "dnetIndex (%d) is >= width (%d) * height (%d)\n", dnetIndex, dnetP -> width, dnetP -> height);
    return DNET_BAD_TERRAIN_QUERY;
  }

  nodeP = dnetP -> nv + dnetIndex;

  free (roadCodes);
  free (riverCodes);
  roadCodes = 0;
  riverCodes = 0;
  * roadCount = 0;
  * riverCount = 0;

  for (neighbor = 0; neighbor < nodeP -> nbc; neighbor ++) {
    neighborIndex = nodeP -> nbv [neighbor];

    /* rivers can separate grid cell centers from other grid cell centers, as well as from roads */
    for (river = 0; river < nodeP -> ncnd [neighbor] . riverCount; river ++) {
      (* riverCount) ++;
      
      if (! (riverCodes = (int *) realloc (riverCodes, sizeof (int) * * riverCount))) {
	fprintf (stderr, "realloc failed in queryTerrainRoadsAndRivers for %d rivers\n", *riverCount);
	return DNET_MALLOC_FAILED;
      }
      riverCodes [(* riverCount) - 1] = nodeP -> ncnd [neighbor] . riverCodes [river];
    }

    if (neighborIndex >= dnetP -> width * dnetP -> height) { /* neighbor is a road point; collect road too */
      (* roadCount) ++;

      if (! (roadCodes = (int *) realloc (roadCodes, sizeof (int) * * roadCount))) {
	fprintf (stderr, "realloc failed in queryTerrainRoadsAndRivers for %d roads\n", *roadCount);
	return DNET_MALLOC_FAILED;
      }
      roadCodes [(* roadCount) - 1] = nodeP -> ncnd [neighbor] . roadType;
    }
  }

  * roadFeatureCodes = roadCodes;
  * riverFeatureCodes = riverCodes;

  return DNET_HAPPY;
}

int roadPathToNeighbor (dnet * dnetP, int thisDnetIndex, int neighborDnetIndex, int * roadPathFeatureCode) {
  int neighbor;

  if (thisDnetIndex < 0 || thisDnetIndex >= dnetP -> width * dnetP -> height ||
      neighborDnetIndex < 0 || neighborDnetIndex >= dnetP -> width * dnetP -> height)
    return 0;

  /* find the data for this neighbor */
  for (neighbor = 0; neighbor < dnetP -> nv [thisDnetIndex] . nbc; neighbor ++)
    if (dnetP -> nv [thisDnetIndex] . nbv [neighbor] == neighborDnetIndex) { /* found it */
      * roadPathFeatureCode = dnetP -> nv [thisDnetIndex] . ncnd [neighbor] . roadType;
      return 1;
    }
  
  fprintf (stderr, "roadPathToNeighbor asked about non neighboring dnet nodes %d and %d\n", thisDnetIndex, neighborDnetIndex);

  return 0;
}
