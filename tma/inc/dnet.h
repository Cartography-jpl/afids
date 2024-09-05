#ifndef _dnet_h_
#define _dnet_h_

#include "rnf.h"

#ifdef __cplusplus
extern "C" {
#endif

#define IDX2SAMPLE(idx, dnetP) ((idx) % ((dnetP) -> width))
#define IDX2LINE(idx, dnetP) ((idx) / ((dnetP) -> width))

typedef struct {
  /* what type of road, if any, is crossed to travel to this neighbor? */
  /* what types of river, if any, are crossed to travel to this neighbor? */

  /* if a node's index is < gridLines * gridSamples, then it is a grid node, otherwise it is a road node */

  /* if a pair of neighboring dnet nodes are both road nodes,
     then the roadType is the feature code of the road segment connecting them */
  /* if a pair of neighboring dnet nodes are both grid cell centers,
     then the riverCount and riverCodes hold the feature codes of the rivers separating the grid cell centers */
  /* otherwise, one neighbor is a road node and the other is a grid cell center,
     then the roadType is the feature code of the road and the riverCount and riverCodes hold the feature codes 
     of the rivers separating the grid cell center and the road node */

  int roadType;
  int riverCount;
  int * riverCodes;
} nonCostNeighborData;

typedef struct {
  double bestTime;
  char alreadyWet;
  char withinBorder;
  int flowFrom;
  int gpIndex;			/* use macros above to get line, sample from this */

  int nbc;			/* neighbor count */
  int * nbv;			/* neighbor index vector */
  double * nbcv;		/* cost to each neighbor in nbv*/
  nonCostNeighborData * ncnd;
} dnetNode;

typedef struct {
  int nc;			/* node count */
  dnetNode * nv;		/* node vector */
  int width, height;		/* used by macros above */
} dnet;

/* DNET status return values */
#define DNET_HAPPY 0
#define DNET_BAD_PATH 1
#define DNET_WRITE_FAILED 2
#define DNET_READ_FAILED 3
#define DNET_MALLOC_FAILED 4
#define DNET_BAD_TERRAIN_QUERY 5

/* These return DNET status */
int buildDnet (double *** gridCosts, dRoadDescription * roadNetworkP, int roadCount, int width, int height, dnet ** dnetP, gridCell * gridP);
int checkDnet (dnet * dnetP, int verbose);

/* These do not yet preserve the non cost neighbor data */
int writeDnet (char * path, dnet * dnetP);
int readDnet (char * path, dnet ** dnetPP);

void freeDnet (dnet * dnetP);

/* Returns 1 and sets roadPathFeatureCode when thisDnetIndex and neighborDnetIndex are neighboring road derived dnet nodes */
/* Returns 0 otherwise */
int roadPathToNeighbor (dnet * dnetP, int thisDnetIndex, int neighborDnetIndex, int * roadPathFeatureCode);
/* Do not free the returned feature code lists */
int queryTerrainRoadsAndRivers (dnet * dnetP, int line, int sample, int * roadCount, int ** roadFeatureCodes, int * riverCount, int ** riverFeatureCodes);
/* Jumps from grid to road, if possible */
int preferRoad (dnet * dnetP, int * dnetIndex);

  void writeDnetAscii (char * filename, dnet * dnetP, double nwlat, double selat, double nwlon, double selon);

#ifdef __cplusplus
}
#endif

#endif
