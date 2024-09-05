#ifndef _rnf_h_
#define _rnf_h_

#ifdef __cplusplus
extern "C" {
#endif

#ifndef TOO_HUGE
/* from TMASRV_tma.h */
#define TOO_HUGE 1.e15
#endif

/* uses row then column order */
#define IJ2IDX(i, j) ((i) + ((j) * width))
#define IDX2I(idx) ((idx) % width)
#define IDX2J(idx) ((idx) / width)

typedef struct {
  int gpIndex;			/* < 0 => this is an intersection */
  union {
    struct {
      double distToGP;
      int riverCodeCount;
      int * riverCodes;
    } roadPointDetails; /* not needed for intersections */
    struct {
      int otherRoadIndex;	/* index into roadNetwork */
      int otherRoadNode;	/* index into roadNodes */
      int gpIndex;		/* gp where the intersection occurs */
    } intersectionDetails; /* not needed for road points */
  } u;
  double distToNextNode;
  int dnetIndex;		/* used to build Dijkstra network */
} roadNode;

typedef struct {
  roadNode rn;

  /* local cost data */
  double costToNextNode;
  double costToGP;		/* valid only for rn.gpIndex >= 0 */
} dRoadNode;

typedef struct {
  roadNode rn;
  int oldSegIndex;
} sRoadNode;

typedef struct {
  int featureCode;
  int nodeCount;
  sRoadNode * sRoadNodes;
} roadDescription;

typedef struct {
  int featureCode;
  int nodeCount;
  dRoadNode * roadNodes;
} dRoadDescription;

typedef struct {
  int roadID;
  int nodeIndex;
} roadPoint;

typedef struct {
  int roadPointCount;
  roadPoint * roadPoints;
} roadPointList;

typedef struct {
  roadNode rn;
  int newSegID;			/* used to map old segid to new segid, left behind with old road */
  double x, y;			/* discarded when structural roads transcribed to new roads */
} auxNode;

typedef struct {
  double x, y;
  int auxNodeCount;
  auxNode * auxNodes;
} structuralNode;

typedef struct {
  int featureCode;
  int snodeCount;
  structuralNode * snodes;
} structuralRoad;

typedef struct {
  int roadID;
  int pointID;
} gridRoadPoint;

typedef struct {
  int riverCodeCount;
  int * riverCodes;
} gp2gpRiverCrossing;

typedef struct {
  int roadPointCount;
  gridRoadPoint * roadPoints;
  gp2gpRiverCrossing riverCodes [8];
} gridCell;

/* returns 0 when happy */
#define LOADRNF_HAPPY 0
#define LOADRNF_BAD_PATH 1
#define LOADRNF_MALLOC_FAILED 2
#define LOADRNF_READ_FAILED 3

int loadRNF (char * path, dRoadDescription ** roadNetworkP, int * roadCount, gridCell ** gridP, int * width, int * height);

/* returns 0 when happy */
int checkRNF (dRoadDescription * roadNetworkP, int roadCount, gridCell * gridP, int width, int height, int verbose);

void freeRNF (dRoadDescription * roadNetworkP, int roadCount, gridCell * gridP, int width, int height);

#ifdef __cplusplus
}
#endif

#endif
