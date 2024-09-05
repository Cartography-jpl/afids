#ifndef _tma_h_
#define _tma_h_

#include "region.h"
#include "subregion.h"
#include "mover.h"
#include "sblinear.h"
#include "rnf.h"
#include "dnet.h"

#define SUCCESS 0
#define FAILURE -1
#define TRUE 1
#define FALSE 0
//#define MALLOC_ERROR -10
//#define FILE_ERROR -20
#define NO_PATH -30
//#define SSB_ERROR -50
//#define COORD_ERROR -60
//#define CORRELATION 0
//#define NO_CORRELATION 1

#define GRIDSIZE		128
//#define FLOW_SOURCE		-1
#define LOG			1

#define TOO_HUGE 1.e15
#define EPSILON   1.e-15

#define OUTPUT_DATA 1

#define FROM 0
#define TO 1

/*
##############################################################################
###############################   DATA TYPES   ###############################
##############################################################################
*/

typedef struct {
    double lat;
    double lon;
} Gpoint;

typedef struct {
    int line;
    int sample;
} lspoint;

/* Contour data.  */                                                   
typedef struct {                                                       
    int level;                  /* stores the contour level  */        
    int npoints;                /* stores the length of the line  */   
    lspoint *coords;		/* stores the coordinates  */          
} graphics_struct;

typedef struct {
  int k;  // k < 0 means roadId and segmentId is meaningful.
  int roadId;
  int segmentId;
} flowInfo;

/*
##############################################################################
###############################  GLOBAL  DATA  ###############################
##############################################################################
*/

#ifdef MAIN
int tma_ldir[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
int tma_sdir[8] = {-1, 0, 1, -1, 1, -1, 0, 1};
#else
extern int tma_ldir[8];
extern int tma_sdir[8];
#endif

#ifdef MAIN
double **minTimeSoFar = 0;
flowInfo **directionFlowIsFrom = 0;
#else
extern double **minTimeSoFar;
extern flowInfo **directionFlowIsFrom;
#endif


/*
##############################################################################
###############################   PROTOTYPES   ###############################
##############################################################################
*/

double **tmaComputeTerrainTimes(SubRegion &aoi, Mover &mover);

void tmaMinPath(char *regionName, SbVec2f &nw, SbVec2f &se, int lines, int samples,
                char *moverName,  SbVec2f &start, SbVec2f &end, char *rnfName, char *dataSet, char *aoiId);

void tmaContours(char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
                 char *moverName,  SbVec2f &start,
                 double firstIntervalTime, double nextIntervalTime,
                 int numContours, char *rnfName, char *dataSet, char *aoiId);

int dijkstra(int startLine, int startSample,
             int endLine,   int endSample,   int endPointSet,
             dnet *dijkstraNet, SubRegion &aoi);

int getOneContour(lspoint *contour, double passedContourTime,
	int start_y, int start_x, int _numLines, int _numSamples);

SubRegion &setup(int dir, char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
		 char *moverName, SbVec2f &start, SbVec2f &end, int endSet,
		 dnet **addrDijkstraNet, char *rnfName, char *dataSet, char *aoiId);

#endif
