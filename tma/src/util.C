#include <math.h>
#include <string.h>

#include "tma.h"
#include "util.h"
#include "minpath.h"
#include "dnet.h"

#include <tcl.h>

static Tcl_Interp *interp;

void initTcl() {
  interp = Tcl_CreateInterp();
  if (Tcl_Init(interp) == TCL_ERROR) {
    fprintf(stderr, "Could not perform Tcl_AppInit.\n");
    fprintf(stderr, "%s\n", interp->result);
    exit(1);
  }
  char cmd[128];
  strcpy(cmd, "package require dbtcl");
  fprintf(stderr, "%s\n", cmd);
  int code = Tcl_Eval(interp, cmd);
  if (*interp->result != 0) {
    fprintf(stderr, "%s\n", interp->result);
  } else {
    fprintf(stderr, "Tcl error.\n");
    exit(1);
  }
  strcpy(cmd, "package require helpers");  // genMover.tcl and findAOI.tcl
  fprintf(stderr, "%s\n", cmd);
  code = Tcl_Eval(interp, cmd);
  if (*interp->result != 0) {
    fprintf(stderr, "%s\n", interp->result);
  } else {
    fprintf(stderr, "Tcl error.\n");
    exit(1);
  }
}

Tcl_Interp *getInterp() {
  return interp;
}

void checkTclShell (Tcl_Interp *interp, int expression, char * message) {
  if (expression) {
    fprintf (stderr, message);
    if (* (interp -> result) != 0)
      fprintf (stderr, "%s\n", interp -> result);
    exit (1);
  }
}

//*************************************************************************
// My fceil.
//*************************************************************************
double fceil(double x)
{
return (double) ceil((double) x);
}

//*************************************************************************
// My ftrunc.
//*************************************************************************
double ftrunc(double x)
{
if (x < 0)
  return (double) ceil((double) x);
else
  return (double) floor((double) x);
}

//*************************************************************************
// Utility.
//*************************************************************************
void latLon2str(char *latLonStr, double lat, double lon)
{
int	latDegrees, latMinutes, latSeconds,
	lonDegrees, lonMinutes, lonSeconds;
double fractional;
char latDir, lonDir;

if (lat < 0)
  {
  latDir = 'S';
  lat = fabs(lat);
  }
else
  latDir = 'N';

latDegrees = (int) ftrunc(lat);
fractional = fabs(lat - latDegrees);
latMinutes = (int) ftrunc(60.*fractional);
fractional -= latMinutes/60.;
latSeconds = (int) ftrunc(3600.0*fractional);

if (lon < 0)
  {
  lonDir = 'W';
  lon = fabs(lon);
  }
else
  lonDir = 'E';

lonDegrees = (int) ftrunc(lon);
fractional = fabs(lon - lonDegrees);
lonMinutes = (int) ftrunc(60.*fractional);
fractional -= lonMinutes/60.;
lonSeconds = (int) ftrunc(3600.0*fractional);

sprintf(latLonStr, "%c%d %d' %d'' %c%d %d' %d''",
	latDir, latDegrees, latMinutes, latSeconds,
	lonDir, lonDegrees, lonMinutes, lonSeconds);
}

//*************************************************************************
// Utility.
//*************************************************************************
void str2latLon(double *lat, double *lon, char *latLonStr)
{
int	latDegrees, latMinutes, latSeconds,
	lonDegrees, lonMinutes, lonSeconds;
char latDir, lonDir;

sscanf(latLonStr, "%c%d %d' %d'' %c%d %d' %d''",
	&latDir, &latDegrees, &latMinutes, &latSeconds,
	&lonDir, &lonDegrees, &lonMinutes, &lonSeconds);

if (latDir == 'S')
  *lat = -1*(latDegrees + latMinutes/60. + latSeconds/3600.);
else
  *lat = latDegrees + latMinutes/60. + latSeconds/3600.;

if (lonDir == 'W')
  *lon = -1*(lonDegrees + lonMinutes/60. + lonSeconds/3600.);
else
  *lon = lonDegrees + lonMinutes/60. + lonSeconds/3600.;

cerr << "str2latLon : " << latLonStr << " " << *lat << " " << *lon << endl;
}

//*************************************************************************
//    Utility to convert lspoint array to Gpoint array using aoi raster spec.
//*************************************************************************
void ls2GpointArray(int npoints, lspoint *ls, Gpoint *G, SubRegion &aoi)
{
int i;

for (i=0; i<npoints; i++)
   aoi.lineSample2latLon(&(G[i].lat), &(G[i].lon),
			 ls[i].line, ls[i].sample);

}

//*************************************************************************
//	int matrix allocation.
//*************************************************************************
unsigned short **intMatrix(int lines, int samples)
{
int i;
unsigned short **m;

/* Allocate pointers to rows. */
m = new unsigned short*[lines];
/* Allocate lines and set pointers to them. */
for (i=0; i<lines; i++) {
   m[i] = new unsigned short[samples];
   }
return m;
}

//*************************************************************************
//	Free int matrix.
//*************************************************************************
void freeIntMatrix(unsigned short **m, int lines)
{
if (m==0) return;
int i;
for (i=0; i<lines; i++)
   delete[] m[i];
delete[] m;
m=0;
}

//*************************************************************************
//	double matrix allocation.
//*************************************************************************
double **doubleMatrix(int lines, int samples)
{
int i;
double **m;

/* Allocate pointers to rows. */
m = new double*[lines];

/* Allocate lines and set pointers to them. */
for (i=0; i<lines; i++)
   m[i] = new double[samples];

return m;
}

//*************************************************************************
//	Free double matrix.
//*************************************************************************
void freeDoubleMatrix(double **m, int lines)
{
if (m==0) return;
int i;
for (i=0; i<lines; i++)
   delete[] m[i];
delete[] m;
m = 0;
}

//*************************************************************************
//	Matrix allocation.
//*************************************************************************
unsigned char **ucharMatrix(int lines, int samples)
{
int i;
unsigned char **m;

/* Allocate pointers to rows. */
m = new unsigned char*[lines];

/* Allocate lines and set pointers to them. */
for (i=0; i<lines; i++)
   m[i] = new unsigned char[samples];

return m;
}

//*************************************************************************
//	Free uchar matrix.
//*************************************************************************
void freeUcharMatrix(unsigned char **m, int lines)
{
if (m==0) return;
int i;
for (i=0; i<lines; i++)
   delete[] m[i];
delete m;
m = 0;
}

//*************************************************************************
//	Wrapper for queryTerrainRoadsAndRivers.
//*************************************************************************

extern "C" {
int queryTerrainRoadsAndRivers (dnet * dnetP, int line, int sample, int * roadCount, int ** roadFeatureCodes, int * riverCount, int ** riverFeatureCodes);
}


int queryTerrainRoadsAndRiversWrapper(int line, int sample,
				      int * roadCount, int ** roadFeatureCodes,
				      int * riverCount,
				      int ** riverFeatureCodes) {
  if (minPathDijkstraNet != NULL) {
    cerr << "----- 1";
    cerr << line << endl;
    cerr << sample << endl;
    return queryTerrainRoadsAndRivers(minPathDijkstraNet, line, sample,
				      roadCount, roadFeatureCodes,
				      riverCount, riverFeatureCodes);
  }
  else {
    cerr << "----- 2";
    return DNET_BAD_TERRAIN_QUERY;
  }
}
//*************************************************************************
//	Wrapper for roadPathToNeighbor.
//*************************************************************************
int roadPathToNeighborWrapper(int thisDnetIndex, int neighborDnetIndex,
			      int * roadPathFeatureCode) {
  if (minPathDijkstraNet != NULL) {
    return roadPathToNeighbor(minPathDijkstraNet,
			      thisDnetIndex, neighborDnetIndex,
			      roadPathFeatureCode);
  }
  else {
    cerr << "minPathDijkstraNet == NULL in roadPathToNeighborWrapper()\n";
    return 0;
  }
}
