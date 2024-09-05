#ifndef _util_h_
#define _util_h_

#include "tma.h"
#include "subregion.h"
#include <tcl.h>
#include <tk.h>

void latLon2str(char *latLonStr, double lat, double lon);
double fceil(double x);
double ftrunc(double x);
void ls2GpointArray(int npoints, lspoint *ls, Gpoint *G, SubRegion &aoi);
unsigned short **intMatrix(int lines, int samples);
void freeIntMatrix(unsigned short **m, int lines);
double **doubleMatrix(int lines, int samples);
void freeDoubleMatrix(double **m, int lines);
unsigned char **ucharMatrix(int lines, int samples);
void freeUcharMatrix(unsigned char **m, int lines);
void str2latLon(double *lat, double *lon, char *latLonStr);
int queryTerrainRoadsAndRiversWrapper(int line, int sample,
				      int * roadCount, int ** roadFeatureCodes,
				      int * riverCount,
				      int ** riverFeatureCodes);
int roadPathToNeighborWrapper(int thisDnetIndex, int neighborDnetIndex,
			      int * roadPathFeatureCode);
void initTcl();
Tcl_Interp *getInterp();
void checkTclShell (Tcl_Interp *interp, int expression, char * message);

#endif
