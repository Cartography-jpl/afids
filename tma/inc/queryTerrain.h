#ifndef _queryTerrain_h_
#define _queryTerrain_h_

#include <tcl.h>

int getTerrainPixel(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);
int getElevationPixel(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);
int getRoadsAndRiversInCell(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);
int getRoadOnLink(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);

#endif
