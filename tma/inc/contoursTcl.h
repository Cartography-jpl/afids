#ifndef _contoursTcl_h_
#define _contoursTcl_h_

#include <tcl.h>

int tcl_contours(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);
int tcl_multiSourceContours(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);
int tcl_corridor (ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]);

#endif
