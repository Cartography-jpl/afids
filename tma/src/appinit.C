#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>

#include <sys/types.h>

#define MAIN
#include "tma.h"
#include "minpathTcl.h"
#include "contoursTcl.h"
#include "queryTerrain.h"

extern "C" {

int Tmasrv_Init(Tcl_Interp *interp)
{
    Tcl_PkgProvide(interp, "TMASRV", "1.0");

    Tcl_CreateCommand(interp, "MinPathCmd", tcl_minPath,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "ContoursCmd", tcl_contours,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "MultiSourceContoursCmd", tcl_multiSourceContours,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "CorridorCmd", tcl_corridor,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    // Query terrain procs
    Tcl_CreateCommand(interp, "GetTerrainPixel", getTerrainPixel,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    
    Tcl_CreateCommand(interp, "GetElevationPixel", getElevationPixel,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "GetRoadsAndRiversInCell", getRoadsAndRiversInCell,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "GetRoadOnLink", getRoadOnLink,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}

}

//
// Tcl_AppInit.
//
int Tcl_AppInit(Tcl_Interp *interp)
{
    int status;

    status = Tcl_Init(interp);
    if (status != TCL_OK)
        return TCL_ERROR;

    status = Tk_Init(interp);
    if (status != TCL_OK)
        return TCL_ERROR;

    return TCL_OK;
}

//
// Main
//

int main(int argc, char *argv[])
{
    if (argc !=2) {
        fprintf(stderr, "Wrong number of arguments: ");
        fprintf(stderr, "should be: %s filename\n", argv[0]);
        exit(1);
    }

    Tk_Main(argc, argv, Tcl_AppInit);

    exit(0);
}





