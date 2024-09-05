// Indicate we are using large files (64 bit) on linux.
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>

#include "vif.h"

extern "C" {
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <tcl.h>
}

static int vifClientProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vifClient ip port file\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 4 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * ip = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  char * port = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );
  char * file = strdup( Tcl_GetStringFromObj( objv [3], 0 ) );

  if ( ! vifNotifyServer( ip, port, file ) )
    tclStatus = TCL_OK;

  free( ip );
  free( port );
  free( file );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "vifNotifyServer failed", -1 ) );

  return tclStatus;
}

static int vifServerProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"vifServer clientPort senderPort\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 3 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * client = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );
  char * server = strdup( Tcl_GetStringFromObj( objv [2], 0 ) );

  if ( ! vifStartServer( client, server ) )
    tclStatus = TCL_OK;

  free( client );
  free( server );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "vifStartServer failed", -1 ) );

  return tclStatus;
}

static int mkfifoProc( ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv [] ) {
  const char * usage = "wrong # args: should be \"mkfifo path\"";
  int tclStatus = TCL_ERROR;

  if ( objc != 2 ) {
    Tcl_SetResult( interp, const_cast<char*>( usage ), TCL_VOLATILE );
    return TCL_ERROR;
  }

  char * inPath = strdup( Tcl_GetStringFromObj( objv [1], 0 ) );

  if ( ! mkfifo( inPath, 0644 ) )
    tclStatus = TCL_OK;

  free( inPath );

  if ( tclStatus != TCL_OK )
    Tcl_SetObjResult( interp, Tcl_NewStringObj( "mkfifo failed", -1 ) );

  return tclStatus;
}

extern "C" int Notifytcl_Init( Tcl_Interp *interp ) {
  Tcl_CreateObjCommand( interp, "mkfifo", mkfifoProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vifClient", vifClientProc, 0, 0 );
  Tcl_CreateObjCommand( interp, "vifServer", vifServerProc, 0, 0 );

  return TCL_OK;
}
