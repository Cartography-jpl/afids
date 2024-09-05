#include <math.h>

#include <tcl.h>
#include <tk.h>

#include "tma.h"
#include "util.h"
#include "subregion.h"
#include "element.h"
#include "minpath.h"
#include "minpathTcl.h"

///////////////////////////////////////////////////////////////////////////
//
// Main.
// This does minimal path.
//
///////////////////////////////////////////////////////////////////////////

//
// Tcl command procedure.  Returns path in lat/long string format.
//
int tcl_minPath(ClientData clientData, Tcl_Interp *interp,
                int argc, const char *argv[]) {
//    FILE * tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "tcl_minPath (%s", argv [0]);
//    for (int i = 1; i < argc; i ++)
//      fprintf (tmaLog, ", %s", argv[i]);
//    fprintf (tmaLog, ")\n");
//    fclose (tmaLog);

  if (argc != 12) {
    cerr << "Usage: " << argv[0];
    cerr << " regionName nw se lines samples moverName start end rnfName dataSet aoiId" << endl;
    exit(0);
  }
  int i;
  for (i=0; i<11; i++)
    cerr << i << " " << argv[i] << endl;

  double lat, lon;
  //str2latLon(&lat, &lon, argv[2]);
  sscanf(argv[2], "%lf %lf", &lat, &lon);
  SbVec2f nw(lat, lon);
  //str2latLon(&lat, &lon, argv[3]);
  sscanf(argv[3], "%lf %lf", &lat, &lon);
  SbVec2f se(lat, lon);
  int lines = atoi(argv[4]);
  int samples = atoi(argv[5]);
  //str2latLon(&lat, &lon, argv[7]);
  sscanf(argv[7], "%lf %lf", &lat, &lon);
  SbVec2f start(lat, lon); 
  //str2latLon(&lat, &lon, argv[8]);
  sscanf(argv[8], "%lf %lf", &lat, &lon);
  SbVec2f end(lat, lon);
  cerr << nw << " " << se << " " << start << " " << end << endl;
  initTcl(); 

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "tcl_minPath calling tmaMinPath...\n");
//    fclose (tmaLog);

  tmaMinPath(argv[1], nw, se, lines, samples,
             argv[6], start, end, argv[9], argv[10], argv[11]);

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "tmaMinPath returned to tcl_minPath\n");
//    fclose (tmaLog);

  Tcl_ResetResult(interp);
  return TCL_OK;
}

