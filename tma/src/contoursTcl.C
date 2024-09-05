#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <fstream>
#include <tcl.h>
#include <tk.h>
#include <vector>

#include "util.h"
#include "tma.h"
#include "contours.h"
#include "contoursTcl.h"

// This file contains routines for generating isochronal contour lines.

//	The tcl command procedure is tma_contours.
//	The tcl command is tma_ContoursCmd.

//
//
// Tcl command procedure.  Returns path in lat/long string format.
//
// Returns path in lat/long string format.
//
int tcl_contours(ClientData clientData, Tcl_Interp *interp,
					int argc, const char *argv[]) {
  if (argc != 11) {
    cerr << "Usage: " << argv[0];
//     cerr << " regionName nw se lines samples moverName start initialTime interTime numIntervals rnfName dataSet aoiId" << endl;
    cerr << " regionName nw se lines samples moverName start rnfName dataSet aoiId" << endl;
    exit(0);
  }

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

//   double firstIntervalTime = atof(argv[8]);
//   double nextIntervalTime = atof(argv[9]);
//   int numContours = atoi(argv[10]);
  initTcl(); 
  tmaContours(argv[1], nw, se, lines, samples,
              argv[6], start,
//               firstIntervalTime, nextIntervalTime, numContours,
// 	      argv[11], argv[12], argv [13]);
	      argv[8], argv[9], argv [10]);

  Tcl_ResetResult(interp);
  return TCL_OK;
}

int tcl_corridor (ClientData clientData, Tcl_Interp *interp,
					int argc, const char *argv[]) {
  if (argc != 12) {
    cerr << "Usage: " << argv[0];
//     cerr << " regionName nw se lines samples moverName start end maxTime rnfName dataSet aoiId" << endl;
    cerr << " regionName nw se lines samples moverName start end rnfName dataSet aoiId" << endl;
    exit(0);
  }

  double lat, lon;

  sscanf(argv[2], "%lf %lf", &lat, &lon);
  SbVec2f nw(lat, lon);

  sscanf(argv[3], "%lf %lf", &lat, &lon);
  SbVec2f se(lat, lon);
  int lines = atoi(argv[4]);
  int samples = atoi(argv[5]);

  sscanf(argv[7], "%lf %lf", &lat, &lon);
  SbVec2f start(lat, lon);

  sscanf(argv[8], "%lf %lf", &lat, &lon);
  SbVec2f end(lat, lon);

//   double maxTime = atof(argv[9]);

  initTcl(); 
  tmaCorridor(argv[1], nw, se, lines, samples,
              argv[6], start, end,
//               maxTime, argv[10], argv[11], argv[12]);
              argv[9], argv[10], argv[11]);

  Tcl_ResetResult(interp);
  return TCL_OK;
}

int tcl_multiSourceContours(ClientData clientData, Tcl_Interp *interp,
					int argc, const char *argv[]) {
  if (argc != 11) {
    cerr << "Usage: " << argv[0];
//     cerr << " regionName nw se lines samples moverName locations initialTime interTime numIntervals rnfName dataSet aoiId" << endl;
    cerr << " regionName nw se lines samples moverName locations rnfName dataSet aoiId" << endl;
    exit(0);
  }

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

  vector <SbVec2f> locations;
  const char * p = argv[7];
  cerr << "Locations arg: " << p << endl;
  while (1) {
    if (sscanf(p, "{%lf %lf}", &lat, &lon) != 2) break;
    cerr << "Pushing " << lat << " " << lon << " on locations vector\n";
    locations.push_back (SbVec2f (lat, lon));
    p = strchr (p, ' ');
    if (!p) break;
    cerr << "Locations after fwding a space: " << p << endl;
    p ++;
    p = strchr (p, ' ');
    if (!p) break;
    cerr << "Locations after fwding a space: " << p << endl;
    p ++;
  }

//   double firstIntervalTime = atof(argv[8]);
//   double nextIntervalTime = atof(argv[9]);
//   int numContours = atoi(argv[10]);
  initTcl(); 
  tmaMultiSourceContours(argv[1], nw, se, lines, samples,
			 argv[6], locations,
// 			 firstIntervalTime, nextIntervalTime, numContours,
			 argv[8], argv[9], argv [10]);

  Tcl_ResetResult(interp);
  return TCL_OK;
}

