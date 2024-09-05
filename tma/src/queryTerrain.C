#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>

#include "util.h"
#include "dnet.h"
#include "queryTerrain.h"

  int getTerrainPixel(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]) {
    char *buf = (char *) malloc (1000);
    /* verify arg count */
    if (argc != 5) {
      sprintf (buf, "wrong # args (%d): should be \"GetTerrainPixel file samples line sample\"", argc);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }

    FILE *filePtr;
    //printf("Opening %s.\n", argv[1]);
    if ( (filePtr = fopen(argv[1], "r")) == NULL ) {
      sprintf (buf, "Cannot open %s.\n", argv[1]);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }
    
    int d1, d2, d3;
    fscanf(filePtr, "P5\n%d %d\n%d", &d1, &d2, &d3);
    // Read the 1 white space character.
    fgetc(filePtr);
    long beginDataInFile = ftell(filePtr);
    
    int samples = atoi(argv[2]);
    int line    = atoi(argv[3]);
    int sample  = atoi(argv[4]);
    
    fseek(filePtr,
	  (long) sample +
	  ((long) samples * (long) line) +
	  beginDataInFile, 0);
    int pixel = fgetc(filePtr);
    sprintf(buf, "%d", pixel);
    fclose(filePtr);
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    free (buf);
    return TCL_OK;
  }
  //////////////////////////////////////////////////////////////////////////////////////////////////////   
  int getElevationPixel(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]) {
    char *buf = (char *) malloc (1000);
    /* verify arg count */
    if (argc != 5) {
      sprintf (buf, "wrong # args (%d): should be \"GetElevationPixel file samples line sample\"", argc);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }

    FILE *filePtr;
    if ( (filePtr = fopen(argv[1], "r")) == NULL ) {
      sprintf (buf, "Cannot open %s.\n", argv[1]);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }
    
    int samples = atoi(argv[2]);
    int line    = atoi(argv[3]);
    int sample  = atoi(argv[4]);
    
    fseek(filePtr,
	  (long) 2 * ((long) sample +
	  ((long) samples * (long) line)), 0);

    unsigned short pixel;
    fread(&pixel, sizeof(unsigned short), 1, filePtr);
    sprintf(buf, "%d", pixel);
    fclose(filePtr);
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    free (buf);
    return TCL_OK;
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////// 
  int getRoadsAndRiversInCell(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]) {
    char *buf = (char *) malloc (1000);
    /* verify arg count */
    if (argc != 3) {
      sprintf (buf, "wrong # args (%d): should be \"getRoadsAndRiversInCell line sample\"", argc);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }

    int line    = atoi(argv[1]);
    int sample  = atoi(argv[2]);
    int roadCount;
    int *roadFeatureCodes = 0;
    int riverCount;
    int *riverFeatureCodes = 0;
    if (queryTerrainRoadsAndRiversWrapper(line, sample,
					  &roadCount, &roadFeatureCodes,
					  &riverCount, &riverFeatureCodes) !=
	DNET_HAPPY) {
      fprintf(stderr, "roadCount = %d\n", roadCount);
      fprintf(stderr, "riverCount = %d\n", riverCount);
      sprintf (buf, "Run a minimum path calulation to preload data.");
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }

    int i;
    char buf2[80];
    sprintf(buf, "{");
    // Roads
    //strcat(buf, "{");
    fprintf(stderr, "road and river counts = %d %d\n", roadCount, riverCount);
    for (i=0; i<roadCount; i++) {
      sprintf(buf2, "%d ", roadFeatureCodes[i]);
      strcat(buf, buf2);
    }
    strcat(buf, "}");
    // Rivers
    strcat(buf, " {");
    for (i=0; i<riverCount; i++) {
      sprintf(buf2, "%d ", riverFeatureCodes[i]);
      strcat(buf, buf2);
    }
    strcat(buf, "}");
    //strcat(buf, "}");

    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    free (buf);
    return TCL_OK;
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////// 
  int getRoadOnLink(ClientData clientData, Tcl_Interp *interp, int argc, const char *argv[]) {
    char *buf = (char *) malloc (1000);
    /* verify arg count */
    if (argc != 3) {
      sprintf (buf, "wrong # args (%d): should be \"getRoadOnLink nodeIndex neighborNodeIndex\"", argc);
      Tcl_SetResult (interp, buf, TCL_VOLATILE);
      free (buf);
      return TCL_ERROR;
    }

    int nodeIndex         = atoi(argv[1]);
    int neighborNodeIndex = atoi(argv[2]);
    int roadPathFeatureCode;
    if (roadPathToNeighborWrapper(nodeIndex, neighborNodeIndex,
				  &roadPathFeatureCode) != 0) {
      sprintf(buf, "%d", roadPathFeatureCode);
    } else {
      sprintf(buf, "Not a road link");
    }
    Tcl_SetResult (interp, buf, TCL_VOLATILE);
    free (buf);
    return TCL_OK;
  }

