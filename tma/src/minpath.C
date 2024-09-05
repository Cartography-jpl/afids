#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <fstream>
#include "util.h"
#include "tma.h"
#include "element.h"
#include "dnet.h"
#include "minpath.h"
#include "realwork.h"

//////////////////////////////////////////////////////////////////////////
//
//   Trace the path from the endpoint (line, samp) to the  origin.
//   The total path length in meters is stored in "length,"
//   and the path itself, in (line, sample) points, is stored in "path."
//
int tmaTracePath(int endLine, int endSample, double *length, lspoint *path,
                 SubRegion &_aoi, const char *regionName, dnet *dijkstraNet) {

  dnetNode *net = dijkstraNet->nv;
  int node;

  FILE *logFilePtr;
  char filename[512];
  sprintf(filename, "%s/logFile", getenv("TMA_DATA"));

  int lines = _aoi.getLines();
  int samples = _aoi.getSamples();
  int line, sample, /*dir,*/ count;
  double dist[8], meters;

  double metersPerLinePixel = _aoi.getMetersPerLinePixel();
  double metersPerSamplePixel = _aoi.getMetersPerSamplePixel();

  dist[0] = sqrt(metersPerLinePixel*metersPerLinePixel + 
                 metersPerSamplePixel*metersPerSamplePixel);
  dist[1] = dist[6] = metersPerLinePixel;
  dist[3] = dist[4] = metersPerSamplePixel;
  dist[2] = dist[5] = dist[7] = dist[0];
  double diag = dist[0];
  double side = dist[3];
  double up   = dist[1];

  path[0].sample = endSample;
  path[0].line   = endLine;
  node = endSample + endLine * samples;

  // if last leg of path was from road network to grid, back out cost of that last leg
  if (dijkstraNet -> nv [node].flowFrom >= lines * samples)
    node = dijkstraNet -> nv [node].flowFrom;

  //if (directionFlowIsFrom[line][sample].k == 99)
  //   return(NO_PATH);
  count = 1;
  meters = 0.;

  if (LOG) {
      logFilePtr = fopen(filename, "w");
      fprintf(logFilePtr, "TOP OF FILE\n");
      fclose(logFilePtr);
    }

  int lastNode = node;
  while ((node = net[node].flowFrom) > 0) {
    int lastSample = net[lastNode].gpIndex % samples;
    int lastLine = net[lastNode].gpIndex / samples;	
    
    sample = net[node].gpIndex % samples;
    line = net[node].gpIndex / samples;	

    // add distance from lastNode to this node
    //    for (int neighbor = 0; neighbor < net[node].nbc; neighbor ++)
    //      if (net[node].nbv[neighbor] == lastNode) {
    //	meters += net[node].nbcv[neighbor];
    //	break;
    //      }

    // don't lengthen the path if the new node is in the same grid cell
    if (sample != lastSample || line != lastLine) {
      // set new point line, sample
      path[count].line = line;
      path[count].sample = sample;

      // add distance to new point
      if (path[count].line == path[count-1].line) {
	if (path[count].sample != path[count-1].sample) {
	 meters += side;
	}
      } else {
	if (path[count].sample == path[count-1].sample) {
	 meters += up;
	} else {
	 meters += diag;
	}
      }

      // increment point count
       count++;
      if (count > 9999) break;

      if (LOG)
	{
	  logFilePtr = fopen(filename, "a+");
	  fprintf(logFilePtr, "tmaTracePath:  line = %d, sample = %d\n",
		  line , sample);
	  fclose(logFilePtr);
	}
    }

    lastNode = node;
  }

  *length = meters;
  return(count);
}

/////////////////////////////////////////////////////////////////////////
//
// Perform the local mobility and signal propagation,
// then trace out the minimal path from end to start.
// The results are passed back in *path at (line, sample) coordinates,
// *length gets the path length,
// *time gets the travel time in seconds,
// and the return value is the length of the path.
//
// Writes path to file.
//

// keep the dnet around for terrain queries
dnet *minPathDijkstraNet = 0;

void tmaMinPath(const char *regionName, SbVec2f &nw, SbVec2f &se, int lines, int samples,
                const char *moverName,  SbVec2f &start, SbVec2f &end, const char *rnfName, const char *dataSet, const char *aoiId) {
lspoint *lsPath = new lspoint[10000];

//    FILE * tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog,
//  	   "tmaMinPath (region=%s, n=%f, w=%f, s=%f, e=%f, lines=%d, samples=%d, moverId=%s, startLat=%f, startLon=%f, endLat=%f, endLon=%f, rnf=%s, dataSet=%s)\n",
//  	   regionName, nw.lat(), nw.lon(), se.lat(), se.lon(), lines, samples, moverName, start.lat(), start.lon(), end.lat(), end.lon(), rnfName, dataSet);
//    fclose (tmaLog);

cerr << "In tmaMinPath\n";
cerr << "region = " << regionName << endl;
cerr << "mover = " << moverName << endl;
cerr << "nw = " << nw << endl;
cerr << "se = " << se << endl;
cerr << "start = " << start << endl;
cerr << "end = " << end << endl;


freeDnet(minPathDijkstraNet);

 SubRegion &aoi = setup(FROM, regionName, nw, se, lines, samples,
                       moverName, start, end, 1, &minPathDijkstraNet, rnfName, dataSet, aoiId);

//   tmaLog = fopen ("/tmp/tma.log", "a");
//   fprintf (tmaLog, "After setup\n");
//   fclose (tmaLog);

cerr << "After setup\n";

 int /*startLine, startSample,*/ endLine, endSample;
//aoi.latLon2lineSample(&startLine, &startSample, start.lat(), start.lon());
aoi.latLon2lineSample(&endLine, &endSample, end.lat(), end.lon());

double minLength;
//int samples = aoi.getSamples();
double minTime = minPathDijkstraNet->nv[endSample + endLine * samples].bestTime;
// minTimeSoFar[endLine][endSample];
//dijkstraNet
int numPoints = tmaTracePath(endLine, endSample, &minLength, lsPath, aoi,
                             regionName, minPathDijkstraNet);

//writeDnet("/tmp/dnet.tma", minPathDijkstraNet);

//freeDoubleMatrix(minTimeSoFar, aoi.getLines());
//int line;
//for (line=0; line<aoi.getLines(); line++)
//   delete[] directionFlowIsFrom[line];
//delete[] directionFlowIsFrom;

// Output path to file.

cerr << "After tmaTracePath.\nNum points = " << numPoints << endl;
cerr << "minLength = " << minLength << ", minTime = " << minTime << endl;

// Convert (line, sample) path to (lat, lon) using aoi.
Gpoint *gPath = new Gpoint[numPoints];
ls2GpointArray(numPoints, lsPath, gPath, aoi);


 // Low pass filter the gPath array.
 int i, j;
 Gpoint *filterOutput = new Gpoint[numPoints];
 double filterLat, filterLon;
 int filterWidth = 3;
 int numFilterInputs;
 for (i=1; i<numPoints-1; i++) {
   filterLat = 0.0;
   filterLon = 0.0;
   numFilterInputs = 0;
   for (j=-filterWidth; j<=filterWidth; j++) {
     if (((i+j) >= 0) && ((i+j) < numPoints)) {
       numFilterInputs++;
       filterLat += gPath[i+j].lat;
       filterLon += gPath[i+j].lon;
     }
   }
   filterOutput[i].lat = filterLat / (double) numFilterInputs;
   filterOutput[i].lon = filterLon / (double) numFilterInputs;
 }
 for (i=1; i<numPoints-1; i++) {
   gPath[i].lat = filterOutput[i].lat;
   gPath[i].lon = filterOutput[i].lon;
 }
 delete[] filterOutput;
 
// Put minimum path in file as result.
char filename[512];
sprintf(filename, "%s/minPath.dat", getenv("TMA_DATA"));
 ofstream pathFile(filename, ios::out /*, filebuf::openprot*/);
pathFile << minTime << endl;
pathFile << minLength << endl;
pathFile << numPoints << endl;
char latLonStr[255];
for (i=0; i<numPoints; i++)
   {
   // Convert lat/long minPath to lat/Long strings to pass back.
   latLon2str(latLonStr, gPath[i].lat, gPath[i].lon);
   pathFile << latLonStr << endl;
   }
pathFile.close();

// Also put minimum path in file as result in lat/lon numeric format.
 sprintf(filename, "%s/minPathN.dat", getenv("TMA_DATA"));
 FILE *outFile = fopen(filename, "w");
 fprintf(outFile, "%lf\n%lf\n%d\n", minTime, minLength, numPoints); 
 for (i=0; i<numPoints; i++) {
   fprintf(outFile, "%lf %lf\n", gPath[i].lat, gPath[i].lon);
 }
 fclose(outFile);

// Also put minimum path in file as result as dnet node indices.
 sprintf(filename, "%s/minPathI.dat", getenv("TMA_DATA"));
 outFile = fopen(filename, "w");
 fprintf(outFile, "%lf\n%lf\n%d\n", minTime, minLength, numPoints);
 int dnetNodeIndex =
   minPathDijkstraNet->nv[endSample + endLine * samples].gpIndex;
 double linkCost;
 int flowFromNode;
 for (i=0; i<numPoints; i++) {
   flowFromNode = minPathDijkstraNet->nv[dnetNodeIndex].flowFrom;

   if (flowFromNode < 0)
     break; // should this happen?

   linkCost = minPathDijkstraNet->nv[dnetNodeIndex].bestTime -
     minPathDijkstraNet->nv[flowFromNode].bestTime;
   fprintf(outFile, "%d %d %f\n", dnetNodeIndex, flowFromNode, linkCost);
   dnetNodeIndex = flowFromNode;
 }
 fclose(outFile);

// Also put minimum path in file as result showing everything
 sprintf(filename, "%s/minPathE.dat", getenv("TMA_DATA"));
 outFile = fopen(filename, "w");
 fprintf(outFile, "%lf\n%lf\n%d\n", minTime, minLength, numPoints);
 dnetNodeIndex =
   minPathDijkstraNet->nv[endSample + endLine * samples].gpIndex;
 for (i=0; i<numPoints; i++) {
   flowFromNode = minPathDijkstraNet->nv[dnetNodeIndex].flowFrom;

   if (flowFromNode < 0)
     break; // should this happen?

   linkCost = minPathDijkstraNet->nv[dnetNodeIndex].bestTime -
     minPathDijkstraNet->nv[flowFromNode].bestTime;
   latLon2str(latLonStr, gPath[i].lat, gPath[i].lon);
   fprintf(outFile, "%s", latLonStr);
   fprintf(outFile, " \t(%lf %lf)", gPath[i].lat, gPath[i].lon);
   fprintf(outFile, " node %d fromNode %d cost %f\n", dnetNodeIndex, flowFromNode, linkCost);
   dnetNodeIndex = flowFromNode;
 }
 fclose(outFile);

// Clean up
delete &aoi;
delete[] lsPath;
delete[] gPath;
}


