// This file contains routines for generating isochronal contour lines.
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <fstream>
#include <vector>

#include "tma.h"
#include "util.h"
#include "dnet.h"
#include "contours.h"
#include "realwork.h"

//
// This routine computes the isochronal contours for *entity_code, starting
//    at *start, writing numints contint intervals into *contour. It returns
//    the number of contour lines written to *contour.
//
//
// Tcl command procedure.  Returns path in lat/long string format.
//
//
void tmaContours(const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
                 const char *moverName,  SbVec2f &start,
//                  double firstIntervalTime, double nextIntervalTime, int numContours,
		 const char *rnfName, const char *dataSet, const char *aoiId)
{
  //graphics_struct *contour;
  //Gpoint Gpath[2000];
  //char latLonStr[255];

SbVec2f end(0,0);

dnet *dijkstraNet;
 SubRegion &aoi = setup(FROM, regionName, nw, se, lines, samples,
                       moverName, start, end, 0, &dijkstraNet, rnfName, dataSet, aoiId);

 //int i, n;
int startLine, startSample;
aoi.latLon2lineSample(&startLine, &startSample,
                      start.lat(), start.lon());

// Create/fill minTimeSoFar matrix
//int lines = aoi.getLines();
//int samples = aoi.getSamples();
minTimeSoFar = doubleMatrix(lines, samples);
 for (int line = 0; line < lines; line ++)
   for (int sample = 0; sample < samples; sample ++)
     minTimeSoFar [line] [sample] = TOO_HUGE;

for (int node = 0; node < dijkstraNet -> nc; node ++) {
  int sample = dijkstraNet -> nv [node] . gpIndex % samples;
  int line = dijkstraNet -> nv [node] . gpIndex / samples;	
  
  if (dijkstraNet -> nv [node] . bestTime < minTimeSoFar [line] [sample])
    minTimeSoFar [line] [sample] = dijkstraNet -> nv [node] . bestTime;
}

// Output minTimeSoFar surface as lines (int) samples (int) and lines*samples doubles
 char filename[512];
 sprintf(filename, "%s/minTimeSoFar.dat", getenv("TMA_DATA"));
 int aoiLines = aoi.getLines();
 int aoiSamples = aoi.getSamples();
 FILE * minTimeDat = fopen (filename, "w");
 /*fwrite (& aoiLines, sizeof (int), 1, minTimeDat);
   fwrite (& aoiSamples, sizeof (int), 1, minTimeDat);*/
 for (int line = 0; line < aoiLines; line ++)
   fwrite (minTimeSoFar [line], sizeof (double), aoiSamples, minTimeDat);
 fclose (minTimeDat);

// Free.
freeDoubleMatrix(minTimeSoFar, lines);
freeDnet(dijkstraNet);

delete &aoi;
}

void tmaCorridor(const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
                 const char *moverName,  SbVec2f &start, SbVec2f &end,
//                  double maxTime,
		 const char *rnfName, const char *dataSet, const char * aoiId)
{
  //const int numContours = 1;

  //char latLonStr[255];

  dnet *dijkstraNetFROM, *dijkstraNetTO;

  SubRegion &aoiFROM = setup(FROM, regionName, nw, se, lines, samples,
			 moverName, start, end, 0, &dijkstraNetFROM, rnfName, dataSet, aoiId);
  SubRegion &aoiTO = setup(TO, regionName, nw, se, lines, samples,
			 moverName, end, start, 0, &dijkstraNetTO, rnfName, dataSet, aoiId);

  // Compute contours.

  //int i, n;

  // Compute FROM global costs into minTimeSoFarFROM
  double ** minTimeSoFarFROM = doubleMatrix(lines, samples);
  for (int line = 0; line < lines; line ++)
    for (int sample = 0; sample < samples; sample ++)
      minTimeSoFarFROM [line] [sample] = TOO_HUGE;

  for (int node = 0; node < dijkstraNetFROM -> nc; node ++) {
    int sample = dijkstraNetFROM -> nv [node] . gpIndex % samples;
    int line = dijkstraNetFROM -> nv [node] . gpIndex / samples;	
  
    if (dijkstraNetFROM -> nv [node] . bestTime < minTimeSoFarFROM [line] [sample])
      minTimeSoFarFROM [line] [sample] = dijkstraNetFROM -> nv [node] . bestTime;
  }

  // Compute TO global costs into minTimeSoFar
  minTimeSoFar = doubleMatrix(lines, samples);
  for (int line = 0; line < lines; line ++)
    for (int sample = 0; sample < samples; sample ++)
      minTimeSoFar [line] [sample] = TOO_HUGE;

  for (int node = 0; node < dijkstraNetTO -> nc; node ++) {
    int sample = dijkstraNetTO -> nv [node] . gpIndex % samples;
    int line = dijkstraNetTO -> nv [node] . gpIndex / samples;	
  
    if (dijkstraNetTO -> nv [node] . bestTime < minTimeSoFar [line] [sample])
      minTimeSoFar [line] [sample] = dijkstraNetTO -> nv [node] . bestTime;
  }

  // Add minTimeSoFarFROM to minTimeSoFar
  for (int line = 0; line < lines; line ++)
    for (int sample = 0; sample < samples; sample ++)
      minTimeSoFar [line] [sample] += minTimeSoFarFROM [line] [sample];

// Output minTimeSoFar surface as lines (int) samples (int) and lines*samples doubles
  char filename[512];
 sprintf(filename, "%s/minTimeSoFar.dat", getenv("TMA_DATA"));
 int aoiLines = aoiFROM.getLines();
 int aoiSamples = aoiFROM.getSamples();
 FILE * minTimeDat = fopen (filename, "w");
 /*fwrite (& aoiLines, sizeof (int), 1, minTimeDat);
   fwrite (& aoiSamples, sizeof (int), 1, minTimeDat);*/
 for (int line = 0; line < aoiLines; line ++)
   fwrite (minTimeSoFar [line], sizeof (double), aoiSamples, minTimeDat);
 fclose (minTimeDat);

  // Free.
  freeDoubleMatrix(minTimeSoFar, lines);
  freeDoubleMatrix(minTimeSoFarFROM, lines);
  freeDnet(dijkstraNetFROM);
  freeDnet(dijkstraNetTO);

  delete &aoiFROM;
  delete &aoiTO;
}

void tmaMultiSourceContours(const char *regionName, SbVec2f &nw, SbVec2f &se,
			    int lines, int samples,
			    const char *moverName,  vector <SbVec2f> & locations,
// 			    double firstIntervalTime, double nextIntervalTime, int numContours,
			    const char *rnfName, const char *dataSet, const char *aoiId) {

  //char latLonStr[255];

  SbVec2f end(0,0);

  cerr << "tmaMultiSourceContours given " << locations.size() << " locations\n";

  SbVec2f & start = locations[0];

  dnet *dijkstraNet;
  cerr << "Setting up first source at " << start.lat() << " " << start.lon() << endl;
  SubRegion &aoi = setup(FROM, regionName, nw, se, lines, samples,
			 moverName, start, end, 0, &dijkstraNet, rnfName, dataSet, aoiId);

  // Compute contours.

  //int i, n;
  int startLine, startSample;
  aoi.latLon2lineSample(&startLine, &startSample,
			start.lat(), start.lon());

  // Create/fill minTimeSoFar matrix
  //int lines = aoi.getLines();
  //int samples = aoi.getSamples();
  minTimeSoFar = doubleMatrix(lines, samples);
  for (int line = 0; line < lines; line ++)
    for (int sample = 0; sample < samples; sample ++)
      minTimeSoFar [line] [sample] = TOO_HUGE;

  for (int node = 0; node < dijkstraNet -> nc; node ++) {
    int sample = dijkstraNet -> nv [node] . gpIndex % samples;
    int line = dijkstraNet -> nv [node] . gpIndex / samples;	
  
    if (dijkstraNet -> nv [node] . bestTime < minTimeSoFar [line] [sample])
      minTimeSoFar [line] [sample] = dijkstraNet -> nv [node] . bestTime;
  }

  // Min other sources into minTimeSoFar
  for (unsigned int source = 1; source < locations.size(); source ++) {
    freeDnet(dijkstraNet);
    cerr << "Setting up source " << source << " at " << locations[source].lat() << " " << locations[source].lon() << endl;
    SubRegion &nextAoi = setup(FROM, regionName, nw, se, lines, samples,
			       moverName, locations[source], end, 0, &dijkstraNet, rnfName, dataSet, aoiId);

    for (int node = 0; node < dijkstraNet -> nc; node ++) {
      int sample = dijkstraNet -> nv [node] . gpIndex % samples;
      int line = dijkstraNet -> nv [node] . gpIndex / samples;	
  
      if (dijkstraNet -> nv [node] . bestTime < minTimeSoFar [line] [sample])
	minTimeSoFar [line] [sample] = dijkstraNet -> nv [node] . bestTime;
    }

    delete &nextAoi;
  }

// Output minTimeSoFar surface as lines (int) samples (int) and lines*samples doubles
  char filename[512];
 sprintf(filename, "%s/minTimeSoFar.dat", getenv("TMA_DATA"));
 int aoiLines = aoi.getLines();
 int aoiSamples = aoi.getSamples();
 FILE * minTimeDat = fopen (filename, "w");
 /*fwrite (& aoiLines, sizeof (int), 1, minTimeDat);
   fwrite (& aoiSamples, sizeof (int), 1, minTimeDat);*/
 for (int line = 0; line < aoiLines; line ++)
   fwrite (minTimeSoFar [line], sizeof (double), aoiSamples, minTimeDat);
 fclose (minTimeDat);

  // Free.
  freeDoubleMatrix(minTimeSoFar, lines);
  delete &aoi;
}
