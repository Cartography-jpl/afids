#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <fstream>
#include "util.h"
#include "tma.h"
#include "element.h"
#include "region.h"
#include "subregion.h"
#include "distanceBetween2Points.h"
#include "rnf.h"
#include "dnet.h"

#include "tcl.h"
#include "realwork.h"
#include "concave.h"

//*************************************************************************
//	Compute local costs within road network..
//
void computeLocalRoadNetworkCosts(SubRegion &aoi, Mover &mover,
                                  dRoadDescription *roadNetworkP, int roadCount) {
  int road, segment;
  double lat0 = aoi.getNw().lat();
  double lon0 = aoi.getNw().lon();
  double d, degrees, lat1, speed;
  int r, riverCode;
  for (road=0; road<roadCount; road++)
    for (segment=0; segment<roadNetworkP[road].nodeCount; segment++) {
       speed = mover.getValue(roadNetworkP[road].featureCode);
       degrees = roadNetworkP[road].roadNodes[segment].rn.distToNextNode;
       lat1 = lat0 + degrees;
       d = distanceBetween2Points(lat0, lon0, lat1, lon0);  // units are km.
       roadNetworkP[road].roadNodes[segment].costToNextNode = 60.0 * (d / speed);  // minutes.

       if (roadNetworkP[road].roadNodes[segment].rn.gpIndex >= 0) {
	 // should speed be updated here to be the grid cell terrain speed (rather than road speed)?
	 degrees = roadNetworkP[road].roadNodes[segment].rn.u.roadPointDetails.distToGP;
	 lat1 = lat0 + degrees;
	 d = distanceBetween2Points(lat0, lon0, lat1, lon0);  // units are km.
	 roadNetworkP[road].roadNodes[segment].costToGP = 60.0 * (d / speed); // minutes

	 for (r=0; r<roadNetworkP[road].roadNodes[segment].rn.u.roadPointDetails.riverCodeCount; r++) {
            riverCode = roadNetworkP[road].roadNodes[segment].rn.u.roadPointDetails.riverCodes[r];
	    roadNetworkP[road].roadNodes[segment].costToGP += mover.getValue(riverCode);
            }
       }
       else
	 roadNetworkP[road].roadNodes[segment].costToGP = 0;
    }
}

//*************************************************************************
//	Add river delays to localCosts.
//
void updateLocalCostsWithRiverDelays(SubRegion &aoi, Mover &mover, double ***linkTime,
                                     gridCell *gridP) {
  int line, sample;
  int lines = aoi.getLines();
  int samples = aoi.getSamples();
  int k, r;
  int riverCode;  // This is element id of river features.
  for (line=0; line<lines; line++)
     for (sample=0; sample<samples; sample++)
        for (k=0; k<8; k++)
	  for (r=0; r<gridP[line*samples + sample].riverCodes[k].riverCodeCount; r++) {
             riverCode = gridP[line*samples + sample].riverCodes[k].riverCodes[r];
	     linkTime[line][sample][k] += mover.getValue(riverCode);
          }
}

//*************************************************************************
//	Get terrain data.
//
unsigned char **getTerrain(SubRegion &_aoi, MyRegion &_region) {

int lines, samples, maxValue;
double skip = 1.0/_aoi.getReduction();
cerr << "skip = " << skip << endl;
unsigned char **data = ucharMatrix(_aoi.getLines(), _aoi.getSamples());

char filename[512];
  // Get terrain filename from database using Data Layer.
  Tcl_Interp *interp = getInterp();
 
  char script[1000];
  sprintf(script,
	  "Data_Select tma_regionThemes {TEXT_FILENAME} {{REGION_NAME %s} {THEME_TYPE Terrain}}",
	   _region.getName());
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sprintf(filename, "%s/%s", getenv("TMA_DATA"), interp->result);
  } else {
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

FILE *filePtr;
cerr << "Opening " << filename << " for reading\n";
if ( (filePtr = fopen(filename, "r")) == NULL ) {
  cerr << "Error: Cannot open " << filename << " for reading\n";
  return NULL;
}
fscanf(filePtr, "P5\n%d %d\n%d", &samples, &lines, &maxValue);
cerr << lines << " " << samples << " " << maxValue << endl;
// Read the 1 white space character.
cerr << "White space character = " << fgetc(filePtr) << endl;
long beginDataInFile = ftell(filePtr);

int startLine = _aoi.getStartLine();
int startSample = _aoi.getStartSample();
int regionSamples = _region.getSamples();
 fprintf(stderr, "AOI startLine = %d, startSample = %d, regionSamples = %d\n",
	 startLine, startSample, regionSamples);

lines = _aoi.getLines();
samples = _aoi.getSamples();
int numUnsampledSamples = (int)ceil((double)samples * skip);
unsigned char *buffer = new unsigned char[numUnsampledSamples];

cerr << "aoi lines, samples = " << lines << " " << samples << endl;
int line, sample;
for (line=0; line<lines; line++) {
  fseek(filePtr,
        (long) ((long) (startLine+
                (long) floor(line*skip)) *
                (long) regionSamples + (long) startSample) +
                beginDataInFile, 0);
  fread(buffer, 1, numUnsampledSamples, filePtr);
  for (sample=0; sample<samples; sample++)
     data[line][sample] = buffer[(int)floor(sample*skip)];
}
fclose(filePtr);
delete[] buffer;
return data;
}

//*************************************************************************
//	Get elevation data.
//
unsigned short **getElev(SubRegion &_aoi, MyRegion &_region) {
int lines, samples;
//int maxValue;
double skip = 1.0/_aoi.getReduction();
cerr << "aoi lines = " << _aoi.getLines() << endl;
cerr << "aoi samples = " << _aoi.getSamples() << endl;
cerr << "region lines = " << _region.getLines() << endl;
cerr << "region samples = " << _region.getSamples() << endl;
unsigned short **data = intMatrix(_aoi.getLines(), _aoi.getSamples());

char filename[512];
  // Get terrain filename from database using Data Layer.
  Tcl_Interp *interp = getInterp();
 
  char script[1000];
  sprintf(script,
	  "Data_Select tma_regionThemes {TEXT_FILENAME} {{REGION_NAME %s} {THEME_TYPE Elevation}}",
	   _region.getName());
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sprintf(filename, "%s/%s", getenv("TMA_DATA"), interp->result);
  } else {
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

FILE *filePtr;
cerr << "Opening " << filename << " for reading\n";
if ( (filePtr = fopen(filename, "r")) == NULL ) {
  cerr << "Error: Cannot open " << filename << " for reading\n";
  return NULL;
}
//fscanf(filePtr, "P5\n%d %d\n%d", &samples, &lines, &maxValue);
//cerr << lines << " " << samples << " " << maxValue << endl;
// Read the 1 white space character.
//cerr << "White space character = " << fgetc(filePtr) << endl;

int startLine = _aoi.getStartLine();
int startSample = _aoi.getStartSample();
int regionSamples = _region.getSamples();

lines = _aoi.getLines();
samples = _aoi.getSamples();
int numUnsampledSamples = (int)ceil((double)samples * skip);
unsigned short *buffer = new unsigned short[numUnsampledSamples];

int line, sample;
for (line=0; line<lines; line++) {
  fseek(filePtr,
        (long) ((long) sizeof(unsigned short)*(
                (long) (startLine+
                (long) floor(line*skip)) *
                (long) regionSamples + (long) startSample)), 0);
  fread(buffer, sizeof(unsigned short), numUnsampledSamples, filePtr);
  for (sample=0; sample<samples; sample++)
     data[line][sample] = buffer[(int)floor(sample*skip)];
}
fclose(filePtr);
delete[] buffer;

// Output elevation data for debugging.
if (OUTPUT_DATA)
	{
        FILE *outFilePtr;
	// Open output debug file.
	sprintf(filename, "%s/debugElev.raw", getenv("TMA_DATA"));
	outFilePtr = fopen(filename, "wb");
	for (line=0; line<lines; line++)
           fwrite(data[line], sizeof(unsigned short),
                  samples, outFilePtr);
        fclose(outFilePtr);
        }
return data;
}

//************************************************************************
//  Compute terrain times not considering slope factors or delays.
//
double **computeTerrainTimes(SubRegion &_aoi, MyRegion &_region, Mover &_mover) {
int line, sample, pixelValue;
FILE *outFilePtr;

int lines = _aoi.getLines();
int samples = _aoi.getSamples();
double **terrainTimes = doubleMatrix(lines, samples);
unsigned char **terr = getTerrain(_aoi, _region);

// Make delays into speeds using cellLength.
// There shouldn't be any delay terrain features terrain raster data,
// so, don't need to make delays into speeds using metersPerLinePixel.
//double metersPerLinePixel = _aoi.getMetersPerLinePixel();

// moverTimes[index] is the time (minutes) it take to cross a cell
// with terrain type associated with index in the raster
// terrain file.  Generally there should only be terrain
// features with affect = speed. This is because factors
// are generally determined by slope which is determined
// by the elevation data.  Delays are generally associated
// with rivers.  In this version of TMA, rivers are handled
// separately from the raster terrain data.
// Initialize the moverTimes to a nominal speed = 5 km/hr.
double moverTimes[256];
int i;
for (i=0; i<256; i++)
   moverTimes[i] = (60.0/1000.0) / 5.0; 

for (i=0; i<_mover.getNumElements(); i++)
   if (_mover.getElement(i).getAffect() == rspeed)
      moverTimes[_mover.getElement(i).getIndex()] =
        (60.0/1000.0) /_mover.getElement(i).getValue();

// moverTimes[] has units of minutes/meter.

//for (i=0; i<255; i++)
//   cerr << "moverTimes[" << i << "] = " << moverTimes[i] << endl;

// Set up terrainTimes[][] by using the correspondence between
// between stored values in the raster file and index into moverTimes[].

char filename[512];
if (OUTPUT_DATA)
	{
	// Open output debug file.
	sprintf(filename, "%s/debugTerr.pgm", getenv("TMA_DATA"));
	outFilePtr = fopen(filename, "w");
	fprintf(outFilePtr, "P5\n%d %d\n255\n", samples, lines);
	}

for (line=0; line<lines; line++)
   for (sample=0; sample<samples; sample++)
      {
      pixelValue = terr[line][sample];
      if (OUTPUT_DATA) fputc(pixelValue, outFilePtr);
      terrainTimes[line][sample] = moverTimes[pixelValue];
      }

if (OUTPUT_DATA) fclose(outFilePtr);
freeUcharMatrix(terr, lines);
return terrainTimes;
}

//************************************************************************
//	Compute local costs.
//	Is done for every query -- no intermediate results are saved.
//	dir is used to select convention about positive or negative slopes.
//	Used to determine if going away from source (start point) or
//	going towards source (times to get to source). 
//      They are different because of slopes.
//
double ***computeLocalCosts(int dir, MyRegion &_region, SubRegion &_aoi,
                            Mover &_mover) {
    int line, sample, k;
    unsigned short elevHere;
    double  eucdist[3][3], deltaZ, deltaV;
    double  trueDistance, baseTime, modifiers;

//      FILE * tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "computeLocalCosts getting elevation data...\n");
//      fclose (tmaLog);

cerr << "Getting elevation data ..." << endl;
unsigned short **elev = getElev(_aoi, _region);
cerr << "Done getElev()." << endl;

//   tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "computeLocalCosts got elevation data...\n");
//      fclose (tmaLog);

int lines   = _aoi.getLines();
int samples = _aoi.getSamples();

//   tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "computeLocalCosts got lines and samples...\n");
//      fclose (tmaLog);

// Allocate linkTime.
// Stores the times between locations in the grid.
// 3D matrix allocation.

double ***linkTime = new double**[lines];
for (line=0; line<lines; line++)
   {
   linkTime[line] = new double*[samples];
   for (sample=0; sample<samples; sample++)
      linkTime[line][sample] = new double[8];
   }

//   tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "computeLocalCosts calling computeTerrainTimes...\n");
//      fclose (tmaLog);

cerr << "tmaComputeTerrainTimes ..." << endl << flush;
double **terrainTimes = computeTerrainTimes(_aoi, _region, _mover);

// Slope multipliers (positive number meaning uphill)

// Assumes no more than 100 slope factors per mover.
double slopeFactors[100];
double slopeLower[100];
double slopeUpper[100];

int numSlopeFactors = 0;
int e;
//cerr << "num elements = " << _mover.getNumElements() << endl;
for (e=0; e<_mover.getNumElements(); e++) {
    //cerr << "affect # " << e << " = " << _mover.getElement(e).getAffect()
    //     << endl;
    if (_mover.getElement(e).getAffect() == slopeAffect) {
      //cerr << "Found slope\n";
      slopeFactors[numSlopeFactors] = _mover.getElement(e).getValue();
      slopeLower[numSlopeFactors]   = _mover.getElement(e).getLower();
      slopeUpper[numSlopeFactors]   = _mover.getElement(e).getUpper();
      numSlopeFactors++;
    }
}
//for (e=0; e<numSlopeFactors; e++)
  //cerr << "slope " << slopeFactors[e] << " " << slopeLower[e] << " " <<
  //        slopeUpper[e] << endl;

//  Compute signed distance from central pixel to each of its
//  8 neighbors in meters / pixel.

double metersPerLinePixel = _aoi.getMetersPerLinePixel();
double metersPerSamplePixel = _aoi.getMetersPerSamplePixel();
// Diagonal directions.
eucdist[1+1][1+1] =  sqrt(metersPerLinePixel   * metersPerLinePixel 
                        + metersPerSamplePixel * metersPerSamplePixel);
eucdist[-1+1][1+1]  = eucdist[1+1][1+1];
eucdist[1+1][-1+1]  = eucdist[1+1][1+1];
eucdist[-1+1][-1+1] = eucdist[1+1][1+1];
// Horizontal directions.
eucdist[0+1][1+1]  = metersPerSamplePixel;
eucdist[0+1][-1+1] = metersPerSamplePixel;
// Vertical directions.
eucdist[1+1][0+1]  = metersPerLinePixel;
eucdist[-1+1][0+1] = metersPerLinePixel;

int slopeBin;
double slopeFactor, slopeValue, slopeDir;
if (dir == FROM)
  slopeDir = 1;
else
  slopeDir = -1;

//
// Loop grid locations.
// Compute linkTime.
//   linkTime[line][sample][k] is time in minutes
//	to get from (line,sample) to adjacent pixel in "k" direction.
//   It will be set to
//	basetime / (slopeFactor * weatherFactors * obstacles),
//   where basetime is the time defined for the terrain feature
//   at (line,sample).
//

// The edges of linkTime will be set to the maximum time.
// This will ensure that there is not a faster time through a
// location on the edge to an interior location.

//   tmaLog = fopen ("/tmp/tma.log", "a");
//      fprintf (tmaLog, "computeLocalCosts starting linkTime compute loop...\n");
//      fclose (tmaLog);

int neighborLine, neighborSample;
for (line=0; line<lines; line++)
   for (sample=0; sample<samples; sample++) {
      elevHere = elev[line][sample];
      //cerr << "Elev = " << elev[line][sample] << endl;

      // 8 directions.
      for (k=0; k<8; k++) {

         // Get slopeValue : delta z / delta v
         // Compensates for diagonal grid directions.

         neighborLine   = line   + tma_ldir[k];
         neighborSample = sample + tma_sdir[k];
         if ((neighborLine<0) || (neighborLine>lines-1) ||
             (neighborSample<0) || (neighborSample>samples-1))
           continue;

         deltaZ = elev[ neighborLine ][ neighborSample ] - elevHere;
         deltaV = eucdist[ tma_ldir[k]+1 ][ tma_sdir[k]+1 ];
         slopeValue = slopeDir * deltaZ / deltaV;

         trueDistance =
         sqrt(  eucdist[ tma_ldir[k]+1 ][ tma_sdir[k]+1 ] *
                eucdist[ tma_ldir[k]+1 ][ tma_sdir[k]+1 ] +
                deltaZ * deltaZ  );

         // Quantize slope to bins:
         for (slopeBin=0; slopeBin<numSlopeFactors; slopeBin++)
            if ((slopeValue <= slopeUpper[slopeBin]) &&
                (slopeValue >= slopeLower[slopeBin])) {
              slopeFactor = slopeFactors[slopeBin];
              break;
              }

         //cerr << deltaZ << " / " << deltaV << endl;
         //cerr << "slopeValue = " << slopeValue << endl;

         // Get the base time and modifying factor.
         // baseTime is in minutes.

         baseTime = terrainTimes[line][sample] * trueDistance;

         // modifiers = tma_weather[line][sample] * tma_obs[line][sample] *
         //             slopeFactor;
         modifiers = slopeFactor;

         if ((modifiers < EPSILON) || (baseTime > TOO_HUGE))
           linkTime[line][sample][k] = TOO_HUGE;
         else
           linkTime[line][sample][k] = baseTime / modifiers;

      } // for (k)
   } // for (sample) for (line)

//
//  Put TOO_HUGE on the edge of linkTime.
//

// Top line.
line = 0;
for (sample=0; sample<samples; sample++)
  {
    linkTime[line][sample][0] = TOO_HUGE;
    linkTime[line][sample][1] = TOO_HUGE;
    linkTime[line][sample][2] = TOO_HUGE;
  }
// Bottom line.
line = lines-1;
for (sample=0; sample<samples; sample++)
   {
     linkTime[line][sample][5] = TOO_HUGE;
     linkTime[line][sample][6] = TOO_HUGE;
     linkTime[line][sample][7] = TOO_HUGE;
   }
// Left column.
sample = 0;
for (line=0; line<lines; line++)
   {
      linkTime[line][sample][0] = TOO_HUGE;
      linkTime[line][sample][3] = TOO_HUGE;
      linkTime[line][sample][5] = TOO_HUGE;
   }
// Right column.
sample = samples-1;
for (line=0; line<lines; line++)
   {
      linkTime[line][sample][2] = TOO_HUGE;
      linkTime[line][sample][4] = TOO_HUGE;
      linkTime[line][sample][7] = TOO_HUGE;
   }

freeIntMatrix(elev, lines);
freeDoubleMatrix(terrainTimes, lines);

//for (line=0; line<lines; line++)
//  for (sample=0; sample<samples; sample++)
//	for (k=0; k<8; k++)
//           cerr << line << " " << sample << " " << k << " " <<
//             linkTime[line][sample][k] << endl;

return(linkTime);
}

// Create Obstacle/Weather Mask returns 0 when happy
int createOWMask (SubRegion &aoi, Mover &mover, const char * aoiId, const char * moverId);
// Factor in obstacle and weather effects
void updateDnetWithOWMask (dnet * dnetP);

//////////////////////////////////////////////////////////////////////////////
// regionName, nw and se corners of aoi, moverName, start and end points,
// and flag that indicates whether the end point is set.
SubRegion &setup(int dir, const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
		 const char *moverName, SbVec2f &start, SbVec2f &end, int endSet,
		 dnet **addrDijkstraNet, const char *rnfName, const char *dataSet, const char *aoiId) {

  // roadNetworkP is an array of road descriptions.  Road based.

  // Read stateFile to get parameters of last query.
  // They will be used to compare with current parameters
  // to see if some computations can be skipped, and
  // old data read instead.

//    FILE * tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "setup (%s, %f, %f, %f, %f, %d, %d, %s, %f, %f, %f, %f, %s, %s)\n", regionName, nw.lat(), nw.lon(), se.lat(), se.lon(), lines, samples,
//  	   moverName, start.lat(), start.lon(), end.lat(), end.lon(), rnfName, dataSet);
//    fclose (tmaLog);

cerr << "Making region ...\n";
cerr << "Region name = " << regionName << endl;
//  Region region(regionName);
 MyRegion region(regionName);  // Needed so that region is the DTED region.

//   tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Making subregion...\n");
//    fclose (tmaLog);

cerr << "Making subregion ...\n";
  SubRegion *aoi = new SubRegion(region, nw, se, lines, samples);
cerr << "Done making subregion ...\n";
  int startLine, startSample, endLine, endSample;
  aoi->latLon2lineSample(&startLine, &startSample, start.lat(), start.lon());
  aoi->latLon2lineSample(&endLine, &endSample, end.lat(), end.lon());

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Making mover...\n");
//    fclose (tmaLog);

cerr << "Making mover ...\n";
  Mover mover(regionName, moverName, dataSet);
cerr << "Done making mover.\n";

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Done making mover...\n");
//    fclose (tmaLog);

//  char roadFilename[512], riverFilename[512], intersectionFilename[512];
  // Get vector filenames from database using Data Layer.
//  Tcl_Interp *interp = getInterp();
 
//  char script[1000];
#if 0
  // BEWARE: Do not compile this without checking the database.
  sprintf(script,
	  "Data_Select tma_regionThemes {TEXT_FILENAME} {{REGION_NAME %s} {THEME_TYPE Road}}",
	   _region.getName());
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sprintf(roadFilename, "%s/%s", getenv("TMA_DATA"), interp->result);
  } else {
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

  sprintf(script,
	  "Data_Select tma_regionThemes {TEXT_FILENAME} {{REGION_NAME %s} {THEME_TYPE River}}",
	   _region.getName());
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sprintf(riverFilename, "%s/%s", getenv("TMA_DATA"), interp->result);
  } else {
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

  sprintf(script,
	  "Data_Select tma_regionThemes {TEXT_FILENAME} {{REGION_NAME %s} {THEME_TYPE Intersection}}",
	   _region.getName());
  fprintf(stderr, "%s\n", script);
  int code = Tcl_Eval(interp, script);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sprintf(intersectionFilename, "%s/%s", getenv("TMA_DATA"), interp->result);
  } else {
    fprintf(stderr, "Error in:\n%s\n", script);
    exit(1);
  }

#endif

  // change to #if 1 to generate rnf file.
#if 0
  char dir[512];
  sprintf(dir, "%s", getenv("TMA_DATA"));
  char command[1024];
  sprintf(command,
    "../bin/rnf %s/maui/tig_merged.tvf %s/maui/hydlndlg.tvf %s/maui/tig_merged.rif %s/%s %f %f %f %f %d %d %s",
	dir, dir, dir, dir, rnfName,
        aoi->getNw().lon(), aoi->getSe().lat(),
        aoi->getSe().lon(), aoi->getNw().lat(),
        aoi->getSamples(), aoi->getLines(), regionName);
cerr << "Generating road data\n";
cerr << "Running " << command << endl;
system(command);
#endif

#if 0
  // Last 4 parameters are background color, grid line color, grid spacing sample and width direction.
  sprintf(command,
    "../bin/rasterize %s/roadsNrivers.tvf %s/roadsNrivers.pgm %f %f %f %f %d %d %s/pgmprio 0 0 1 1",
        dir, dir,
        aoi->getNw().lon(), aoi->getSe().lat(),
        aoi->getSe().lon(), aoi->getNw().lat(),
        4*aoi->getSamples(), 4*aoi->getLines(), dir);
cerr << "Running " << command << endl;
system(command);
#endif

cerr << "Loading road network file\n";
  // Load (Road Network File) RNF file.

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Setup loading RNF...\n");
//    fclose (tmaLog);

  char rnfPath[512];
  sprintf(rnfPath, "%s/%s", getenv("TMA_DATA"), rnfName);
  int roadCount;
  gridCell *gridP;
  // gridP is array[][] of grid cell river crossings and road points in each cell.
  // Grid (cell) based.
  int width;
  int height;
  dRoadDescription *roadNetworkP;
  int status = 0;
  
  if ((status = loadRNF (rnfPath, &roadNetworkP, &roadCount, &gridP, &width, &height)) != LOADRNF_HAPPY) {
    cerr << "Loadrnf unhappy with " << rnfPath << ", returned " << status << "\n";
  }

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Setup survived the loadRNF call\n");
//    fclose (tmaLog);

  if ((width != aoi->getSamples()) || (height != aoi->getLines())) {
    cerr << "Bad magic number\n";
    cerr << "width = " << width << " height = " << height << endl;
    cerr << "aoi samples, lines = " << aoi->getSamples() << " " <<  aoi->getLines() << endl;
  }
  // Check consistency
  cerr << "Consistent road network (0 is good) = " <<
    checkRNF(roadNetworkP, roadCount, gridP, width, height, 0) << endl;

  // looks up mover speed for each node; calcs distance; stores cost in minutes
  computeLocalRoadNetworkCosts(*aoi, mover, roadNetworkP, roadCount);

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Setup computing local costs...\n");
//    fclose (tmaLog);

cerr << "Computing Local Costs\n";
  double ***linkTime = computeLocalCosts(dir, region, *aoi, mover);
cerr << "Done computing local costs.\n";

  // Rivers
 cerr << "Computing river delays.\n";
  updateLocalCostsWithRiverDelays(*aoi, mover, linkTime, gridP);
  cerr << "Done with river delays.\n";

  cerr << "Consistent road network (0 is good) = " <<
    checkRNF(roadNetworkP, roadCount, gridP, width, height, 0) << endl;

  // Build dijkstra network.
  buildDnet (linkTime, roadNetworkP, roadCount, width, height, addrDijkstraNet, gridP);

  cerr << "Creating OWMask\n";

  if (createOWMask (*aoi, mover, aoiId, moverName))
    cerr << "createOWMask not happy\n";

  cerr << "Updating dnet with OWMask\n";

  updateDnetWithOWMask (* addrDijkstraNet);

  cerr << "Freeing RNF\n";

  freeRNF(roadNetworkP, roadCount, gridP, width, height);

  //cerr << "Consistent road network (0 is good) = " <<
   //checkRNF(roadNetworkP, roadCount, gridP, width, height, 0) << endl;

//    tmaLog = fopen ("/tmp/tma.log", "a");
//    fprintf (tmaLog, "Setup calling Dijkstra\n");
//    fclose (tmaLog);

cerr << "Calling dijkstra\n";
  dijkstra(startLine, startSample, endLine, endSample, endSet,
           *addrDijkstraNet, *aoi);
cerr << "Done dijkstra.\n";
cerr << "Returning from setup\n"; 

 double nwlat = aoi->getNw().lat();
 double selat = aoi->getSe().lat();
 double nwlon = aoi->getNw().lon();
 double selon = aoi->getSe().lon();
 writeDnetAscii("dnet.txt", *addrDijkstraNet, nwlat, selat, nwlon, selon);

//cerr << "Consistent road network (0 is good) = " <<
   //checkRNF(roadNetworkP, roadCount, gridP, width, height, 0) << endl;

  return(*aoi);
}

typedef struct {
  float weatherSpeedFactor;
  char obstacleFlag;
} OWMask;

typedef void  pointPlotter (int x, int y);

static int plotLine (int x0, int y0, int x1, int y1, pointPlotter plotOne);

int maskLines = 0;
int maskSamples = 0;
OWMask ** owMask = 0;

// factor in obstacles and weather to the dnet
void updateDnetWithOWMask (dnet * dnetP) {
  int line, sample;

  // foreach dnet node
  for (int node = 0; node < dnetP -> nc; node ++) {
    line = IDX2LINE (dnetP -> nv [node] . gpIndex, dnetP);
    sample = IDX2SAMPLE (dnetP -> nv [node] . gpIndex, dnetP);
    // if its grid cell is an obstacle
    if (owMask [line] [sample] . obstacleFlag)
      // set all neighbor costs to TOO_HUGE for the obstacle cell
      for (int nb = 0; nb < dnetP -> nv [node] . nbc; nb ++)
	dnetP -> nv [node] . nbcv [nb] = TOO_HUGE;
    // check for weather effects
    else if (owMask [line] [sample] . weatherSpeedFactor != 1.0)
      // factor in weather effect for each neighbor
      for (int nb = 0; nb < dnetP -> nv [node] . nbc; nb ++) {
	dnetP -> nv [node] . nbcv [nb] /= owMask [line] [sample] . weatherSpeedFactor;
	// don't exceed TOO_HUGE
	if (dnetP -> nv [node] . nbcv [nb] > TOO_HUGE)
	  dnetP -> nv [node] . nbcv [nb] = TOO_HUGE;
      }
  }
}

void plotObstaclePoint (int x, int y) {
  owMask [y] [x] . obstacleFlag = 1;
}

static float speedFactor;
void plotWeatherPoint (int y, int xl, int xr) {
  for (int x = xl; x <= xr; x ++)
    owMask [y] [x] . weatherSpeedFactor *= speedFactor;
}

// returns 0 when happy
int createOWMask (SubRegion &aoi, Mover &mover, const char * aoiId, const char * moverId) {
  int line, sample;
  static int maskLines;
  static int maskSamples;

  if (owMask) {
    for (int i = 0; i < maskLines; i ++)
      delete[] owMask[i];
    delete[] owMask;
  }

  maskLines = aoi.getLines();
  maskSamples = aoi.getSamples();

  cerr << "mallocing mask\n";

  owMask = new OWMask*[maskLines];
  for (int i=0; i < maskLines; i ++)
    owMask [i] = new OWMask[maskSamples];

  if (!owMask) {
    cerr << "createOWMask failed to malloc a " << maskLines << " by " << maskSamples << " matrix\n";
    return 1;
  }

  cerr << "initializing mask\n";

  // Initialize mask
  for (line=0; line < maskLines; line++)
    for (sample=0; sample < maskSamples; sample++) {
      owMask [line] [sample] . weatherSpeedFactor = 1.0;
      owMask [line] [sample] . obstacleFlag = 0;
    }

  cerr << "mask initialized\n";

  Tcl_Interp *interp = getInterp();
  char script[1000];

  // Get obstacles
 
  sprintf (script, "set obstacles [Data_Select tma_polygon {POINTS} {{AOI_ID %s} {TYPE Obstacle}}]; llength $obstacles", aoiId);
  //  fprintf(stderr, "%s\n", script);

  int code = Tcl_Eval(interp, script);

  if (code != TCL_OK) {
    if (*interp->result)
      //fprintf(stderr, "%s\n", interp->result)
      ;
    else
      fprintf (stderr, "Tcl didn't return OK\n");
    return 1;
  }

  int obstacleCount;

  if (sscanf (interp->result, "%d", & obstacleCount) != 1) {
    fprintf(stderr, "error parsing obstacleCount from %s\n", interp->result);
    return 1;
  }
  
  cerr << "got " << obstacleCount << " obstacles\n";

  // for each obstacle

  for (int obstacle = 0; obstacle < obstacleCount; obstacle ++) {

    sprintf (script, "set obstacle [lindex [lindex $obstacles %d] 0]; llength $obstacle", obstacle);
    //    fprintf(stderr, "%s\n", script);

    code = Tcl_Eval(interp, script);

    if (code != TCL_OK) {
      if (*interp->result)
	// fprintf(stderr, "%s\n", interp->result)
	;
      else
	fprintf (stderr, "Tcl didn't return OK\n");
      return 1;
    }

    int pointCount;

    if (sscanf (interp->result, "%d", & pointCount) != 1) {
      fprintf(stderr, "error parsing pointCount from %s\n", interp->result);
      return 1;
    }
  
    cerr << pointCount << " points in obstacle\n";

    // for each point pair
    for (int point = 0; point < pointCount - 1; point ++) {
      // Rasterize obstacle lines into mask
      
      sprintf (script, "format \"%%.15lf %%.15lf %%.15lf %%.15lf\" [lindex [lindex $obstacle %d] 0] [lindex [lindex $obstacle %d] 1] [lindex [lindex $obstacle %d] 0] [lindex [lindex $obstacle %d] 1]", point, point, point + 1, point + 1);
      //      fprintf(stderr, "%s\n", script);

      code = Tcl_Eval(interp, script);

      if (code != TCL_OK) {
	if (*interp->result)
	  // fprintf(stderr, "%s\n", interp->result)
	  ;
	else
	  fprintf (stderr, "Tcl didn't return OK\n");
	return 1;
      }

      double lat0, lon0, lat1, lon1;

      if (sscanf (interp->result, "%lf %lf %lf %lf", & lat0, & lon0, & lat1, & lon1) != 4) {
	fprintf(stderr, "error parsing obstacle segment from %s\n", interp->result);
	return 1;
      }

      int line0, sample0, line1, sample1;

      aoi.latLon2lineSample(& line0, & sample0, lat0, lon0);
      aoi.latLon2lineSample(& line1, & sample1, lat1, lon1);

      plotLine (sample0, line0, sample1, line1, plotObstaclePoint);
    }
  }

  // Get weathers
  sprintf (script, "set weathers [Data_Select tma_polygon {SUBTYPE POINTS} {{AOI_ID %s} {TYPE Weather}}]; llength $weathers", aoiId);
  //  fprintf(stderr, "%s\n", script);

  code = Tcl_Eval(interp, script);

  if (code != TCL_OK) {
    if (*interp->result)
      // fprintf(stderr, "%s\n", interp->result)
      ;
    else
      fprintf (stderr, "Tcl didn't return OK\n");
    return 1;
  }

  int weatherCount;

  if (sscanf (interp->result, "%d", & weatherCount) != 1) {
    fprintf(stderr, "error parsing weatherCount from %s\n", interp->result);
    return 1;
  }
  
  cerr << "got " << weatherCount << " weathers\n";

  GemsWindow window;
  
  window . x0 = 0;
  window . x1 = maskSamples - 1;
  window . y0 = 0;
  window . y1 = maskLines - 1;
  
  // for each weather
  
  for (int weather = 0; weather < weatherCount; weather ++) {

    sprintf (script, "set weatherSubtype [lindex [lindex $weathers %d] 0]; set weatherPoints [lindex [lindex $weathers %d] 1]; llength $weatherPoints", weather, weather);
    // fprintf(stderr, "%s\n", script);

    code = Tcl_Eval(interp, script);

    if (code != TCL_OK) {
      if (*interp->result)
	// fprintf(stderr, "%s\n", interp->result)
	  ;
      else
	fprintf (stderr, "Tcl didn't return OK\n");
      return 1;
    }

    int pointCount;

    if (sscanf (interp->result, "%d", & pointCount) != 1) {
      fprintf(stderr, "error parsing pointCount from %s\n", interp->result);
      return 1;
    }
  
    cerr << pointCount << " points in weather\n";

    sprintf (script, "format $weatherSubtype");
    //    fprintf(stderr, "%s\n", script);

    code = Tcl_Eval(interp, script);

    if (code != TCL_OK) {
      if (*interp->result)
	// fprintf(stderr, "%s\n", interp->result)
	  ;
      else
	fprintf (stderr, "Tcl didn't return OK\n");
      return 1;
    }

    // get the weather terrain value for the mover
    sprintf (script, "set terrainValue [Data_Select tma_mover {VALUE} {{APPLICATION_ID %s} {MOVER_ID %s} {TERRAIN_ID %s}}]",
	     getenv ("TMA_APPLICATION_ID"), moverId, interp -> result);
        fprintf(stderr, "%s => %s\n", script, interp->result);

  code = Tcl_Eval(interp, script);

  if (code != TCL_OK) {
    if (*interp->result)
      // fprintf(stderr, "%s\n", interp->result)
      ;
    else
      fprintf (stderr, "Tcl didn't return OK\n");
    return 1;
  }

  if (sscanf (interp->result, "%f", & speedFactor) != 1) {
    fprintf(stderr, "error parsing weatherFactor from %s\n", interp->result);
    return 1;
  }
  
  cerr << "weatherFactor = " << speedFactor << endl;


    Point2 * weatherPoints = new Point2 [pointCount];

    // for each point
    for (int point = 0; point < pointCount; point ++) {
      // Rasterize weather lines into mask
      
      sprintf (script, "format \"%%.15lf %%.15lff\" [lindex [lindex $weatherPoints %d] 0] [lindex [lindex $weatherPoints %d] 1] ", point, point);
      //      fprintf(stderr, "%s\n", script);

      code = Tcl_Eval(interp, script);

      if (code != TCL_OK) {
	if (*interp->result)
	  // fprintf(stderr, "%s\n", interp->result)
	    ;
	else
	  fprintf (stderr, "Tcl didn't return OK\n");
	return 1;
      }

      double lat, lon;
      if (sscanf (interp->result, "%lf %lf", & lat, & lon) != 2) {
	fprintf(stderr, "error parsing weather point from %s\n", interp->result);
	return 1;
      }

      int line, sample;
      aoi.latLon2lineSample(& line, & sample, lat, lon);
      weatherPoints [point].x = sample;
      weatherPoints [point].y = line;
    }

    // Rasterize weather polygons into mask
    concave(pointCount, weatherPoints, & window, plotWeatherPoint);
  }


  // Get region obstacles
  sprintf (script, "set regionObstacles [Data_Select tma_polygon {SUBTYPE POINTS} {{AOI_ID %s} {TYPE RegionObstacle}}]; llength $regionObstacles", aoiId);
  //  fprintf(stderr, "%s\n", script);

  code = Tcl_Eval(interp, script);

  if (code != TCL_OK) {
    if (*interp->result)
      // fprintf(stderr, "%s\n", interp->result)
	;
    else
      fprintf (stderr, "Tcl didn't return OK\n");
    return 1;
  }

  int regionObstacleCount;

  if (sscanf (interp->result, "%d", & regionObstacleCount) != 1) {
    fprintf(stderr, "error parsing regionObstacleCount from %s\n", interp->result);
    return 1;
  }
  
  cerr << "got " << regionObstacleCount << " regionObstacles\n";

  window . x0 = 0;
  window . x1 = maskSamples - 1;
  window . y0 = 0;
  window . y1 = maskLines - 1;
  
  // for each region obstacle
  
  for (int regionObstacle = 0; regionObstacle < regionObstacleCount; regionObstacle ++) {
    sprintf (script, "set regionObstacleValue [lindex [lindex $regionObstacles %d] 0]; set regionObstaclePoints [lindex [lindex $regionObstacles %d] 1]; llength $regionObstaclePoints", regionObstacle, regionObstacle);
    // fprintf(stderr, "%s\n", script);

    code = Tcl_Eval(interp, script);

    if (code != TCL_OK) {
      if (*interp->result)
	// fprintf(stderr, "%s\n", interp->result)
	  ;
      else
	fprintf (stderr, "Tcl didn't return OK\n");
      return 1;
    }

    int pointCount;

    if (sscanf (interp->result, "%d", & pointCount) != 1) {
      fprintf(stderr, "error parsing pointCount from %s\n", interp->result);
      return 1;
    }
  
    cerr << pointCount << " points in regionObstacle\n";

    sprintf (script, "format $regionObstacleValue");

    code = Tcl_Eval(interp, script);

    if (code != TCL_OK) {
      if (*interp->result)
	// fprintf(stderr, "%s\n", interp->result)
	  ;
      else
	fprintf (stderr, "Tcl didn't return OK\n");
      return 1;
    }

  if (sscanf (interp->result, "%f", & speedFactor) != 1) {
    fprintf(stderr, "error parsing regionObstacleFactor from %s\n", interp->result);
    return 1;
  }
  
  cerr << "regionObstacleFactor = " << speedFactor << endl;


    Point2 * regionObstaclePoints = new Point2 [pointCount];

    // for each point
    for (int point = 0; point < pointCount; point ++) {
      // Rasterize regionObstacle lines into mask
      
      sprintf (script, "format \"%%.15lf %%.15lff\" [lindex [lindex $regionObstaclePoints %d] 0] [lindex [lindex $regionObstaclePoints %d] 1] ", point, point);

      code = Tcl_Eval(interp, script);

      if (code != TCL_OK) {
	if (*interp->result)
	  // fprintf(stderr, "%s\n", interp->result)
	    ;
	else
	  fprintf (stderr, "Tcl didn't return OK\n");
	return 1;
      }

      double lat, lon;
      if (sscanf (interp->result, "%lf %lf", & lat, & lon) != 2) {
	fprintf(stderr, "error parsing regionObstacle point from %s\n", interp->result);
	return 1;
      }

      int line, sample;
      aoi.latLon2lineSample(& line, & sample, lat, lon);
      regionObstaclePoints [point].x = sample;
      regionObstaclePoints [point].y = line;
    }

    // Rasterize regionObstacle polygons into mask
    concave(pointCount, regionObstaclePoints, & window, plotWeatherPoint);
  }


  return 0;
}

#ifndef SQR
#define SQR(x) ((x) * (x))
#endif
#define SQDIST(xa, ya, xb, yb) (SQR ((xb) - (xa)) + (SQR ((yb) - (ya))))

static float sqdistoline (int x0, int y0, int x1, int y1, int xp, int yp) {
  int pdot1 = (xp - x0) * (x1 - x0) + (yp - y0) * (y1 - y0);
  int dot1 = SQR (x1 - x0) + SQR (y1 - y0);
  float projx = x0 + (x1 - x0) * pdot1 / dot1;
  float projy = y0 + (y1 - y0) * pdot1 / dot1;

  return SQDIST (xp, yp, projx, projy);
}

// it looks like this was copied from plotline.c
// TODO - this should be factored out

static int plotLine (int x0, int y0, int x1, int y1, pointPlotter plotOne) {
  int dx = (x1 - x0);
  int dy = (y1 - y0);
  int dxPos = dx > 0;
  int dyPos = dy > 0;
  int dxDominates = abs (dx) > abs (dy);
  /* direction holds three bits:
   * dx > 0 where dx is from x0 to x1
   * dy > 0 where dy is from y0 to y1
   * abs(dx) > abs(dy) */

  int direction = (dxPos << 2) | (dyPos << 1) | dxDominates;

  int x, y;

  for (x = x0, y = y0; x != x1 || y!= y1; ) {
    plotOne (x, y);
    
    switch (direction) {
    case 0: /* dx <= 0, dy <=0, abs(dx) <= abs(dy) */
    case 1: /* dx <= 0, dy <=0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x - 1, y) > sqdistoline (x0, y0, x1, y1, x, y - 1))
	y--;
      else
	x--;
      break;
    case 2: /* dx <= 0, dy > 0, abs(dx) <= abs(dy) */
    case 3: /* dx <= 0, dy > 0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x - 1, y) > sqdistoline (x0, y0, x1, y1, x, y + 1))
	y++;
      else
	x--;
      break;
    case 4: /* dx >  0, dy <=0, abs(dx) <= abs(dy) */
    case 5: /* dx >  0, dy <=0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x + 1, y) > sqdistoline (x0, y0, x1, y1, x, y - 1))
	y--;
      else
	x++;
      break;
    case 6: /* dx >  0, dy > 0, abs(dx) <= abs(dy) */
    case 7: /* dx >  0, dy > 0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x + 1, y) > sqdistoline (x0, y0, x1, y1, x, y + 1))
	y++;
      else
	x++;
      break;
    default:
      printf ("rasterize: bad direction\n");
      return 1;
    }
    plotOne (x, y);
  }
  return 0;
}

