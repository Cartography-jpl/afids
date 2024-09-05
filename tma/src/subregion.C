#include <math.h>

#include "region.h"
#include "subregion.h"

///////////////////////////////////////////////////////////////////////////
//
// Constructor.
//
///////////////////////////////////////////////////////////////////////////
SubRegion::SubRegion(MyRegion &region,
                     SbVec2f &_nw, SbVec2f &_se, int inLines, int inSamples) {

  lines = inLines;
  samples = inSamples;

  nw = _nw;
  se = _se;
  region.latLon2lineSample(&startLine, &startSample, nw.lat(), nw.lon());
  region.latLon2lineSample(&endLine, &endSample, se.lat(), se.lon());

  int linesInRegionSpace = endLine - startLine + 1;
  int samplesInRegionSpace = endSample - startSample + 1;

  cerr << "In SubRegion constructor.\n";
  cerr << "lines samples = " << lines << " " << samples << endl;
  cerr << "startLine startSample = " << startLine << " " << startSample << endl;
  cerr << "endLine endSample = " << endLine << " " << endSample << endl;

  // Determine subsampling rate.
  // reduction = 1/2 means aoi will 1/2 have as many lines as before.
  if (lines < samples)
    reduction = (double)lines/(double)linesInRegionSpace;
  else
    reduction = (double)samples/(double)samplesInRegionSpace;
 
  metersPerLinePixel = region.getMetersPerLinePixel() / reduction;
  metersPerSamplePixel = region.getMetersPerSamplePixel() / reduction;

  cerr << "Reduction = " << reduction << endl;
}

///////////////////////////////////////////////////////////////////////////
//
// Destructor.
//
///////////////////////////////////////////////////////////////////////////
SubRegion::~SubRegion() {
}

///////////////////////////////////////////////////////////////////////////
//
//  Conversion utilities.
//  Convert between lat/long and line/sample space.
//	 Assumes data stored in normal english reading order.
//       I.e. line 0 is top, sample 0 is left.
//	 Does not work where longitude crosses a -180, +180 boundary.
//
///////////////////////////////////////////////////////////////////////////
void SubRegion::latLon2lineSample(int *line, int *sample,
			          double lat, double lon)
{
double totalDegrees, proportion;

totalDegrees = fabs(nw.lat() - se.lat());
proportion = fabs(lat - nw.lat()) / totalDegrees;
*line = (int) (proportion * (double) lines);

//cerr << "totalDegrees = " << totalDegrees << "\n";
//cerr << "proportion = " << proportion << "\n";
//cerr << "line = " << *line << "\n";

totalDegrees = fabs(nw.lon() - se.lon());
proportion = fabs(lon - nw.lon()) / totalDegrees;
*sample = (int) (proportion * (double) samples);

//cerr << "totalDegrees = " << totalDegrees << "\n";
//cerr << "proportion = " << proportion << "\n";
//cerr << "sample = " << *sample << "\n";

// Make sure line and sample are within a one pixel boundary of grid.
// Region::latLon2lineSample() does not do this.
if (*line <= 0) *line = 1;
if (*line >= lines-1) *line = lines-2;
if (*sample <= 0) *sample = 1;
if (*sample >= samples-1) *sample = samples-2;
}

///////////////////////////////////////////////////////////////////////////
void SubRegion::lineSample2latLon(double *lat, double *lon,
                                  int line, int sample)
{
double proportion;

proportion = (double) line/(double) lines;
*lat = nw.lat() + proportion * (se.lat() - nw.lat());

proportion = (double) sample/(double) samples;
*lon = nw.lon() + proportion * (se.lon() - nw.lon());
}

///////////////////////////////////////////////////////////////////////////
int SubRegion::getLines() {
  return lines;
}

///////////////////////////////////////////////////////////////////////////
int SubRegion::getSamples() {
  return samples;
}

///////////////////////////////////////////////////////////////////////////
double SubRegion::getMetersPerLinePixel() {
  return metersPerLinePixel;
}

///////////////////////////////////////////////////////////////////////////
double SubRegion::getMetersPerSamplePixel() {
  return metersPerSamplePixel;
}

///////////////////////////////////////////////////////////////////////////
SbVec2f &SubRegion::getNw() {
  return nw;
}

///////////////////////////////////////////////////////////////////////////
SbVec2f &SubRegion::getSe() {
  return se;
}

///////////////////////////////////////////////////////////////////////////
double SubRegion::getReduction() {
  return reduction;
}

///////////////////////////////////////////////////////////////////////////
int SubRegion::getStartLine() {
  return startLine;
}

///////////////////////////////////////////////////////////////////////////
int SubRegion::getStartSample() {
  return startSample;
}
