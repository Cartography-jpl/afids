#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "tma.h"
#include "util.h"
#include "element.h"
#include "mover.h"
#include "distanceBetween2Points.h"

#include "tcl.h"

///////////////////////////////////////////////////////////////////////////
//
// Constructor.
//
///////////////////////////////////////////////////////////////////////////
MyRegion::MyRegion(const char *_name, SbVec2f &_nw, SbVec2f &_se,
	       int _lines, int _samples) {
  strncpy(name, _name, 255);
  nw      = _nw;
  se      = _se;
  lines   = _lines;
  samples = _samples;
  double d = distanceBetween2Points(nw.lat(), nw.lon(), se.lat(), se.lon());
  d *= 1000.0;  // Convert to meters.
  metersPerLinePixel = d/(double)lines;
  metersPerSamplePixel = d/(double)samples;
}
//////////////////////////////////////////////////////////////////////////
MyRegion::MyRegion(const char *_name) {
  strncpy(name, _name, 255);
  double nwLat, nwLon, seLat, seLon;
  // Get region data from database.
  Tcl_Interp *interp = getInterp();
  char cmd[512];
  sprintf(cmd, "Data_Select tma_regionThemes {NWLAT NWLON SELAT SELON} {{REGION_NAME %s} {THEME_TYPE Elevation}}",
	  name);
  fprintf(stderr, "%s\n", cmd);
  int code = Tcl_Eval(interp, cmd);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sscanf(interp->result, "{%lf %lf %lf %lf}", &nwLat, &nwLon, &seLat, &seLon);
  } else {
    fprintf(stderr, "Error in:\n%s\n", cmd);
    exit(1);
  }

  nw = SbVec2f(nwLat, nwLon);
  se = SbVec2f(seLat, seLon);

  sprintf(cmd, "Data_Select tma_regionThemes {COUNT_HEIGHT COUNT_WIDTH} {{REGION_NAME %s} {THEME_TYPE Elevation}}",
	  name);
  fprintf(stderr, "%s\n", cmd);
  code = Tcl_Eval(interp, cmd);

  if ((*interp->result != 0) && (code == TCL_OK)) {
    fprintf(stderr, "%s\n", interp->result);
    sscanf(interp->result, "{%d %d}", &lines, &samples);
  } else {
    fprintf(stderr, "Error in:\n%s\n", cmd);
    exit(1);
  }

  cerr << "Read : " << nwLat << " " << nwLon << endl;
  cerr << "Read : " << seLat << " " << seLon << endl;
  cerr << "Read : " << lines << " " << samples << endl;

  double d = distanceBetween2Points(nw.lat(), nw.lon(), se.lat(), se.lon());
  d *= 1000.0;  // Convert to meters.
  metersPerLinePixel = d/(double)lines;
  metersPerSamplePixel = d/(double)samples;
}

///////////////////////////////////////////////////////////////////////////
//
// Destructor.
//
///////////////////////////////////////////////////////////////////////////
MyRegion::~MyRegion() {
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
void MyRegion::latLon2lineSample(int *line, int *sample,
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
}

///////////////////////////////////////////////////////////////////////////
void MyRegion::lineSample2latLon(double *lat, double *lon,
                               int line, int sample)
{
double proportion;

proportion = (double) line/(double) lines;
*lat = nw.lat() + proportion * (se.lat() - nw.lat());

proportion = (double) sample/(double) samples;
*lon = nw.lon() + proportion * (se.lon() - nw.lon());
}

///////////////////////////////////////////////////////////////////////////
int MyRegion::getLines() {
  return lines;
}

///////////////////////////////////////////////////////////////////////////
int MyRegion::getSamples() {
  return samples;
}

///////////////////////////////////////////////////////////////////////////
double MyRegion::getMetersPerLinePixel() {
  return metersPerLinePixel;
}

///////////////////////////////////////////////////////////////////////////
double MyRegion::getMetersPerSamplePixel() {
  return metersPerSamplePixel;
}

///////////////////////////////////////////////////////////////////////////
SbVec2f &MyRegion::getNw() {
  return nw;
}

///////////////////////////////////////////////////////////////////////////
SbVec2f &MyRegion::getSe() {
  return se;
}

///////////////////////////////////////////////////////////////////////////
char *MyRegion::getName() {
  return name;
}











