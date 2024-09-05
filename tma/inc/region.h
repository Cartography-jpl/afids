#ifndef _region_h_
#define _region_h_

#include "sblinear.h"
/* Former name "Region" conflicts with a typedef in Xutil.h on the
   Mac. Rename this so we don't have a conflict. */

class MyRegion {
  private:
    SbVec2f nw;
    SbVec2f se;
    int lines, samples;
    double metersPerLinePixel, metersPerSamplePixel;
    char name[256];

  public:
    MyRegion(const char *_name);
    MyRegion(const char *_name, SbVec2f &_nw, SbVec2f &_se,
           int _lines, int _samples);
    ~MyRegion();
    void latLon2lineSample(int *line, int *sample,
			   double lat, double lon);
    void lineSample2latLon(double *lat, double *lon,
                           int line, int sample);
    int getLines();
    int getSamples();
    double getMetersPerLinePixel();
    double getMetersPerSamplePixel();
    SbVec2f &getNw();
    SbVec2f &getSe();
    char *getName();
};

#endif

