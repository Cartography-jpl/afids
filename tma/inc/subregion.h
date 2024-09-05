#ifndef _subregion_h_
#define _subregion_h_

#include "region.h"
#include "sblinear.h"

class SubRegion {
  private:
    SbVec2f nw;
    SbVec2f se;
    int lines, samples;
    double metersPerLinePixel, metersPerSamplePixel;
    int startLine, startSample, endLine, endSample;
    double reduction;

  public:
    SubRegion(MyRegion &region,
              SbVec2f &nw, SbVec2f &se, int inLines, int inSamples);
    ~SubRegion();
    double getReduction();
    int getStartLine();
    int getStartSample();
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
};

#endif

