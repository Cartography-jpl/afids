#ifndef _contours_h_
#define _contours_h_

#include <vector>

void tmaCorridor(const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
                 const char *moverName,  SbVec2f &start, SbVec2f &end,
		 const char *rnfName, const char *dataSet, const char * aoiId);
void tmaContours(const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
                 const char *moverName,  SbVec2f &start,
		 const char *rnfName, const char *dataSet, const char *aoiId);
void tmaMultiSourceContours(const char *regionName, SbVec2f &nw, SbVec2f &se,
			    int lines, int samples,
			    const char *moverName,  vector <SbVec2f> & locations,
			    const char *rnfName, const char *dataSet, const char *aoiId);

#endif
