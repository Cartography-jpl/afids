#ifndef _realwork_h_
#define _realwork_h_

#include "subregion.h"
#include "sblinear.h"
#include "dnet.h"

SubRegion &setup(int dir, const char *regionName, SbVec2f &nw, SbVec2f &se,
		 int lines, int samples,
		 const char *moverName, SbVec2f &start, SbVec2f &end, int endSet,
		 dnet **addrDijkstraNet, const char *rnfName, const char *dataSet, const char *aoiId);

#endif
