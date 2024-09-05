#ifndef _minpath_h_
#define _minpath_h_

#include "dnet.h"

// keep the dnet around for terrain queries
extern dnet *minPathDijkstraNet;

void tmaMinPath(const char *regionName, SbVec2f &nw, SbVec2f &se, int lines, int samples,
                const char *moverName,  SbVec2f &start, SbVec2f &end, const char *rnfName, const char *dataSet, const char *aoiId);

#endif
