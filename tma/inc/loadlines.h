#ifndef _loadLines_h_
#define _loadLines_h_

#include "gems.h"

typedef struct {
  int featureCode;
  int length;
  Point2 * points;
} line;

#if 0
int addLinePoint (line * aLine, double x, double y);
#endif

/* returns 0 when happy */
/* path is an FCF file path */
/* lc is a pointer to the line count */
/* lv is a pointer to the line vector */
/* shift is a flag for coordinates processed as non-negative values */
int loadLines (char * path, int * lc, line **lv, int shift);

#endif
