#ifndef _pointToSegment_h_
#define _pointToSegment_h_

#include <math.h>

#ifndef SQR
#define SQR(x) ((x) * (x))
#endif
#define DIST(x1, y1, x2, y2) (sqrt (SQR ((x2) - (x1)) + SQR ((y2) - (y1))))

double pointToSegment (double startX, double startY,
		       double endX, double endY,
		       double x, double y,
		       double * xp, double * yp);

#endif

