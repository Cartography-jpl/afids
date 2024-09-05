#ifndef _shiftNscale_h_
#define _shiftNscale_h_

/* Various geometrical routines are written for integers and/or assume
   positive values. None of them understand that +180 is equivalent to
   -180, when the units are degrees longitude. */

#define xShiftTerm 180.0
#define yShiftTerm 90.0
#define xScaleFactor 100000.0
#define yScaleFactor 100000.0

#define shiftX(x) ((x) + xShiftTerm)
#define unShiftX(x) ((x) - xShiftTerm)
#define shiftY(y) ((y) + yShiftTerm)
#define unShiftY(y) ((y) - yShiftTerm)

#define shiftNscaleX(x) (((x) + xShiftTerm) * xScaleFactor)
#define unShiftNscaleX(x) (((x) / xScaleFactor) - xShiftTerm)
#define shiftNscaleY(y) (((y) + yShiftTerm) * yScaleFactor)
#define unShiftNscaleY(y) (((y) / yScaleFactor) - yShiftTerm)

#endif
