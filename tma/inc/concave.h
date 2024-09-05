#ifndef _concave_h_
#define _concave_h_

#include "gems.h"

#ifdef __cplusplus
extern "C" {
#endif

void concave(int nvert,		  /* number of vertices */
	     Point2 *point,	  /* vertices of polygon */
	     GemsWindow *win,	  /* screen clipping window */
	     void (*spanproc)(int y, int xl, int xr));  /* called for each span of pixels */

#ifdef __cplusplus
}
#endif

#endif
