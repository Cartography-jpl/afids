#ifndef _xlines_h_
#define _xlines_h_

#define	DONT_INTERSECT    0
#define	DO_INTERSECT      1
#define COLLINEAR         2

int lines_intersect( double x1, double y1,   /* First line segment */
		     double x2, double y2,

		     double x3, double y3,   /* Second line segment */
		     double x4, double y4,

		     double * x,
		     double *y);         /* Output value: point of intersection */

#endif
