#ifndef _plotline_h_
#define _plotline_h_

typedef void  pointPlotter (int x, int y);

int plotLine (int x0, int y0, int x1, int y1, pointPlotter plotOne);

#endif
