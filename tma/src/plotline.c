#include <stdlib.h>
#include <stdio.h>
#include "plotline.h"

#define SQR(x) ((x) * (x))
#define SQDIST(xa, ya, xb, yb) (SQR ((xb) - (xa)) + (SQR ((yb) - (ya))))

static double sqdistoline (int x0, int y0, int x1, int y1, int xp, int yp) {
  int pdot1 = (xp - x0) * (x1 - x0) + (yp - y0) * (y1 - y0);
  int dot1 = SQR (x1 - x0) + SQR (y1 - y0);
  double projx = x0 + (x1 - x0) * pdot1 / dot1;
  double projy = y0 + (y1 - y0) * pdot1 / dot1;

  return SQDIST (xp, yp, projx, projy);
}

int plotLine (int x0, int y0, int x1, int y1, pointPlotter plotOne) {
  int dx = (x1 - x0);
  int dy = (y1 - y0);
  int dxPos = dx > 0;
  int dyPos = dy > 0;
  int dxDominates = abs (dx) > abs (dy);
  /* direction holds three bits:
   * dx > 0 where dx is from x0 to x1
   * dy > 0 where dy is from y0 to y1
   * abs(dx) > abs(dy) */

  int direction = (dxPos << 2) | (dyPos << 1) | dxDominates;

  int x, y;

  for (x = x0, y = y0; x != x1 || y!= y1; ) {
    plotOne (x, y);
    
    switch (direction) {
    case 0: /* dx <= 0, dy <=0, abs(dx) <= abs(dy) */
    case 1: /* dx <= 0, dy <=0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x - 1, y) > sqdistoline (x0, y0, x1, y1, x, y - 1))
	y--;
      else
	x--;
      break;
    case 2: /* dx <= 0, dy > 0, abs(dx) <= abs(dy) */
    case 3: /* dx <= 0, dy > 0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x - 1, y) > sqdistoline (x0, y0, x1, y1, x, y + 1))
	y++;
      else
	x--;
      break;
    case 4: /* dx >  0, dy <=0, abs(dx) <= abs(dy) */
    case 5: /* dx >  0, dy <=0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x + 1, y) > sqdistoline (x0, y0, x1, y1, x, y - 1))
	y--;
      else
	x++;
      break;
    case 6: /* dx >  0, dy > 0, abs(dx) <= abs(dy) */
    case 7: /* dx >  0, dy > 0, abs(dx) > abs(dy) */
      if (sqdistoline (x0, y0, x1, y1, x + 1, y) > sqdistoline (x0, y0, x1, y1, x, y + 1))
	y++;
      else
	x++;
      break;
    default:
      printf ("rasterize: bad direction\n");
      return 1;
    }
    plotOne (x, y);
  }
  return 0;
}

