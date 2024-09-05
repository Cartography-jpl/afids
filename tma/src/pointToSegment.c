#include <math.h>
#include "pointToSegment.h"

typedef struct {
  double x, y;
} Vec2D;

static Vec2D Vec2DMake (double x, double y) {
  Vec2D v = {x, y};
  return v;
}

static Vec2D Vec2DAdd (Vec2D a, Vec2D b) {
  return Vec2DMake (a.x + b.x, a.y + b.y);
}

static Vec2D Vec2DSubtract (Vec2D a, Vec2D b) {
  return Vec2DMake (a.x - b.x, a.y - b.y);
}

static double Vec2DDot (Vec2D a, Vec2D b) {
  return a.x * b.x + a.y * b.y;
}

static double Vec2DMagSquared (Vec2D v) {
  return v.x * v.x + v.y * v.y;
}

static Vec2D Vec2DScale (Vec2D v, double x) {
  return Vec2DMake (v.x * x, v.y * x);
}

double pointToSegment (double startX, double startY,
		       double endX, double endY,
		       double x, double y,
		       double * xp, double * yp) {
  Vec2D start = Vec2DMake (startX, startY);
  Vec2D end = Vec2DMake (endX, endY);
  Vec2D p = Vec2DMake (x, y);
  Vec2D b = Vec2DSubtract (end, start);
  double bMagSquared = Vec2DMagSquared (b);
  Vec2D pMinusStart = Vec2DSubtract (p, start);
  double pMinusStartDotB = Vec2DDot (pMinusStart, b);

  if (pMinusStartDotB <= 0) {
    * xp = start.x;
    * yp = start.y;
  } else {
    if (pMinusStartDotB >= bMagSquared) {
      * xp = end.x;
      * yp = end.y;
    } else {
      Vec2D v = Vec2DAdd (start, Vec2DScale (b, pMinusStartDotB / bMagSquared));
      * xp = v.x;
      * yp = v.y;
    }
  }

  return DIST (x, y, *xp, *yp);
}

