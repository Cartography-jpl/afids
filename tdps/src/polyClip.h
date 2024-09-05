#ifndef _polyClip_h_
#define _polyClip_h_

typedef struct Vertex {
  double x, y;
  short fromClip;			/* generated from clipping */
  short saved;
  struct Vertex * partner;
  struct Vertex * next;
} Vertex;

Vertex * Vertex_create (double _x, double _y, int _fromClip, Vertex * _next);
void Vertex_destroy (Vertex * pn);

void cropPolygon (Vertex * polyIn, Vertex *** polysOut, int * polyOutCount, double n, double e, double w, double s);

#endif
