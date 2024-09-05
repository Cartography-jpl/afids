#include <stdio.h>

#include "polyClip.h"

int main (int argc, char * argv []) {

#if 1
  printf ("case 1:\n");

  {
    Vertex * v = Vertex_create (1, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (2, 3, 0, v);
    v = Vertex_create (3, 1, 0, v);
    v = Vertex_create (1, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 4, 4, 0, 0);

    printf ("match:\npoly (1.0, 1.0) (3.0, 1.0) (2.0, 3.0) (1.0, 1.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 2:\n");

  {
    Vertex * v = Vertex_create (0, 0, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (5, 5, 0, v);
    v = Vertex_create (10, 0, 0, v);
    v = Vertex_create (0, 0, 0, v);
    cropPolygon (v, & polys, & polyCount, 3, 6, 4, 1);

    printf ("match:\npoly (6.0, 3.0) (4.0, 3.0) (4.0, 1.0) (6.0, 1.0) (6.0, 3.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 3:\n");

  {
    Vertex * v = Vertex_create (1, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (1, 3, 0, v);
    v = Vertex_create (2, 3, 0, v);
    v = Vertex_create (2, 2, 0, v);
    v = Vertex_create (4, 2, 0, v);
    v = Vertex_create (4, 1, 0, v);
    v = Vertex_create (1, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 4, 3, 0, 0);

    printf ("match:\npoly (1.0, 1.0) (3.0, 1.0) (3.0, 2.0) (2.0, 2.0) (2.0, 3.0) (1.0, 3.0) (1.0, 1.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 4:\n");

  {
    Vertex * v = Vertex_create (2, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (2, 2, 0, v);
    v = Vertex_create (3, 2, 0, v);
    v = Vertex_create (3, 1, 0, v);
    v = Vertex_create (2, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 3, 3, 0, 0);

    printf ("match:\npoly (2.0, 1.0) (3.0, 1.0) (3.0, 2.0) (2.0, 2.0) (2.0, 1.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 5:\n");

  {
    Vertex * v = Vertex_create (0, 0, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 3, 0, v);
    v = Vertex_create (3, 3, 0, v);
    v = Vertex_create (3, 0, 0, v);
    v = Vertex_create (0, 0, 0, v);
    cropPolygon (v, & polys, & polyCount, 2, 3, 2, 1);

    printf ("match:\npoly (3.0, 2.0) (2.0, 2.0) (2.0, 1.0) (3.0, 1.0) (3.0, 2.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 6:\n");

  {
    Vertex * v = Vertex_create (0, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (5, 6, 0, v);
    v = Vertex_create (10, 1, 0, v);
    v = Vertex_create (0, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 4, 8, 1, 0);

    printf ("match:\npoly (8.0, 1.0) (8.0, 3.0) (7.0, 4.0) (3.0, 4.0) (1.0, 2.0) (1.0, 1.0) (8.0, 1.0)\n");

    for (pi = 0; pi < polyCount; pi ++) {
      printf ("poly ");
      for (v = polys [pi]; v; v = v -> next)
	printf ("(%.1f, %.1f) ", v -> x, v -> y);
      printf ("\n");
    }    
  }

  printf ("case 7:\n");

  {
    Vertex * v = Vertex_create (0, 0, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 5, 0, v);
    v = Vertex_create (4, 5, 0, v);
    v = Vertex_create (4, 4, 0, v);
    v = Vertex_create (1, 4, 0, v);
    v = Vertex_create (1, 1, 0, v);
    v = Vertex_create (4, 1, 0, v);
    v = Vertex_create (4, 0, 0, v);
    v = Vertex_create (0, 0, 0, v);
    cropPolygon (v, & polys, & polyCount, 3, 3, 2, 2);

    printf ("match:\nno polys left\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.1f, %.1f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 8:\n");

  {
    Vertex * v = Vertex_create (0, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 7, 0, v);
    v = Vertex_create (1, 8, 0, v);
    v = Vertex_create (3, 6, 0, v);
    v = Vertex_create (1, 4, 0, v);
    v = Vertex_create (1, 2, 0, v);
    v = Vertex_create (4, 2, 0, v);
    v = Vertex_create (4, 3, 0, v);
    v = Vertex_create (3, 4, 0, v);
    v = Vertex_create (5, 6, 0, v);
    v = Vertex_create (7, 6, 0, v);
    v = Vertex_create (7, 1, 0, v);
    v = Vertex_create (0, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 8, 8, 3, 0);

    printf ("match:\npoly (7.0, 1.0) (7.0, 6.0) (5.0, 6.0) (3.0, 4.0) (4.0, 3.0) (4.0, 2.0) (3.0, 2.0) (3.0, 1.0) (7.0, 1.0)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.1f, %.1f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 8b:\n");

  {
    Vertex * v = Vertex_create (0, 1, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 7, 0, v);
    v = Vertex_create (3, 7, 0, v);
    v = Vertex_create (1, 5, 0, v);
    v = Vertex_create (1, 2, 0, v);
    v = Vertex_create (4, 2, 0, v);
    v = Vertex_create (4, 3, 0, v);
    v = Vertex_create (3, 4, 0, v);
    v = Vertex_create (5, 4, 0, v);
    v = Vertex_create (5, 1, 0, v);
    v = Vertex_create (0, 1, 0, v);
    cropPolygon (v, & polys, & polyCount, 8, 7, 3, 0);

    printf ("match:\npoly (5.0, 1.0) (5.0, 4.0) (3.0, 4.0) (4.0, 3.0) (4.0, 2.0) (3.0, 2.0) (3.0, 1.0) (5.0, 1.0)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.1f, %.1f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 9:\n");

  {
    Vertex * v = Vertex_create (0, 0, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 5, 0, v);
    v = Vertex_create (4, 5, 0, v);
    v = Vertex_create (4, 4, 0, v);
    v = Vertex_create (1, 4, 0, v);
    v = Vertex_create (1, 1, 0, v);
    v = Vertex_create (4, 1, 0, v);
    v = Vertex_create (4, 0, 0, v);
    v = Vertex_create (0, 0, 0, v);
    cropPolygon (v, & polys, & polyCount, 6, 5, 2, -1);

    printf ("match:\npoly (4.0, 0.0) (4.0, 1.0) (2.0, 1.0) (2.0, 0.0) (4.0, 0.0)\npoly (4.0, 4.0) (4.0, 5.0) (2.0, 5.0) (2.0, 4.0) (4.0, 4.0)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.1f, %.1f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 9b:\n");

  {
    Vertex * v = Vertex_create (0, 0, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;
    v = Vertex_create (0, 5, 0, v);
    v = Vertex_create (4, 5, 0, v);
    v = Vertex_create (4, 4, 0, v);
    v = Vertex_create (1, 4, 0, v);
    v = Vertex_create (1, 1, 0, v);
    v = Vertex_create (4, 1, 0, v);
    v = Vertex_create (4, 0, 0, v);
    v = Vertex_create (0, 0, 0, v);
    cropPolygon (v, & polys, & polyCount, 5, 4, 1, 0);

    printf ("match:\npoly (4.0, 1.0) (1.0, 1.0) (1.0, 0.0) (4.0, 0.0) (4.0, 1.0)\npoly (4.0, 4.0) (4.0, 5.0) (1.0, 5.0) (1.0, 4.0) (4.0, 4.0)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.1f, %.1f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 10:\n");

  {
    Vertex * v = Vertex_create (38.66552734375, 35, 0, 0);
    Vertex ** polys;
    int polyCount;
    int pi;

    v = Vertex_create (38.650943756103516, 35.019084930419922, 0, v); 
    v = Vertex_create (38.64141845703125, 35.025917053222656, 0, v); 
    v = Vertex_create (38.631362915039062, 35.025859832763672, 0, v); 
    v = Vertex_create (38.614749908447266, 35.013862609863281, 0, v); 
    v = Vertex_create (38.60211181640625, 35.006832122802734, 0, v); 
    v = Vertex_create (38.601444244384766, 35.006443023681641, 0, v); /* sufficient */
    v = Vertex_create (38.597137451171875, 35, 0, v); 
    v = Vertex_create (38.596500396728516, 34.99908447265625, 0, v); 
    v = Vertex_create (38.595417022705078, 34.992668151855469, 0, v); 
    v = Vertex_create (38.602832794189453, 34.974334716796875, 0, v); 
    v = Vertex_create (38.611804962158203, 34.967056274414062, 0, v); 
    v = Vertex_create (38.625720977783203, 34.967109680175781, 0, v); 
    v = Vertex_create (38.650138854980469, 34.976387023925781, 0, v); 
    v = Vertex_create (38.670082092285156, 34.991165161132812, 0, v); 
    v = Vertex_create (38.670055389404297, 34.994388580322266, 0, v);  /* sufficient */
    v = Vertex_create (38.66552734375, 35, 0, v);  /* sufficient */

    cropPolygon (v, & polys, & polyCount, 36, 39, 35, 35);
    printf ("match:\npoly (38.601, 35.006) (38.602, 35.007) (38.615, 35.014) (38.631, 35.026) (38.641, 35.026) (38.651, 35.019) (38.666, 35.000) (38.597, 35.000) (38.601, 35.006) \n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 11a left:\n");

  {
    Vertex * v = 0;
    Vertex ** polys;
    int polyCount;
    int pi;
    
    v = Vertex_create (45.966774, 41.729729, 0, v);
    v = Vertex_create (45.999146, 41.748425, 0, v);
    v = Vertex_create (46.027710, 41.736794, 0, v);
    v = Vertex_create (46.000000, 41.731956, 0, v); /* bottom -- should discard this */
    v = Vertex_create (46.000000, 41.733711, 0, v); /* top */
    v = Vertex_create (45.989822, 41.731720, 0, v); /* left */
    v = Vertex_create (46.000000, 41.730263, 0, v);
    v = Vertex_create (46.000000, 41.724651, 0, v);
    v = Vertex_create (46.026814, 41.715012, 0, v);
    v = Vertex_create (46.000000, 41.700661, 0, v);
    v = Vertex_create (46.000000, 41.705612, 0, v);
    v = Vertex_create (45.966774, 41.729729, 0, v);

    cropPolygon (v, & polys, & polyCount, 44, 46, 40, 34);
    printf ("match:\npoly (45.967, 41.730) (46.000, 41.706) (46.000, 41.730) (45.990, 41.732) (46.000, 41.734) (46.000, 41.748) (45.999, 41.748) (45.967, 41.730)\n");
    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 11b left:\n");

  {
    Vertex * v = 0;
    Vertex ** polys;
    int polyCount;
    int pi;
    
    v = Vertex_create (45.966774, 41.729729, 0, v);
    v = Vertex_create (46.000000, 41.705612, 0, v);
    v = Vertex_create (46.000000, 41.700661, 0, v);
    v = Vertex_create (46.026814, 41.715012, 0, v);
    v = Vertex_create (46.000000, 41.724651, 0, v);
    v = Vertex_create (46.000000, 41.730263, 0, v);
    v = Vertex_create (45.989822, 41.731720, 0, v);
    v = Vertex_create (46.000000, 41.733711, 0, v);
    v = Vertex_create (46.000000, 41.731956, 0, v);
    v = Vertex_create (46.027710, 41.736794, 0, v);
    v = Vertex_create (45.999146, 41.748425, 0, v);
    v = Vertex_create (45.966774, 41.729729, 0, v);

    cropPolygon (v, & polys, & polyCount, 44, 46, 40, 34);
    printf ("match:\npoly (45.967, 41.730) (45.999, 41.748) (46.000, 41.748) (46.000, 41.734) (45.990, 41.732) (46.000, 41.730) (46.000, 41.706) (45.967, 41.730)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 11a right:\n");

  {
    Vertex * v = 0;
    Vertex ** polys;
    int polyCount;
    int pi;
    
    v = Vertex_create (45.966774, 41.729729, 0, v);
    v = Vertex_create (45.999146, 41.748425, 0, v);
    v = Vertex_create (46.027710, 41.736794, 0, v);
    v = Vertex_create (46.000000, 41.731956, 0, v);
    v = Vertex_create (46.000000, 41.733711, 0, v);
    v = Vertex_create (45.989822, 41.731720, 0, v);
    v = Vertex_create (46.000000, 41.730263, 0, v);
    v = Vertex_create (46.000000, 41.724651, 0, v);
    v = Vertex_create (46.026814, 41.715012, 0, v);
    v = Vertex_create (46.000000, 41.700661, 0, v);
    v = Vertex_create (46.000000, 41.705612, 0, v);
    v = Vertex_create (45.966774, 41.729729, 0, v);

    cropPolygon (v, & polys, & polyCount, 44, 180, 46, 34);
    printf ("match:\npoly (46.027, 41.715) (46.000, 41.725) (46.000, 41.701) (46.027, 41.715)\n");
    printf ("poly (46.028, 41.737) (46.000, 41.748) (46.000, 41.732) (46.028, 41.737)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  printf ("case 11b right:\n");

  {
    Vertex * v = 0;
    Vertex ** polys;
    int polyCount;
    int pi;
    
    v = Vertex_create (45.966774, 41.729729, 0, v);
    v = Vertex_create (46.000000, 41.705612, 0, v);
    v = Vertex_create (46.000000, 41.700661, 0, v);
    v = Vertex_create (46.026814, 41.715012, 0, v);
    v = Vertex_create (46.000000, 41.724651, 0, v);
    v = Vertex_create (46.000000, 41.730263, 0, v);
    v = Vertex_create (45.989822, 41.731720, 0, v);
    v = Vertex_create (46.000000, 41.733711, 0, v);
    v = Vertex_create (46.000000, 41.731956, 0, v);
    v = Vertex_create (46.027710, 41.736794, 0, v);
    v = Vertex_create (45.999146, 41.748425, 0, v);
    v = Vertex_create (45.966774, 41.729729, 0, v);

    cropPolygon (v, & polys, & polyCount, 44, 180, 46, 34);
    printf ("match:\npoly (46.028, 41.737) (46.000, 41.732) (46.000, 41.748) (46.028, 41.737)\n");
    printf ("poly (46.027, 41.715) (46.000, 41.701) (46.000, 41.725) (46.027, 41.715) \n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

#endif

  printf ("case 12a left:\n");

  {
    Vertex * v = 0;
    Vertex ** polys;
    int polyCount;
    int pi;
    
    v = Vertex_create (46.005428, 41.699635, 0, v);
    v = Vertex_create (46.000000, 41.700661, 0, v);
    v = Vertex_create (46.000000, 41.701454, 0, v);
    v = Vertex_create (46.000000, 41.705360, 0, v);
    v = Vertex_create (46.000000, 41.705612, 0, v);
    v = Vertex_create (45.992912, 41.705833, 0, v);
    v = Vertex_create (45.989246, 41.746532, 0, v);
    v = Vertex_create (46.000000, 41.758461, 0, v);
    v = Vertex_create (46.000000, 41.754894, 0, v);
    v = Vertex_create (46.005291, 41.739491, 0, v);
    v = Vertex_create (46.005249, 41.732948, 0, v);
    v = Vertex_create (46.000000, 41.731956, 0, v);
    v = Vertex_create (46.000000, 41.733711, 0, v);
    v = Vertex_create (45.998276, 41.734577, 0, v);
    v = Vertex_create (45.996151, 41.731861, 0, v);
    v = Vertex_create (45.997269, 41.728210, 0, v);
    v = Vertex_create (46.000000, 41.730263, 0, v);
    v = Vertex_create (46.000000, 41.724651, 0, v);
    v = Vertex_create (46.005344, 41.721367, 0, v);
    v = Vertex_create (46.005428, 41.699635, 0, v);

    cropPolygon (v, & polys, & polyCount, 44, 46, 40, 34);
    printf ("match:\npoly (45.997, 41.728) (45.996, 41.732) (45.998, 41.735) (46.000, 41.734) (46.000, 41.758) (45.989, 41.747) (45.993, 41.706) (46.000, 41.706) (46.000, 41.730) (45.997, 41.728)\n");

    if (! polyCount)
      printf ("no polys left\n");
    else
      for (pi = 0; pi < polyCount; pi ++) {
	printf ("poly ");
	for (v = polys [pi]; v; v = v -> next)
	  printf ("(%.3f, %.3f) ", v -> x, v -> y);
	printf ("\n");
      }    
  }

  return 0;
}
