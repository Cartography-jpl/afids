#include <stdlib.h>
#include <stdio.h>

#include "polyClip.h"
#include "linesIntersect.h"

int vertexCount = 0;

Vertex * Vertex_create (double _x, double _y, int _fromClip, Vertex * _next) {
  Vertex * pn = malloc (sizeof (Vertex));

  if (pn) {
    pn -> x = _x;
    pn -> y = _y;
    pn -> fromClip = _fromClip;
    pn -> saved = 0;
    pn -> partner = 0;
    pn -> next = _next;
  }

  return pn;
}

void Vertex_destroy (Vertex * pn) {
  Vertex * p, * pfree;

  p = pn;

  while (p) {
    pfree = p;
    p = p -> next;
    pfree -> next = 0;
    free (pfree);
  }
}

#define SIDE_N 0
#define SIDE_E 1
#define SIDE_W 2
#define SIDE_S 3

/* clip one polygon set alone one edge
   limit is the clip boundary value
   side is one of SIDE_N, SIDE_E, SIDE_W, SIDE_S
 */
static void clipPolygon (Vertex ** polysIn, int polyInCount, Vertex *** polysOut, int * polyOutCount, double limit, int side) {
  /* Based on Sutherland-Hodgman algorithm

     Foreach of the news clip lines
       Foreach segment crossing clip line, ignoring on-line special case by including with one side
         Find its intersection with the clip line
	 Insert the intersection into the polygon, noting that it was added
       Collect and sort the added vertices along the clip line, pairing adjacent, noting pair partners
       For each vertex in the polygon, skipping added intersections
         If it hasn't been saved as part of a new polygon and it's on the right side of the clip line
	   Start a new polygon, setting the vertex as the current vertex
	   Loop
	     Add the current vertex to the polygon
	     Mark the current vertex as saved
	     If the vertex is outside the clipped area
	       mark the polygon as clipped
	     Elseif the current vertex has a pair partner
	       Make it the current vertex
	       Add it to the new polygon
	       Mark it as saved
	     Set the next vertex (possible wrapping) as the current vertex
	     If the current vertex is already saved, break
  */

  Vertex * v = 0;
  double x, y;
  Vertex ** pairedVertices = 0;
  int index;
  int swappedOne;
  Vertex * tempVertexP;
  int preClipIndex;
  int postClipCnt = 0;
  Vertex ** postClipPolys = 0;
  Vertex * currentNew, * currentOld;
  int flag = 0;

  for (preClipIndex = 0; preClipIndex < polyInCount; preClipIndex ++) {
    int pointsOnClip = 0;
    Vertex * firstVertex = polysIn [preClipIndex];
    Vertex * lastVertex = firstVertex;
    while (lastVertex -> next)
      lastVertex = lastVertex -> next;

    /* if this and the next vertex are on *opposite* sides of the clip line, add a vertex
       else if this vertex is *on* the clip line
           if the previous and next are on *opposite* sides of the clip line OR
	      one of previous and next is not on the line, and the other is
	   then
	      set the fromClip flag for this vertex
    */
    
    for (v = firstVertex; v -> next; v = v -> next) {
      switch (side) {
      case SIDE_N:
	flag =
	  (v -> y < limit && v -> next -> y > limit) ||
	  (v -> y > limit && v -> next -> y < limit);
	break;
      case SIDE_E:
	flag =
	  (v -> x < limit && v -> next -> x > limit) ||
	  (v -> x > limit && v -> next -> x < limit);
	break;
      case SIDE_W:
	flag =
	  (v -> x > limit && v -> next -> x < limit) ||
	  (v -> x < limit && v -> next -> x > limit);
	break;
      case SIDE_S:
	flag =
	  (v -> y > limit && v -> next -> y < limit) ||
	  (v -> y < limit && v -> next -> y > limit);
	break;
      }

      /* if this and the next vertex are on opposite sides of the clip line, add a vertex */
      if (flag) {
	if (side == SIDE_N || side == SIDE_S)
	  lines_intersect (-1000.0, limit, 1000.0, limit, v -> x, v -> y, v -> next -> x, v -> next -> y, & x, & y);
	else
	  lines_intersect (limit, -1000.0, limit, 1000.0, v -> x, v -> y, v -> next -> x, v -> next -> y, & x, & y);
	v -> next = Vertex_create (x, y, 1, v -> next);
	pointsOnClip ++;
	v = v -> next;
      } else if ((side==SIDE_N || side==SIDE_S) ? (v->y==limit) : (v->x==limit)) { /* v is on the limit line */
	/* else if this vertex is *on* the clip line
	   if the previous and next are on *opposite* sides of the clip line OR
	   one of previous and next is not on the line, and the other is
	   then
	   set the fromClip flag for this vertex

	   first compute before/after vertices
	*/
	Vertex * beforeV;
	Vertex * afterV;

	/* if this vertex is the first in the vector, then the second to last will be beforeV */
	if (v == firstVertex) {
	  beforeV = firstVertex;
	  while (beforeV -> next -> next)
	    beforeV = beforeV -> next;
	} else {		/* start at the beginning and look forward for this paired vertex */
	  beforeV = firstVertex;
	  while (beforeV -> next != v)
	    beforeV = beforeV -> next;
	}
	/* if this vertex is the last vector, then afterV will be the second */
	if (v -> next == 0)
	  afterV = firstVertex -> next;
	else
	  afterV = v -> next;

	if (side==SIDE_N || side==SIDE_S) { /* then v->y==limit */
	  if ((beforeV -> y < limit && afterV -> y > limit) || /* beforeV and afterV on opposite sides of limit */
	      (beforeV -> y > limit && afterV -> y < limit) || /* ditto */
	      (beforeV -> y == limit && afterV -> y != limit) || /* one is on the limit, the other not */
	      (beforeV -> y != limit && afterV -> y == limit)) { /* ditto */
	    v -> fromClip = 1;
	    pointsOnClip ++;
	  }
	} else {		/* v->x==limit */
	  if ((beforeV -> x < limit && afterV -> x > limit) || /* beforeV and afterV on opposite sides of limit */
	      (beforeV -> x > limit && afterV -> x < limit) || /* ditto */
	      (beforeV -> x == limit && afterV -> x != limit) || /* one is on the limit, the other not */
	      (beforeV -> x != limit && afterV -> x == limit)) { /* ditto */
	    v -> fromClip = 1;
	    pointsOnClip ++;
	  }
	}
      }
    } /* for (v = firstVertex; v -> next; v = v -> next) */

    if (pointsOnClip) {
      /* collect new vertices */
      pairedVertices = (Vertex **) malloc (sizeof (Vertex *) * pointsOnClip);
      for (index = 0, v = firstVertex; v -> next; v = v -> next)
	if (v -> fromClip) {
	  pairedVertices [index] = v;
	  index ++;
	}

      /* just in case the last vertex is on the clip line, but the first one hasn't been included */
      if (v -> fromClip && pairedVertices [0] != firstVertex && index < pointsOnClip) {
	pairedVertices [index] = v;
      }


      /* bubble sort pairedVertices along clip line */  
      swappedOne = 1;
      while (swappedOne) {
	swappedOne = 0;

	if (side==SIDE_N || side==SIDE_S) {
	  for (index = 0; index < pointsOnClip - 1; index ++)
	    if (pairedVertices [index] -> x > pairedVertices [index + 1] -> x) {
	      tempVertexP = pairedVertices [index];
	      pairedVertices [index] = pairedVertices [index + 1];
	      pairedVertices [index + 1] = tempVertexP;
	      swappedOne = 1;
	    }
	} else {			/* (side==SIDE_E || side==SIDE_W) */
	  for (index = 0; index < pointsOnClip - 1; index ++)
	    if (pairedVertices [index] -> y > pairedVertices [index + 1] -> y) {
	      tempVertexP = pairedVertices [index];
	      pairedVertices [index] = pairedVertices [index + 1];
	      pairedVertices [index + 1] = tempVertexP;
	      swappedOne = 1;
	    }
	}
      }
      
      {
	/* reduce adjacent pairs */
	/* if two vertices are adjacent in the ordering that also are adjacent in the vector */
	/* then if their outer neighbors are on the same side, remove both */
	/* else remove the one on the clipped side */
	int acceptedVertices = 0;

	for (index = 0; index < pointsOnClip - 1; index++) {
	  Vertex * earlier;	/* earlier -> next will be pairedVertices [index] */
	  Vertex * later;	/* pairedVertices [index + 1] -> next will be later */
	  int earlierInClip = 0, laterInClip = 0;
	  int adjacent, reversed = 0;
	  Vertex * vp;
	  
	  vp = pairedVertices [index];
	  while (vp &&
		 ((side==SIDE_N || side==SIDE_S) ? (vp -> y == limit) : (vp -> x == limit)) &&
		 vp != pairedVertices [index + 1])
	    vp = vp -> next;
	  if (! vp) {
	    vp = firstVertex;
	    while (vp &&
		   ((side==SIDE_N || side==SIDE_S) ? (vp -> y == limit) : (vp -> x == limit)) &&
		   vp != pairedVertices [index + 1])
	      vp = vp -> next;
	  }
	  if (vp == pairedVertices [index + 1]) {
	    adjacent = 1;
	    reversed = 0;
	  } else
	    adjacent = 0;

	  if (! adjacent) {
	    vp = pairedVertices [index + 1];
	    while (vp &&
		   ((side==SIDE_N || side==SIDE_S) ? (vp -> y == limit) : (vp -> x == limit)) &&
		   vp != pairedVertices [index])
	      vp = vp -> next;
	    if (! vp) {
	      vp = firstVertex;
	      while (vp &&
		     ((side==SIDE_N || side==SIDE_S) ? (vp -> y == limit) : (vp -> x == limit)) &&
		     vp != pairedVertices [index])
		vp = vp -> next;
	    }
	    if (vp == pairedVertices [index]) {
	      adjacent = 1;
	      reversed = 1;
	    } else
	      adjacent = 0;

	  }

	  /* if they're adjacent, compute the neighbors */
	  if (adjacent) {
	    if (! reversed) {
	      /* if this vertex is the first in the vector, then the second to last will be the earlier */
	      if (pairedVertices [index] == firstVertex) {
		earlier = firstVertex;
		while (earlier -> next)
		  earlier = earlier -> next;
	      } else {		/* start at the beginning and look forward for this paired vertex */
		earlier = firstVertex;
		while (earlier -> next != pairedVertices [index])
		  earlier = earlier -> next;
	      }
	      /* if this + 1 vertex is the last vector, then the later will be the second */
	      if (pairedVertices [index + 1] -> next == 0)
		later = firstVertex -> next;
	      else
		later = pairedVertices [index + 1] -> next;
	    } else {
	      /* if this + 1 vertex is the first in the vector, then the second to last will be the earlier */
	      if (pairedVertices [index + 1] == firstVertex) {
		earlier = firstVertex;
		while (earlier -> next)
		  earlier = earlier -> next;
	      } else {		/* start at the beginning and look forward for this + 1 paired vertex */
		earlier = firstVertex;
		while (earlier -> next != pairedVertices [index + 1])
		  earlier = earlier -> next;
	      }
	      /* if this vertex is the last vector, then the later will be the second */
	      if (pairedVertices [index] -> next == 0)
		later = firstVertex -> next;
	      else
		later = pairedVertices [index] -> next;
	    }

	    /* see which, if any, neighbors are in the clip rect */
	    switch (side) {
	    case SIDE_N: 
	      earlierInClip = earlier -> y < limit;
	      laterInClip = later -> y < limit;
	      break;
	    case SIDE_E:
	      earlierInClip = earlier -> x < limit;
	      laterInClip = later -> x < limit;
	      break;
	    case SIDE_W:
	      earlierInClip = earlier -> x > limit;
	      laterInClip = later -> x > limit;
	      break;
	    case SIDE_S:
	      earlierInClip = earlier -> y > limit;
	      laterInClip = later -> y > limit;
	      break;
	    }
	  
	    /* acceptedVertices is a running total of how many we've used so far */
	    /* this checks to see if we're working on the second vertex of a pairing */
	    /* because we allow both earlierInClip and laterInClip in this case */
	    if (! (acceptedVertices % 2) && earlierInClip && laterInClip) {
	      /* they're on the same side, and we're looking for the first of two vertices, so remove both */
	      pairedVertices [index] -> fromClip = 0;
	      pairedVertices [index + 1] -> fromClip = 0;
	    } else {
	      if (! earlierInClip) {
		if (! reversed)
		  pairedVertices [index] -> fromClip = 0;
		else
		  pairedVertices [index + 1] -> fromClip = 0;
	      }
	      if (! laterInClip) {
		if (! reversed)
		  pairedVertices [index + 1] -> fromClip = 0;
		else
		  pairedVertices [index] -> fromClip = 0;
	      }
	      if (earlierInClip || laterInClip)
		acceptedVertices ++;
	    }
	  } else /* if they're adjacent, compute the neighbors */
	    acceptedVertices ++;
	}
      } /* closes block */

      /* mark pairs */
      index = 0;
      while (index < pointsOnClip - 1) {
	int left, right;

	while (index < pointsOnClip && ! pairedVertices [index] -> fromClip)
	  index ++;
	if (index < pointsOnClip) {
	  left = index;
	  index ++;
	} else
	  break;

	while (index < pointsOnClip && ! pairedVertices [index] -> fromClip)
	  index ++;
	if (index < pointsOnClip) {
	  right = index;
	  index ++;
	} else
	  break;

	pairedVertices [left] -> partner = pairedVertices [right];
	pairedVertices [right] -> partner = pairedVertices [left];

	/* tie in both vector end vertices */
	if (pairedVertices [left] == firstVertex)
	  lastVertex -> partner = pairedVertices [right];
	else if (pairedVertices [left] == lastVertex)
	  firstVertex -> partner = pairedVertices [right];

	if (pairedVertices [right] == firstVertex)
	  lastVertex -> partner = pairedVertices [left];
	else if (pairedVertices [right] == lastVertex)
	  firstVertex -> partner = pairedVertices [left];
      }

      free (pairedVertices);
      pairedVertices = 0;
    } /* if (pointsOnClip) */

    /* For each vertex in the polygon */
    for (index = 0, v = firstVertex; v -> next; v = v -> next) {
      /* skipping added intersections */
      /* If it hasn't been saved as part of a new polygon and it's inside of the clip line */
      switch (side) {
      case SIDE_N: flag = v -> y >= limit; break;
      case SIDE_E: flag = v -> x >= limit; break;
      case SIDE_W: flag = v -> x <= limit; break;
      case SIDE_S: flag = v -> y <= limit; break;
      }
      if (v -> fromClip || v -> saved || flag)
	continue;
      
      /* start a new polygon, setting the vertex as the current vertex */
      postClipCnt ++;
      postClipPolys = (Vertex **) realloc (postClipPolys, sizeof (Vertex *) * postClipCnt);
      postClipPolys [postClipCnt - 1] = currentNew = 0;
      currentOld = v;

      while (1) {
	/* Add the current vertex to the polygon */
	if (currentNew) {
	  currentNew -> next = (Vertex *) Vertex_create (currentOld -> x, currentOld -> y, 0, 0);
	  currentNew = currentNew -> next;
	} else {		/* new polygon */
	  postClipPolys [postClipCnt - 1] = currentNew = (Vertex *) Vertex_create (currentOld -> x, currentOld -> y, 0, 0);
	}

	/* Mark the current vertex as saved */
	currentOld -> saved = 1;

	/* if the current vertex has a pair partner */
	if (currentOld -> partner) {
	  /* Make it the current vertex */
	  currentOld = currentOld -> partner;
	  /* Add it to the new polygon */
	  currentNew -> next = (Vertex *) Vertex_create (currentOld -> x, currentOld -> y, 0, 0);
	  currentNew = currentNew -> next;
	  /* Mark it as saved */
	  currentOld -> saved = 1;
	}

	/* check for wrapping */
	if (! currentOld -> next && ! firstVertex -> saved) {
	  currentOld = firstVertex;
	  currentOld -> saved = 1;
	}

	/* Set the next vertex as the current vertex */
	currentOld = currentOld -> next;

	/* If the current vertex is already saved, break */
	if (! currentOld || currentOld -> saved)
	  break;
      }	/* while (1) */

      /* close the new polygon, by duplicating the first vertex */
      if (currentNew -> x != postClipPolys [postClipCnt - 1] -> x ||
	  currentNew -> y != postClipPolys [postClipCnt - 1] -> y)
	currentNew -> next = (Vertex *) Vertex_create (postClipPolys [postClipCnt - 1] -> x, postClipPolys [postClipCnt - 1] -> y, 0, 0);
    } /* for (index = 0, v = firstVertex -> first; v -> next; v = v -> next) */
  } /* for (preClipIndex = 0; preClipIndex < polyInCount; preClipIndex ++) */

  * polysOut = postClipPolys;
  * polyOutCount = postClipCnt;
}

void cropPolygon (Vertex * polyIn, Vertex *** polysOut, int * polyOutCount, double n, double e, double w, double s) {
  Vertex ** tempPolys, ** tempPolys2;
  int tempPolyCount, tempPolyCount2;
  int i;

  clipPolygon (& polyIn, 1, & tempPolys, & tempPolyCount, n, SIDE_N);

  tempPolys2 = tempPolys;
  tempPolyCount2 = tempPolyCount;
  clipPolygon (tempPolys2, tempPolyCount2, & tempPolys, & tempPolyCount, e, SIDE_E);

  /* free tempPolys2 */
  for (i = 0; i < tempPolyCount2; i ++)
    Vertex_destroy (tempPolys2 [i]);

  tempPolys2 = tempPolys;
  tempPolyCount2 = tempPolyCount;
  clipPolygon (tempPolys2, tempPolyCount2, & tempPolys, & tempPolyCount, w, SIDE_W);

  /* free tempPolys2 */
  for (i = 0; i < tempPolyCount2; i ++)
    Vertex_destroy (tempPolys2 [i]);

  tempPolys2 = tempPolys;
  tempPolyCount2 = tempPolyCount;
  clipPolygon (tempPolys2, tempPolyCount2, polysOut, polyOutCount, s, SIDE_S);

  /* free tempPolys2 */
  for (i = 0; i < tempPolyCount2; i ++)
    Vertex_destroy (tempPolys2 [i]);
}
