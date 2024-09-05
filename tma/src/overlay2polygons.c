#include <stdio.h>
#include <stdlib.h>

/*****************************************************************************
*
*	polygons.c
**
*	Vectorizes the outline of an contour in a set of sampled
*	data.  Uses Freeman chain encoding.
*
*****************************************************************************/

typedef struct {
    int line;
    int sample;
} lspoint;

/***	a myVector is one link in a simple chain that follows
	the edge of a contour from sample point to sample point	***/

struct myvector
{
	short		dir;
	struct myvector *	next;
};

typedef struct myvector	myVector;

/***	these are the 'dir' values in a myVector:

THIS ASSUMES Y+ IS UP AND X+ IS RIGHT.
BUT SINCE Y+ IS DOWN IN AN ARRAY, REPORT() SWAPS DIRECTIONS.

	0	right
	1	right and up
	2	up
	3	left and up
	4	left
	5	left and down
	6	down
	7	right and down		***/

/***	this points to the first member in the Freeman chain of myvectors	***/

static myVector *	chain;

/***	this is the time of the contour to be outlined		***/

static double contourTime;

unsigned char **image;
int lines, samples;

/*****************************************************************************
*
*	in_cont(x, y)
*
*	Determines whether the sample point at 'x, y' is within the contour
*	being outlined.  Points outside of the array of samples are not
*	in the contour.
*
*	Returns 0 if the point is not in the contour.
*	Returns 1 if the point is     in the contour.
*
*****************************************************************************/

short
in_cont(int x, int y)
{
	if ( (x < 0) || (x > samples-1) || (y < 0) || (y > lines-1) )
		return(0);
	if (image[y][x] == contourTime)
		return(1);
	else
		return(0);
}

/*****************************************************************************
*
*	probe(x, y, dir, new_x, new_y)
*
*	Checks a sample neighboring 'x, y' to see if it is in the contour
*	being outlined.  'dir' specifies which neighboring sample to check.
*	'new_x, new_y' always get the coordinates of the neighbor.
*
*	Returns 0 if the neighbor is not in the contour.
*	Returns 1 if the neighbor is     in the contour.
*
*****************************************************************************/

short
probe(int x, int y, int dir, int *new_x, int *new_y)
{
	/***	figure out coordinates of neighbor	***/

	if ( (dir < 2) || (dir > 6) )
		++x;

	if ( (dir > 2) && (dir < 6) )
		--x;

	if ( (dir > 0) && (dir < 4) )
		++y;

	if (dir > 4)
		--y;

	/***	always return the new coordinates	***/

	*new_x = x;
	*new_y = y;

	/***	determine if the new sample point is in the contour	***/

	return(in_cont(x, y));
}

/*****************************************************************************
*
*	neighbor(x, y, last_dir, new_x, new_y)
*
*	Finds a neighbor of the sample at 'x, y' that is in the same
*	contour.  Always follows the contour in a clockwise direction.
*	'last_dir' is the direction that was used to get to 'x, y'
*	when it was found.  'new_x, new_y' always get the coordinates
*	of the neighbor.
*
*	This procedure should only be called for a sample that has at
*	least one neighbor in the same contour.
*
*	Returns the direction to the neighbor.
*
*****************************************************************************/

int
neighbor(int x, int y, int last_dir, int *new_x, int *new_y)
{
int	n;
int	new_dir;

	/***	figure out where to start looking for a neighbor --
		always look ahead and to the left of the last direction

		if the last myvector was 0
		then start looking at  1

		if the last myvector was 1
		then start looking at  3

		if the last myvector was 2
		then start looking at  3

		if the last myvector was 3
		then start looking at  5

		if the last myvector was 4
		then start looking at  5

		if the last myvector was 5
		then start looking at  7

		if the last myvector was 6
		then start looking at  7

		if the last myvector was 7
		then start looking at  1	***/

	if (last_dir & 0x01)
	{
		/***	last dir is odd --
			add 2 to it		***/

		new_dir = last_dir + 2;
	}
	else
	{
		/***	last dir is even --
			add 1 to it		***/

		new_dir = last_dir + 1;
	}

	/***	keep new_dir in the range 0 through 7	***/

	if (new_dir > 7)
		new_dir -= 8;

	/***	probe the neighbors, looking for one on the edge	***/

	for (n = 0; n < 8; n++)
	{
		if (probe(x, y, new_dir, new_x, new_y))
		{
			/***	found the next clockwise edge neighbor --
				its coordinates have already been
				stuffed into new_x, new_y		***/

			return(new_dir);
		}
		else
		{
			/***	check the next clockwise neighbor	***/

			if (--new_dir < 0)
				new_dir += 8;
		}
	}

/***	should never exit routine by this way!	***/
return(-1);
}

/*****************************************************************************
*
*	build()
*
*	Builds a Freeman chain of myvectors describing the edge of the
*	contour with elevation 'elev'.  Always follows the contour
*	in a clockwise direction.  Uses 'start_x, start_y' as tentative
*	starting point; modifies them to hold coordinates of first point
*	in chain.
*
*	Returns 0 if unsuccessful.
*	Returns 1 if   successful.
*
*****************************************************************************/

short
build(int *start_x, int *start_y)
{
int		x;
int		y;
int		new_x;
int		new_y;
int		dir;
int		last_dir;
myVector *	newV;
myVector *	prev;

	/***	go left in the starting row until out of the contour	***/
	while (in_cont(*start_x, *start_y))
	{
	  /* printf("inCont %d %d\n", *start_x, *start_y); */
	  (*start_x)--;
	}

	/***	move back right one point, to the leftmost edge
		in the contour, in that row			***/

	(*start_x)++;
	/* printf("Final: inCont %d %d\n", *start_x, *start_y); */

	/***	create the head of the chain	***/

	chain = (myVector *)0;
	prev = (myVector *)0;

	/***	check if the starting point
		has no neighbors in the contour --
		the starting direction to check is arbitrary	***/

	x = *start_x;
	y = *start_y;

	dir = 0;

	for ( ; ; )
	{
		if (probe(x, y, dir, &new_x, &new_y))
		{
			/***	found a neighbor in that direction
				(its coordinates are in new_x, new_y
				but we don't use them here)		***/

			break;
		}

		/***	try next direction	***/

		if (++dir == 8)
		{
			/***	starting point has no neighbors --
				make the chain one myvector long	***/
			
			/***	allocate storage for the myvector	***/

			/* chain = new myVector; */
			if ( (chain = (myVector *)malloc(sizeof(myVector))) ==
			     0)
			{
				fprintf(stderr,
					"Insufficient memory available.\n");
				return(1);
			}

			/***	fill in the myvector --
				the direction is arbitrary,
				since the point is isolated	***/

			chain->dir = 0;
			chain->next = (myVector *)0;

			return(1);
		}
	}

	/***	get ready to follow the edge --
		since we are at the left edge,
		force initial probe to be to upper left
		by initializing last_dir to 1		***/

	last_dir = 1;

	/***	follow the edge clockwise, building a Freeman chain	***/

	for ( ; ; )
	{
		/***	get the next point on the edge
			and the myvector to it		***/

		dir = neighbor(x, y, last_dir, &new_x, &new_y);

		/***	allocate storage for the new myvector	***/

		// Changed malloc to new.
		/* newV = new myVector; */

		if ( (newV = (myVector *)malloc(sizeof(myVector))) ==
		     0)
		  {
		    fprintf(stderr, "Insufficient memory available.\n");
		    return(1);
		  }

		/***	fill in the new myvector	***/

		newV->dir = dir;
		newV->next = (myVector *)0;

		if (prev)
		{
			/***	add it to the existing chain	***/

			prev->next = newV;
		}
		else
		{
			/***	it is the first myvector in the chain	***/

			chain = newV;
		}

		/***	maybe done with contour	***/

		if ( (new_x == *start_x) && (new_y == *start_y) )
			return(1);

		/***	else get ready to continue following the edge	***/

		prev = newV;
		x = new_x;
		y = new_y;
		last_dir = dir;
	}
}

/*****************************************************************************
*
*	report()
*
*	Follows the Freeman chain of myvectors describing the edge of the
*	contour with elevation 'elev'.  Reports the elevation, start point,
*	direction myvectors, and the number of myvectors in the chain.
*
*****************************************************************************/

#define MAX_COORDS 2000

int
report(lspoint *contour, int start_x, int start_y)
{
myVector *	p;
myVector *    head;
int		n;
int dirX[8] = {1, 1, 0, -1, -1, -1, 0, 1};
int dirY[8] = {0, 1, 1, 1, 0, -1, -1, -1};
int x, y;

	printf("Start point (line, sample) = %d, %d\n", start_y, start_x);

	contour[0].line = start_y;
	contour[0].sample = start_x;

	p = chain;
	n = 1;

        x = start_x;
	y = start_y;
	while (p && n < MAX_COORDS)
	{
		x += dirX[p->dir];
		y += dirY[p->dir];
		/*printf("%d %d\n", x, y);*/

		contour[n].line = y;
		contour[n].sample = x;

		head = p;
		p = p->next;
		free( head );
		n++;

	}

	if (n >= MAX_COORDS)
	  printf("%s:%d: EXCEEDED MAX_COORDS\n", __FILE__, __LINE__);
	else if (n > 1)
	  printf("%d myvectors in the chain.\n", n);
	else
	  printf("1 myvector in the chain.\n");

return (n);
}

/*****************************************************************************
*
*	Describes the outline of an contour in the sampled data.
*
*	Returns  0 if   successful.
*	Returns -1 if unsuccessful.
*
*****************************************************************************/

/*	(start_x, start_y) is a point inside the contour. */
/*	contourTime is the boundary time. */

int getOneContour(lspoint *contour, double passedContourTime,
		int start_y, int start_x)
{
int localStart_y = start_y;
int localStart_x = start_x;

printf("Point inside contour : %d %d\n", start_y, start_x);
 
	contourTime = passedContourTime;	// static global.

	/***	follow the edge of the contour,
		building a Freeman chain of myvectors		***/

	if (build(&localStart_x, &localStart_y))
	{
		/***	report the results	***/

		return( report(contour, localStart_x, localStart_y) );
	}
	else
	{
		/***	failed	***/

		return(-1);
	}
}

int main (int argc, char * argv []) {
  char * argPattern =
    "inFile outFile numLevels";
  int maxValue;
  int line, sample, pixel, found;
  FILE *inFilePtr, *outFilePtr;
  lspoint coords[MAX_COORDS];
  int numPoints, level;
  int numLevels, n;
  int lineOffset, sampleOffset, lineN, sampleN;

  if (argc != 4) {
    fprintf(stderr, "Usage: %s %s\n", argv[0], argPattern);
    return 1;
  }

  /* Open in file */
  if ( (inFilePtr = fopen(argv[1], "r")) == NULL ) {
    fprintf (stderr, "%s: could not open %s for read.\n",
	     argv[0], argv[1]);
    return 1;
  }

  /* Open out file */
  if ( (outFilePtr = fopen(argv[2], "w")) == NULL ) {
    fprintf (stderr, "%s: could not open %s for write.\n",
	     argv[0], argv[2]);
    fclose(inFilePtr);
    return 1;
  }

  /* get numLevels */
  numLevels = atoi(argv[3]);
  printf("numLevels = %d\n", numLevels);

  /* Read inFile (pgm format) */
  fscanf(inFilePtr, "P5\n%d %d\n%d", &samples, &lines, &maxValue);
  /* Read the 1 white space character. */
  printf("White space character = %d\n", fgetc(inFilePtr));

  /* malloc 2d image and read in image */
  image = (unsigned char **) malloc(lines*sizeof(unsigned char *));
  for (line=0; line<lines; line++) {
    image[line] = (unsigned char *) malloc(samples*sizeof(unsigned char));
    fread(image[line], sizeof(unsigned char), samples, inFilePtr);
  }
  fclose(inFilePtr);

  /* for each level
     go through and extract the contours and
     save them to file. */
  for (level=0; level<numLevels; level++) {
    pixel = 2*level+1;
    do {
      /* Find point in image with pixel value = 2*level+1 */
      found = 0;
      for (line=0; line<lines; line++) {
	for (sample=0; sample<samples; sample++) {
	  if (image[line][sample] == pixel) {
	    found = 1;
	    break;
	  }
	}
	if (found) break;
      }
      if (found) {
	numPoints =
	  getOneContour(coords, 2*level+1, line, sample);

	/* Output to outFile :
	   level
	   numPoints
	   coords (in aoi space) ...
	*/
	fprintf(outFilePtr, "%d %d\n", level, numPoints);
	for (n=0; n<numPoints; n++) {
	  fprintf(outFilePtr, "%d %d\n",
		  coords[n].line, coords[n].sample);
	  
	  /* remove contour from image */
	  /* remove the 8 neighbors around the contour to reduce the
	     number of small pieces */
	  image[coords[n].line][coords[n].sample] = 2*level;
	  for (lineOffset=-1; lineOffset<=1; lineOffset++) {
	    for (sampleOffset=-1; sampleOffset<=1; sampleOffset++) {
	      lineN = coords[n].line + lineOffset;
	      sampleN = coords[n].sample + sampleOffset;
	      if (lineN>0 && lineN<lines && sampleN>0 && sampleN <samples) 
		if (image[lineN][sampleN] == 2*level+1)
		  image[lineN][sampleN] = 2*level;
	    }
	  }
	}
      }
    } while (found);
  }
  fclose(outFilePtr);
  return 0;
}


