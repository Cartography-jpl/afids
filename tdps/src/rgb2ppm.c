#include <stdio.h>
#include <stdlib.h>

int main (int argc, char * argv []) {
  char * usage = "usage: rgb2ppm r g b height width pgm sl ss nl ns zoom";
  FILE *r, *g, *b, *pgm;
  int height, width, sl, ss, nl, ns;
  char *rBuf, *gBuf, *bBuf;
  int line, sample, zoom, repeat, lineIteration, sampIteration, increment;

  if (argc != 12) {
    fprintf (stderr, "%s\n", usage);
    exit (1);
  }

  if (! (r = fopen (argv [1], "r")) ||
      ! (g = fopen (argv [2], "r")) ||
      ! (b = fopen (argv [3], "r"))) {
    fprintf (stderr, "error opening one of r, g, b\n");
    exit (1);
  }

  if (sscanf (argv [4], "%d", & height) != 1 ||
      sscanf (argv [5], "%d", & width) != 1 ||
      sscanf (argv [7], "%d", & sl) != 1 ||
      sscanf (argv [8], "%d", & ss) != 1 ||
      sscanf (argv [9], "%d", & nl) != 1 ||
      sscanf (argv [10], "%d", & ns) != 1 ||
      sscanf (argv [11], "%d", & zoom) != 1) {
    fprintf (stderr, "error parsing height, width, sl, ss, nl, ns or zoom\n");
    exit (1);
  }

  /* should check to see that each of r, g, b have a size == width * height */

  if (! (pgm = fopen (argv [6], "w"))) {
    fprintf (stderr, "error opening pgm\n");
    exit (1);
  }

  if (zoom < 0) {
    repeat = - zoom;
    increment = 1;
    fprintf (pgm, "P6 %d %d %d\n", ns * repeat, nl * repeat, 255);
  } else {
    fprintf (pgm, "P6 %d %d %d\n", ns / zoom, nl / zoom, 255);
    repeat = 1;
    increment = zoom;
  }

  if (! (rBuf = malloc (ns))) {
    fprintf (stderr, "error allocating rBuf\n");
    exit (1);
  }

  if (! (gBuf = malloc (ns))) {
    fprintf (stderr, "error allocating gBuf\n");
    exit (1);
  }

  if (! (bBuf = malloc (ns))) {
    fprintf (stderr, "error allocating bBuf\n");
    exit (1);
  }

  for (line = sl + 1; line < sl + nl + 1; line += increment) { /* +1 to skip VICAR label */
    if (fseek (r, line * width + ss, SEEK_SET) ||
	fseek (g, line * width + ss, SEEK_SET) ||
	fseek (b, line * width + ss, SEEK_SET)) {
      fprintf (stderr, "error seeking r, g or b\n");
      exit (1);
    }

    if (fread (rBuf, ns, 1, r) != 1) {
      fprintf (stderr, "error reading r line %d\n", line);
      exit (1);
    }

    if (fread (gBuf, ns, 1, g) != 1) {
      fprintf (stderr, "error reading g line %d\n", line);
      exit (1);
    }

    if (fread (bBuf, ns, 1, b) != 1) {
      fprintf (stderr, "error reading b line %d\n", line);
      exit (1);
    }

    /* repeat each line */
    for (lineIteration = 0; lineIteration < repeat; lineIteration ++) {
      for (sample = 0; sample < ns; sample += increment) {
	for (sampIteration = 0; sampIteration < repeat; sampIteration ++) {
	  if (fwrite (rBuf + sample, 1, 1, pgm) != 1) {
	    fprintf (stderr, "error writing red to pgm\n");
	    exit (1);
	  }

	  if (fwrite (gBuf + sample, 1, 1, pgm) != 1) {
	    fprintf (stderr, "error writing grn to pgm\n");
	    exit (1);
	  }

	  if (fwrite (bBuf + sample, 1, 1, pgm) != 1) {
	    fprintf (stderr, "error writing blu pgm\n");
	    exit (1);
	  }
	}
      }
    }
  }

  fclose (pgm);
  fclose (b);
  fclose (g);
  fclose (r);

  exit (0);
}
