#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int main (int argc, char * argv []) {
  char * argPattern =
    "lines samples inFile outFile level1 level2 level3 ...";
  int arg, i, line, sample, lines, samples, numLevels, edge;
  int lineOffset, sampleOffset, lineN, sampleN;
  double * levels;
  double neighbor, timeValue;
  double * minTimeSoFar;
  char header[500];
  unsigned char * buf;
  unsigned char pixel;
  FILE *inFile, *outFile;
  
  /* check arg count */
  if (argc < 6) {
    fprintf (stderr, "usage: %s %s\n", argv[0], argPattern);
    return 1;
  }
  
  /* parse line count */
  if (sscanf (argv[1], "%d", & lines) != 1 || lines < 1) {
    fprintf (stderr, "%s: failed parsing lines (int > 0) from %s\n",
	     argv[0], argv[1]);
    return 1;
  }
  
  /* parse sample count */
  if (sscanf (argv[2], "%d", & samples) != 1 || samples < 1) {
    fprintf (stderr, "%s: failed parsing samples (int > 0) from %s\n",
	     argv[0], argv[2]);
    return 1;
  }

  /* Open in file */
  if ( (inFile = fopen(argv[3], "r")) == NULL ) {
    fprintf (stderr, "%s: could not open %s for read.\n",
	     argv[0], argv[3]);
    return 1;
  }

  /* Open out file */
  if ( (outFile = fopen(argv[4], "w")) == NULL ) {
    fprintf (stderr, "%s: could not open %s for write.\n",
	     argv[0], argv[4]);
    fclose(inFile);
    return 1;
  }

  /* Open out file */

  numLevels = (argc - 5);

  /* malloc contour level (altitude) values array */
  if (! (levels = (double *) malloc (numLevels * sizeof (double)))) {
    fprintf (stderr, "%s: failed mallocing %d levels\n", argv[0], numLevels);
    fclose(inFile);
    fclose(outFile);
    return 1;
  }

  /* parse contour level (altitude) values */
  for (arg = 5, i = 0; arg < argc; arg++, i++) {
    if (sscanf (argv[arg], "%lf", levels + i) != 1) {
      fprintf (stderr, "%s: failed parsing level (double) from %s\n",
	       argv[0], argv[arg]);
      fclose(inFile);
      fclose(outFile);
      return 1;
    }
    if (i && (levels[i] <= levels[i - 1])) {
      fprintf (stderr,
	       "%s: contour levels must be increasing but %f <= %f\n",
	       argv[0], levels[i], levels[i - 1]);
      fclose(inFile);
      fclose(outFile);
      return 1;
    }
    printf("level[%d] = %f\n", i, levels[i]);
  }

  /* malloc space for source image (a lines * samples array of doubles) */
  if (! (minTimeSoFar = (double *) malloc (lines * samples * sizeof (double)))) {
    fprintf (stderr, "%s: failed mallocing %d doubles for source image\n",
	     argv[0], lines * samples);
    fclose(inFile);
    fclose(outFile);
    return 1;
  }
  
  /* read the source image from in file */
  if (fread (minTimeSoFar, sizeof (double), lines * samples, inFile) !=
      lines * samples) {
    fprintf (stderr, "%s: failed reading %d source image pixels from %s\n",
	     argv[0], lines * samples, argv[3]);
    fclose(inFile);
    fclose(outFile);
    return 1;
  }
  fclose(inFile);

  sprintf (header, "P5\n%d %d\n255\n", samples, lines);
  if (fwrite (header, strlen(header), 1, outFile) != 1) {
    fprintf (stderr, "%s: failed writing header to %s\n", argv[0], argv[4]);
    fclose(outFile);
    return 1;
  }

  if (! (buf = (unsigned char *) malloc(samples))) {
    fprintf (stderr, "%s: failed mallocing buf\n", argv[0]);
    fclose(outFile);
    return 1;
  }

  /*
    pixel value inside level 0 edge = 0
    pixel value at level 0 edge = 1
    pixel value inside level n edge = 2*n
    pixel value at level n edge = 2*n+1
    pixel value outside level numLevels = 2 * numLevels
  */
  /* edge is a flag */
  for (line = 0; line < lines; line++) {
    for (sample = 0; sample < samples; sample++) {
      timeValue = minTimeSoFar[sample + line * samples];
      edge = 0;
      /* check for edge */
      for (i = 0; !edge && (i < numLevels); i++) {
	if (timeValue == levels[i]) {
	  /* this pixel is right on this contour level */
	  edge = 1;
	  pixel = 2*i+1;
	} else if (timeValue > levels[i]) {
	  /* this pixel is above this contour level */
	  for (lineOffset = -1; !edge && (lineOffset <= 1); lineOffset++) {
	    for (sampleOffset = -1; !edge && (sampleOffset <= 1);
		 sampleOffset++) {
	      if (lineOffset || sampleOffset) {
		/* center is not a neighbor */
		lineN = line + lineOffset;
		sampleN = sample + sampleOffset;
		if ((lineN >= 0) && (lineN < lines) &&
		    (sampleN >= 0) && (sampleN < samples)) {
		  neighbor = minTimeSoFar[sampleN + lineN * samples];
		  if (neighbor < levels[i]) {
		    /* this neighbor is below this contour level */
		    edge = 1;
		    pixel = 2*i+1;
		  }
		}
	      }
	    }
	  }
	}
      } /* end for(numLevels) */
      if (!edge) {
	/* if not edge then check for interior level */
	pixel = 2*numLevels; /* this is pixel for outside largest level */
	for (i = 0; i < numLevels; i++) {
	  if (timeValue < levels[i]) {
	    pixel = 2*i;
	    break;
	  }
	}
      }
      buf[sample] = pixel;
    } /* end for(sample) */
    if (fwrite (buf, 1, samples, outFile) != samples) {
      fprintf (stderr, "%s: failed writing image line to %s\n",
	       argv[0], argv[4]);
      fclose(outFile);
      return 1;
    }
  } /* end for(line) */
  
  printf ("Finished mintime2contours().\n");
  fclose(outFile);
  return 0;
}


