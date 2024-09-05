#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define ROUND(x) ((int) (abs((x)-floor(x)) >= abs((x)-ceil(x)) ? ceil(x) : floor(x)))

int main (int argc, char * argv []) {
  char * argPattern =
    "inFile outFile scale color0 color1 color2 ...";
  int maxValue;
  int line, sample, lines, samples;
  FILE *inFilePtr, *outFilePtr;
  int i, numColors;
  double scale;
  int *colors;
  int arg;
  unsigned char *inBuffer;
  unsigned char *outBuffer;
  int scaledLines, scaledSamples;
  unsigned char inBufferValue;
  int lineThreshold;
  int seeThru;

printf("argc = %d\n", argc);

  if (argc < 5) {
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
  scale = atof(argv[3]);
  printf("scale = %f\n", scale);

  /* parse contour color values */
  numColors = argc - 4;
  printf("numColors = %d\n", numColors);

  /* malloc color array */
  if (! (colors = (int *) malloc (sizeof (int)*numColors))) {
    fprintf (stderr, "%s: failed mallocing %d colors\n", argv[0], numColors);
    return 1;
  }

  for (arg = 4, i = 0; arg < argc; arg++, i++)
    if (sscanf (argv[arg], "%x", colors + i) != 1) {
      fprintf (stderr, "%s: failed parsing color (hex integer) from %s\n", argv [0], argv [arg]);
      return 1;
    }

  /* Read inFile (pgm format) */
  fscanf(inFilePtr, "P5\n%d %d\n%d", &samples, &lines, &maxValue);
  /* Read the 1 white space character. */
  printf("White space character = %d\n", fgetc(inFilePtr));

  scaledLines   = ROUND(scale*lines);
  scaledSamples = ROUND(scale*samples);
  fprintf(outFilePtr, "P6\n%d %d\n%d\n", scaledSamples, scaledLines, maxValue);

  inBuffer  = (unsigned char *) malloc(        samples*sizeof(unsigned char));
  outBuffer = (unsigned char *) malloc(3*scaledSamples*sizeof(unsigned char));
  lineThreshold = 0;
  for (line=0; line<scaledLines; line++) {
    if ((int)floor(line/scale) >= lineThreshold) {
      lineThreshold++;
      fread(inBuffer, sizeof(unsigned char), samples, inFilePtr);
      /*printf("lineThreshold = %d\n", lineThreshold);*/
    }
    seeThru = line % 3;
    for (sample=0; sample<scaledSamples; sample++) {
      if (seeThru >= 3) seeThru=0; else seeThru++;
      inBufferValue = inBuffer[(int)floor(sample/scale)];
      outBuffer[3*sample]   = 0;
      outBuffer[3*sample+1] = 0;
      outBuffer[3*sample+2] = 0;
      if (seeThru==0) {
	for (i=0; i<numColors; i++) {
	  if (inBufferValue == i) {
	    outBuffer[3*sample]   = (colors[i] & 0xff0000) >> 16;
	    outBuffer[3*sample+1] = (colors[i] & 0x00ff00) >> 8;
	    outBuffer[3*sample+2] = (colors[i] & 0x0000ff);
	    break;
	  }
	}
      }
    }
    fwrite(outBuffer, sizeof(unsigned char), 3*scaledSamples, outFilePtr);
  }
  fclose(inFilePtr);
  fclose(outFilePtr);
  free(inBuffer);
  free(outBuffer);
  free(colors);
  return 0;
}








