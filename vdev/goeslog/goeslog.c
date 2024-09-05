#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* need these for stat */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
/* ntohl and ntohs byte order converters */
#include <netinet/in.h>
#endif

#include "vicmain_c.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoLoggerUtils.h"

/*
  program goeslog

  1) Reads a GOES image named in the first inp arg
  2) Reads GOES lon and lat files named in the second and third inp args
  3) Writes the VICAR image to the file named in the first out arg
  4) Writes the IBIS navigation file named in the second out arg

  Parameters:
  inp(1) - the name of the input image archive file
  inp(2) - the name of the input longitude navigation file
  inp(3) - the name of the input latitude navigation file
  out(1) - the name of the extracted VICAR image file
  out(2) - the name of the extracted IBIS navigation file
  sl     - start line (one based value with (1,1) pixel in nw corner)
  ss     - start sample (one based value)
  nl     - number of lines to extract
  ns     - number of samples to extract

  Assumptions:

  The input GOES IR (band 4 or 5) image file is a 500 pixel square
  image with little endian 4 byte unsigned pixel values.

  The input GOES longitude and latitude files are 500 value square
  arrays with big endian 2 byte signed values converted to longitude
  or latitude by dividing by -100 or 100, respectively.

  Input data sets will originate in the Western Hemisphere.

*/

#define _goeslog_version_ "Tue Feb 26 2008"

void main44(void)
{
  int sl, ss, nl, ns, numPixels;
  int lineSkip, sampSkip;
  int parmct, parmdf;
  int status;
  int vunit;
  char infilename [3][99];	/* image, lon and lat */
  int goesLines, goesSamples;
  int goesPixels;

  FILE * infile;

  int ibisRow;
  double * lineColumn = 0, * sampleColumn = 0, * lonColumn = 0, * latColumn = 0;
  double dLat, dLon;
  int pixelSize;
  int * goesIRImage = 0, * outIRImageLine = 0;
  unsigned short * goesVisualImage = 0, * outVisualImageLine = 0;
  unsigned short * lat, * lon;

  int line, zeroBasedLine, outLine, sample, zeroBasedSample, outSample;
  struct stat statBuf;
  int i;
  char msgBuf [100];

  checkLoggerUtilsVersion (19);

  sprintf (msgBuf, "goeslog version %s", _goeslog_version_);
  zifmessage (msgBuf);
   
  /* fetch params */
  zvp("sl", &sl, &parmct);
  zvp("ss", &ss, &parmct);
  zvp("nl", &nl, &parmct);
  zvp("ns", &ns, &parmct);
  zvp("lines", &goesLines, &parmct);
  zvp("samples", &goesSamples, &parmct);
  zvp("lineSkip", &lineSkip, &parmct);
  zvp("sampSkip", &sampSkip, &parmct);
  numPixels = nl * ns;
  zvparm ("inp", infilename, &parmct, &parmdf, 3, 99);

  goesPixels = goesLines * goesSamples;

  /* open GOES image input */
  if (! (infile = fopen (infilename [0], "rb"))) {
    sprintf (msgBuf, "error opening %s for input", infilename [0]);
    zmabend (msgBuf);
  }

  /* stat the source image file to determine size, hence band */
  if (stat(infilename [0], & statBuf)) {
    sprintf (msgBuf, "error stating image file \"%s\"", infilename [0]);
    zmabend (msgBuf);
  }

  if (statBuf.st_size > goesPixels * 2) {
    /* IR data */
    pixelSize = 4;
    goesIRImage = (int *) malloc (goesPixels * pixelSize);
    outIRImageLine = (int *) malloc (ns * pixelSize);

    /* read the image data */
    if ((status = fread (goesIRImage, pixelSize, goesPixels, infile)) != goesPixels)
      zmabend ("error reading image data");

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
    /* make it little endian for i80x86 */
    for (i = 0; i < goesPixels; i ++)
      goesIRImage [i] = ntohl (goesIRImage [i]);
#endif
  } else {
    /* Visual data */
    pixelSize = 2;
    goesVisualImage = (unsigned short *) malloc (goesPixels * pixelSize);
    outVisualImageLine = (unsigned short *) malloc (ns * pixelSize);

    /* read the image data */
    if ((status = fread (goesVisualImage, pixelSize, goesPixels, infile)) != goesPixels) {
      sprintf (msgBuf, "error reading image data: read %d pixels but expected %d", status, goesPixels);
      zmabend (msgBuf);
    }

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
    /* make it little endian for i80x86 */
    for (i = 0; i < goesPixels; i ++)
      goesVisualImage [i] = ntohs (goesVisualImage [i]);
#endif
  }

  /* done with the GOES image input */
  fclose (infile);

  /* malloc lon and lat arrays */
  lon = (unsigned short *) malloc (goesPixels * 2);
  lat = (unsigned short *) malloc (goesPixels * 2);

  /* read lon data */
  if (! (infile = fopen (infilename [1], "rb"))) {
    sprintf (msgBuf, "error opening %s for input", infilename [1]);
    zmabend (msgBuf);
  }
  if ((status = fread (lon, 2, goesPixels, infile)) != goesPixels)
    zmabend ("error reading lon nav data");

  fclose (infile);

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
  /* make it little endian for i80x86 */
  for (i = 0; i < goesPixels; i ++)
    lon [i] = ntohs (lon [i]);
#endif

  /* read lat data */
  if (! (infile = fopen (infilename [2], "rb"))) {
    sprintf (msgBuf, "error opening %s for input", infilename [2]);
    zmabend (msgBuf);
  }
  if ((status = fread (lat, 2, goesPixels, infile)) != goesPixels)
    zmabend ("error reading lat nav data");

  fclose (infile);

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
  /* make it little endian for i80x86 */
  for (i = 0; i < goesPixels; i ++)
    lat [i] = ntohs (lat [i]);
#endif

  /* create columns LINE, SAMPLE, LON, LAT for IBIS navigation file */
  if (! (lineColumn = (double *) malloc (sizeof (double) * numPixels / lineSkip / sampSkip)) ||
      ! (sampleColumn = (double *) malloc (sizeof (double) * numPixels / lineSkip / sampSkip)) ||
      ! (lonColumn = (double *) malloc (sizeof (double) * numPixels / lineSkip / sampSkip)) ||
      ! (latColumn = (double *) malloc (sizeof (double) * numPixels / lineSkip / sampSkip))) {
    zmabend ("error allocating IBIS columns");
  } else
    printf ("allocated %d nav samples\n", numPixels / lineSkip / sampSkip);

  /* open VICAR image output */
  if ((status = zvunit (&vunit, "OUT", 1, NULL)) != 1)
    zmabend ("zvunit failed");
  if (outIRImageLine) {		/* IR */
    if ((status = zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
			  "O_FORMAT", "FULL", NULL)) != 1) 
      zmabend ("zvopen failed");
  } else				/* visual */
    if ((status = zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
			  "O_FORMAT", "HALF", NULL)) != 1)
      zmabend ("zvopen failed");

  /* extract image pixels from goes image format */
  ibisRow = 0;
  for (line = sl; line < sl + nl; line ++) {
    /* line is one based */
    zeroBasedLine = line - 1;
    outLine = line - sl;

    for (sample = ss; sample < ss + ns; sample ++) {
      /* sample is one based */
      zeroBasedSample = sample - 1;
      outSample = sample - ss;

      if (outIRImageLine)
	outIRImageLine [outSample] = goesIRImage [zeroBasedSample + goesSamples * zeroBasedLine];
      else
	outVisualImageLine [outSample] = goesVisualImage [zeroBasedSample + goesSamples * zeroBasedLine];

      /* compute navigation data for pixel*/
      dLon = (double) lon [zeroBasedSample + goesSamples * zeroBasedLine] / -100.0;
      dLat = (double) lat [zeroBasedSample + goesSamples * zeroBasedLine] / 100.0;

      if (! (line % lineSkip) && ! (sample % sampSkip)) {
	lineColumn   [ibisRow] = line - sl + 1;   /* one based IBIS value */
	sampleColumn [ibisRow] = sample - ss + 1; /* one based IBIS value */
	lonColumn    [ibisRow] = dLon;
	latColumn    [ibisRow] = dLat;
	ibisRow ++;
      }
    }

    /* write one image line to VICAR image output */
    if (outIRImageLine)
      zvwrit (vunit, outIRImageLine, "LINE", outLine + 1, "SAMP", 1, "NSAMPS", ns, NULL);
    else
      zvwrit (vunit, outVisualImageLine, "LINE", outLine + 1, "SAMP", 1, "NSAMPS", ns, NULL);
  }

  /* done with VICAR image output */
  zvclose (vunit, NULL);

  /* log the nav data */
  if (logNavDataToIBIS ("OUT", 2, numPixels / lineSkip / sampSkip, lineColumn, sampleColumn, latColumn, lonColumn) != 1)
    zmabend ("error logging nav data");

  free (lineColumn);
  free (sampleColumn);
  free (lonColumn);
  free (latColumn);
}
