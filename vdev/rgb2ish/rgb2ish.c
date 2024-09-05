#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"
#include "vicmain_c.h"

void rgb2ish (double maxPixVal, double R, double G, double B, double * H, double * S, double * I);
void ish2rgb (double maxPixVal, double H, double S, double I, double * R, double * G, double * B);

/*
  program rgb2ish

  1) Reads byte, half or full word VICAR image
  2) Writes same, with RGB/ISH conversion
  3) Conversion direction depends on "reverse" parm
  4) Negative values are set to zero

  Parameters:
  inp - the name of the input file
  out - the name of the output file
  reverse - the direction flag (0=> rgb2ish; !0=> ish2rgb)

  Assumptions:

  The input Meteosat archive file format agrees with the description
  in The Meteosat Archive Format Guide No. 1 Basic Imagery OpenMTP
  Format EUM FG 1 Revision 2.1 April 2000 

  Navigation of the image is performed as described in Meteosat
  Archive User Handbook Issue 2.5, March 2001

  The archive is of a full disk image, not a user-specified sub-area
  of the full disk image.

*/

#define _rgb2ish_version_ "Wed Nov  5 2008"

void main44(void)
{
  int nl, ns;
  int parmct, parmdf;
  int status;
  char inpfilename [3] [99];
  char outfilename [3] [99];
  int reverse, noneg;
  int inpunit [3];
  int outunit [3];
  int tmpnl, tmpns;
  char fmtStr [100];
  char tmpFmtStr [100];
  char progVersionID [100];
  char format [100];
  int i;
  char msgBuf [1000];
  char * tmpVicNameIn [3] = {0, 0, 0}, * tmpVicNameOut [3] = {0, 0, 0};

  checkLoggerUtilsVersion (14);

  sprintf (progVersionID, "rgb2ish version %s", _rgb2ish_version_);
  zifmessage (progVersionID);

  /* fetch params */
  zvparm ("inp", inpfilename, &parmct, &parmdf, 3, 99);
  zvparm ("out", outfilename, &parmct, &parmdf, 3, 99);
  zvp ("reverse", & reverse, & parmct);
  zvp ("noneg", & noneg, & parmct);
  zvparm ("fileformat", format, &parmct, &parmdf, 1, 99);


  if (! strcmp (format, "geotiff"))
    zifmessage ("processing geotiff; must not be run inside TAE");

  /* open input files and get nl, ns, format */
  for (i = 0; i < 3; i ++) {
    /* if from geotiff, convert to vicar */
    if (! strcmp (format, "geotiff")) {
      char command [1000];
      tmpVicNameIn [i] = (char *) tempnam ("./", NULL);
      sprintf (command, "vtiff3 -tovic inp=%s out=%s > /dev/null", inpfilename [i], tmpVicNameIn [i]);
      system (command);

      if (zvunit( & inpunit [i], "notINP", i + 1, "U_NAME", tmpVicNameIn [i], NULL) != 1)
	zmabend ("zvunit failed on input");	/* die */
    } else {
      if (zvunit (& inpunit [i], "INP", i + 1, NULL) != 1) {
	sprintf (msgBuf, "zvunit failed on input \"%s\"", inpfilename [i]);
	zmabend (msgBuf);	/* die */
      }
    }

    if (zvopen (inpunit [i], "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
      sprintf (msgBuf, "zvopen failed on input \"%s\"", inpfilename [i]);
      zmabend (msgBuf);	/* die */
    }
  }

  /* make sure that the three input files are the same shape and pixel size */
  zvget (inpunit [0], "NL", & nl, NULL);
  zvget (inpunit [0], "NS", & ns, NULL);
  zvget (inpunit [0], "FORMAT", fmtStr, NULL);

  if (strcmp (fmtStr, "BYTE") && strcmp (fmtStr, "HALF"))
    zmabend ("Input must be BYTE or HALF word files");

  zvget (inpunit [1], "NL", & tmpnl, NULL);
  zvget (inpunit [1], "NS", & tmpns, NULL);
  zvget (inpunit [1], "FORMAT", tmpFmtStr, NULL);

  if (tmpnl != nl || tmpns != ns)
    zmabend ("Input file 2 is a different shape than file 1");
  if (strcmp (tmpFmtStr, fmtStr))
    zmabend ("Input file 2 has a different pixel size than file 1");

  zvget (inpunit [2], "NL", & tmpnl, NULL);
  zvget (inpunit [2], "NS", & tmpns, NULL);
  zvget (inpunit [2], "FORMAT", tmpFmtStr, NULL);

  if (tmpnl != nl || tmpns != ns)
    zmabend ("Input file 3 is a different shape than file 1");
  if (strcmp (tmpFmtStr, fmtStr))
    zmabend ("Input file 3 has a different pixel size than file 1");

  /* create output files */
  for (i = 0; i < 3; i ++) {
    /* if from geotiff, convert to vicar */
    if (! strcmp (format, "geotiff")) {
      tmpVicNameOut [i] = (char *) tempnam ("./", NULL);

      if (zvunit( & outunit [i], "notOUT", i + 1, "U_NAME", tmpVicNameOut [i], NULL) != 1)
	zmabend ("zvunit failed on output");	/* die */
    } else {
      if (zvunit (& outunit [i], "OUT", i + 1, NULL) != 1) {
	sprintf (msgBuf, "zvunit failed on output \"%s\"", outfilename [i]);
	zmabend (msgBuf);	/* die */
      }
    }

    if (zvopen (outunit [i], "U_NL", nl, "U_NS", ns, "O_FORMAT", fmtStr,
		"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
      sprintf (msgBuf, "zvopen failed on output \"%s\"", outfilename [i]);
      zmabend (msgBuf);	/* die */
    }
  }

  if (! strcmp (fmtStr, "BYTE")) {
    {
      /* allocate buffers */
      unsigned char * inpLineBufRH = (unsigned char *) malloc (ns);
      unsigned char * inpLineBufGS = (unsigned char *) malloc (ns);
      unsigned char * inpLineBufBI = (unsigned char *) malloc (ns);
      unsigned char * outLineBufRH = (unsigned char *) malloc (ns);
      unsigned char * outLineBufGS = (unsigned char *) malloc (ns);
      unsigned char * outLineBufBI = (unsigned char *) malloc (ns);
      int line, pixel;
      double R, G, B, H, S, I;

      for (line = 0; line < nl; line ++) {
	/* read in a line */
	if ((status = zvread (inpunit [0], inpLineBufRH, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on first channel");
	if ((status = zvread (inpunit [1], inpLineBufGS, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on second channel");
	if ((status = zvread (inpunit [2], inpLineBufBI, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on third channel");

	for (pixel = 0; pixel < ns; pixel ++) {
	  /* convert */
	  if (reverse) {	/* ish2rgb */
	    H = inpLineBufRH [pixel];
	    S = inpLineBufGS [pixel];
	    I = inpLineBufBI [pixel];
	    H /= 255.0;
	    S /= 255.0;
	    I /= 255.0;
	    ish2rgb (255.0, H, S, I, & R, & G, & B);
	    outLineBufRH [pixel] = R;
	    outLineBufGS [pixel] = G;
	    outLineBufBI [pixel] = B;
	  } else {		/* rgb2ish */
	    R = inpLineBufRH [pixel];
	    G = inpLineBufGS [pixel];
	    B = inpLineBufBI [pixel];
	    rgb2ish (255.0, R, G, B, & H, & S, & I);
	    H *= 255.0;
	    S *= 255.0;
	    I *= 255.0;
	    outLineBufRH [pixel] = H;
	    outLineBufGS [pixel] = S;
	    outLineBufBI [pixel] = I;
	  }
	}

	/* write out a line */
	zvwrit (outunit [0], outLineBufRH,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [1], outLineBufGS,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [2], outLineBufBI,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
      }
    }
  } else if (! strcmp (fmtStr, "HALF")) {
    {
      /* allocate buffers */
      short * inpLineBufRH = (short *) malloc (ns * 2);
      short * inpLineBufGS = (short *) malloc (ns * 2);
      short * inpLineBufBI = (short *) malloc (ns * 2);
      short * outLineBufRH = (short *) malloc (ns * 2);
      short * outLineBufGS = (short *) malloc (ns * 2);
      short * outLineBufBI = (short *) malloc (ns * 2);
      int line, pixel;
      double R, G, B, H, S, I;

      for (line = 0; line < nl; line ++) {
	/* read in a line */
	if ((status = zvread (inpunit [0], inpLineBufRH, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on first channel");
	if ((status = zvread (inpunit [1], inpLineBufGS, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on second channel");
	if ((status = zvread (inpunit [2], inpLineBufBI, "LINE", line + 1, "SAMP", 1, "NSAMPS", ns, NULL)) != 1)
	  zmabend ("zvread failed on third channel");

	for (pixel = 0; pixel < ns; pixel ++) {
	  /* force non negative if necessary since VICAR half word is signed, unlike VICAR byte */
	  if (noneg) {
	    if (inpLineBufRH [pixel] < 0)
	      inpLineBufRH [pixel] = 0;
	    if (inpLineBufGS [pixel] < 0)
	      inpLineBufGS [pixel] = 0;
	    if (inpLineBufBI [pixel] < 0)
	      inpLineBufBI [pixel] = 0;
	  }

	  /* convert */
	  if (reverse) {
	    H = inpLineBufRH [pixel];
	    S = inpLineBufGS [pixel];
	    I = inpLineBufBI [pixel];
	    H /= 32767.0;
	    S /= 32767.0;
	    I /= 32767.0;
	    ish2rgb (32767.0, H, S, I, & R, & G, & B);
	    outLineBufRH [pixel] = R;
	    outLineBufGS [pixel] = G;
	    outLineBufBI [pixel] = B;
	  } else {
	    R = inpLineBufRH [pixel];
	    G = inpLineBufGS [pixel];
	    B = inpLineBufBI [pixel];
	    rgb2ish (32767.0, R, G, B, & H, & S, & I);
	    H *= 32767.0;
	    S *= 32767.0;
	    I *= 32767.0;
	    outLineBufRH [pixel] = H;
	    outLineBufGS [pixel] = S;
	    outLineBufBI [pixel] = I;
	  }
	}

	/* write out a line */
	zvwrit (outunit [0], outLineBufRH,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [1], outLineBufGS,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
	zvwrit (outunit [2], outLineBufBI,
		"LINE", line + 1, /* LINE is one based; line is zero based */
		"SAMP", 1,
		"NSAMPS", ns, NULL);
      }
    }
  } else
    zmabend ("internal error 42"); /* impossible */

  /* done with VICAR images */
  for (i = 0; i < 3; i ++) {
    zvclose (inpunit [i], NULL);
    zvclose (outunit [i], NULL);
  }

  /* if from geotiff, convert from tmp vicar back to geotiff */
  if (! strcmp (format, "geotiff")) {
    char command [1000];
    for (i = 0; i < 3; i ++) {
      sprintf (command, "vtiff3 -fromvic inp=%s out=%s > /dev/null", tmpVicNameOut [i], outfilename [i]);
      system (command);
      sprintf (command, "rm %s; rm %s", tmpVicNameIn [i], tmpVicNameOut [i]);
      free (tmpVicNameIn [i]);
      free (tmpVicNameOut [i]);
    }
  }
}

/* Converts between RGB and ISH formats. Adapted from Leiming Qian's
  <lqian@uiuc.edu> conversion of Javascript to C++ from
  www.ndirect.co.uk/~thomas.green/javascripts/hexAndRGB.html */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#undef MIN
#undef MAX
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define RINT(x)	  ((x) - floor (x) > 0.5 ? ceil(x) : floor(x))

void rgb2ish (double maxPixVal, double R, double G, double B, double * H, double * S, double * I) {
  double min, mid, max;

  if (R < 0.0 || R > maxPixVal || G < 0.0 || G > maxPixVal || B < 0.0 || B > maxPixVal)
    zmabend ("Value out of range!");

  /* find out the min, mid, and max of R, G, B */
  if (R > G && R > B) {
    max = R;
    mid = MAX (G, B);
    min = MIN (G, B);
  } else {
    if (G > B) {
      max = G;
      mid = MAX (R, B);
      min = MIN (R, B);
    } else {
      max = B;
      mid = MAX (R, G);
      min = MIN (R, G);
    }
  }

  * I = max / maxPixVal;
  * S = 0.0;
  * H = 0.0;
  if (* I == 0.0 || max == min) {
    /* this is a black image or grayscale image */
    * S = 0.0;
    * H = 0.0;
  } else {
    * S = (* I - min / maxPixVal) / * I;
    /* domains are 60 degrees of */
    /* red, yellow, green, cyan, blue or magenta */
    {
      double domainBase = 0.0;
      double oneSixth = 1.0/6.0;
      double domainOffset = ((mid - min) / (max - min)) / 6.0;

      if (R == max) {
	if (mid == G) {			/* green is ascending */
	  domainBase = 0.0;		/* red domain */
	} else {				/* blue is descending */
	  domainBase = 5.0/6.0;		/* magenta domain */
	  domainOffset = oneSixth - domainOffset;
	}
      } else {
	if (G == max) {
	  if (mid == B) {	        /* blue is ascending */
	    domainBase = 2.0/6.0;	/* green domain */
	  } else {			/* red is ascending */
	    domainBase = 1.0/6.0;	/* yellow domain */
	    domainOffset = oneSixth - domainOffset;
	  }
	} else {
	  if (mid == R) {		/* red is ascending */
	    domainBase = 4.0/6.0;	/* blue domain */
	  } else {			/* green is descending */
	    domainBase = 3.0/6.0;	/* cyan domain */
	    domainOffset = oneSixth - domainOffset;
	  }
	}
      }
      * H = domainBase + domainOffset;
    }
  }
}

void ish2rgb (double maxPixVal, double H, double S, double I, double * R, double * G, double * B) {
  double domainOffset = 0.0;

  if (H < 0.0 || H > 1.0 || S < 0.0 || S > 1.0)
    zmabend ("Value out of range!");

  if (I == 0.0) {
    /* black image */
    * R = * G = * B = 0.0;
  } else {
    if (S == 0.0) {
      /* grayscale image */
      * R = * G = * B = I;
    } else {
      if (H<1.0/6.0) {		/* red domain; green acending */
	domainOffset = H;
	* R = I;
	* B = I * (1.0 - S);
	* G = * B + (I - * B) * domainOffset * 6.0;
      } else {
	if (H < 2.0 / 6) {	/* yellow domain; red acending */
	  domainOffset = H - 1.0 / 6.0;
	  * G = I;
	  * B = I * (1.0 - S);
	  * R = * G - (I - * B) * domainOffset * 6.0;
	} else {
	  if (H < 3.0 / 6) {	/* green domain; blue descending */
	    domainOffset = H - 2.0 / 6;
	    * G = I;
	    * R = I * (1.0 - S);
	    * B = * R + (I - * R) * domainOffset * 6.0;
	  } else {
	    if (H < 4.0 / 6.0) { /* cyan domain, green acsending */
	      domainOffset = H - 3.0 / 6;
	      * B = I;
	      * R = I * (1.0 - S);
	      * G = * B - (I - * R) * domainOffset * 6.0;
	    } else {
	      if (H < 5.0 / 6.0) { /* blue domain, red ascending */
		domainOffset = H - 4.0 / 6.0;
		* B = I;
		* G = I * (1.0 - S);
		* R = * G + (I - * G) * domainOffset * 6.0;
	      } else {		/* magenta domain, blue descending */
		domainOffset = H - 5.0 / 6.0;
		* R = I;
		* G = I * (1.0 - S);
		* B = * R - (I - * G) * domainOffset * 6.0;
	      }
	    }
	  }
	}
      }
    }
  }

  * R = RINT (* R * maxPixVal);
  * G = RINT (* G * maxPixVal);
  * B = RINT (* B * maxPixVal);
}
