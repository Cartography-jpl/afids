#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
/* ntohl byte order converter */
#include <netinet/in.h>
#endif

#include "vicmain_c.h"

#include "sunup.h"
#include "f2c.h"
#include "carto/cartoVicarProtos.h"
#include "carto/cartoLoggerUtils.h"

int refgeo(real * line, real * pixel, real * lat, real * long__, logical1 * visible);
int georef(real * rlat, real * rlong, integer * line, integer * pixel, logical1 * visible);

/*
  program meteolog

  Assumptions:

  The input Meteosat archive file format agrees with the description
  in The Meteosat Archive Format Guide No. 1 Basic Imagery OpenMTP
  Format EUM FG 1 Revision 2.1 April 2000 

  Navigation of the image is performed as described in Meteosat
  Archive User Handbook Issue 2.5, March 2001

*/

#define _meteolog_version_ "Mon Mar 17 2008"

char * getTextHeaderStrAttribute (char * textHeader, char * attributeLabel, int len) {
  char * value;
  char * p ;
  char buf [100];

  p = strstr (textHeader, attributeLabel);
  if (! p) {
    sprintf (buf, "%s not found in text header", attributeLabel);
    zmabend (buf);
  }
  p += strlen (attributeLabel);

  value = malloc (len + 1);

  strncpy (value, p, len);
  value [len] = 0;
  return value;
}

int getTextHeaderIntAttribute (char * textHeader, char * attributeLabel) {
  char * p;
  char buf [100];
  int value;

  p = strstr (textHeader, attributeLabel);
  if (! p) {
    sprintf (buf, "%s not found in text header", attributeLabel);
    zmabend (buf);
  }
  p += strlen (attributeLabel);
  if (sscanf (p, "%d", & value) != 1) {
    sprintf (buf, "error parsing %s from text header", attributeLabel);
    zmabend (buf);
  }
  return value;
}

int lines;
int samples;

void main44()
{
  int sl, ss, nl, ns, numPixels;
  int parmct, parmdf;
  int status;
  char infilename [99];
  char outfilename [2] [99];
  char outprefix [99];
  char imagefilename [99];
  char navfilename [99];
  char metafilename [99];
  char archiveprefix [99];
  int vunit;
  double minLat, maxLat, minLon, maxLon;
  double nadlon;		/* satelite nadir longitude */
  int nonav=0, echometa=0;
  double sunlat, sunlon;
  double riseoff, setoff;
  FILE * infile;

  enum {noDataAvailable, visSData, visNData, visSNData, ir1Data, ir2Data, wv1Data, wv2Data};

#define binaryHeaderOffset 1345
#define nonVisBinaryHeaderSize 144515
#define visBinaryHeaderSize 192999
#define nonVisImageDataOffset (binaryHeaderOffset + nonVisBinaryHeaderSize)
#define visImageDataOffset (binaryHeaderOffset + visBinaryHeaderSize)
#define chanOffset 40	/* within binary header */
#define lineHeaderSize 32
#define imageLinesSize (lines * (lineHeaderSize + samples))

  char textHeader [binaryHeaderOffset + 1];
  int archiveNL, archiveNS, archiveSL, archiveSS;
  char * imageLines;
  char * outImageLine;
  int channel;
  int line, sample, imageSample, oneBasedOutImageLineNum;

  double * lineColumn = 0, * sampleColumn = 0, * lonColumn = 0, * latColumn = 0;
  float fLine, fSample, fLat, fLon;
  logical1 visible;
  int zeroBasedIBISRow;
  char msgBuf [100];
  char loggerVersionID [100];
  int imageDataOffset=0;
  int navskip;
  char * date;
  int slot;
  int outfilenamect;

  checkLoggerUtilsVersion (22);

  sprintf (loggerVersionID, "meteolog version %s", _meteolog_version_);
  zifmessage (loggerVersionID);

  /* fetch params */
  zvparm ("inp", infilename, &parmct, &parmdf, 1, 99);
  zvparm ("out", outfilename, &outfilenamect, &parmdf, 2, 99);
  zvparm ("outprefix", outprefix, & parmct, &parmdf, 1, 99);
  zvparm ("archiveprefix", archiveprefix, & parmct, &parmdf, 1, 99);
  nonav = zvptst("nonav");
  echometa = zvptst("echometa");
  zvp ("skip", & navskip, & parmct);
  zvparmd ("nadlon", & nadlon, & parmct, & parmdf, 1, 0);
  zvparmd ("sunlat", & sunlat, & parmct, & parmdf, 1, 0);
  zvparmd ("sunlon", & sunlon, & parmct, & parmdf, 1, 0);
  zvparmd ("riseoff", & riseoff, & parmct, & parmdf, 1, 0);
  zvparmd ("setoff", & setoff, & parmct, & parmdf, 1, 0);

  /* open Meteosat archive input */
  if (! (infile = fopen (infilename, "rb"))) {
    sprintf (msgBuf, "error opening %s for input", infilename);
    zmabend (msgBuf);
  }

  sprintf (msgBuf, "Opened %s for input", infilename);
  zifmessage (msgBuf);

  /* read text header */
  if (fread (textHeader, binaryHeaderOffset + 1, 1, infile) != 1)
    zmabend ("error reading text header");

  date = getTextHeaderStrAttribute (textHeader, "Date           ", 6);
  slot = getTextHeaderIntAttribute (textHeader, "Slot");
  archiveSL = getTextHeaderIntAttribute (textHeader, "StartLine");
  archiveSS = getTextHeaderIntAttribute (textHeader, "StartPixel");
  archiveNL = getTextHeaderIntAttribute (textHeader, "NumberOfLines");
  archiveNS = getTextHeaderIntAttribute (textHeader, "NumberOfPixels");
  
  /* seek to binary header */
  if (fseek (infile, binaryHeaderOffset + chanOffset, SEEK_SET))
    zmabend ("error fseeking to binary header");

  /* read Meteosat channel number */
  if (fread (& channel, sizeof (channel), 1, infile) != 1)
    zmabend ("error reading image channel number");

#if defined (__i386__) || defined (i386) || defined (__x86_64__)
  /* make it other endian for i80x86 */
  channel = ntohl (channel);  
#endif

  /* make sure it's the right channel */
  switch (channel) {
  case ir1Data:
  case ir2Data:
    imageDataOffset = nonVisImageDataOffset;
    lines = samples = 2500;
    break;
  case visSNData:
    nonav = 1;
    imageDataOffset = visImageDataOffset;
    lines = samples = 5000;
    break;
  default:
    zmabend ("unsupported channel");
  }

  /* if the sub-area is specified in lat/lon, we need to compute the line/sample */
  if (usingLatLonSubArea (lines, samples,
			  & minLat, & maxLat, & minLon, & maxLon,
			  & sl, & ss, & nl, & ns, channel != visSNData)) { /* nav undefined for VIS, so don't allow lat/lon AOI */
    {
      real rlat, rlong;
      integer line, pixel; /* one based, SE corner is origin */
      logical1 visible;
      int minLatPix = lines + 1, minLonPix = samples + 1, maxLatPix = 0, maxLonPix = 0;
      double offsetMinLon = minLon - nadlon;
      double offsetMaxLon = maxLon - nadlon;
      
      /* ul */
      rlat = maxLat;
      rlong = offsetMinLon;
      georef(& rlat, & rlong, & line, & pixel, & visible);
      if (! visible)
	minLatPix = minLonPix = 1;
      else {
	minLatPix = lines - line + 1;
	minLonPix = samples - pixel + 1;
      }

      /* ur */
      rlat = maxLat;
      rlong = offsetMaxLon;
      georef(& rlat, & rlong, & line, & pixel, & visible);
      if (! visible) {
	minLatPix = 1;
	maxLonPix = samples;
      } else {
	minLatPix = MIN (minLatPix, lines - line + 1);
	minLonPix = MIN (minLonPix, samples - pixel + 1);
	maxLatPix = MAX (maxLatPix, lines - line + 1);
	maxLonPix = MAX (maxLonPix, samples - pixel + 1);
      }

      /* lr */
      rlat = minLat;
      rlong = offsetMaxLon;
      georef(& rlat, & rlong, & line, & pixel, & visible);
      if (! visible) {
	maxLatPix = lines;
	maxLonPix = samples;
      } else {
	minLatPix = MIN (minLatPix, lines - line + 1);
	minLonPix = MIN (minLonPix, samples - pixel + 1);
	maxLatPix = MAX (maxLatPix, lines - line + 1);
	maxLonPix = MAX (maxLonPix, samples - pixel + 1);
      }

      /* ll */
      rlat = minLat;
      rlong = offsetMinLon;
      georef(& rlat, & rlong, & line, & pixel, & visible);
      if (! visible) {
	maxLatPix = lines;
	minLonPix = 1;
      } else {
	minLatPix = MIN (minLatPix, lines - line + 1);
	minLonPix = MIN (minLonPix, samples - pixel + 1);
	maxLatPix = MAX (maxLatPix, lines - line + 1);
	maxLonPix = MAX (maxLonPix, samples - pixel + 1);
      }

      sl = minLatPix;
      ss = minLonPix;
      nl = maxLatPix - minLatPix + 1;
      ns = maxLonPix - minLonPix + 1;

      if (sl < 1 || sl > lines || nl < 1 || nl > lines ||
	  ss < 1 || ss > samples || ns < 1 || ns > samples)
	zmabend ("failed computing sl/ss/nl/ns from min/max lat/lon");
    }
  }

  /* generate output filenames */
  if (outfilenamect && strlen(outfilename[0]))
    strcpy (imagefilename, outfilename[0]);
  else
    sprintf (imagefilename, "%s.img", outprefix);

  if (outfilenamect > 1 && strlen(outfilename[1]))
    strcpy (navfilename, outfilename[1]);
  else
    sprintf (navfilename, "%s.int", outprefix);

  sprintf (metafilename, "%s.txt", outprefix);

  /* open VICAR image output */
  if ((status = zvunit (& vunit, "U_NAME", 1, "U_NAME", imagefilename, NULL)) != 1)
    zmabend ("zvunit failed");

  if ((status = zvopen (vunit, "U_NL", nl, "U_NS", ns,
			"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL)) != 1)
    zmabend ("zvopen failed");

  sprintf (msgBuf, "Created \"%s\" for output VICAR image", imagefilename);
  zifmessage (msgBuf);

  /* initialize meta data file */
  initMetaData (metafilename);
  logMetaString (echometa, metafilename, "LOG_ID", loggerVersionID, 1, & vunit);

  sprintf (msgBuf, "Created \"%s\" for output metadata", metafilename);
  zifmessage (msgBuf);

  imageLines = (char *) malloc (imageLinesSize);
  outImageLine = (char *) malloc (samples);

  logMetaString (echometa, metafilename, "ARCH_FILE", infilename, 1, & vunit);

  switch (channel) {
  case ir1Data:
  case ir2Data:
    logMetaString (echometa, metafilename, "ARCH_CHANNEL", "IR", 1, & vunit);
    break;
  case visSNData:
    logMetaString (echometa, metafilename, "ARCH_CHANNEL", "VIS", 1, & vunit);
    break;
  }

  /* calc sunrise/set, daytime, if requested */
  {
    char buf [10];
    int day, month, year, utcTimeInMinutes;

    strncpy (buf, date, 2);
    buf[2] = 0;
    sscanf (buf, "%d", & year);
    if (year < 75)
      year += 2000;
    else
      year += 1900;
    strncpy (buf, date + 2, 2);
    sscanf (buf, "%d", & month);
    strncpy (buf, date + 4, 2);
    sscanf (buf, "%d", & day);

    utcTimeInMinutes = 30 * (slot - 1);

    logMetaInt (echometa, metafilename, "ARCH_UTC_YEAR", year, 1, & vunit);
    logMetaInt (echometa, metafilename, "ARCH_UTC_MONTH", month, 1, & vunit);
    logMetaInt (echometa, metafilename, "ARCH_UTC_DAY", day, 1, & vunit);
    logMetaInt (echometa, metafilename, "ARCH_UTC_MINUTE", utcTimeInMinutes, 1, & vunit);

    if (sunlat > -900.0 && sunlon > -900.0) {
      logMetaDouble (echometa, metafilename, "LOG_SUNLAT", sunlat, 1, & vunit);
      logMetaDouble (echometa, metafilename, "LOG_SUNLON", sunlon, 1, & vunit);
      logMetaDouble (echometa, metafilename, "LOG_RISEOFF", riseoff, 1, & vunit);
      logMetaDouble (echometa, metafilename, "LOG_SETOFF", setoff, 1, & vunit);

      logDayMonthTimeFlag (metafilename, day, month, year, sunlat, sunlon,
			   riseoff, setoff, utcTimeInMinutes, 1, & vunit, echometa);
    }
  }

  if (minLat > -900.0)
    logMetaDouble (echometa, metafilename, "LOG_MINLAT", minLat, 1, & vunit);
  if (maxLat > -900.0)
    logMetaDouble (echometa, metafilename, "LOG_MAXLAT", maxLat, 1, & vunit);
  if (minLon > -900.0)
    logMetaDouble (echometa, metafilename, "LOG_MINLON", minLon, 1, & vunit);
  if (maxLon > -900.0)
    logMetaDouble (echometa, metafilename, "LOG_MAXLON", maxLon, 1, & vunit);
  logMetaInt (echometa, metafilename, "LOG_SL", sl, 1, & vunit);
  logMetaInt (echometa, metafilename, "LOG_SS", ss, 1, & vunit);
  logMetaInt (echometa, metafilename, "LOG_NL", nl, 1, & vunit);
  logMetaInt (echometa, metafilename, "LOG_NS", ns, 1, & vunit);

  numPixels = (1 + nl / navskip) * (1 + ns / navskip); /* 1 + for fenceposts */
  
  logMetaInt (echometa, metafilename, "ARCHIVE_NL", archiveNL, 1, & vunit);
  logMetaInt (echometa, metafilename, "ARCHIVE_NS", archiveNS, 1, & vunit);
  logMetaInt (echometa, metafilename, "ARCHIVE_SL", archiveSL, 1, & vunit);
  logMetaInt (echometa, metafilename, "ARCHIVE_SS", archiveSS, 1, & vunit);

  /* seek to the image data */
  if (fseek (infile, imageDataOffset, SEEK_SET))
    zmabend ("error fseeking to image data");

  /* read the image data */
  if (archiveNL == lines && archiveNS == samples) {
    /* read whole disk */
    if ((status = fread (imageLines, imageLinesSize, 1, infile)) != 1)
      zmabend ("error reading image data");
  } else {
    /* zero fill and read sub-area */
    memset (imageLines, 0, imageLinesSize);
    for (line = archiveSL; line < archiveSL + archiveNL; line ++) {
      /* skip line header */
      if (fseek (infile, lineHeaderSize, SEEK_CUR))
    	zmabend ("error fseeking in image data");

      if (fread (imageLines + (line - 1) * (lineHeaderSize + samples) + archiveSS - 1, archiveNS, 1, infile) != 1)
  	zmabend ("error reading image sub area data");
    }
  }

  /* done with the Meteosat archive input */
  fclose (infile);

  if (! nonav)
    /* create columns LINE, SAMPLE, LON, LAT for IBIS navigation file */
    if (! (lineColumn = (double *) malloc (sizeof (double) * numPixels)) ||
	! (sampleColumn = (double *) malloc (sizeof (double) * numPixels)) ||
	! (lonColumn = (double *) malloc (sizeof (double) * numPixels)) ||
	! (latColumn = (double *) malloc (sizeof (double) * numPixels)))
      zmabend ("error allocating IBIS columns");
  
  /* extract image pixels from Meteosat archive format */
  /* indices are reversed because Meteosat format has (1,1) pixel in SE corner */
  zeroBasedIBISRow = 0;
  
  for (line = lines - sl, oneBasedOutImageLineNum = 1;
       line >= lines - nl - sl + 1;
       line --, oneBasedOutImageLineNum ++) {
    for (sample = samples - ss, imageSample = 0;
	 sample >= samples - ns - ss + 1;
	 sample --, imageSample ++) {
      /* copy image pixel */
      outImageLine [imageSample] = imageLines [line * (lineHeaderSize + samples) + lineHeaderSize + sample];

      if (! nonav) {
	/* compute navigation data for pixel*/
	if (! (line % navskip) && ! (sample % navskip)) {
	  fLine = line + 1.0;		                /* one based refgeo value */
	  fSample = sample + 1.0;	                        /* one based refgeo value */
	  refgeo (& fLine, & fSample, & fLat, & fLon, & visible);
	  if (nadlon != 0.0) {
	    fLon += nadlon;
	    if (fLon > 180.0)
	      fLon -= 360.0;
	    if (fLon < -180.0)
	      fLon += 360.0;
	  }

	  if (! visible) {		/* image points off the Earth disk are called (-999,-999) */
	    fLon = -999.0;
	    fLat = -999.0;
	  }

	  if (zeroBasedIBISRow >= numPixels) {
	    printf ("line %d sample %d pixel %d / %d\n", line, sample, zeroBasedIBISRow, numPixels);
	    return;
	  }
	  lineColumn   [zeroBasedIBISRow] = oneBasedOutImageLineNum;  /* one based IBIS value */
	  sampleColumn [zeroBasedIBISRow] = imageSample + 1;          /* one based IBIS value */
	  lonColumn    [zeroBasedIBISRow] = fLon;
	  latColumn    [zeroBasedIBISRow] = fLat;
	  zeroBasedIBISRow ++;
	}
      }
    }

    /* write one image line to VICAR image output */
    zvwrit (vunit, outImageLine, "LINE", oneBasedOutImageLineNum, "SAMP", 1, "NSAMPS", ns, NULL);
  }

  /* done with VICAR image output */
  zvclose (vunit, NULL);

  if (! nonav) {
    if (logNavDataToIBIS (navfilename, 2, numPixels, lineColumn, sampleColumn, latColumn, lonColumn) != 1)
      zmabend ("error logging nav data");

    sprintf (msgBuf, "Created \"%s\" for output IBIS nav data", navfilename);
    zifmessage (msgBuf);

    free (lineColumn);
    free (sampleColumn);
    free (lonColumn);
    free (latColumn);
  }
}

/* Translated by f2c (version 19950906) from Section 5.2.4.2 of
   Meteosat Archive User Handbook Issue 2.5, March 2001. EUMETSAT EUM
   TD 06. Requires FORTRAN and math libraries.  */

/* Subroutine */ 
int refgeo(real * line, real * pixel, real * lat, real * long__, logical1 * visible)
{
  /* System generated locals */
  real r__1;

  /* Builtin functions */
  double tan(), sqrt(), atan(), cos();

  /* Local variables */
  static real step, rpol, altitude, a, b, c__, k, p, q, r__, aline, x, y, 
    z__, tanal, asamp, tanas, pi, cenlat, oblate, deg_to_rad__, 
    rad_to_deg__, det, req;

  /* This subroutine converts digital to geographical co-ordinates. */

  /* Input parameters: */
  /* line - line number, measured from southern end of frame */
  /* pixel - pixel number, measured from eastern end of frame */

  /* Line and pixel values are real numbers to enable sub-pixel
     accuracy. Integer values correspond to the middle of the pixel,
     e.g.(500, 800) would correspond to the middle of the pixel with
     corners (499.5, 799.5), (499.5, 800.5), (500.5, 799.5), (500.5,
     800.5).*/

  /* Output parameters */
  /* lat - latitude of this pixel (degrees North from Equator) */
  /* long - longitude of this pixel (degrees East from Greenwich) */
  /* visible - flag set to TRUE if pixel is on visible disc, */
  /* - flag set to FALSE if pixel is in space. */
  /* (c) EUMETSAT 1997 */
  /* Set up constants. */
  /* altitude = distance from earth centre to satellite */
  /* req = Equatorial earth radius */
  /* rpol = Polar earth radius */
  /* oblate = earth oblateness */
  /* deg_to_rad and rad_to_deg are conversion factors */
  altitude = (float)42164.;
  req = (float)6378.14;
  rpol = (float)6356.755;
  oblate = (float).0033528131778969143;
  pi = (float)3.141592653;
  deg_to_rad__ = pi / (float)180.;
  rad_to_deg__ = (float)180. / pi;
  /* Step is the radiometer step as seen by the spacecraft, */
  /* in degrees. The image represents an 18deg x 18deg field */
  /* of view divided up on an equi-angular basis. For this */
  /* program an IR channel of 2500 x 2500 is assumed but */
  /* in the real code the size of each channel must be accounted */
  /* for. */
  step = (float).0071999999999999998;
  /*     Convert line/pixel values to angular offsets from centre point */
  /* asamp = -(doublereal)(*pixel - (float)1250.5) * step; */
  asamp = -(doublereal)(*pixel - ((float)(samples) / 2.0 + 0.5)) * step;
  /* aline = (*line - (float)1250.5) * step; */
  aline = (*line - ((float)(samples) / 2.0 + 0.5)) * step;
  asamp *= deg_to_rad__;
  aline *= deg_to_rad__;
  /* Calculate tangents of angles */
  tanal = tan(aline);
  tanas = tan(asamp);
  /* Calculate components of an arbitrary vector from the spacecraft */
  /* in the viewing direction. */
  p = (float)-1.;
  q = tanas;
  r__ = tanal * sqrt(q * q + (float)1.);
  /* The location of the point on the earth can be identified by */
  /* solving a quadratic equation for the intersection between */
  /* the earth's surface and the viewing line from the spacecraft. */
  /* If this equation has no real roots then there is no intersection; */
  /* otherwise the required root is the one nearer to the spacecraft */
  /* (on the visible side of the earth). */
  /* Computing 2nd power */
  r__1 = r__ * req / rpol;
  a = q * q + r__1 * r__1 + p * p;
  b = altitude * (float)2. * p;
  c__ = altitude * altitude - req * req;
  /* Calculate determinant. If it is negative (no real roots to */
  /* quadratic equation) there is no intersection between the */
  /* line of sight and the disc and so the pixel does not correspond */
  /* to visible data. */
  det = b * b - a * 4 * c__;
  if (det <= (float)0.) {
    *visible = FALSE_;
    *lat = (float)0.;
    *long__ = (float)0.;
    goto L999;
  } else {
    *visible = TRUE_;
  }
  k = (-(doublereal)b - sqrt(det)) / (a * (float)2.);
  x = altitude + k * p;
  y = k * q;
  z__ = k * r__;
  *long__ = atan(y / x);
  cenlat = atan(z__ * cos(*long__) / x);
  /* This is the geocentric latitude. Convert it to the geodetic */
  /* (or geographic) latitude before returning it to the calling program */
  /* Computing 2nd power */
  r__1 = (float)1. - oblate;
  *lat = atan(tan(cenlat) / (r__1 * r__1));
  /*     Convert from radians to degrees */
  *lat *= rad_to_deg__;
  *long__ *= rad_to_deg__;
 L999:
  return 0;
} /* refgeo_ */

/* Subroutine */ int georef(real *rlat, real *rlong, integer *line, integer *
	pixel, logical1 *visible)
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double tan(doublereal), atan(doublereal), cos(doublereal), sin(doublereal)
	    , sqrt(doublereal);

    /* Local variables */
    static real long__, rpol, altitude, x, y, z__, aline, asamp, pi, oblate, 
	    deg_to_rad__, rad_to_deg__, geolat, rtheta;
    static integer nlines, nsamps;
    static real lat, req, dotprod;

/*     This subroutine converts pixel position from geographical */
/*     (lat / long) co-ordinates to digital (line / pixel) co-ordinates. */
/*     Input parameters: */
/*     rlat - latitude of pixel (North is +ve, South is -ve) */
/*     rlong - longitude of pixel (East is +ve, West is -ve) */

/*     Note that these are standard geographic co-ordinates as would */
/*     be found in an atlas. */
/*     Output parameters */
/*     line - line number, measured from southern end of frame */
/*     pixel - pixel number, measured from eastern end of frame */
/*     visible - flag set to TRUE if pixel is on visible disc, */
/*     - flag set to FALSE if pixel is in space. */
/*     (c) EUMETSAT 1997 */
/*     Set up constants. */
/*     altitude = distance from earth centre to satellite */
/*     req = Equatorial earth radius */
/*     rpol = Polar earth radius */
/*     oblate = earth oblateness */
/*     deg_to_rad and rad_to_deg are conversion factors */
    altitude = 42164.f;
    req = 6378.14f;
    rpol = 6356.755f;
    oblate = .003352813177896914f;
    pi = 3.141592653f;
    deg_to_rad__ = pi / 180.f;
    rad_to_deg__ = 180.f / pi;
/*     Convert inputs to radians */
    geolat = *rlat * deg_to_rad__;
    long__ = *rlong * deg_to_rad__;
/*     Convert geodetic latitudes (as input) to geocentric latitudes */
/*     for use within the algorithm */
/* Computing 2nd power */
    r__1 = 1.f - oblate;
    lat = atan(r__1 * r__1 * tan(geolat));
/*     Calculate rtheta. This is the distance from the earth centre to */
/*     a point on the surface at latitude 'lat'. */
/* Computing 2nd power */
    r__1 = rpol;
/* Computing 2nd power */
    r__2 = cos(lat);
/* Computing 2nd power */
    r__3 = req;
/* Computing 2nd power */
    r__4 = sin(lat);
    rtheta = req * rpol / sqrt(r__1 * r__1 * (r__2 * r__2) + r__3 * r__3 * (
	    r__4 * r__4));
/*     Calculate Cartesian co-ordinates of target point. This is */
/*     basic geometry. The co-ordinate system is geocentric with */
/*     the x-axis towards the spacecraft, the y-axis to the East */
/*     and the x-axis towards the N pole. */
    x = rtheta * cos(lat) * cos(long__);
    y = rtheta * cos(lat) * sin(long__);
    z__ = rtheta * sin(lat);
/*     Check for invisibility. This is done using the basic geometric */
/*     theorem that the dot product of two vectors A and B is equal */
/*     to */
/*     |A||B| cos (theta) */

/*     where theta is the angle between them. In this case, the test */
/*     is simple. The horizon is defined as the locus of points where */
/*     the local normal is perpendicular to the spacecraft sightline */
/*     vector. All visible points have (theta) less than 90 */
/*     and all invisible points have (theta) greater than 900. */
/*     The test therefore reduces to whether the sign of the dot */
/*     product is +ve or -ve; if it is -ve the point is invisible. */
/*     The vector from the point to the spacecraft has components */
/*     Rs-x, -y, -z where Rs is the distance from the origin to the */
/*     satellite. The vector for the normal has components */
/*     x y z(Re/Rp)^2 */
/* Computing 2nd power */
    r__1 = req / rpol;
    dotprod = (altitude - x) * x - y * y - z__ * z__ * (r__1 * r__1);
    if (dotprod <= 0.f) {
	*visible = FALSE_;
	*line = 0;
	*pixel = 0;
	goto L999;
    } else {
	*visible = TRUE_;
    }
/*     In this co-ordinate system the spacecraft (S) is at position */
/*     (altitude,0,0), the earth centre (O) at (0,0,0) and the point (P) */
/*     at (x,y,z). Two additional points need to be defined, so that the */
/*     angles from the reference planes to the target point (i.e. the */
/*     position of the point in the sensor FOV) can be extracted. */
/*     These points are defined by dropping lines perpendicularly from P */
/*     onto the equatorial plane and the Greenwich meridian plane. */
/*     Their co-ordinates are defined as: */

/*     O' = (x, y, 0) and O'' = (x, 0, z). */

/*     With these points, right-angled triangles can be defined SO'P */
/*     and SO''P which can be used directly to determine the angular */
/*     co-ordinates (aline, asamp) of P in the FOV. */
    asamp = atan(y / (altitude - x));
/* Computing 2nd power */
    r__1 = y;
/* Computing 2nd power */
    r__2 = altitude - x;
    aline = atan(z__ / sqrt(r__1 * r__1 + r__2 * r__2));
/*     Convert back to degrees */
    asamp *= rad_to_deg__;
    aline *= rad_to_deg__;
/*     Calculate line, pixel. Note that since pixels are measured from */
/*     the right of the image, and the angular conversion was measured in */
/*     the x (east) direction, a sign correction has to be included for */
/*     pixels. The image represents an 180 x 180 field of view */
/*     divided up on an equi-angular basis. */
    nlines = lines;
    nsamps = samples;
    asamp /= 18.f / (real) nsamps;
    aline /= 18.f / (real) nlines;
    if (asamp >= 0.f) {
	*pixel = nsamps / 2 - (integer) asamp;
    } else {
	*pixel = nsamps / 2 + 1 - (integer) asamp;
    }
    if (aline >= 0.f) {
	*line = nlines / 2 + 1 + (integer) aline;
    } else {
	*line = nlines / 2 + (integer) aline;
    }
L999:
    return 0;
} /* georef_ */

