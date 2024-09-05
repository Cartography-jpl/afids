#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"

/*
  program quickbirdlog

*/

static char * getLabeledString (char * metaData, char * label, int stripQuotes) {
  char * p, * p2;
  static char buf [100];

  buf [0] = 0;

  if (! (p = strstr (metaData, label)))
    return buf;
  p += strlen (label);
  if (! (p = strchr (p, '=')))
    return buf;
  p += 2; /* skip '=' and space */

  if (stripQuotes)
    p ++;

  if (! (p2 = strchr (p, (stripQuotes ? '"' : ';'))))
    return buf;

  strncpy (buf, p, p2 - p);
  buf [p2 - p] = 0;

  return buf;
}

static double getLabeledDouble (char * metaData, char * label) {
  char * p, * p2;
  char buf [30];
  double d;

  if (! (p = strstr (metaData, label)))
    return 0.0;
  p += strlen (label);
  if (! (p = strchr (p, '=')))
    return 0.0;
  p ++;
  if (! (p2 = strchr (p, ';')))
    return 0.0;
  strncpy (buf, p, p2 - p);
  buf [p2 - p] = 0;
  if (sscanf (buf, "%30lf", & d) != 1)
    return 0.0;

  return d;
}

static int getLabeledInt (char * metaData, char * label) {
  char * p, * p2;
  char buf [30];
  int i;

  if (! (p = strstr (metaData, label)))
    return 0;
  p += strlen (label);
  if (! (p = strchr (p, '=')))
    return 0;
  p ++;
  if (! (p2 = strchr (p, ';')))
    return 0;
  strncpy (buf, p, p2 - p);
  buf [p2 - p] = 0;
  if (sscanf (buf, "%d", & i) != 1)
    return 0;

  return i;
}

#define _quickbirdlog_version_ "Sat Dec 29 2007"

void main44(void)
{
  int parmct, parmdf;
  char imdin [99];
  char txtout [99];
  char * inputMetaData;
  char buf [100];

  checkLoggerUtilsVersion (15);

  sprintf (buf, "quickbirdlog version %s", _quickbirdlog_version_);
  zifmessage (buf);

  /* fetch params */
  zvparm ("imdin", imdin, &parmct, &parmdf, 1, 99);
  zvparm ("txtout", txtout, &parmct, &parmdf, 1, 99);

  /* read input meta data */
  inputMetaData = mallocAndRead (imdin);

  /* create output meta data */
  initMetaData (txtout);
  logMetaString (0, txtout, "QUICKBIRDLOG_VERSION", _quickbirdlog_version_, 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_FORMAT", "NITF", 0, 0);
    
  logMetaString (0, txtout, "ARCH_PLATFORM", "quickbird", 0, 0);

  /* get the corners */
  logMetaDouble (0, txtout, "QUICKBIRD_UL_LAT", getLabeledDouble (inputMetaData, "ULLat"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_UL_LON", getLabeledDouble (inputMetaData, "ULLon"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_UR_LAT", getLabeledDouble (inputMetaData, "URLat"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_UR_LON", getLabeledDouble (inputMetaData, "URLon"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_LR_LAT", getLabeledDouble (inputMetaData, "LRLat"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_LR_LON", getLabeledDouble (inputMetaData, "LRLon"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_LL_LAT", getLabeledDouble (inputMetaData, "LLLat"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_LL_LON", getLabeledDouble (inputMetaData, "LLLon"), 0, 0);

  /* get the angles */
  logMetaDouble (0, txtout, "QUICKBIRD_IN_TRACK_VIEW_ANGLE", getLabeledDouble (inputMetaData, "inTrackViewAngle"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_CROSS_TRACK_VIEW_ANGLE", getLabeledDouble (inputMetaData, "crossTrackViewAngle"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_OFF_NADIR_VIEW_ANGLE", getLabeledDouble (inputMetaData, "offNadirViewAngle"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_ORIENTATION_ANGLE", getLabeledDouble (inputMetaData, "orientationAngle"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_SUN_AZIMUTH", getLabeledDouble (inputMetaData, "sunAz"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_SUN_ELEVATION", getLabeledDouble (inputMetaData, "sunEl"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_SAT_AZIMUTH", getLabeledDouble (inputMetaData, "satAz"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_SAT_ELEVATION", getLabeledDouble (inputMetaData, "satEl"), 0, 0);
    
  /* get projection data */
  logMetaString (0, txtout, "QUICKBIRD_EARLIEST_ACQUISITION_TIME", getLabeledString (inputMetaData, "earliestAcqTime", 0), 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_DATUM_NAME", getLabeledString (inputMetaData, "datumName", 1), 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_PROJ_NAME", getLabeledString (inputMetaData, "mapProjName", 1), 0, 0);
  logMetaInt (0, txtout, "QUICKBIRD_MAP_ZONE", getLabeledInt (inputMetaData, "mapZone"), 0, 0); /* make int */
  logMetaString (0, txtout, "QUICKBIRD_MAP_HEMI", getLabeledString (inputMetaData, "mapHemi", 1), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_ORIGIN_X", getLabeledDouble (inputMetaData, "originX"), 0, 0);
  logMetaDouble (0, txtout, "QUICKBIRD_ORIGIN_Y", getLabeledDouble (inputMetaData, "originY"), 0, 0);

  /* get UTM data */
  {
    double ulx = getLabeledDouble (inputMetaData, "ULX");
    double uly = getLabeledDouble (inputMetaData, "ULY");
    double urx = getLabeledDouble (inputMetaData, "URX");
    double lly = getLabeledDouble (inputMetaData, "LLY");
    int columns = getLabeledInt (inputMetaData, "numColumns");
    int rows = getLabeledInt (inputMetaData, "numRows");

    logMetaInt (0, txtout, "QUICKBIRD_COLUMNS", columns, 0, 0);
    logMetaInt (0, txtout, "QUICKBIRD_ROWS", rows, 0, 0);

    logMetaDouble (0, txtout, "QUICKBIRD_UL_X", ulx, 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_UL_Y", uly, 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_UR_X", urx, 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_UR_Y", getLabeledDouble (inputMetaData, "URY"), 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_LR_X", getLabeledDouble (inputMetaData, "LRX"), 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_LR_Y", getLabeledDouble (inputMetaData, "LRY"), 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_LL_X", getLabeledDouble (inputMetaData, "LLX"), 0, 0);
    logMetaDouble (0, txtout, "QUICKBIRD_LL_Y", lly, 0, 0);
    logMetaDouble (0, txtout, "CALC_UTM_XRES", (urx - ulx) / (double) (columns - 1), 0, 0);
    logMetaDouble (0, txtout, "CALC_UTM_YRES", (uly - lly) / (double) (rows - 1), 0, 0);
  }

  /* get other interesting data */
  logMetaString (0, txtout, "QUICKBIRD_IMAGE_DESCRIPTOR", getLabeledString (inputMetaData, "imageDescriptor", 1), 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_BAND_ID", getLabeledString (inputMetaData, "bandId", 1), 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_PRODUCT_LEVEL", getLabeledString (inputMetaData, "productLevel", 1), 0, 0);
  logMetaString (0, txtout, "QUICKBIRD_COMPRESSION_TYPE", getLabeledString (inputMetaData, "compressionType", 1), 0, 0);
  logMetaInt (0, txtout, "QUICKBIRD_BITS_PER_PIXEL", getLabeledInt (inputMetaData, "bitsPerPixel"), 0, 0);

  free (inputMetaData);

  return;
}
