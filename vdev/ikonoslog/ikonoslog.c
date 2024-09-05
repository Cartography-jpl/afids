#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"

/*
  program ikonoslog

*/

#define _ikonoslog_version_ "Thu Jan  3 2008"

static void getLatLon (char * label, char * p, double * lat, double * lon) {
  char * p2;

  if (! (p2 = strstr (p, label)))
    zmabend ("txtin not in expected format gll1");
  p2 += strlen ("label");

  if (! (p2 = strstr (p2, "Latitude: ")))
    zmabend ("txtin not in expected format gll2");
  p2 += strlen ("Latitude: ");

  if (sscanf (p2, "%20lf", lat) != 1)
    zmabend ("txtin not in expected format gll3");
  
  if (! (p2 = strstr (p2, "Longitude: ")))
    zmabend ("txtin not in expected format gll4");
  p2 += strlen ("Longitude: ");

  if (sscanf (p2, "%20lf", lon) != 1)
    zmabend ("txtin not in expected format gll5");
}
  
static void getNLNS (char * p, int * nl, int * ns) {
  char * p2;

  if (! (p2 = strstr (p, "Columns:")))
    zmabend ("hdrin not in expected format gnlns2");
  p2 += strlen ("Columns:");

  if (sscanf (p2, "%d", ns) != 1)
    zmabend ("hdrin not in expected format gnlns3");
  
  if (! (p2 = strstr (p2, "Rows:")))
    zmabend ("hdrin not in expected format gnlns4");
  p2 += strlen ("Rows:");

  if (sscanf (p2, "%d", nl) != 1)
    zmabend ("hdrin not in expected format gnlns5");
}
  
static int getNE (char * p, double * northing, double * easting) {
  char * p2;

  if (! (p2 = strstr (p, "UL Map X (Easting): ")))
    return 1;
  p2 += strlen ("UL Map X (Easting): ");

  if (sscanf (p2, "%15lf", easting) != 1)
    return 2;
  
  if (! (p2 = strstr (p2, "UL Map Y (Northing): ")))
    return 3;
  p2 += strlen ("UL Map Y (Northing): ");

  if (sscanf (p2, "%15lf", northing) != 1)
    return 4;

  return 0;
}
  
static void getPS (char * p, double * sizeX, double * sizeY) {
  char * p2;

  if (! (p2 = strstr (p, "Pixel Size X: ")))
    zmabend ("hdrin not in expected format gne2");
  p2 += strlen ("Pixel Size X: ");

  if (sscanf (p2, "%15lf", sizeX) != 1)
    zmabend ("hdrin not in expected format gne3");
  
  if (! (p2 = strstr (p2, "Pixel Size Y: ")))
    zmabend ("hdrin not in expected format gne4");
  p2 += strlen ("Pixel Size Y: ");

  if (sscanf (p2, "%15lf", sizeY) != 1)
    zmabend ("hdrin not in expected format gne5");
}
  
void main44(void)
{
  int parmct, parmdf;
  char imgin [99];
  char txtin [99];
  char hdrin [99];
  char txtout [99];
  char * inputMetaData;
  char * p, * p2;
  char buf [1000];
  double az, el, sunAz, sunEl, lat, lon;
  int columns, rows;
  double northing, easting, sizeX, sizeY;
  char * coordinatesSearchString;

  checkLoggerUtilsVersion (15);

  sprintf (buf, "ikonoslog version %s", _ikonoslog_version_);
  zifmessage (buf);

  /* fetch params */
  zvparm ("imgin", imgin, &parmct, &parmdf, 1, 99);
  zvparm ("txtin", txtin, &parmct, &parmdf, 1, 99);
  zvparm ("hdrin", hdrin, &parmct, &parmdf, 1, 99);
  zvparm ("txtout", txtout, &parmct, &parmdf, 1, 99);

  /* read input meta data */
  inputMetaData = mallocAndRead (txtin);

  /* create output meta data */
  initMetaData (txtout);

  if (! strncmp ("tif", imgin + strlen (imgin) - 3, 3)) {
    logMetaString (0, txtout, "IKONOS_FORMAT", "GeoTIFF", 0, 0);
    coordinatesSearchString = "Tile File Name: ";
  } else {
    logMetaString (0, txtout, "IKONOS_FORMAT", "NITF", 0, 0);
    coordinatesSearchString = "Component File Name: ";
  }
    
  /* NITF metadata files have one or more sections looking like:

     Product Image ID: 000
     Component File Name: po_120898_red_0000000.ntf po_120898_grn_0000000.ntf po_120898_blu_0000000.ntf po_120898_nir_0000000.ntf 
     Thumbnail File Name: po_120898_rgb_0000000_ovr.jpg
     Country Code: US
     Component Geographic Corner Coordinates

     where the Component File Name line may have only one file name.

     GeoTIFF metadata files have one or more sections looking like:

     Product Image ID: 000
     Tile File Name: po_77829_pan_0000000.tif po_77829_red_0000000.tif po_77829_grn_0000000.tif po_77829_blu_0000000.tif po_77829_nir_0000000.tif 
     Tile Geographic Corner Coordinates

     where the Tile File Name line may have only one file name.

     Both NITF and GeoTIFF Component/Tile File Name lines are soon
     followed by a Component/Tile Geographic Corner Coordinates
     section containing corner Lat/Lons.

     Both NITF and GeoTIFF have sections including parameters such as
     Nominal Collection Azimuth, associated with Product Image ID.  

     In NITF, each Product Image ID line is followed by either a
     Component File Name line or a Sensor: line.

     The Sensor: lines are found following the single Source Image Metadata line.

     In GeoTIFF, each Product Image ID line is followed by either a
     Tile File Name line or a Sensor: line.

     Image filenames are found both on (one) Component/Tile File Name
     line, and sometimes on a Stereo Mate File Name: line, following
     the Component/Tile File Name line of another file.

     Algorithm:

     The Component/Tile File Name lines are searched for the imgin
     name to find the corner coordinates and the Product Image ID. The
     Product Image ID is used to find the metadata in the Source Image
     Metadata section.

  */

  /* Find the image filename in the metadata. The line begins with
   coordinatesSearchString and has the image filename somewhere
   following it before the \n. */

  p = inputMetaData;
  /* while p is within the inputMetaData */
  while (p - inputMetaData < strlen (inputMetaData)) { 
    p = strstr (p, coordinatesSearchString);
    if (! p)
      zmabend ("imgin component/tile file name not found in txtin metadata");
    p += strlen (coordinatesSearchString);
    /* if the filename is on this component/tile file name line, we found it */
    if (strstr (p, imgin) < strstr (p, "\n"))
      break;
  }

  /* back up to beginning of component/tile file name line */
  while (p >= inputMetaData && * p != '\n')
    p --;
  if (p >= inputMetaData)
    p --;			/* skip \n */
  else
    zmabend ("txtin not in expected format 1");

  /* double char \n in DOS */
  if (p >= inputMetaData && (* p == '\r' || * p == '\v'))
    p --;

  /* p now points to end of Product Image Id line */

  p2 = p; /* save end of Product Image ID... line */

  /* back up to beginning of Product Image Id line */
  while (p >= inputMetaData && * p != '\n')
    p --;
  if (p >= inputMetaData)
    p ++;
  else
    zmabend ("txtin not in expected format 2");

  /* Make sure we have the Product Image ID line */
  if (strncmp (p, "Product Image ID:", strlen ("Product Image ID:")))
    zmabend ("txtin not in expected format 3");

  strncpy (buf, p, p2 - p);
  buf [p2 - p] = '\0';		/* buf now has "Product Image ID: 00X" */

  /* get the corners, nl, ns */

  if (! (p = strstr (p2, "Geographic Corner Coordinates")))
    zmabend ("txtin not in expected format 3.1");

  /* check assumption that there are 4 coordinates (corners) given */
  if (! (p2 = strstr (p, "Number of Coordinates:")))
    zmabend ("txtin not in expected format 3.2");
  p2 += strlen ("Number of Coordinates:");

  {
    int numCoord;
    if (sscanf (p2, "%d", & numCoord) != 1)
      zmabend ("txtin not in expected format 3.3");
    if (numCoord != 4)
      zmabend ("txtin not in expected format 3.4");
  }

  logMetaString (0, txtout, "ARCH_PLATFORM", "ikonos", 0, 0);

  getLatLon ("Coordinate: 1", p, & lat, & lon);
  logMetaDouble (0, txtout, "IKONOS_COORD1_LAT", lat, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_COORD1_LON", lon, 0, 0);

  getLatLon ("Coordinate: 2", p, & lat, & lon);
  logMetaDouble (0, txtout, "IKONOS_COORD2_LAT", lat, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_COORD2_LON", lon, 0, 0);

  getLatLon ("Coordinate: 3", p, & lat, & lon);
  logMetaDouble (0, txtout, "IKONOS_COORD3_LAT", lat, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_COORD3_LON", lon, 0, 0);

  getLatLon ("Coordinate: 4", p, & lat, & lon);
  logMetaDouble (0, txtout, "IKONOS_COORD4_LAT", lat, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_COORD4_LON", lon, 0, 0);

  /* get the angles */
  if (! (p = strstr (inputMetaData, "Source Image Metadata")))
    zmabend ("txtin not in expected format 4");

  if (! (p = strstr (p, buf)))
    zmabend ("txtin not in expected format 5");
    
  if (! (p2 = strstr (p, "Nominal Collection Azimuth:")))
    zmabend ("txtin not in expected format 6");
  p2 += strlen ("Nominal Collection Azimuth:");
  if (sscanf (p2, "%lf", & az) != 1)
    zmabend ("txtin not in expected format 7");
    
  if (! (p2 = strstr (p, "Nominal Collection Elevation:")))
    zmabend ("txtin not in expected format 8");
  p2 += strlen ("Nominal Collection Elevation:");
  if (sscanf (p2, "%lf", & el) != 1)
    zmabend ("txtin not in expected format 9");
    
  if (! (p2 = strstr (p, "Sun Angle Azimuth:")))
    zmabend ("txtin not in expected format 10");
  p2 += strlen ("Sun Angle Azimuth:");
  if (sscanf (p2, "%lf", & sunAz) != 1)
    zmabend ("txtin not in expected format 11");
    
  if (! (p2 = strstr (p, "Sun Angle Elevation:")))
    zmabend ("txtin not in expected format 12");
  p2 += strlen ("Sun Angle Elevation:");
  if (sscanf (p2, "%lf", & sunEl) != 1)
    zmabend ("txtin not in expected format 13");
    
  if (! (p2 = strstr (p, "Acquisition Date/Time:")))
    zmabend ("txtin not in expected format 14");
  p2 += strlen ("Acquisition Date/Time:");
  p2 ++;
  p2 [16] = 0;
  logMetaString (0, txtout, "IKONOS_ACQUISITION_DATE_TIME", p2, 0, 0);

  logMetaDouble (0, txtout, "IKONOS_NOMCOL_AZIMUTH", az, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_NOMCOL_ELEVATION", el, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_SUN_AZIMUTH", sunAz, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_SUN_ELEVATION", sunEl, 0, 0);

  if ((p = strstr (inputMetaData, "UTM Specific Parameters"))) {
    if ((p2 = strstr (p, "Hemisphere: "))) {
      p2 += strlen ("Hemisphere: ");
      buf [0] = p2 [0];
      buf [1] = 0;
      logMetaString (0, txtout, "IKONOS_UTM_HEMISPHERE", buf, 0, 0);
    } else
      zmabend ("txtin not in expected format 15");

    if ((p2 = strstr (p, "Zone Number: "))) {
      int zone;
      p2 += strlen ("Zone Number: ");
      if (sscanf (p2, "%d", & zone) != 1)
	zmabend ("txtin not in expected format 16");
      logMetaInt (0, txtout, "IKONOS_UTM_ZONE", zone, 0, 0);
    } else
      zmabend ("txtin not in expected format 17");
  }

  free (inputMetaData);

  /* read input meta data */
  inputMetaData = mallocAndRead (hdrin);

  if (! getNE (inputMetaData, & northing, & easting)) {
    logMetaDouble (0, txtout, "IKONOS_UL_NORTHING", northing, 0, 0);
    logMetaDouble (0, txtout, "IKONOS_UL_EASTING", easting, 0, 0);
  }

  getPS (inputMetaData, & sizeX, & sizeY);
  logMetaDouble (0, txtout, "IKONOS_PIXEL_SIZE_X", sizeX, 0, 0);
  logMetaDouble (0, txtout, "IKONOS_PIXEL_SIZE_Y", sizeY, 0, 0);

  getNLNS (inputMetaData, & rows, & columns);
  logMetaInt (0, txtout, "IKONOS_COLUMNS", columns, 0, 0);
  logMetaInt (0, txtout, "IKONOS_ROWS", rows, 0, 0);

  free (inputMetaData);

  return;
}
