#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"
#undef VOID			/* Defined in VICAR, conflicts with
				   HDF library */
#include "carto/hdfIncludes.h"

/*
  program asterlog

  1) Reads an ASTER data set named in the inp arg
  2) Writes VICAR img file
  3) IBIS nav file
  4) txt meta data file

  Parameters:
  inp    - the name of the archive file
  sensor - the name of the sensor (one of VNIR, SWIR, TIR)
  out    - the names of the images (3 to 6 names) and IBIS nav file name
  meta   - the name of the meta file

  sl     - start line (one based value with (1,1) pixel in nw corner
  ss     - start sample (one based value)
  nl     - number of lines to extract
  ns     - number of samples to extract

  Assumptions:

  The input ASTER archive must be in HDF-EOS format as documented in
  "ASTER LEVEL 1 DATA PRODUCTS SPECIFICATION (GDS Version) Version
  1.3" (AG-E-E-2209-R03), dated June 25, 2001.

*/

#define _asterlog_version_ "Sat Dec 29 2007"

static char msgBuf [200];

static int attachToNamedSwath (int hdfFileId, char * swathName) {
  int swathHandle;

  if ((swathHandle = SWattach (hdfFileId, swathName)) == -1) {
    sprintf (msgBuf, "SWattach failed attaching to swath \"%s\"", swathName);
    zmabend (msgBuf);
  }

  return swathHandle;
}

void main44(void)
{
  int sl, ss, nl, ns;
  int parmct, parmdf;
  int status;
  char infilename [99];
  char outfilename [99];
  char outImageName [200];
  char outNavName [200];
  char outMetaName [200];

  char sensor [99];
  char logIdBuf [50];
  int32 hdfFileId;
  int32 swathHandle;
  int lines, samples;
  int lookback;

  char product[50];		/* product type, one of 1A, 1B, or a surface derived product */
  char latname [99];
  int * vunits = 0;
  int image;
  int imageCount;
  char * swathsBuf = 0;
  double latMatrix [11] [11];
  double lonMatrix [11] [11];
  char * coremetadata = 0, * productmetadata = 0;
  char date [20];
  char time [20];
  enum {pixel8bit, pixel16bit} pixelSize; /* TIR and Kinetic are 16-bit, else 8-bit */
  int firstBand, lastBand;
  int32 ndims;
  int geoconv = 1;		/* true for 1A and 1B, but not surface drived products, which are already in geodetic */
  int echoMeta = 0;
  int doimg;
  int pseudo;

  checkLoggerUtilsVersion (14);

  pseudo = zvptst("pseudo");

  sprintf (logIdBuf, "asterlog version %s", _asterlog_version_);
  zifmessage (logIdBuf);

  /* fetch parms */
  zvparm ("inp", infilename, &parmct, &parmdf, 1, 99);
  zvparm ("out", outfilename, &parmct, &parmdf, 1, 99);

  zvp("sl", &sl, &parmct);
  zvp("ss", &ss, &parmct);
  zvp("nl", &nl, &parmct);
  zvp("ns", &ns, &parmct);

  zvparm ("sensor", sensor, &parmct, &parmdf, 1, 99);
  zvp("lookback", &lookback, &parmct); /* get lookback flag */

  zvp("doimg", &doimg, &parmct);
  zvp("echoMeta", &echoMeta, &parmct);

  /* get coremetadata.0 and productmetadata.0 */
  {
    int32 attr_index;
    char nambuf[1000];
    int32 data_type, n_values;
    int sd_id = SDstart (infilename, DFACC_READ); /* open hdf file */

    /* get coremetadata.0 attribute index */
    attr_index = SDfindattr (sd_id, "coremetadata.0");
      
    /* see how long the coremetadata.0 string is */
    if (SDattrinfo(sd_id, attr_index, nambuf, &data_type,&n_values) == -1)
      zmabend ("SDattrinfo failed on coremetadata");

    /* allocate space for it */
    coremetadata = malloc (n_values + 1);
      
    /* read it */
    if (SDreadattr(sd_id, attr_index, coremetadata) == -1)
      zmabend ("SDreadattr failed on coremetadata");

    /* get productmetadata.0 index */
    attr_index = SDfindattr (sd_id, "productmetadata.0");
      
    /* see how long the productmetadata.0 string is */
    if (SDattrinfo(sd_id, attr_index, nambuf, &data_type,&n_values) == -1)
      zmabend ("SDattrinfo failed on productmetadata");

    /* allocate space for it */
    productmetadata = malloc (n_values + 1);
      
    /* read it */
    if (SDreadattr(sd_id, attr_index, productmetadata) == -1)
      zmabend ("SDreadattr failed on productmetadata");

    /* close the hdf file */
    SDend (sd_id);
  }

  /* get product type */
  {
    char *p;

    p = strstr (coremetadata, "PROCESSINGLEVELID"); /* 1A or 1B */
    if (p) {
      p = strstr (p, "VALUE");
      p = strstr (p, "\"");
      p ++;
      strncpy (product, p, 2);
      product [2] = 0;
    } else {
      p = strstr (coremetadata, "PARAMETERNAME"); /* Emissivity (05), Kinetic (08), Radiance (09T) */
      if (p) {
	p = strstr (p, "VALUE");
	p = strstr (p, "\"Surface ");
	p += strlen ("\"Surface ");
	strncpy (product, p, 2);
	product [2] = 0;
      } else
	zmabend ("Unable to determine Aster product type from coremetadata (expecting one of 1A, 1B, 05, 08, 09T)");
    }

    /* assume Surface Product, if not 1A or 1B */
    if (strcmp (product, "1A") && strcmp (product, "1B")) { /* Surface Product */
      if (! strcmp (product, "Ki"))
	strcpy (product, "Kinetic");
      else if (! strcmp (product, "Em"))
	strcpy (product, "Emissivity");
      else if (! strcmp (product, "Ra"))
	strcpy (product, "Radiance");
      else
	zmabend ("Surface derived product is not one if (Emissivity-05, Kinetic-08, Radiance-09T)");

      if ( strcmp (sensor, "") && ! pseudo ) {
	sprintf (msgBuf,
		 "Sensor \"%s\" selected, but not available, for non-1A/1B product", sensor);
	zmabend (msgBuf);
      }

      strcpy (latname, "GeodeticLatitude");
    } else {			/* 1A or 1B */
      /* check requested sensor name */
      if (strcmp (sensor, "VNIR") &&
	  strcmp (sensor, "SWIR") &&
	  strcmp (sensor, "TIR")) {
	sprintf (msgBuf, "Expected one of (VNIR, SWIR, TIR) for sensor name, but got \"%s\"", sensor);
	zmabend (msgBuf);
      }

      if (!strcmp (product, "1B")) /* It's 1B */
	strcpy (latname, "Latitude"); /* nav not processed for 1A */
    }
  }

  /* determine type and number of images */
  if (! strcmp (product, "Kinetic")) {
    firstBand = lastBand = -1;
    geoconv = 0;		/* Kinetic is already in geodetic */
    if (lookback)
      zmabend ("Lookback flag set, but channel 3B not available, for Surface Kinetic Temperature product");
    pixelSize = pixel16bit;
    imageCount = 1;		/* SurfaceKineticTemperature is the only image */
  } else if (! strcmp (product, "Emissivity") || ! strcmp (product, "Radiance")) {
    geoconv = 0;		/* These are already in geodetic */
    if (lookback)
      zmabend ("Lookback flag set, but channel 3B not available, for non-1A/1B products");
    firstBand = 10;
    lastBand = 14;
    pixelSize = pixel16bit;
    imageCount = 5;		/* Band10 through Band14 */
  } else {			/* It's 1A or 1B; VNIR, SWIR or TIR */
    if (! strcmp (sensor, "TIR")) { /* It's TIR */
      if (lookback)
	zmabend ("Lookback flag set, but channel 3B not available, for selected TIR sensor");
      firstBand = 10;
      lastBand = 14;
      pixelSize = pixel16bit;
      imageCount = 5;		/* TIR_Band10 through TIR_Band14 */
    } else {			/* It's VNIR or SWIR */
      pixelSize = pixel8bit;
      if (! strcmp (sensor, "VNIR")) { /* It's VNIR */
	lastBand = 3;
	if (lookback) {
	  firstBand = 3;
	  imageCount = 1;	/* band 3B */
	} else {
	  firstBand = 1;
	  imageCount = 3;	/* bands 1, 2, 3N */
	}
      } else { 			/* It's SWIR */
	if (lookback)
	  zmabend ("Lookback flag set, but channel 3B not available for selected SWIR sensor");
	firstBand = 4;
	lastBand = 9;
	imageCount = 6;		/* bands 4, 5, 6, 7, 8, 9 */
      }
    }
  }

  /* get acquisition date/time */
  {
    char *p;

    p = strstr (coremetadata, "SINGLEDATETIME");
    p = strstr (p, "CALENDARDATE");
    p = strstr (p, "VALUE");
    p = strstr (p, "\"");
    p ++;
    if (! strcmp (product, "Kinetic") || ! strcmp (product, "Emissivity") || ! strcmp (product, "Radiance")) { /* It's a derived surface product */
      strncpy (date, p, 4);
      date [4] = 0;
      strncat (date, p + 5, 2);
      strncat (date, p + 8, 2);
    } else {			/* 1A and 1B have a different date format */
      strncpy (date, p, 10);
      date [10] = 0;
    }

    p = strstr (coremetadata, "SINGLEDATETIME");
    p = strstr (p, "TIMEOFDAY");
    p = strstr (p, "VALUE");
    p = strstr (p, "\"");
    p ++;
    if (! strcmp (product, "Kinetic") || ! strcmp (product, "Emissivity") || ! strcmp (product, "Radiance")) { /* It's a derived surface product */
      strncpy (time, p, 2);
      time [2] = 0;
      strncat (time, p + 3, 2);
      strncat (time, p + 6, 2);
      strncat (time, p + 9, 2);
    } else {			/* 1A and 1B have a different time format */
      strncpy (time, p, 10);
      time [10] = 0;
    }
  }

  /* compute output filename prefix */
  if (strlen (outfilename) == 0) {
    char shortTime [7];
    strncpy (shortTime, time, 6);
    shortTime [6] = 0;
    sprintf (outfilename, "TERRA_ASTER_%s_%s", date, shortTime);
  }

  if (! strcmp (product, "Kinetic") && ! pseudo )
    sprintf (outMetaName, "%s_SKT-08.txt", outfilename);
  else if (! strcmp (product, "Emissivity") && ! pseudo )
    sprintf (outMetaName, "%s_SE-05.txt", outfilename);
  else if (! strcmp (product, "Radiance"))
    sprintf (outMetaName, "%s_SRTIR-09T.txt", outfilename);
  else if (! strcmp (sensor, "VNIR") && lookback)
    sprintf (outMetaName, "%s_3B.txt", outfilename);
  else if (! strcmp (sensor, "VNIR") && ! lookback)
    sprintf (outMetaName, "%s_VNIR.txt", outfilename);
  else if (! strcmp (sensor, "SWIR"))
    sprintf (outMetaName, "%s_SWIR.txt", outfilename);
  else
    sprintf (outMetaName, "%s_TIR.txt", outfilename);

  /* create metadata file  */
  initMetaData (outMetaName);

  /* check swath availability */
  if (strcmp (product, "Kinetic")) { /* It's not Kinetic */
    {
      int32 bufSize;

      /* query which swaths are available */
      if (SWinqswath (infilename, NULL, & bufSize) < 1)
	zmabend ("Error querying swaths for bufSize");

      swathsBuf = malloc (bufSize + 1);
      if (SWinqswath (infilename, swathsBuf, & bufSize) < 1)
	zmabend ("Error querying swaths");

      /* make sure the requested sensor is here */
      if (! strstr (swathsBuf, sensor) && ! pseudo ) {
	sprintf (msgBuf, "Sensor %s requested, but only %s swaths available", sensor, swathsBuf);
	zmabend (msgBuf);
      }
    }
  }

  /* open hdf file */
  if ((hdfFileId = SWopen (infilename, DFACC_READ)) == -1) {
    sprintf (msgBuf, "Failed opening %s with SWopen", infilename);
    zmabend (msgBuf);
  }
  
  /* grab swath handle for image dimensions */
  {
      char swathName [99];
      if (! strcmp (product, "1A")) {
	if (lookback) {
	  strcpy (swathName, "VNIR_Band3B");
	} else {
	  sprintf (swathName, "%s_Band%d", sensor, firstBand);
	}
      } else if (! strcmp (product, "1B")) {
	sprintf (swathName, "%s_Swath", sensor);
      } else if (! strcmp (product, "Kinetic")) {
	strcpy (swathName, "SurfaceKineticTemperature");
      } else if (! strcmp (product, "Emissivity")) {
	strcpy (swathName, "SurfaceEmissivity");
      } else if (! strcmp (product, "Radiance")) {
	strcpy (swathName, "SurfaceRadianceTIR");
      }

      swathHandle = attachToNamedSwath (hdfFileId, swathName);
  }
  
  /* get image dimensions */
  {
    char buf [200];
    int32 dims [8];
    char * imageLineP, * imagePixelP, * p;
    int commaCount;

    ndims = SWinqdims (swathHandle, buf, dims);

    if (lookback && ndims == 4)
      zmabend ("Lookback requested for archive lacking Lines3B dimension");

    imageLineP = strstr (buf, "ImageLine");
    imagePixelP = strstr (buf, "ImagePixel");

    commaCount = 0;
    for (p = buf; p < imageLineP; p++)
      if (*p == ',')
	commaCount ++;

    lines = dims [commaCount];

    commaCount = 0;
    for (p = buf; p < imagePixelP; p++)
      if (*p == ',')
	commaCount ++;

    samples = dims [commaCount];
  }

  /* check sub area shape */
  forceSubAreaSanity (& sl, & ss, & nl, & ns, lines, samples);

  /* create output image files, if requested */
  if (doimg) {

    vunits = (int *) malloc (sizeof (int) * imageCount);

    for (image = 0; image < imageCount; image ++) {
      if (! strcmp (product, "Kinetic"))
	if ( pseudo )		/* pretend TIR band 10 */
	  sprintf (outImageName, "%s_10.img", outfilename );
	else
	  sprintf (outImageName, "%s_SKT-08.img", outfilename);
      else if (! strcmp (product, "Emissivity"))
	if ( pseudo )		/* pretend TIR band */
	  sprintf (outImageName, "%s_%d.img", outfilename, image + firstBand);
	else
	  sprintf (outImageName, "%s_SE-05_%d.img", outfilename, image + firstBand);
      else if (! strcmp (product, "Radiance"))
	sprintf (outImageName, "%s_SRTIR-09T_%d.img", outfilename, image + firstBand);
      else if (! strcmp (sensor, "VNIR") && lookback)
	sprintf (outImageName, "%s_3B.img", outfilename);
      else if (! strcmp (sensor, "VNIR") && image == 2)
	sprintf (outImageName, "%s_3N.img", outfilename);
      else
	sprintf (outImageName, "%s_%d.img", outfilename, image + firstBand);

      if (zvunit (& vunits [image], "ignore", image + 1, "U_NAME", outImageName, NULL) != 1)
	zmabend ("zvunit failed for out image");

      switch (pixelSize) {
      case pixel8bit:
	if (zvopen (vunits [image],
		    "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", NULL) != 1)
	  zmabend ("zvopen failed for out image");
	break;
      case pixel16bit:
	if (zvopen (vunits [image],
		    "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
		    "O_FORMAT", "HALF", NULL) != 1)
	  zmabend ("zvopen failed for out image");
	break;
      default:
	zmabend ("Illegal pixel size");
      }
    }
  }

  logMetaString (echoMeta, outMetaName, "ASTER_PRODUCT_TYPE", product, doimg?imageCount:0, & vunits [0]);
  logMetaString (echoMeta, outMetaName, "ASTER_CALENDARDATE", date, doimg?imageCount:0, & vunits [0]);
  logMetaString (echoMeta, outMetaName, "ASTER_TIMEOFDAY", time, doimg?imageCount:0, & vunits [0]);
  if (swathsBuf)
    logMetaString (echoMeta, outMetaName, "ASTER_SWATHS", swathsBuf, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "ARCH_LINES", lines, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "ARCH_SAMPLES", samples, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "LOG_SL", sl, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "LOG_SS", ss, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "LOG_NL", nl, doimg?imageCount:0, & vunits [0]);
  logMetaInt (echoMeta, outMetaName, "LOG_NS", ns, doimg?imageCount:0, & vunits [0]);
  if (strcmp (product, "Kinetic")) /* It's not kinetic */
    logMetaString (echoMeta, outMetaName, "LOG_SENSOR", sensor, doimg?imageCount:0, & vunits [0]);

  if (doimg) {
    /* get the images */
    if (! strcmp (product, "1A")) { /* It's 1A */
      if (! strcmp (sensor, "VNIR")) { /* It's VNIR */
	{
	  int line;
	  uint8 * imageRow = malloc (sizeof (uint8) * ns);
	  char swathName [99];
	  char * fieldName = "ImageData";

	  for (image = 0; image < imageCount; image ++) {
	    /* compute swathName */
	    if (lookback)
	      strcpy (swathName, "VNIR_Band3B");
	    else {
	      switch (image) {
	      case 0:
	      case 1:
		sprintf (swathName, "%s_Band%d", sensor, image + 1);
		break;
	      case 2:
		strcpy (swathName, "VNIR_Band3N");
		break;
	      default:		/* impossible */
		zmabend ("internal error 42");
	      }
	    }

	    swathHandle = attachToNamedSwath (hdfFileId, swathName);

	    for (line = sl; line < sl + nl; line ++) {
	      {
		int32 start [2];
		int32 edge [2];
		start [0] = line - 1; /* these are zero based */
		start [1] = ss - 1; /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "VNIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      } else if (! strcmp (sensor, "SWIR")) {
	{
	  int line;
	  uint8 * imageRow = malloc (sizeof (uint8) * ns);
	  char swathName [99];
	  char * fieldName = "ImageData";

	  for (image = 0; image < imageCount; image ++) {
	    /* compute swathName */
	    sprintf (swathName, "%s_Band%d", sensor, image + 4);

	    swathHandle = attachToNamedSwath (hdfFileId, swathName);

	    for (line = sl; line < sl + nl; line ++) {
	      {
                int32 start [2];
		int32 edge [2];

                start [0] = line - 1;  /* these are zero based */
                start [1] = ss - 1;    /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "SWIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      } else if (! strcmp (sensor, "TIR")) {
	{
	  int line;
	  uint16 * imageRow = malloc (sizeof (uint16) * ns);
	  char swathName [99];
	  char * fieldName = "ImageData";

	  for (image = 0; image < imageCount; image ++) {
	    /* compute swathName */
	    sprintf (swathName, "%s_Band%d", sensor, image + 10);

	    swathHandle = attachToNamedSwath (hdfFileId, swathName);

	    for (line = sl; line < sl + nl; line ++) {
	      {
		int32 start [2]; /* these are zero based */
		int32 edge [2];

		start [0] = line - 1; /* these are zero based */
		start [1] = ss - 1; /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "TIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      }
    } else if (! strcmp (product, "1B")) { /* It's 1B */
      if (! strcmp (sensor, "VNIR")) { /* It's VNIR */
	{
	  int line;
	  uint8 * imageRow = malloc (sizeof (uint8) * ns);
	  char fieldName [100];

	  for (image = 0; image < imageCount; image ++) {
	    /* compute fieldName */
	    if (lookback)
	      strcpy (fieldName, "ImageData3B");
	    else {
	      switch (image) {
	      case 0:
	      case 1:
		sprintf (fieldName, "ImageData%d", image + 1);
		break;
	      case 2:
		strcpy (fieldName, "ImageData3N");
		break;
	      default:		/* impossible */
		zmabend ("internal error 42");
	      }
	    }

	    for (line = sl; line < sl + nl; line ++) {
	      {
		int32 start [2]; /* these are zero based */
		int32 edge [2];

		start [0] = line - 1; /* these are zero based */
		start [1] = ss - 1;   /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "VNIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      } else if (! strcmp (sensor, "SWIR")) {
	{
	  int line;
	  uint8 * imageRow = malloc (sizeof (uint8) * ns);
	  char fieldName [100];

	  for (image = 0; image < imageCount; image ++) {
	    sprintf (fieldName, "ImageData%d", image + 4);

	    for (line = sl; line < sl + nl; line ++) {
	      {
		int32 start [2]; /* these are zero based */
		int32 edge [2];

		start [0] = line - 1; /* these are zero based */
		start [1] = ss - 1; /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "SWIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      } else if (! strcmp (sensor, "TIR")) {
	{
	  int line;
	  uint16 * imageRow = malloc (sizeof (uint16) * ns);
	  char fieldName [100];

	  for (image = 0; image < imageCount; image ++) {
	    sprintf (fieldName, "ImageData%d", image + 10);

	    for (line = sl; line < sl + nl; line ++) {
	      {
		int32 start [2]; /* these are zero based */
		int32 edge [2];

		start [0] = line - 1; /* these are zero based */
		start [1] = ss - 1; /* these are zero based */
		edge [0] = 1;
		edge [1] = ns;

		/* get an image line */
		if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow) == -1) {
		  sprintf (msgBuf, "TIR SWreadfield failed in %s on source line %d", fieldName, line);
		  zmabend (msgBuf);
		}

		/* write image line to VICAR image output */
		zvwrit (vunits [image],
			imageRow,
			"LINE", line - sl + 1,
			"SAMP", 1,
			"NSAMPS", ns, NULL);
	      }
	    }
	  }
	}
      }
    } else if (! strcmp (product, "Emissivity") || ! strcmp (product, "Radiance")) {
      int line;
      uint16 * imageRow16 = malloc (sizeof (uint16) * ns);
      char fieldName [100];

      logMetaInt (echoMeta, outMetaName, "ASTER_IMAGE_PIXEL_BITS", 16, doimg?imageCount:0, & vunits [0]);

      for (image = 0; image < imageCount; image ++) {
	sprintf (fieldName, "Band%d", image + 10);


	for (line = sl; line < sl + nl; line ++) {
	  {
	    int32 start [2]; /* these are zero based */
	    int32 edge [2];

	    start [0] = line - 1; /* these are zero based */
	    start [1] = ss - 1; /* these are zero based */
	    edge [0] = 1;
	    edge [1] = ns;

	    /* get an image line */
	    if (SWreadfield (swathHandle, fieldName, start, NULL, edge, imageRow16) == -1) {
	      sprintf (msgBuf, "Custom16 SWreadfield failed on source line %d", line);
	      zmabend (msgBuf);
	    }

	    /* write image line to VICAR image output */
	    zvwrit (vunits [image],
		    imageRow16,
		    "LINE", line - sl + 1,
		    "SAMP", 1,
		    "NSAMPS", ns, NULL);
	  }
	}
      }
    } else if (! strcmp (product, "Kinetic")) {
      int line;
      uint16 * imageRow16 = malloc (sizeof (uint16) * ns);;

      logMetaInt (echoMeta, outMetaName, "ASTER_IMAGE_PIXEL_BITS", 16, doimg?imageCount:0, & vunits [0]);

      for (line = sl; line < sl + nl; line ++) {
	{
	  int32 start [2]; /* these are zero based */
	  int32 edge [2];

	  start [0] = line - 1; /* these are zero based */
	  start [1] = ss - 1; /* these are zero based */
	  edge [0] = 1;
	  edge [1] = ns;

	  /* get an image line */
	  if (SWreadfield (swathHandle, "KineticTemperature", start, NULL, edge, imageRow16) == -1) {
	    sprintf (msgBuf, "Custom16 SWreadfield failed in KineticTemperature on source line %d", line);
	    zmabend (msgBuf);
	  }

	  /* write image line to VICAR image output */
	  zvwrit (vunits [0],
		  imageRow16,
		  "LINE", line - sl + 1,
		  "SAMP", 1,
		  "NSAMPS", ns, NULL);
	}
      }
    }
  }

  sprintf (outNavName, "%s.int", outfilename);

  /* We don't yet know how to interpret 1A nav data */
  if (strcmp (product, "1A")) {	/* It's 1B or derived surface product */
    double * lineColumn = 0, * sampleColumn = 0, * lonColumn = 0, * latColumn = 0;

    /* create columns LINE, SAMPLE, LON, LAT for IBIS navigation file */
    if (! (lineColumn = (double *) malloc (sizeof (double) * 121)) ||
	! (sampleColumn = (double *) malloc (sizeof (double) * 121)) ||
	! (lonColumn = (double *) malloc (sizeof (double) * 121)) ||
	! (latColumn = (double *) malloc (sizeof (double) * 121)))
      zmabend ("error allocating IBIS columns");

    /* get the nav data */
    {
      int line, sample;
      float64 lonRow[11];
      float64 latRow[11];
      int IBISrow = 0;

      for (line = 0; line < 11; line ++) {
	{
	  int32 start [2];
	  int32 edge [2] = {1, 11};

	  start [0] = line;
	  start [1] = 0;

	  if (SWreadfield (swathHandle, "Longitude", start, NULL, edge, lonRow) == -1) {
	    sprintf (msgBuf, "SWreadfield failed on Longitude");
	    zmabend (msgBuf);
	  }
	  if (SWreadfield (swathHandle, latname, start, NULL, edge, latRow) == -1) {
	    sprintf (msgBuf, "SWreadfield failed on \"%s\" for Latitude", latname);
	    zmabend (msgBuf);
	  }
	  for (sample = 0; sample < 11; sample ++) {
	    lineColumn [IBISrow] = line + 1;
	    sampleColumn [IBISrow] = sample + 1;
	    lonColumn [IBISrow] = lonRow [sample];
	    if (geoconv)
	      latColumn [IBISrow] = geocentricToGeodetic (latRow [sample]);
	    else    
	      latColumn [IBISrow] = latRow [sample];

	    /* for export to GeoTiff */
	    latMatrix [sample] [line] = latColumn [IBISrow];
	    lonMatrix [sample] [line] = lonColumn [IBISrow];

	    IBISrow ++;
	  }
	}
      }
    }

    /* log the nav data */
    if (logNavDataToIBIS (outNavName, 0, 121, lineColumn, sampleColumn, latColumn, lonColumn) != 1)
      zmabend ("error logging nav data");

    free (lineColumn);
    free (sampleColumn);
    free (lonColumn);
    free (latColumn);
  } else {			/* No nav matrix for 1A, so get corners */
    char *p;
    double lat, lon;
    double lineColumn  [4], sampleColumn [4], lonColumn [4], latColumn [4];

    if ((p = strstr (productmetadata, "UPPERLEFT"))) {
      p = strstr (p, "VALUE");
      p = strstr (p, "(");

      sscanf (p, "(%lf,%lf)", & lat, & lon);

      logMetaDouble (echoMeta, outMetaName, "ASTER_UL_LAT", lat, doimg?imageCount:0, & vunits [0]);
      logMetaDouble (echoMeta, outMetaName, "ASTER_UL_LON", lon, doimg?imageCount:0, & vunits [0]);

      lineColumn [0] = 0;
      sampleColumn [0] = 0;
      lonColumn [0] = lon;
      latColumn [0] = lat;
    }

    if ((p = strstr (productmetadata, "UPPERRIGHT"))) {
      p = strstr (p, "VALUE");
      p = strstr (p, "(");

      sscanf (p, "(%lf,%lf)", & lat, & lon);

      logMetaDouble (echoMeta, outMetaName, "ASTER_UR_LAT", lat, doimg?imageCount:0, & vunits [0]);
      logMetaDouble (echoMeta, outMetaName, "ASTER_UR_LON", lon, doimg?imageCount:0, & vunits [0]);

      lineColumn [1] = 0;
      sampleColumn [1] = samples - 1;
      lonColumn [1] = lon;
      latColumn [1] = lat;
    }

    if ((p = strstr (productmetadata, "LOWERLEFT"))) {
      p = strstr (p, "VALUE");
      p = strstr (p, "(");

      sscanf (p, "(%lf,%lf)", & lat, & lon);

      logMetaDouble (echoMeta, outMetaName, "ASTER_LL_LAT", lat, doimg?imageCount:0, & vunits [0]);
      logMetaDouble (echoMeta, outMetaName, "ASTER_LL_LON", lon, doimg?imageCount:0, & vunits [0]);

      lineColumn [2] = lines - 1;
      sampleColumn [2] = 0;
      lonColumn [2] = lon;
      latColumn [2] = lat;
    }

    if ((p = strstr (productmetadata, "LOWERRIGHT"))) {
      p = strstr (p, "VALUE");
      p = strstr (p, "(");

      sscanf (p, "(%lf,%lf)", & lat, & lon);

      logMetaDouble (echoMeta, outMetaName, "ASTER_LR_LAT", lat, doimg?imageCount:0, & vunits [0]);
      logMetaDouble (echoMeta, outMetaName, "ASTER_LR_LON", lon, doimg?imageCount:0, & vunits [0]);

      lineColumn [3] = lines - 1;
      sampleColumn [3] = samples - 1;
      lonColumn [3] = lon;
      latColumn [3] = lat;
    }

    /* log the nav data */
    if (logNavDataToIBIS (outNavName, 0, 4, lineColumn, sampleColumn, latColumn, lonColumn) != 1)
      zmabend ("error logging nav data");
  }

  /* close hdf file using EOS extensions*/
  if (SWclose (hdfFileId) == -1)
    zmabend ("SWclose failed to close HDF file");

  if (! strcmp (product, "1A") || ! strcmp (product, "1B")) { /* This is used for registration and we only use 1A and 1B for registration */
    /* open as non EOS hdf file */
    char * p;
    double angle, angle2, lat, lon;
    int i;
      
    /* get the pointing angle */
    sprintf (msgBuf, "\"%s\"", sensor);
    p = strstr (productmetadata, "POINTINGANGLESCONTAINER");
    p = strstr (p, msgBuf);
    p = strstr (p, "POINTINGANGLE");
    p = strstr (p, "VALUE");
    p = strstr (p, "=");
    p ++;
    sscanf (p, "%lf", & angle);
    logMetaDouble (echoMeta, outMetaName, "ASTER_POINTING_ANGLE", angle, doimg?imageCount:0, & vunits [0]);

    /* get the gain information */
    for(i = 0; i < 10; i++)
    {
       char gain[14];
       char label[20];

       p = strstr(p, "GAININFORMATIONCONTAINER");
       if(p == NULL) break;
       p = strstr(p, "VALUE");
       p = strstr(p, "(");
       strncpy(gain, p, 13);
       gain[13] = 0;

       sprintf(label, "GAIN_INFORMATION_%d", i+1);
       logMetaString (echoMeta, outMetaName, label, gain, doimg?imageCount:0, & vunits [0]);
    }

    /* get the solar direction */
    p = strstr (productmetadata, "SOLARDIRECTION");
    p = strstr (p, "VALUE");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & angle);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & angle2);
    logMetaDouble (echoMeta, outMetaName, "ASTER_SOLAR_AZIMUTH", geoconv?geocentricToGeodetic(angle):angle,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_SOLAR_ELEVATION", angle2, doimg?imageCount:0, & vunits [0]);

    /* get the four corners */
    p = strstr (productmetadata, "SCENEFOURCORNERS");
    p = strstr (p, "UPPERLEFT");
    p = strstr (p, "VALUE");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & lat);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & lon);
    logMetaDouble (echoMeta, outMetaName, "ASTER_UPPERLEFT_LAT", geoconv?geocentricToGeodetic(lat):lat,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_UPPERLEFT_LON", lon, doimg?imageCount:0, & vunits [0]);

    p = strstr (productmetadata, "SCENEFOURCORNERS");
    p = strstr (p, "UPPERRIGHT");
    p = strstr (p, "VALUE");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & lat);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & lon);
    logMetaDouble (echoMeta, outMetaName, "ASTER_UPPERRIGHT_LAT", geoconv?geocentricToGeodetic(lat):lat,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_UPPERRIGHT_LON", lon, doimg?imageCount:0, & vunits [0]);

    p = strstr (productmetadata, "SCENEFOURCORNERS");
    p = strstr (p, "LOWERLEFT");
    p = strstr (p, "VALUE");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & lat);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & lon);
    logMetaDouble (echoMeta, outMetaName, "ASTER_LOWERLEFT_LAT", geoconv?geocentricToGeodetic(lat):lat,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_LOWERLEFT_LON", lon, doimg?imageCount:0, & vunits [0]);

    p = strstr (productmetadata, "SCENEFOURCORNERS");
    p = strstr (p, "LOWERRIGHT");
    p = strstr (p, "VALUE");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & lat);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & lon);
    logMetaDouble (echoMeta, outMetaName, "ASTER_LOWERRIGHT_LAT", geoconv?geocentricToGeodetic(lat):lat,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_LOWERRIGHT_LON", lon, doimg?imageCount:0, & vunits [0]);

    /* get the scene center */
    p = strstr (productmetadata, "SCENECENTER");
    p = strstr (p, "(");
    p ++;
    sscanf (p, "%lf", & lat);
    p = strstr (p, ",");
    p ++;
    sscanf (p, "%lf", & lon);
    logMetaDouble (echoMeta, outMetaName, "ASTER_SCENECENTER_LAT", geoconv?geocentricToGeodetic(lat):lat,
		   doimg?imageCount:0, & vunits [0]);
    logMetaDouble (echoMeta, outMetaName, "ASTER_SCENECENTER_LON", lon, doimg?imageCount:0, & vunits [0]);
  }

  free (productmetadata);

  /* add the 11x11 nav matrix as a VICARGT label */
  if (doimg) {
    for (image = 0; image < imageCount; image ++) {
      if (strcmp (product, "1A")) {	/* It's not 1A */
	/* stuff meta data in a Vicar label */
	metaToLabel (outMetaName, vunits [image]);

	/* stuff nav matrices in Vicar GeoTiff tags */
	if ((status = zladd (vunits [image], "PROPERTY",
			     "GTMODELTYPEGEOKEY", "2 (ModelTypeGeographic)",
			     "PROPERTY", "GEOTIFF",
			     "FORMAT", "STRING", NULL)) != 1)
	  zmabend ("zladd failed to add a label for GTMODELTYPEGEOKEY");
	if ((status = zladd (vunits [image], "PROPERTY",
			     "GTRASTERTYPEGEOKEY", "2 (RasterPixelIsPoint)",
			     "PROPERTY", "GEOTIFF",
			     "FORMAT", "STRING", NULL)) != 1)
	  zmabend ("zladd failed to add a label for GTRASTERTYPEGEOKEY");

	{
	  int line, sample;
	  char buf [150];

	  for (line = 0; line < 11; line ++) {
	    for (sample = 0; sample < 11; sample ++) {
	      /* raster X (sample), Y (line), Z (zero), model X (lon), Y (lat), Z (zero) */
	      sprintf (buf, "(%.13lf,%.13lf,0.0,%.13lf,%.13lf,0.0)",
		       (double) (sample * ns) / 10.0 + 1.0, /* + 1.0 because image cs is 1-based */
		       (double) (line * nl) / 10.0 + 1.0,   /* ditto */
		       lonMatrix [sample] [line],
		       latMatrix [sample] [line]);

	      if ((status = zladd (vunits [image], "PROPERTY",
				   "MODELTIEPOINTTAG", buf,
				   "PROPERTY", "GEOTIFF",
				   "FORMAT", "STRING",
				   "MODE", "INSERT", NULL)) != 1)
		zmabend ("zladd failed to add a label for MODELTIEPOINTTAG");
	    }
	  }
	}
      }

      /* done with VICAR image output */
      zvclose (vunits [image], NULL);
    }
  }
}
