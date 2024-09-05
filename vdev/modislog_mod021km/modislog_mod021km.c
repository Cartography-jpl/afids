#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#if defined(__i386__) || defined (__xb86_64__)
/* ntohl and ntohs byte order converters */
#include <netinet/in.h>
#endif

#include "carto/cartoVicarProtos.h"
#include "carto/cartoLoggerUtils.h"
#undef VOID			/* Defined in VICAR, conflicts with
				   HDF library */
#include "carto/hdfIncludes.h"
#include "vicmain_c.h"

/*
  program modislog_mod021km
*/

#define _modislog_mod021km_version_ "Mon Jan 28 2008"

/*
  Band     SDS Name          Cube Index
   1    EV_250_Aggr1km_RefSB     0
   2    EV_250_Aggr1km_RefSB     1
   3    EV_500_Aggr1km_RefSB     0
   4    EV_500_Aggr1km_RefSB     1
   5    EV_500_Aggr1km_RefSB     2
   6    EV_500_Aggr1km_RefSB     3
   7    EV_500_Aggr1km_RefSB     4
   8    EV_1KM_RefSB             0
   9    EV_1KM_RefSB             1
  10    EV_1KM_RefSB             2
  11    EV_1KM_RefSB             3
  12    EV_1KM_RefSB             4
  13L   EV_1KM_RefSB             5
  13H   EV_1KM_RefSB             6
  14L   EV_1KM_RefSB             7
  14H   EV_1KM_RefSB             8
  15    EV_1KM_RefSB             9
  16    EV_1KM_RefSB            10
  17    EV_1KM_RefSB            11
  18    EV_1KM_RefSB            12
  19    EV_1KM_RefSB            13
  20    EV_1KM_Emissive          0
  21    EV_1KM_Emissive          1
  22    EV_1KM_Emissive          2
  23    EV_1KM_Emissive          3
  24    EV_1KM_Emissive          4
  25    EV_1KM_Emissive          5
  26    EV_1KM_RefSB            14
  27    EV_1KM_Emissive          6
  28    EV_1KM_Emissive          7
  29    EV_1KM_Emissive          8
  30    EV_1KM_Emissive          9
  31    EV_1KM_Emissive         10
  32    EV_1KM_Emissive         11
  33    EV_1KM_Emissive         12
  34    EV_1KM_Emissive         13
  35    EV_1KM_Emissive         14
  36    EV_1KM_Emissive         15
*/

static int latSDSIndex, lonSDSIndex, zenithSDSIndex, azimuthSDSIndex;

/* sl, ss are one-based */
void readLatsLonsLine (int sd_id, float * lats, float * lons, int sl, int ss, int ns) {
  int32 start [2], edges [2];
  int sds_id;
    
  start [0] = sl - 1;		/* start is zero-based */
  start [1] = ss - 1;
  edges [0] = 1;		/* get one line */
  edges [1] = ns;		/* get ns samples */

  /* read latitudes */
  sds_id = SDselect (sd_id, latSDSIndex);
  if (SDreaddata (sds_id, start, 0, edges, lats))
    zmabend ("readLatsLonsLine: SDreaddata failed on latitudes");
  SDendaccess (sds_id);
	
  /* read longitudes */
  sds_id = SDselect (sd_id, lonSDSIndex);
  if (SDreaddata (sds_id, start, 0, edges, lons))
    zmabend ("SDreaddata failed on longitudes");
  SDendaccess (sds_id);
}

void main44()
{
  int parmct, parmdf;
  char inImage [99];
  char inGeo [99];
  char outname [99];
  char outImageName [200];
  char outGeoName [200];
  char metaName [200];
  int lines, samples, bands;
  int sl, ss, nl, ns;
  double minLat, maxLat, minLon, maxLon;
  int vunit;
  char logIdBuf [100];
  int imageSDSIndex;
  int band, high, bandIndex = -1;
  int image_sd_id = -1, geo_sd_id = -1;
  int lineSkip, sampSkip;
  float archMinLat = 999.0, archMaxLat = -999.0, archMinLon = 999.0, archMaxLon = -999.0;
  int ascendingFlag = 0;
  int echoMeta = 0;

  checkLoggerUtilsVersion (15);

  sprintf (logIdBuf, "modislog_mod021km version %s", _modislog_mod021km_version_);
  zifmessage (logIdBuf);

  /* fetch image and geo files */
  zvparm ("inp", inImage, &parmct, &parmdf, 1, 99);
  zvparm ("geo", inGeo, &parmct, &parmdf, 1, 99);

  if (! strlen (inGeo))
    zifmessage ("no geo (MOD03) file, nav and sensor angle data will not be exported to IBIS");

  /* fetch out file name */
  zvparm ("out", outname, &parmct, &parmdf, 1, 99);

  /* fetch the nav data downsampling skip values */
  zvp("lineSkip", & lineSkip, &parmct);
  zvp("sampSkip", & sampSkip, &parmct);

  /* fetch the requested band */
  zvp("band", & band, &parmct);
  zvp("high", & high, &parmct);
  if (high && band != 13 && band != 14)
    zifmessage ("high flag ignored since band not 13 or 14");
  
  zvp("echoMeta", &echoMeta, &parmct);

  /* open image file */
  image_sd_id = SDstart (inImage, DFACC_READ);
  if (image_sd_id < 0) {
      char msgBuf [1000];
      sprintf (msgBuf, "SDstart failed on input image file %s", inImage);
      zmabend (msgBuf);
    }

  /* find image dataset index and metadata in MOD02 file */
  {
    char sds_name [257];
    int32 data_type;
    int32 n_datasets, n_file_attrs;
    int dsIndex;
    int sds_id = -1;
    int32 rank, n_attrs, dim_sizes [32];
    char * coremetadata = 0;
    char beginningDate [11], beginningTime [6], platform [20], sensor [20];
    char * requiredSDSName = 0;

    if (band == 1 || band == 2) {
      requiredSDSName = "EV_250_Aggr1km_RefSB";
      bandIndex = band - 1;
    }
    else if (band >= 3 && band <= 7) {
      requiredSDSName = "EV_500_Aggr1km_RefSB";
      bandIndex = band - 3;
    }
    else if ((band >= 8 && band <= 19) || band == 26) {
      requiredSDSName = "EV_1KM_RefSB";
      if (band >= 8 && band <= 12)
	bandIndex = band - 8;
      else if (band == 13) {
	if (high)
	  bandIndex = 6;
	else
	  bandIndex = 5;
      } else if (band == 14) {
	if (high)
	  bandIndex = 8;
	else
	  bandIndex = 7;
      } else if (band >= 15 && band <= 19)
	bandIndex = band - 15;
      else /* band must be 26 */
	bandIndex = 14;
    }
    else if ((band >= 20 && band <= 25) || (band >= 27 && band <= 36)) {
      requiredSDSName = "EV_1KM_Emissive";
      if  (band >= 20 && band <= 25)
	bandIndex = band - 20;
      else 
	bandIndex = band - 27;
    }
    else
      zmabend ("band must be in the range 1..36");
      
    if (SDfileinfo (image_sd_id, &n_datasets, &n_file_attrs) == -1)
      zmabend ("SDfileinfo failed on input image file");

    /* find lines, samples, image data set index */
    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (image_sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (strstr (sds_name, requiredSDSName))
	break;
      else
	SDendaccess (sds_id);
    }
    SDendaccess (sds_id);

    if (dsIndex >= n_datasets) {
      char msgBuf [1000];
      sprintf (msgBuf, "Did not find %s dataset", requiredSDSName);
      zmabend (msgBuf);
    }

    assert (rank == 3);
    assert (data_type == DFNT_UINT16);
    bands = dim_sizes [0];
    lines = dim_sizes [1];
    samples = dim_sizes [2];
    imageSDSIndex = dsIndex;

    if (strstr (requiredSDSName, "EV_250_Aggr1km_RefSB") && bands != 2)
      zmabend ("Expected dataset with 2 EV_250_Aggr1km_RefSB bands");
    else if (strstr (requiredSDSName, "EV_500_Aggr1km_RefSB") && bands != 5)
      zmabend ("Expected dataset with 5 EV_500_Aggr1km_RefSB bands");
    else if (strstr (requiredSDSName, "EV_1KM_RefSB") && bands != 15)
      zmabend ("Expected dataset with 15 EV_1KM_RefSB bands");
    else if (strstr (requiredSDSName, "EV_1KM_Emissive") && bands != 16)
      zmabend ("Expected dataset with 16 EV_1KM_Emissive bands");

    /* get metadata */
    {
      int32 attr_index;
      char nambuf[1000];
      int32 data_type, n_values;

      /* get coremetadata.0 attribute index */
      attr_index = SDfindattr (image_sd_id, "CoreMetadata.0");
      
      /* see how long the coremetadata.0 string is */
      if (SDattrinfo(image_sd_id, attr_index, nambuf, &data_type,&n_values) == -1)
	zmabend ("SDattrinfo failed on coremetadata");

      /* allocate space for it */
      coremetadata = checkedMalloc (n_values + 1, "coremetadata buffer");
      
      /* read it */
      if (SDreadattr(image_sd_id, attr_index, coremetadata) == -1)
	zmabend ("SDreadattr failed on coremetadata");
    }

    /* beginning date, e.g. "2001-02-27" */
    {
      char *p;

      p= strstr (coremetadata, "RANGEBEGINNINGDATE");
      p = strstr (p, "VALUE");
      p = strstr (p, "\"");
      p ++;
      strncpy (beginningDate, p, 10);
      beginningDate [10] = 0;
    }

    /* beginning time, e.g. "05:23"*/
    {
      char *p;

      p= strstr (coremetadata, "RANGEBEGINNINGTIME");
      p = strstr (p, "VALUE");
      p = strstr (p, "\"");
      p ++;
      strncpy (beginningTime, p, 5);
      beginningTime [5] = 0;
    }

    /* platform, e.g. "Terra" */
    {
      char *p;

      p= strstr (coremetadata, "ASSOCIATEDPLATFORMSHORTNAME");
      p = strstr (p, "VALUE");
      p = strstr (p, "\"");
      p ++;
      strncpy (platform, p, 19);
      platform [19] = 0;
      if (strstr (platform, "\""))
	* strstr (platform, "\"") = 0;
    }

    /* sensor, e.g. "MODIS" */
    {
      char *p;

      p= strstr (coremetadata, "ASSOCIATEDINSTRUMENTSHORTNAME");
      p = strstr (p, "VALUE");
      p = strstr (p, "\"");
      p ++;
      strncpy (sensor, p, 19);
      sensor [19] = 0;
      if (strstr (sensor, "\""))
	* strstr (sensor, "\"") = 0;
    }

    free (coremetadata);

    if (! strlen (outname)) {
      sprintf (outname, "%s_%s_1km_", platform, sensor);
      strncat (outname, beginningDate, 4);
      strncat (outname, beginningDate + 5, 2);
      strncat (outname, beginningDate + 8, 2);
      strcat (outname, "_");
      strncat (outname, beginningTime, 2);
      strncat (outname, beginningTime + 3, 2);
    }
  }

  /* get geo data hdf indices */
  if (strlen (inGeo)) {
    /* open geo file */
    geo_sd_id = SDstart (inGeo, DFACC_READ);
    if (geo_sd_id < 0) {
      char msgBuf [1000];
      sprintf (msgBuf, "SDstart failed on input image file %s", inGeo);
      zmabend (msgBuf);
    }

    {
      char sds_name [257];
      int32 data_type;
      int32 n_datasets, n_file_attrs;
      int dsIndex;
      int sds_id = -1;
      int32 rank, n_attrs, dim_sizes [32];

      if (SDfileinfo (geo_sd_id, &n_datasets, &n_file_attrs) == -1)
	zmabend ("SDfileinfo failed on input geo file\n");

      for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
	sds_id = SDselect (geo_sd_id, dsIndex);
	SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

	if (strstr (sds_name, "Latitude"))
	  break;
	else
	  SDendaccess (sds_id);
      }
      SDendaccess (sds_id);

      assert (dsIndex < n_datasets);
      assert (rank == 2);
      printf( "dim_sizes[0] %d lines %d\n", dim_sizes[0], lines);
      printf( "dim_sizes[1] %d samples %d\n", dim_sizes[1], samples);
      assert (dim_sizes [0] == lines);
      assert (dim_sizes [1] == samples);
      assert (data_type == DFNT_FLOAT32);
      latSDSIndex = dsIndex;

      for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
	sds_id = SDselect (geo_sd_id, dsIndex);
	SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

	if (strstr (sds_name, "Longitude"))
	  break;
	else
	  SDendaccess (sds_id);
      }
      SDendaccess (sds_id);

      assert (dsIndex < n_datasets);
      assert (rank == 2);
      assert (dim_sizes [0] == lines);
      assert (dim_sizes [1] == samples);
      assert (data_type == DFNT_FLOAT32);
      lonSDSIndex = dsIndex;

      for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
	sds_id = SDselect (geo_sd_id, dsIndex);
	SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

	if (strstr (sds_name, "SensorZenith"))
	  break;
	else
	  SDendaccess (sds_id);
      }
      SDendaccess (sds_id);

      assert (dsIndex < n_datasets);
      assert (rank == 2);
      assert (dim_sizes [0] == lines);
      assert (dim_sizes [1] == samples);
      assert (data_type == DFNT_INT16);
      zenithSDSIndex = dsIndex;

      for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
	sds_id = SDselect (geo_sd_id, dsIndex);
	SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

	if (strstr (sds_name, "SensorAzimuth"))
	  break;
	else
	  SDendaccess (sds_id);
      }
      SDendaccess (sds_id);

      assert (dsIndex < n_datasets);
      assert (rank == 2);
      assert (dim_sizes [0] == lines);
      assert (dim_sizes [1] == samples);
      assert (data_type == DFNT_INT16);
      azimuthSDSIndex = dsIndex;
    }
  }

  /* If the sub-area is specified in lat/lon, we need to compute the
     line/sample.  The last arg is a flag meaning "allow lat/lon
     subarea". So if no geo file was provided, then any lat/lon
     subarea will be ignored. */
  if (usingLatLonSubArea (lines, samples,
			  & minLat, & maxLat, & minLon, & maxLon,
			  & sl, & ss, & nl, & ns, strlen (inGeo))) {
    /* for each of the four corners, specified in lat/lon */
    /*     compute the distance between the corner and every point in the image, saving the closest */
    /*     while we're at it, compute the max/min lat/lon for the archive */
    float ulMin = 6.02e23, urMin = 6.02e23, lrMin = 6.02e23, llMin = 6.02e23;
    int ulBestLine = 0, ulBestSample = 0, urBestLine = 0, urBestSample = 0;
    int lrBestLine = 0, lrBestSample = 0, llBestLine = 0, llBestSample = 0;
    float dist;
    float * lats = (float *) checkedMalloc (sizeof (float) * samples, "lats array for checking corners");
    float * lons = (float *) checkedMalloc (sizeof (float) * samples, "lons array for checking corners");
    int line, sample;
    float lat, lon;
    float firstLat = 0.0, lastLat = 0.0;

    for (line = 0; line < lines; line ++) {
      /* get the line nav data */
      readLatsLonsLine (geo_sd_id, lats, lons, line + 1, 1, samples);
      if (line)
	lastLat = lats [0];
      else
	firstLat = lats[0];

      /* check corners */
      for (sample = 0; sample < samples; sample ++) {
	lat = lats [sample];
	lon = lons [sample];

	if (lat < -90.0 || lon < -180.0) /* ignore bogus values */
	  continue;

	archMinLat = MIN (archMinLat, lat);
	archMaxLat = MAX (archMaxLat, lat);
	archMinLon = MIN (archMinLon, lon);
	archMaxLon = MAX (archMaxLon, lon);

#define DISTSQ(lat1, lon1, lat2, lon2) \
              (((lat1) - (lat2)) * ((lat1) - (lat2)) + ((lon1) - (lon2)) * ((lon1) - (lon2)))

	/* ul corner */
	dist = DISTSQ (lat, lon, maxLat, minLon);
	if (dist < ulMin) {
	  ulMin = dist;
	  ulBestLine = line + 1; /* line is zero based */
	  ulBestSample = sample + 1; /* sample is zero based */
	}

	/* ur corner */
	dist = DISTSQ (lat, lon, maxLat, maxLon);
	if (dist < urMin) {
	  urMin = dist;
	  urBestLine = line + 1; /* line is zero based */
	  urBestSample = sample + 1; /* sample is zero based */
	}

	/* ll corner */
	dist = DISTSQ (lat, lon, minLat, minLon);
	if (dist < llMin) {
	  llMin = dist;
	  llBestLine = line + 1; /* line is zero based */
	  llBestSample = sample + 1; /* sample is zero based */
	}

	/* lr corner */
	dist = DISTSQ (lat, lon, minLat, maxLon);
	if (dist < lrMin) {
	  lrMin = dist;
	  lrBestLine = line + 1; /* line is zero based */
	  lrBestSample = sample + 1; /* sample is zero based */
	}
      }
    }

    ascendingFlag = lastLat > firstLat;

    sl = MIN (ulBestLine, MIN (urBestLine, MIN (lrBestLine, llBestLine)));
    ss = MIN (ulBestSample, MIN (urBestSample, MIN (lrBestSample, llBestSample)));
    nl = MAX (ulBestLine, MAX (urBestLine, MAX (lrBestLine, llBestLine))) - sl + 1;
    ns = MAX (ulBestSample, MAX (urBestSample, MAX (lrBestSample, llBestSample))) - ss + 1;

    free (lats);
    free (lons);

    if (! sl || ! ss)
      zifmessage ("requested area not in image");
  } else { /* just check to see if the acquisition was ascending */
    float * lats = (float *) checkedMalloc (sizeof (float) * samples, "lats array for checking corners");
    float * lons = (float *) checkedMalloc (sizeof (float) * samples, "lons array for checking corners");
    float firstLat = 0.0, lastLat = 0.0;

    readLatsLonsLine (geo_sd_id, lats, lons, 1, 1, samples);
    firstLat = lats[0];

    readLatsLonsLine (geo_sd_id, lats, lons, lines, 1, samples);
    lastLat = lats [0];

    ascendingFlag = lastLat > firstLat;

    free (lats);
    free (lons);
  }

  /* compute output file names */
  sprintf (outGeoName, "%s.int", outname);
  sprintf (metaName, "%s.txt", outname);

  /* create output VICAR file */

  if (band == 13 || band == 14) {
    if (high)
      sprintf (outImageName, "%s_%dH.img", outname, band);
    else
      sprintf (outImageName, "%s_%dL.img", outname, band);
  } else
    sprintf (outImageName, "%s_%d.img", outname, band);

  if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outImageName, NULL) != 1)
    zmabend ("zvunit failed for out image");
  if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
	      "O_FORMAT", "HALF", NULL) != 1)
    zmabend ("zvopen failed for out image");
  initMetaData (metaName);

#define echoData
  /* log some meta data */
  logMetaString (echoMeta, metaName, "LOGGER_ID", logIdBuf, 1, & vunit);
  logMetaString (echoMeta, metaName, "ARCH_IMG_NAME", inImage, 1, & vunit);
  if (strlen (inGeo))
    logMetaString (echoMeta, metaName, "ARCH_GEO_NAME", inGeo, 1, & vunit);
  logMetaString (echoMeta, metaName, "CALC_OUT_PREFIX", outname, 1, & vunit);
  logMetaInt (echoMeta, metaName, "ARCH_LINES", lines, 1, & vunit);
  logMetaInt (echoMeta, metaName, "ARCH_SAMPLES", samples, 1, & vunit);
  logMetaInt (echoMeta, metaName, "CALC_SL", sl, 1, & vunit);
  logMetaInt (echoMeta, metaName, "CALC_NL", nl, 1, & vunit);
  logMetaInt (echoMeta, metaName, "CALC_SS", ss, 1, & vunit);
  logMetaInt (echoMeta, metaName, "CALC_NS", ns, 1, & vunit);
  if (minLat > -999.0) {
    logMetaDouble (echoMeta, metaName, "LOG_MIN_LAT", minLat, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "LOG_MAX_LAT", maxLat, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "LOG_MIN_LON", minLon, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "LOG_MAX_LON", maxLon, 1, & vunit);
  }
  logMetaInt (echoMeta, metaName, "LOG_LINE_SKIP", lineSkip, 1, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_SAMP_SKIP", sampSkip, 1, & vunit);
  if (archMinLat < 999.0) {
    logMetaDouble (echoMeta, metaName, "ARCH_MIN_LAT", archMinLat, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "ARCH_MAX_LAT", archMaxLat, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "ARCH_MIN_LON", archMinLon, 1, & vunit);
    logMetaDouble (echoMeta, metaName, "ARCH_MAX_LON", archMaxLon, 1, & vunit);
  }
  if (strlen (inGeo))
    logMetaInt (echoMeta, metaName, "LOG_ASCENDING", ascendingFlag, 1, & vunit);

  /* get the image data */
  {
    short * lineBuf = (short *) checkedMalloc (sizeof (short) * samples, "image line buffer");
    int line;
    int sds_id = SDselect (image_sd_id, imageSDSIndex);
    int32 start [3], edges [3];
    
    start [0] = bandIndex;
    start [2] = ss - 1;		/* ss is one-based */
    edges [0] = 1;
    edges [1] = 1;
    edges [2] = ns;
    for (line = sl; line < sl + nl; line ++) {
      memset (lineBuf, 0, sizeof (short) * samples);
      start [1] = line - 1;

      assert (sds_id >= 0);
      if (SDreaddata (sds_id, start, 0, edges, lineBuf))
	zmabend ("SDreaddata failed on image");
	
      /* write image line to VICAR image output */
      zvwrit (vunit,
	      lineBuf,
	      "LINE", line - sl + 1,
	      "SAMP", 1,
	      "NSAMPS", ns, NULL);
    }

    SDendaccess (sds_id);
  }

  SDend (image_sd_id);

  /* get geolocation and sensor angle data */
  if (strlen (inGeo)) {
    {
      float * lats = (float *) checkedMalloc (sizeof (float) * nl * ns, "lats array");
      double * latColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "lat column");
      float * lons = 0;
      double * lonColumn = 0;
      short * zeniths = 0;
      double * zenithColumn = 0;
      short * azimuths = 0;
      double * azimuthColumn = 0;
      double * lineColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "line column");
      double * sampleColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "sample column");
      int line, sample;
      int32 start [2], edges [2];
      int sds_id;
      int navPoints;
    
      /* set line and sample columns */
      navPoints = 0;
      for (line = sl; line < sl + nl; line += lineSkip)
	for (sample = ss; sample < ss + ns; sample += sampSkip) {
	  lineColumn [navPoints] = line;
	  sampleColumn [navPoints] = sample;
	  navPoints ++;
	}
	
      logMetaInt (echoMeta, metaName, "CALC_NAV_POINTS", navPoints, 1, & vunit);

      start [0] = sl - 1;	/* sl is one-based */
      start [1] = ss - 1;	/* ss is one-based */
      edges [0] = nl;
      edges [1] = ns;

      /* read latitudes */
      sds_id = SDselect (geo_sd_id, latSDSIndex);
      if (SDreaddata (sds_id, start, 0, edges, lats))
	zmabend ("SDreaddata failed on latitudes");
      SDendaccess (sds_id);
	
      /* copy lats from HDF floats to IBIS doubles */
      navPoints = 0;
      for (line = sl; line < sl + nl; line += lineSkip)
	for (sample = ss; sample < ss + ns; sample += sampSkip) {
	  latColumn [navPoints] = lats [(line - 1) * ns + (sample - 1)]; /* line, sample are 1-based */
	  navPoints ++;
	}
	
      free (lats);
      lons = (float *) checkedMalloc (sizeof (float) * nl * ns, "lons array");
      lonColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "lon column");

      /* read longitudes */
      sds_id = SDselect (geo_sd_id, lonSDSIndex);
      if (SDreaddata (sds_id, start, 0, edges, lons))
	zmabend ("SDreaddata failed on longitudes");
      SDendaccess (sds_id);
	
      /* copy lons from HDF floats to IBIS doubles */
      navPoints = 0;
      for (line = sl; line < sl + nl; line += lineSkip)
	for (sample = ss; sample < ss + ns; sample += sampSkip) {
	  lonColumn [navPoints] = lons [(line - 1) * ns + (sample - 1)]; /* line, sample are 1-based */
	  navPoints ++;
	}
	
      free (lons);
      zeniths = (short *) checkedMalloc (sizeof (short) * nl * ns, "zeniths array");
      zenithColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "zenith column");

      /* read zeniths */
      sds_id = SDselect (geo_sd_id, zenithSDSIndex);
      if (SDreaddata (sds_id, start, 0, edges, zeniths))
	zmabend ("SDreaddata failed on zeniths");
      SDendaccess (sds_id);
	
      /* copy zeniths from HDF int16s to IBIS doubles */
      navPoints = 0;
      for (line = sl; line < sl + nl; line += lineSkip)
	for (sample = ss; sample < ss + ns; sample += sampSkip) {
	  zenithColumn [navPoints] = zeniths [(line - 1) * ns + (sample - 1)] / 100.0; /* line, sample are 1-based */
	  navPoints ++;
	}

      free (zeniths);
      azimuths = (short *) checkedMalloc (sizeof (short) * nl * ns, "azimuths zrray");
      azimuthColumn = (double *) checkedMalloc (sizeof (double) * nl * ns, "azimuth column");

      /* read azimuths */
      sds_id = SDselect (geo_sd_id, azimuthSDSIndex);
      if (SDreaddata (sds_id, start, 0, edges, azimuths))
	zmabend ("SDreaddata failed on azimuths");
      SDendaccess (sds_id);
	
      /* copy azimuths from HDF int16s to IBIS doubles */
      navPoints = 0;
      for (line = sl; line < sl + nl; line += lineSkip)
	for (sample = ss; sample < ss + ns; sample += sampSkip) {
	  azimuthColumn [navPoints] = azimuths [(line - 1) * ns + (sample - 1)] / 100.0; /* line, sample are 1-based */
	  navPoints ++;
	}

      free (azimuths);

      logNavAndAnglesDataToIBIS (outGeoName, 0, navPoints,
				 lineColumn, sampleColumn, latColumn, lonColumn, zenithColumn, azimuthColumn);
      logMetaString (echoMeta, metaName, "IBIS_COL_ORDER", "LINE,SAMP,LAT,LON,ZENITH,AZIMUTH", 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_LINE_COL", 1, 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_SAMP_COL", 2, 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_LAT_COL", 3, 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_LON_COL", 4, 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_ZENITH_COL", 5, 1, & vunit);
      logMetaInt (echoMeta, metaName, "IBIS_AZIMUTH_COL", 6, 1, & vunit);
	
      free (lineColumn);
      free (sampleColumn);
      free (latColumn);
      free (lonColumn);
      free (zenithColumn);
      free (azimuthColumn);
    }

    SDend (geo_sd_id);
  }

  zvclose (vunit, NULL);
}
