#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "carto/cartoLoggerUtils.h"
#include "carto/cartoVicarProtos.h"
#include "vicmain_c.h"
#undef VOID			/* Defined in VICAR, conflicts with
				   HDF library */
#include "carto/hdfIncludes.h"

/*
  program hyperionlog

   Image HDF File Attributes
       ImageStartTime
           string, example 2001278054320.859
	   represents date/time when image acquisition began as YYYYDDDHHMMSS.SSS

   Image HDF File Data Sets
       <name matching dataset name>
       Data Set Attributes (all 32-bit signed integers)
           Number of Cross Track Pixels
	   Number of Along Track Pixels

   ACS HDF File Attributes (all 64-bit floats)
       A@ACSKFQ1C(TIME_MEASURED)
       P@ACSKFQ1C
       A@ACSKFQ2C(TIME_MEASURED)
       P@ACSKFQ2C
       A@ACSKFQ3C(TIME_MEASURED)
       P@ACSKFQ3C
       A@ACSKFQ4C(TIME_MEASURED)
       P@ACSKFQ4C
       A@ACSSCLATE(TIME_MEASURED)
       P@ACSSCLATE
       A@ACSSCLONGE(TIME_MEASURED)
       P@ACSSCLONGE
       A@ACSSCPOSXE(TIME_MEASURED)
       P@ACSSCPOSXE
       A@ACSSCPOSYE(TIME_MEASURED)
       P@ACSSCPOSYE
       A@ACSSCPOSZE(TIME_MEASURED)
       P@ACSSCPOSZE
       A@ACSSCVELXE(TIME_MEASURED)
       P@ACSSCVELXE
       A@ACSSCVELYE(TIME_MEASURED)
       P@ACSSCVELYE
       A@ACSSCVELZE(TIME_MEASURED)
       P@ACSSCVELZE
*/

#define frameRate 223.4 /* Hz */
#define altitude 705
#define samples 256
#define bands 242

#define _hyperionlog_version_ "Fri Dec 28 2007"

/* convert 3-d vector to unit length 3-d vector */
static void vecToUnitVec3 (double * vec, double * unitVec) {
  double length = sqrt (vec [0] * vec [0] + vec [1] * vec [1] + vec [2] * vec [2]);

  if (length == 0.0)
    unitVec [0] = unitVec [1] = unitVec [2] = 0.0;
  else {
    unitVec [0] = vec [0] / length;
    unitVec [1] = vec [1] / length;
    unitVec [2] = vec [2] / length;
  }
}

/* negate 3-d vector */
static void negate3 (double * vec) {
  vec [0] = - vec [0];
  vec [1] = - vec [1];
  vec [2] = - vec [2];
}

/* cross product of two 3-d vectors (C = AxB)*/
static void cross3 (double * C, double * A, double * B) {
  C [0] = A [1] * B [2] - A [2] * B [1];
  C [1] = A [2] * B [0] - A [0] * B [2];
  C [2] = A [0] * B [1] - A [1] * B [0];
}

/* compute transpose of 3x3 matrix */
static void transpose3x3 (double from [3] [3], double to [3] [3]) {
  int row, col;
  for (row = 0; row < 3; row ++)
    for (col = 0; col < 3; col ++)
      to [row] [col] = from [col] [row];
}

/* 
   quat (4) to rot matrix (3x3) 
   Adapted from a matlab function writtey by J. Sedlak 23 Sep 96 (gsfc.nasa.gov?)
*/
static void qtoa (double * q, double a [3] [3]) {
  double q1q1 = q [0] * q [0];
  double q2q2 = q [1] * q [1];
  double q3q3 = q [2] * q [2];
  double q4q4 = q [3] * q [3];
  double q1q2 = q [0] * q [1];
  double q1q3 = q [0] * q [2];
  double q1q4 = q [0] * q [3];
  double q2q3 = q [1] * q [2];
  double q2q4 = q [1] * q [3];
  double q3q4 = q [2] * q [3];

  a [0] [0] = q1q1 - q2q2 - q3q3 + q4q4;
  a [1] [0] = 2 * (q1q2 - q3q4);
  a [2] [0] = 2 * (q1q3 + q2q4);
  a [0] [1] = 2 * (q1q2 + q3q4); 
  a [1] [1] = -q1q1 + q2q2 - q3q3 + q4q4;
  a [2] [1] = 2 * (q2q3 - q1q4);
  a [0] [2] = 2 * (q1q3 - q2q4);
  a [1] [2] = 2 * (q2q3 + q1q4);
  a [2] [2] = -q1q1 - q2q2 + q3q3 + q4q4;
}

/* 
   assumes xyz, eul in degrees 
   Adapted from a C++ function written by Vincent De Sapio 2000 (stanford.edu?)
*/
static void atoeul (double a [3] [3], double * eul_deg) {
  double arg_a1, arg_a2, arg_g1, arg_g2;
  double arg_b1 = a [2] [0];
  double arg_b2 = sqrt (a [0] [0] * a [0] [0] + a [1] [0] * a [1] [0]);

#undef PI
#define PI 3.1415926535897932384626433832795
  eul_deg [1]=(180/PI)*atan2(arg_b1, arg_b2);
  if (eul_deg[1]==-90)
    {
      eul_deg [0]=0;
      eul_deg [2]=(180/PI)*atan2(a [0] [1], a [1] [1]);
    }
  else if (eul_deg[1]==90)
    {
      eul_deg [0]=0;
      eul_deg [2]=(180/PI)*atan2(a [0] [1], a [1] [1]);
    }
  else
    {
      arg_a1 = -a [2] [1]/arg_b2;
      arg_a2 = a[2] [2]/arg_b2;
      arg_g1 = -a [2] [0]/arg_b2;
      arg_g2 = a [0] [0]/arg_b2;
      eul_deg [0] = (180/PI)*atan2(arg_a1, arg_a2);
      eul_deg [2] = (180/PI)*atan2(arg_g1, arg_g2);
    }
}

/* 3*3 matrix multiply (C = A * B) */
static void mult3x3 (double C [3] [3], double A [3] [3], double B [3] [3]) {
  int row, col;
  for (col = 0; col < 3; col ++)
    for (row = 0; row < 3; row ++)
      C [row] [col] = A [row] [0] * B [0] [col] + A [row] [1] * B [1] [col] + A [row] [2] * B [2] [col];
}

/*
  Adapted from a matlab function written by S. Shulman 09/19/02 (gsfc.nasa.gov?)
*/
static double calcLookAngle (double pos [3], double vel [3], double q [4]) {
#define ALI_rolloffset 6.46
#define HSI_rolloffset 5.11
  double u_nadir [3], u_normal [3], u_nxp [3];
  double A_GCI2OCS [3] [3], A_GCI2BODY [3] [3];
  double Euler_angles [3];
  double temp3 [3];
  double temp3x3 [3] [3];
  double A_OCS2BODY [3] [3];
  
  /*  % Define GCI to OCS attitude matrix */
  /*  u_nadir = -unitvec(pos); */
  vecToUnitVec3 (pos, u_nadir);
  negate3 (u_nadir);

  /*  u_normal = unitvec(cross(pos,vel)); */
  cross3 (temp3, pos, vel);
  vecToUnitVec3 (temp3, u_normal);

  /*  u_nxp = unitvec(cross(u_normal,pos)); */
  cross3 (temp3, u_normal, pos);
  vecToUnitVec3 (temp3, u_nxp);

  /*  A_GCI2OCS  = [u_nxp,-u_normal,u_nadir]'; */
  negate3 (u_normal);
  temp3x3 [0] [0] = u_nxp [0];
  temp3x3 [1] [0] = u_nxp [1];
  temp3x3 [2] [0] = u_nxp [2];
	          
  temp3x3 [0] [1] = u_normal [0];
  temp3x3 [1] [1] = u_normal [1];
  temp3x3 [2] [1] = u_normal [2];
	          
  temp3x3 [0] [2] = u_nadir [0];
  temp3x3 [1] [2] = u_nadir [1];
  temp3x3 [2] [2] = u_nadir [2];

  transpose3x3 (temp3x3, A_GCI2OCS);

  /*  % Convert input attitude quaterion to matrix */
  /*  A_GCI2BODY = qtoa(quat); */
  qtoa (q, A_GCI2BODY);

  /*  % Compute OCS to Body matrix */
  /*  A_OCS2BODY = A_GCI2BODY * A_GCI2OCS'; */
  transpose3x3 (A_GCI2OCS, temp3x3);
  mult3x3 (A_OCS2BODY, A_GCI2BODY, temp3x3);

  /*  % Convert to 1-2-3 Euler angles */
  /*  Euler_angles = atoeul(A_OCS2BODY,[1;2;3]); */
  atoeul (A_OCS2BODY, Euler_angles);

  /*  % Extract Roll angle (deg) */
  /*  SC_roll = Euler_angles(1) * 180/pi; */

  /*  % Compute instrument look angles (deg) */
  /*  ALI_lookangle = SC_roll + ALI_rolloffset; */
  /*  HSI_lookangle = SC_roll + HSI_rolloffset; */

  return Euler_angles [0] + HSI_rolloffset;
}

/* returns 1 on failure, 0 on success */
static int interpolate (double refTime, int valueCount, double * values, double * times, double * result) {
  int i;

  if (refTime < times [0] || refTime > times [valueCount - 1]) {
    zifmessage ("interpolation beyond bounds; bad/wrong ACS file?");
    return 1;
  }

  for (i = 0; i < valueCount - 1; i ++) {
    if (times [i] == refTime) {
      * result =  values [i];
      return 0;
    }

    if (times [i] == times [i + 1])
      continue;

    if (times [i + 1] > refTime) { /* it's in this interval */
      * result = values [i] + (refTime - times [i]) * (values [i + 1] - values [i]) / (times [i + 1] - times [i]);
      return 0;
    }
  }

  return 1;			/* should never reach here */
}

void logNeedle (char * buf, char * needle, int echoMeta, char * metaName, int doimg, int vunit) {
  double dval;
  char * p = strstr (buf, needle);

  if (p) {
    p += strlen (needle) + 3;	/* skip " = " */
    if (sscanf (p, "%lf", & dval) == 1)
      logMetaDouble (echoMeta, metaName, needle, dval, doimg?1:0, & vunit);
  }
}

void main44()
{
  int parmct, parmdf;
  char inImage [99];
  char inACS [99];
  char inAcorn [99];
  FILE * acorn = 0;
  char outname [99];
  char outImageName [200];
  char outRawName [200];
  char outCalName [200];
  char outOffsetName [200];
  char metaName [200];
  char * startTimeStr = 0;
  double imageStartTime, imageStopTime;
  int lines;
  char msgBuf [1000];
  double startLat = 0, startLon = 0;
  double stopLat = 0, stopLon = 0;
  int sl, ss, nl, ns, sb, nbnd;
  int vunit;
  FILE * rawOut, * calOut, * offsetOut;
  char logIdBuf [100];
  int navFailed = 0;
  int echoMeta = 0;
  int doimg;
  char * inMET = 0;

#ifdef DO_GAIN
  char outGainName [200];
  double vnirGain, swirGain;
  FILE * gainOut;
#endif

  checkLoggerUtilsVersion (15);

  sprintf (logIdBuf, "hyperionlog version %s", _hyperionlog_version_);
  zifmessage (logIdBuf);

  /* fetch image and acs files */
  zvparm ("inp", inImage, &parmct, &parmdf, 1, 99);
  zvparm ("acs", inACS, &parmct, &parmdf, 1, 99);
  zvparm ("acorn", inAcorn, &parmct, &parmdf, 1, 99);

  zvp("doimg", &doimg, &parmct);
  zvp("echoMeta", &echoMeta, &parmct);

  if (! strlen (inACS))
    zifmessage ("no ACS/MET file, nav information will not be provided");
  else if (! strcasecmp (inACS + strlen (inACS) - 4, ".MET")) {
    inMET = strdup (inACS);
    inACS [0] = 0;
  }

  if (strlen (inACS)) {			/* test the ACS file */
    int sd_id = SDstart (inACS, DFACC_READ);
    if (sd_id < 0) {
      zifmessage ("bad ACS file, nav information will not be calculated");
      inACS [0] = 0;

    } else {
      int32 n_datasets, n_file_attrs;

      if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs) == -1) {
	zifmessage ("bad ACS file, nav information will not be calculated");
	inACS [0] = 0;
      }

      SDend (sd_id);
    }
  }

  if (strlen (inAcorn))
    acorn = fopen (inAcorn, "rb");
  if (acorn)
    zifmessage ("using acorned image source");

  /* fetch out file name */
  zvparm ("out", outname, &parmct, &parmdf, 1, 99);

  /* fetch the subwindow and band selection parms */
  zvp("sl", & sl, &parmct);
  zvp("ss", & ss, &parmct);
  zvp("nl", & nl, &parmct);
  zvp("ns", & ns, &parmct);
  zvp("sb", & sb, &parmct);
  zvp("nbnd", & nbnd, &parmct);
#ifdef DO_GAIN
  zvparmd("vg", & vnirGain, &parmct, & parmdf, 1, 0);
  zvparmd("sg", & swirGain, &parmct, & parmdf, 1, 0);
#endif
  
  /* get image start time and along track pixels */
  {
    int sd_id = SDstart (inImage, DFACC_READ);
    int attrIndex = SDfindattr(sd_id, "ImageStartTime");
    char attr_name [257];
    char sds_name [257];
    int32 data_type, n_values;
    int32 n_datasets, n_file_attrs;
    int dsIndex;
    int sds_id = -1;
    int32 rank, n_attrs, dim_sizes [32];

    if (sd_id < 0) {
      sprintf (msgBuf, "SDstart failed on %s\n", inImage);
      zmabend (msgBuf);
    } else {
      printf ("SDstart succeeded on %s\n", inImage);
    }

    SDattrinfo (sd_id, attrIndex, attr_name, &data_type, &n_values);
    startTimeStr = malloc (n_values + 1);
    SDreadattr (sd_id, attrIndex, startTimeStr);

    if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs) == -1) {
      sprintf (msgBuf, "SDfileinfo failed on %s\n", inImage);
      zmabend (msgBuf);
    }

    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (strstr (sds_name, "L1R"))
	break;
      else
	SDendaccess (sds_id);
    }

    assert (dsIndex < n_datasets);

    for (attrIndex = 0; attrIndex < n_attrs; attrIndex ++) {
      SDattrinfo (sds_id, attrIndex, attr_name, &data_type, &n_values);

      if (! strcmp (attr_name, "Number of Along Track Pixels"))
	break;
    }

    assert (attrIndex < n_attrs);

    SDreadattr (sds_id, attrIndex, & lines);

    SDendaccess (sds_id);

    SDend (sd_id);
  }

  sl = BOUNDED (sl, 1, lines);
  ss = BOUNDED (ss, 1, samples);
  nl = BOUNDED (nl, 1, lines - sl + 1);
  ns = BOUNDED (ns, 1, samples - ss + 1);
  sb = BOUNDED (sb, 1, bands);
  nbnd = BOUNDED (nbnd, 1, bands - sb + 1);

  /* given frame rate, compute start and end times in fractional
     seconds since 1/1/1970 */
  {
    int julianDay, thousandths;
    time_t epochSeconds;
    struct tm time;
    char saveTZ [100];

    time.tm_isdst = 0;
    sscanf (startTimeStr, "%4d%03d%02d%02d%02d.%03d",
	    & time.tm_year,
	    & julianDay,
	    & time.tm_hour,
	    & time.tm_min,
	    & time.tm_sec,
	    & thousandths);

    time.tm_year -= 1900;
    dayOfYearToDate (julianDay, time.tm_year, & time.tm_mon, & time.tm_mday);

    /* while we have the date in hand, let's autogen the outname */
    if (strlen (outname) == 0)
      sprintf (outname, "EO1_HYPERION_%4d%02d%02d_%02d%02d",
	       time.tm_year + 1900, time.tm_mon, time.tm_mday, time.tm_hour, time.tm_min);

    time.tm_mon -= 1;		/* to be consistent with struct tm, January => 0 */

    /* save the timezone */
    if (getenv ("TZ") )
      strcpy (saveTZ, getenv ("TZ"));
    else
      saveTZ [0] = 0;

#ifdef __i386__
    setenv ("TZ", "", 1);	/* set to UTC for mktime */
#else
    putenv ("TZ=\"\"");
#endif

    epochSeconds = mktime (& time);

    /* restore timezone */
    if (saveTZ [0])
#ifdef __i386__
      setenv ("TZ", saveTZ, 1);
#else
    {
      char tmpbuf [20];
      sprintf (tmpbuf, "TZ=%s", saveTZ);
      putenv (tmpbuf);
    }
#endif
    else
#ifdef __i386__
      unsetenv ("TZ");
#else
    putenv ("TZ");
#endif

    imageStartTime = epochSeconds + thousandths/1000.0;
    imageStopTime = imageStartTime + lines/frameRate;
  }

  /* compute output file names */
  if (nbnd == 1) {
    sprintf (outImageName, "%s_%d.img", outname, sb);
    sprintf (outRawName, "%s_%d.raw", outname, sb);
    sprintf (outCalName, "%s_%d.cal", outname, sb);
    sprintf (outOffsetName, "%s_%d.off", outname, sb);
#ifdef DO_GAIN
    sprintf (outGainName, "%s_%d.gain", outname, sb);
#endif
  }
  else {
    sprintf (outImageName, "%s_%d-%d.img", outname, sb, sb + nbnd - 1);
    sprintf (outRawName, "%s_%d-%d.raw", outname, sb, sb + nbnd - 1);
    sprintf (outCalName, "%s_%d-%d.cal", outname, sb, sb + nbnd - 1);
    sprintf (outOffsetName, "%s_%d-%d.off", outname, sb, sb + nbnd - 1);
#ifdef DO_GAIN
    sprintf (outGainName, "%s_%d-%d.gain", outname, sb, sb + nbnd - 1);
#endif
  }
  sprintf (metaName, "%s.txt", outname);

  if (doimg) {
    /* create output VICAR file */
    if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outImageName, NULL) != 1)
      zmabend ("zvunit failed for out image");
    if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
		"O_FORMAT", "HALF", NULL) != 1)
      zmabend ("zvopen failed for out image");
  }

  initMetaData (metaName);

  /* create raw output files */
  rawOut = fopen (outRawName, "w");
  calOut = fopen (outCalName, "w");
  offsetOut = fopen (outOffsetName, "w");
#ifdef DO_GAIN
  gainOut = fopen (outGainName, "w");
#endif

  /* log some meta data */
  logMetaString (echoMeta, metaName, "LOGGER_ID", logIdBuf, doimg?1:0, & vunit);
  logMetaString (echoMeta, metaName, "ARCH_IMG_NAME", inImage, doimg?1:0, & vunit);
  if (strlen (inACS))
    logMetaString (echoMeta, metaName, "ARCH_ACS_NAME", inACS, doimg?1:0, & vunit);
  if (strlen (inAcorn))
    logMetaString (echoMeta, metaName, "ARCH_ACORN_NAME", inAcorn, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "ARCH_LINES", lines, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "ARCH_SAMPLES", samples, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_SL", sl, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_NL", nl, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_SS", ss, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_NS", ns, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_SB", sb, doimg?1:0, & vunit);
  logMetaInt (echoMeta, metaName, "LOG_NBND", nbnd, doimg?1:0, & vunit);

  if (inMET && strlen (inMET)) {
    struct stat statBuf;
    FILE * f;
    char * buf;

    /*
      PRODUCT_UL_CORNER_LAT = 40.532932
      PRODUCT_UL_CORNER_LON = -113.051924
      PRODUCT_UR_CORNER_LAT = 40.517082
      PRODUCT_UR_CORNER_LON = -112.962093
      PRODUCT_LL_CORNER_LAT = 39.693932
      PRODUCT_LL_CORNER_LON = -113.303088
      PRODUCT_LR_CORNER_LAT = 39.678218
      PRODUCT_LR_CORNER_LON = -113.214334
    */

    if (stat (inMET, & statBuf))
      zmabend ("error statting input metadata file");
    if (! (f = fopen (inMET, "r")))
      zmabend ("error opening metadata file");
    if (! (buf = malloc (statBuf.st_size + 1)))
      zmabend ("error mallocing metadata buffer");
    if (fread (buf, statBuf.st_size, 1, f) != 1)
      zmabend ("error reading metadata file");
    buf [statBuf.st_size + 1] = 0;
    
    logNeedle (buf, "PRODUCT_UL_CORNER_LAT", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_UL_CORNER_LON", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_UR_CORNER_LAT", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_UR_CORNER_LON", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_LL_CORNER_LAT", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_LL_CORNER_LON", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_LR_CORNER_LAT", echoMeta, metaName, doimg, vunit);
    logNeedle (buf, "PRODUCT_LR_CORNER_LON", echoMeta, metaName, doimg, vunit);

    free (buf);

    fclose (f);
  }

  if (strlen (inACS)) {
    /* get lat/lon and quaterion arrays */

    /* lat, lon */
    double * lats = 0, * latTimes = 0, * lons = 0, * lonTimes = 0;
    /* position */
    double * posXs = 0, * posXTimes = 0, * posYs = 0, * posYTimes = 0, * posZs = 0, * posZTimes = 0;
    /* velocity */
    double * velXs = 0, * velXTimes = 0, * velYs = 0, * velYTimes = 0, * velZs = 0, * velZTimes = 0;
    /* quaternion */
    double * q1s = 0, * q1Times = 0, * q2s = 0, * q2Times = 0;
    double * q3s = 0, * q3Times = 0, * q4s = 0, * q4Times = 0;

    int sd_id = SDstart (inACS, DFACC_READ);
    int32 n_datasets, n_file_attrs;
    int dsIndex, sds_id;
    int latDimSize = 0, lonDimSize = 0, q1DimSize = 0, q2DimSize = 0, q3DimSize = 0, q4DimSize = 0;
    int posXDimSize = 0, posYDimSize = 0, posZDimSize = 0, velXDimSize = 0, velYDimSize = 0, velZDimSize = 0;
    char sds_name [65]; 
    int32 dim_sizes [32]; 
    int32 rank, data_type, n_attrs; 
    int32 start = 0;

    if (sd_id < 0) {
	printf ("SDstart failed on %s\n", inACS);
    } else {
        printf ("SDstart succeeded on %s\n", inACS);
    }

    if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs) == -1) {
      sprintf (msgBuf, "SDfileinfo failed for ACS file %s", inACS);
      zmabend (msgBuf);
    }

    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (! strcmp (sds_name, "P@ACSSCLATE")) { /* Latitude */
	assert (rank == 1); assert (data_type == 6);
	if (latDimSize) assert (latDimSize == dim_sizes [0]);
	else latDimSize = dim_sizes [0];
	lats = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, lats))
	  zmabend ("SDreaddata failed on P@ACSSCLATE");
      } else if (! strcmp (sds_name, "A@ACSSCLATE(TIME_MEASURED)")) { /* Latitude Time */
	assert (rank == 1); assert (data_type == 6);
	if (latDimSize) assert (latDimSize == dim_sizes [0]);
	else latDimSize = dim_sizes [0];
	latTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, latTimes))
	  zmabend ("SDreaddata failed on A@ACSSCLATE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSSCLONGE")) { /* Longitude */
	assert (rank == 1); assert (data_type == 6);
	if (lonDimSize) assert (lonDimSize == dim_sizes [0]);
	else lonDimSize = dim_sizes [0];
	lons = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, lons))
	  zmabend ("SDreaddata failed on P@ACSSCLONGE");
      } else if (! strcmp (sds_name, "A@ACSSCLONGE(TIME_MEASURED)")) { /* Longitude Time */
	assert (rank == 1); assert (data_type == 6);
	if (lonDimSize) assert (lonDimSize == dim_sizes [0]);
	else lonDimSize = dim_sizes [0];
	lonTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, lonTimes))
	  zmabend ("SDreaddata failed on A@ACSSCLONGE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSKFQ1C")) { /* Q1 */
	assert (rank == 1); assert (data_type == 6);
	if (q1DimSize) assert (q1DimSize == dim_sizes [0]);
	else q1DimSize = dim_sizes [0];
	q1s = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q1s))
	  zmabend ("SDreaddata failed on P@ACSKFQ1C");
      } else if (! strcmp (sds_name, "A@ACSKFQ1C(TIME_MEASURED)")) { /* Q1 Time */
	assert (rank == 1); assert (data_type == 6);
	if (q1DimSize) assert (q1DimSize == dim_sizes [0]);
	else q1DimSize = dim_sizes [0];
	q1Times = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q1Times))
	  zmabend ("SDreaddata failed on A@ACSKFQ1C(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSKFQ2C")) { /* Q2 */
	assert (rank == 1); assert (data_type == 6);
	if (q2DimSize) assert (q2DimSize == dim_sizes [0]);
	else q2DimSize = dim_sizes [0];
	q2s = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q2s))
	  zmabend ("SDreaddata failed on P@ACSKFQ2C");
      } else if (! strcmp (sds_name, "A@ACSKFQ2C(TIME_MEASURED)")) { /* Q2 Time */
	assert (rank == 1); assert (data_type == 6);
	if (q2DimSize) assert (q2DimSize == dim_sizes [0]);
	else q2DimSize = dim_sizes [0];
	q2Times = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q2Times))
	  zmabend ("SDreaddata failed on A@ACSKFQ2C(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSKFQ3C")) { /* Q3 */
	assert (rank == 1); assert (data_type == 6);
	if (q3DimSize) assert (q3DimSize == dim_sizes [0]);
	else q3DimSize = dim_sizes [0];
	q3s = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q3s))
	  zmabend ("SDreaddata failed on P@ACSKFQ3C");
      } else if (! strcmp (sds_name, "A@ACSKFQ3C(TIME_MEASURED)")) { /* Q3 Time */
	assert (rank == 1); assert (data_type == 6);
	if (q3DimSize) assert (q3DimSize == dim_sizes [0]);
	else q3DimSize = dim_sizes [0];
	q3Times = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q3Times))
	  zmabend ("SDreaddata failed on A@ACSKFQ3C(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSKFQ4C")) { /* Q4 */
	assert (rank == 1); assert (data_type == 6);
	if (q4DimSize) assert (q4DimSize == dim_sizes [0]);
	else q4DimSize = dim_sizes [0];
	q4s = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q4s))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSKFQ4C(TIME_MEASURED)")) { /* Q4 Time */
	assert (rank == 1); assert (data_type == 6);
	if (q4DimSize) assert (q4DimSize == dim_sizes [0]);
	else q4DimSize = dim_sizes [0];
	q4Times = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, q4Times))
	  zmabend ("SDreaddata failed on A@ACSKFQ4C(TIME_MEASURED)");

	
      } else if (! strcmp (sds_name, "P@ACSSCPOSXE")) { /* PosX */
	assert (rank == 1); assert (data_type == 6);
	if (posXDimSize) assert (posXDimSize == dim_sizes [0]);
	else posXDimSize = dim_sizes [0];
	posXs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posXs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCPOSXE(TIME_MEASURED)")) { /* PosX Time */
	assert (rank == 1); assert (data_type == 6);
	if (posXDimSize) assert (posXDimSize == dim_sizes [0]);
	else posXDimSize = dim_sizes [0];
	posXTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posXTimes))
	  zmabend ("SDreaddata failed on A@ACSSCPOSXE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSSCPOSYE")) { /* PosY */
	assert (rank == 1); assert (data_type == 6);
	if (posYDimSize) assert (posYDimSize == dim_sizes [0]);
	else posYDimSize = dim_sizes [0];
	posYs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posYs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCPOSYE(TIME_MEASURED)")) { /* PosY Time */
	assert (rank == 1); assert (data_type == 6);
	if (posYDimSize) assert (posYDimSize == dim_sizes [0]);
	else posYDimSize = dim_sizes [0];
	posYTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posYTimes))
	  zmabend ("SDreaddata failed on A@ACSSCPOSYE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSSCPOSZE")) { /* PosZ */
	assert (rank == 1); assert (data_type == 6);
	if (posZDimSize) assert (posZDimSize == dim_sizes [0]);
	else posZDimSize = dim_sizes [0];
	posZs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posZs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCPOSZE(TIME_MEASURED)")) { /* PosZ Time */
	assert (rank == 1); assert (data_type == 6);
	if (posZDimSize) assert (posZDimSize == dim_sizes [0]);
	else posZDimSize = dim_sizes [0];
	posZTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, posZTimes))
	  zmabend ("SDreaddata failed on A@ACSSCPOSZ(TIME_MEASURED)");

      } else if (! strcmp (sds_name, "P@ACSSCVELXE")) { /* VelX */
	assert (rank == 1); assert (data_type == 6);
	if (velXDimSize) assert (velXDimSize == dim_sizes [0]);
	else velXDimSize = dim_sizes [0];
	velXs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velXs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCVELXE(TIME_MEASURED)")) { /* VelX Time */
	assert (rank == 1); assert (data_type == 6);
	if (velXDimSize) assert (velXDimSize == dim_sizes [0]);
	else velXDimSize = dim_sizes [0];
	velXTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velXTimes))
	  zmabend ("SDreaddata failed on A@ACSSCVELXE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSSCVELYE")) { /* VelY */
	assert (rank == 1); assert (data_type == 6);
	if (velYDimSize) assert (velYDimSize == dim_sizes [0]);
	else velYDimSize = dim_sizes [0];
	velYs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velYs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCVELYE(TIME_MEASURED)")) { /* VelY Time */
	assert (rank == 1); assert (data_type == 6);
	if (velYDimSize) assert (velYDimSize == dim_sizes [0]);
	else velYDimSize = dim_sizes [0];
	velYTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velYTimes))
	  zmabend ("SDreaddata failed on A@ACSSCVELYE(TIME_MEASURED)");
      } else if (! strcmp (sds_name, "P@ACSSCVELZE")) { /* VelZ */
	assert (rank == 1); assert (data_type == 6);
	if (velZDimSize) assert (velZDimSize == dim_sizes [0]);
	else velZDimSize = dim_sizes [0];
	velZs = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velZs))
	  zmabend ("SDreaddata failed");
      } else if (! strcmp (sds_name, "A@ACSSCVELZE(TIME_MEASURED)")) { /* VelZ Time */
	assert (rank == 1); assert (data_type == 6);
	if (velZDimSize) assert (velZDimSize == dim_sizes [0]);
	else velZDimSize = dim_sizes [0];
	velZTimes = (double *) malloc (sizeof (double) * dim_sizes [0]);
	if (SDreaddata (sds_id, & start, 0, dim_sizes, velZTimes))
	  zmabend ("SDreaddata failed on A@ACSSCVELZ(TIME_MEASURED)");
      }

      SDendaccess (sds_id);
    }

    SDend (sd_id);

    /* find bounding quaternion and lat/lon values and interpolate
     single fractional values for start/end lines */
    {
      double q[4];
      double pos[3];
      double vel[3];
      double lookAngle;
      
      /* get start/stop lat/lon, quaternion, pos, vel for start look angle computation */

      navFailed =
	interpolate (imageStartTime, latDimSize, lats, latTimes, & startLat) ||
	interpolate (imageStopTime, latDimSize, lats, latTimes, & stopLat) ||
	interpolate (imageStartTime, lonDimSize, lons, lonTimes, & startLon) ||
	interpolate (imageStopTime, lonDimSize, lons, lonTimes, & stopLon) ||
	interpolate (imageStartTime, q1DimSize, q1s, q1Times, & q[0]) ||
	interpolate (imageStartTime, q2DimSize, q2s, q2Times, & q[1]) ||
	interpolate (imageStartTime, q3DimSize, q3s, q3Times, & q[2]) ||
	interpolate (imageStartTime, q4DimSize, q4s, q4Times, & q[3]) ||
	interpolate (imageStartTime, posXDimSize, posXs, posXTimes, & pos[0]) ||
	interpolate (imageStartTime, posYDimSize, posYs, posYTimes, & pos[1]) ||
	interpolate (imageStartTime, posZDimSize, posZs, posZTimes, & pos[2]) ||
	interpolate (imageStartTime, velXDimSize, velXs, velXTimes, & vel[0]) ||
	interpolate (imageStartTime, velYDimSize, velYs, velYTimes, & vel[1]) ||
	interpolate (imageStartTime, velZDimSize, velZs, velZTimes, & vel[2]);
      
      if (! navFailed) {
	logMetaDouble (echoMeta, metaName, "CALC_START_Q1", q[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_Q2", q[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_Q3", q[2], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_Q4", q[3], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_POSX", pos[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_POSY", pos[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_POSZ", pos[2], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_VELX", vel[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_VELY", vel[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_START_VELZ", vel[2], doimg?1:0, & vunit);      

	lookAngle = calcLookAngle (pos, vel, q);
	logMetaDouble (echoMeta, metaName, "CALC_START_LOOK_ANGLE", lookAngle, doimg?1:0, & vunit);      
      }

      /* get quaternion, pos, vel for stop look angle computation */
      navFailed =
	navFailed ||
	interpolate (imageStopTime, q1DimSize, q1s, q1Times, & q[0]) ||
	interpolate (imageStopTime, q2DimSize, q2s, q2Times, & q[1]) ||
	interpolate (imageStopTime, q3DimSize, q3s, q3Times, & q[2]) ||
	interpolate (imageStopTime, q4DimSize, q4s, q4Times, & q[3]) ||
	interpolate (imageStopTime, posXDimSize, posXs, posXTimes, & pos[0]) ||
	interpolate (imageStopTime, posYDimSize, posYs, posYTimes, & pos[1]) ||
	interpolate (imageStopTime, posZDimSize, posZs, posZTimes, & pos[2]) ||
	interpolate (imageStopTime, velXDimSize, velXs, velXTimes, & vel[0]) ||
	interpolate (imageStopTime, velYDimSize, velYs, velYTimes, & vel[1]) ||
	interpolate (imageStopTime, velZDimSize, velZs, velZTimes, & vel[2]);
      
      if (! navFailed) {
	logMetaDouble (echoMeta, metaName, "CALC_STOP_Q1", q[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_Q2", q[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_Q3", q[2], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_Q4", q[3], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_POSX", pos[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_POSY", pos[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_POSZ", pos[2], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_VELX", vel[0], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_VELY", vel[1], doimg?1:0, & vunit);      
	logMetaDouble (echoMeta, metaName, "CALC_STOP_VELZ", vel[2], doimg?1:0, & vunit);      

	lookAngle = calcLookAngle (pos, vel, q);
	logMetaDouble (echoMeta, metaName, "CALC_STOP_LOOK_ANGLE", lookAngle, doimg?1:0, & vunit);      
      }
    }
  }

  if (navFailed)
    zifmessage ("nav computation failed; wrong ACS file?");

  {
    time_t seconds;
    char buf [100];

    seconds = imageStartTime;
    logMetaDouble (echoMeta, metaName, "CALC_STARTTIME_SEC", imageStartTime, doimg?1:0, & vunit);
    strcpy (buf, asctime (gmtime (& seconds)));
    buf [strlen (buf) - 1] = 0;	/* trim trailing newline */
    logMetaString (echoMeta, metaName, "CALC_STARTTIME_ASC", buf, doimg?1:0, & vunit);
    seconds = imageStopTime;
    logMetaDouble (echoMeta, metaName, "CALC_STOPTIME_SEC", imageStopTime, doimg?1:0, & vunit);
    strcpy (buf, asctime (gmtime (& seconds)));
    buf [strlen (buf) - 1] = 0;	/* trim trailing newline */
    logMetaString (echoMeta, metaName, "CALC_STOPTIME_ASC", buf, doimg?1:0, & vunit);
  }

  if (strlen (inACS) && ! navFailed) {
    logMetaDouble (echoMeta, metaName, "CALC_STARTLAT", startLat, doimg?1:0, & vunit);
    logMetaDouble (echoMeta, metaName, "CALC_STOPLAT", stopLat, doimg?1:0, & vunit);
    logMetaDouble (echoMeta, metaName, "CALC_STARTLON", startLon, doimg?1:0, & vunit);
    logMetaDouble (echoMeta, metaName, "CALC_STOPLON", stopLon, doimg?1:0, & vunit);
  }

  /* get the band(s) */
  if (doimg) {
    short * shortBuf = (short *) malloc (sizeof (short) * samples);
    long * lineBuf = (long *) malloc (sizeof (long) * samples);
    int line, sample, band;
    int sd_id = SDstart (inImage, DFACC_READ);
    char sds_name [257];
    int32 data_type;
    int32 n_datasets, n_file_attrs;
    int dsIndex;
    int sds_id = -1;
    int32 rank, n_attrs, dim_sizes [32], start [3], edges [3];
    
    if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs) == -1)
      zmabend ("SDfileinfo failed\n");

    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (strstr (sds_name, "L1R"))
	break;
      else
	SDendaccess (sds_id);
    }

    assert (dsIndex < n_datasets);
    assert (rank == 3);
    assert (data_type == 22);
    assert (dim_sizes [1] == 242); /* bands */
    assert (dim_sizes [2] == 256); /* samples */

    start [2] = ss - 1;
    edges [0] = 1;
    edges [1] = 1;
    edges [2] = ns;
    for (line = sl; line < sl + nl; line ++) {
      memset (lineBuf, 0, sizeof (long) * samples);
      start [0] = line - 1;

      for (band = sb; band < sb + nbnd; band ++) {
	start [1] = band - 1;

	if (acorn) {
	  int ssOffset =
	    (line - 1) * 242 * 256 * 2 + /* line offset */
	    (band - 1)       * 256 * 2 + /* band offset */
	    (ss - 1)               * 2; /* sample offset */
	  if (fseek (acorn, ssOffset, SEEK_SET) ||
	      fread (shortBuf, 2, ns, acorn) != ns)
	    zmabend ("fseek/fread failed on image");
#ifndef __i386__
	  /* make it big endian for Solaris */
	  {
	    int i;
	    for (i = 0; i < ns; i ++)
	      shortBuf [i] = shortSwapBytes (shortBuf [i]);
	  }
#endif
	} else if (SDreaddata (sds_id, start, 0, edges, shortBuf))
	  zmabend ("SDreaddata failed on image");
	
	/* write raw band interleaved */
	fwrite (shortBuf, sizeof (short), samples, rawOut);

	if (nbnd > 1)
	  for (sample = 0; sample < ns; sample ++)
	    lineBuf [sample] += shortBuf [sample];
      }

      if (nbnd > 1)
	for (sample = 0; sample < ns; sample ++)
	  shortBuf [sample] = lineBuf [sample] / nbnd;

      /* write image line to VICAR image output */
      zvwrit (vunit,
	      shortBuf,
	      "LINE", line - sl + 1,
	      "SAMP", 1,
	      "NSAMPS", ns, NULL);
    }

    SDendaccess (sds_id);

    SDend (sd_id);

    if (acorn)
      fclose (acorn);

    zvclose (vunit, NULL);
  }

  /* ACORN calibration file has band center and band width for each
   * channel. These come from the Spectral Center Wavelengths and
   * Spectral Bandwidths data sets. */
  {
    int band;
    int sd_id = SDstart (inImage, DFACC_READ);
    char sds_name [257];
    int32 data_type;
    int32 n_datasets, n_file_attrs;
    int dsIndex;
    int sds_id = -1;
    int32 rank, n_attrs, dim_sizes [32], start [3], edges [3];
    float pixelBandLengths [256];
    float bandLengths [242];
    float bandLengthSum = 0.0;
    float pixelBandWidths [256];
    float bandWidths [242];
    float bandWidthSum = 0.0;
    int i;
#ifdef DO_GAIN
    float pixelGains [256];
    float gains [242];
    float gainSum = 0.0;
#endif
    
    if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs) == -1)
      zmabend ("SDfileinfo failed\n");

    /* get wavelengths */
    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (! strcmp (sds_name, "Spectral Center Wavelengths"))
	break;
      else
	SDendaccess (sds_id);
    }

    assert (dsIndex < n_datasets);
    assert (rank == 2);
    assert (data_type == 5);	/* 32-bit float */
    assert (dim_sizes [0] == 242); /* bands */
    assert (dim_sizes [1] == 256); /* samples */

    for (band = sb; band < sb + nbnd; band ++) {
      start [0] = band - 1;	/* band is one-based */
      start [1] = 0;
      edges [0] = 1;
      edges [1] = 256;

      memset (pixelBandLengths, 0, sizeof (float) * 256);

      if (SDreaddata (sds_id, start, 0, edges, pixelBandLengths))
	zmabend ("SDreaddata failed on band centers");
	
      bandLengthSum = 0.0;
      for (i = 0; i < 256; i ++)
	bandLengthSum += pixelBandLengths [i];
      bandLengths [band - 1] = bandLengthSum / 256; /* band is one-based */
    }

    SDendaccess (sds_id);

    /* get bandwidths */
    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (! strcmp (sds_name, "Spectral Bandwidths"))
	break;
      else
	SDendaccess (sds_id);
    }

    assert (dsIndex < n_datasets);
    assert (rank == 2);
    assert (data_type == 5);	/* 32-bit float */
    assert (dim_sizes [0] == 242); /* bands */
    assert (dim_sizes [1] == 256); /* samples */

    for (band = sb; band < sb + nbnd; band ++) {
      start [0] = band - 1;	/* band is one-based */
      start [1] = 0;
      edges [0] = 1;
      edges [1] = 256;

      memset (pixelBandWidths, 0, sizeof (float) * 256);

      if (SDreaddata (sds_id, start, 0, edges, pixelBandWidths))
	zmabend ("SDreaddata failed on band centers");
	
      bandWidthSum = 0.0;
      for (i = 0; i < 256; i ++)
	bandWidthSum += pixelBandWidths [i];
      bandWidths [band - 1] = bandWidthSum / 256; /* band is one-based */
    }

    SDendaccess (sds_id);

#ifdef DO_GAIN
    /* get gains */
    for (dsIndex = 0; dsIndex < n_datasets; dsIndex ++) {
      sds_id = SDselect (sd_id, dsIndex);
      SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &data_type, &n_attrs);

      if (! strcmp (sds_name, "Gain Coefficients"))
	break;
      else
	SDendaccess (sds_id);
    }

    assert (dsIndex < n_datasets);
    assert (rank == 2);
    assert (data_type == 5);	/* 32-bit float */
    assert (dim_sizes [0] == 242); /* bands */
    assert (dim_sizes [1] == 256); /* samples */

    for (band = sb; band < sb + nbnd; band ++) {
      start [0] = band - 1;	/* band is one-based */
      start [1] = 0;
      edges [0] = 1;
      edges [1] = 256;

      memset (pixelGains, 0, sizeof (float) * 256);

      if (SDreaddata (sds_id, start, 0, edges, pixelGains))
	zmabend ("SDreaddata failed on band centers");
	
      gainSum = 0.0;
      for (i = 0; i < 256; i ++)
	gainSum += pixelGains [i];
      gains [band - 1] = gainSum / 256; /* band is one-based */
    }

    SDendaccess (sds_id);

    SDend (sd_id);
#endif

    /* write calibration file */
    for (band = sb; band < sb + nbnd; band ++) {
      fprintf (calOut, "%f %f\n", bandLengths [band - 1], bandWidths [band - 1]);
    }

#ifdef DO_GAIN
  /* ACORN gain file comes from Gain Coefficients data set, multiplied
     by optional VNIR (1-70) and SWIR (71-242) gain factors */

    /* write gain file */
    for (band = sb; band < sb + nbnd; band ++) {
      if (band < 71)		/* VNIR */
	fprintf (gainOut, "%f\n", gains [band - 1] * vnirGain);
      else			/* SWIR */
	fprintf (gainOut, "%f\n", gains [band - 1] * swirGain);
    }
#endif
  } /* ACORN stuff */


  /* ACORN offsets are zero */
  {
    int i;
    for (i = 0; i < nbnd; i ++)
      fprintf (offsetOut, "0.0\n");
  }

  fclose (rawOut);
  fclose (calOut);
  fclose (offsetOut);
#ifdef DO_GAIN
  fclose (gainOut);
#endif
}

