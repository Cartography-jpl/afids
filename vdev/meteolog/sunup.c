#include <stdio.h>
#include <math.h>
/* for abs */
#include <stdlib.h>
#include "carto/cartoLoggerUtils.h"
#include "sunup.h"

#if 0
/* moved to cartoLoggerUtils.c */
/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
void dayOfYearToDate (int dayOfYear, int year, int * month, int * day) {
  int leapDay = 0;
  int daysInMonth;
  int daysSoFar = 0;

  if (year % 4 == 0)
    leapDay = 1;
  if (year % 100 == 0)
    leapDay = 0;
  if (year % 400 == 0)
    leapDay = 1;

  for (* month = 1; * month <= 12; (* month) ++) {
    switch (* month) {
    case 2:
      daysInMonth = 28 + leapDay;
      break;
    case 4:
    case 6:
    case 9:
    case 11:
      daysInMonth = 30;
      break;
    default:
      daysInMonth = 31;
    }

    if (dayOfYear <= daysSoFar + daysInMonth) {
      * day = dayOfYear - daysSoFar;
      return;
    }

    daysSoFar += daysInMonth;
  }
}
#endif

void logDayTimeFlag (char * metaFileName, unsigned short UTCStartDay, unsigned short UTCStartYear,
		     double sunLat, double sunLon, double riseOff, double setOff, int utcTimeInMinutes,
		     int unitCount, int * vunits) {
  int month, day;

  dayOfYearToDate (UTCStartDay, UTCStartYear, & month, & day);

  logDayMonthTimeFlag (metaFileName, day, month, UTCStartYear,
		       sunLat, sunLon, riseOff, setOff, utcTimeInMinutes, unitCount, vunits, 0);
}

void logDayMonthTimeFlag (char * metaFileName, int day, int month, unsigned short UTCStartYear,
		     double sunLat, double sunLon, double riseOff, double setOff, int utcTimeInMinutes,
			  int unitCount, int * vunits, int echoMeta) {
  int upDownFlag;
  double utcSunrise, utcSunset;
  char buf [100];
  int sunriseMin, sunsetMin, hours;
  int daytimeFlag;

  upDownFlag= calcSunupDown (sunLat, sunLon, UTCStartYear, month, day, & utcSunrise, & utcSunset);
  sunriseMin = utcSunrise;
  sunsetMin = utcSunset;
  hours = sunriseMin / 60;
  sprintf (buf, "%02d:%02d", hours, sunriseMin - 60 * hours);
  logMetaString (echoMeta, metaFileName, "CALC_UTC_SUNRISE", buf, unitCount, vunits);
  hours = sunsetMin / 60;
  sprintf (buf, "%02d:%02d", hours, sunsetMin - 60 * hours);
  logMetaString (echoMeta, metaFileName, "CALC_UTC_SUNSET", buf, unitCount, vunits);

  if (utcSunrise >= 0) {
    if (utcSunset >= 0) {	/* sunrise and sunset */
      if ((utcSunrise < utcSunset &&
	   utcSunrise + riseOff <= utcTimeInMinutes &&
	   utcSunset + setOff >= utcTimeInMinutes) ||
	  (utcSunrise > utcSunset &&
	   ! (utcSunrise + riseOff >= utcTimeInMinutes &&
	      utcSunset + setOff <= utcTimeInMinutes)))

	daytimeFlag = 1;
      else
	daytimeFlag = 0;
    } else {		/* sunrise, but no sunset -- beginning of long summer day */
      if ((utcSunrise < utcSunset &&
	   utcSunrise + riseOff <= utcTimeInMinutes) ||
	  (utcSunrise > utcSunset &&
	   utcSunrise + riseOff >= utcTimeInMinutes)) /* bug? */
	daytimeFlag = 1;
      else
	daytimeFlag = 0;
    }
  } else {
    if (utcSunset >= 0) {	/* no sunrise, but sunset -- end of long summer day */
      if ((utcSunrise < utcSunset &&
	  utcSunset + setOff >= utcTimeInMinutes) ||
	  (utcSunrise > utcSunset &&
	  utcSunset + setOff <= utcTimeInMinutes)) /* bug? */
	daytimeFlag = 1;
      else
	daytimeFlag = 0;
    } else {		/* no sunrise or sunset -- either all day or all night */
      if (upDownFlag == 1)	/* sun up all day -- 24 hour summer day */
	daytimeFlag = 1;
      else			/* sun down all day -- 24 hour winter night */
	daytimeFlag = 0;
    }
  }

  logMetaInt (echoMeta, metaFileName, "CALC_DAYTIME", daytimeFlag, unitCount, vunits);
}

/*
1320 '   This program by Roger W. Sinnott calculates the times of sunrise
1330 '   and sunset on any date, accurate to the minute within several
1340 '   centuries of the present.  It correctly describes what happens in the
1350 '   arctic and antarctic regions, where the Sun may not rise or set on
1360 '   a given date.  Enter north latitudes positive, west longitudes
1370 '   negative.  For the time zone, enter the number of hours west of
1380 '   Greenwich (e.g., 5 for EST, 4 for EDT).  The calculation is
1390 '   discussed in Sky & Telescope for August 1994, page 84.

Translated to C by W. Bunch
*/

static double A [3];
static double D [3];
static double Z0;		/* Time Zone in days west of Greenwich Standard Time */
static double T;		/* Time in days */
static double L5;		/* Longitude in degrees east of Greenwich */
static double DR;		/* Degrees to radians factor */
static double T0;
static double P2;		/* 2Pi */
static double S;		/* sin(Latitude) */
static double C;		/* cos(Latitude) */
static double Z;		/* cos(Zenith distance) */
static double A0;
static double D0;
static double A2;
static double D2;
static double DA;		/* A(2) - A(1) */
static int C0;			/* Range 0 to 23 */
static double K1;
static int M8;
static int W8;
static double V0;
static double V2;
static double A5;
static double D5;
static double J;
static double F;
static double sunriseTime;
static double sunsetTime;
static double * timeBeingSet = 0;
static int iYear, iMonth, iDay;
static double TT;		/* Centuries from 1900.0 */
/* has a value when the sun is up or down all day (near poles) */
static enum {down = -1, neither = 0, up = 1} upDown;

static int Sgn (double d) {
  if (d > 0.0)
    return 1;
  else if (d < 0.0)
    return -1;
  else
    return 0;
}

static void ConstantsDefinition ();
static void ComputeLSTAt0H ();
static void TestAnHour ();
static void SpecialMessage ();
static void FundamentalArgs ();
static void CalendarToJD ();

#ifdef MAKING_SUNUP_TEST_PROGRAM
int main (int argc, char * argv []) {
  double sunrise, sunset;
  int dayNight = 0;

  dayNight = calcSunupDown (35, -117.0, -1, 2002, 1, 25, & sunrise, & sunset);
  printf ("Palmdale sunrise %f sunset %f day/night %d\n", sunrise, sunset, dayNight);

  dayNight = calcSunupDown (35, -117.0, 1, 2002, 6, 25, & sunrise, & sunset);
  printf ("Palmdale jd=1 sunrise %f sunset %f day/night %d\n", sunrise, sunset, dayNight);

  dayNight = calcSunupDown (89, -117.0, -1, 2002, 1, 1, & sunrise, & sunset);
  printf ("Artic 1/1 sunrise %f sunset %f day/night %d\n", sunrise, sunset, dayNight);

  dayNight = calcSunupDown (89, -117.0, -1, 2002, 6, 1, & sunrise, & sunset);
  printf ("Artic 6/1 sunrise %f sunset %f day/night %d\n", sunrise, sunset, dayNight);

  dayNight = calcSunupDown (89, -117.0, -1, 2002, 3, 17, & sunrise, & sunset);
  printf ("Artic 3/17 sunrise %f sunset %f day/night %d\n", sunrise, sunset, dayNight);
  return 1;
}
#endif

/* returns -1 if reason for sunrise and sunset being -999 was that the
   sun was down all day returns 1 if reason for sunrise and sunset
   being -999 was that the sun was down all day
 */
int calcSunupDown (double lat, double lon, /* degrees */
		    int year, int month, int day,
		    double * sunrise, double * sunset /* UTC minutes */
		    ) {
  double B5;			/* Latitude in degrees north of the equator */
  double H;			/* Time Zone in hours west of Greenwich Standard Time */
  double Z1;			/* Zenith distance in radians */
  double DD;			/* D(2) - D(1) */
  double P;			/* Range (1..24)/24 */

  upDown = neither;

  iYear = year; iMonth = month; iDay = day;

  /* '10 '         Sunrise-Sunset */
  /* '20 GoSub 300 */
  ConstantsDefinition ();

  /*  '30 INPUT "Lat, Long (deg)";B5,L5 */
  B5 = lat;
  L5 = lon;

  /*  '40 INPUT "Time zone (hrs)";H */
  H = 0.0;			/* always calculates times in UTC */

  /*  '50 l5 = l5 / 360: z0 = h / 24 */
  L5 = L5 / 360.0;
  Z0 = H / 24.0;

  /*  '60 GoSub 1170: t = (j - 2451545) + f */
  CalendarToJD (J, F);
  T = (J - 2451545.0) + F;

  /*  '70 tt = t / 36525 + 1: ' TT = centuries */
  /*  '80 '               from 1900.0 */
  TT = T / 36525.0 + 1.0;

  /*  '90 GoSub 410: t = t + z0 */
  ComputeLSTAt0H ();
  T = T + Z0;

  /*  '100 ' */
  /*  '110 '       Get Sun's Position */
  /*  '120 GoSub 910: a(1) = A5: d(1) = D5 */
  FundamentalArgs (A5, D5);
  A [1] = A5;
  D [1] = D5;

  /*  '130 t = t + 1 */
  T = T + 1;

  /*  '140 GoSub 910: a(2) = A5: d(2) = D5 */
  FundamentalArgs (A5, D5);
  A [2] = A5;
  D [2] = D5;

  /*  '150 If a(2) < a(1) Then a(2) = a(2) + p2 */
  if (A [2] < A [1]) {
    A [2] = A [2] + P2;
  }

  /*  '160 Z1 = dr * 90.833: ' Zenith dist. */
  Z1 = DR * 90.833;

  /*  '170 s = sin(b5 * dr): C = Cos(b5 * dr) */
  S = sin (B5 * DR);
  C = cos (B5 * DR);

  /*  '180 Z = Cos(Z1): M8 = 0: W8 = 0: Print */
  Z = cos (Z1);
  M8 = 0;
  W8 = 0;

  /*  '190 A0 = a(1): D0 = d(1) */
  A0 = A [1];
  D0 = D [1];

  /*  '200 dA = a(2) - a(1): DD = d(2) - d(1) */
  DA = A [2] - A [1];
  DD = D [2] - D [1];

  /*  '210 For C0 = 0 To 23 */
  for (C0 = 0; C0 <= 23; C0 ++) {
    /* '220 P = (C0 + 1) / 24 */
    P = (C0 + 1.0) / 24.0;
    
    /* '230 A2 = a(1) + P * dA: D2 = d(1) + P * DD */
    A2 = A [1] + P * DA;
    D2 = D [1] + P * DD;
    
    /* '240 GoSub 490 */
    TestAnHour (M8, W8, V0, V2);
    
    /* '250 A0 = A2: D0 = D2: V0 = V2 */
    A0 = A2;
    D0 = D2;
    V0 = V2;
    
    /* '260 Next */
  }

  /* '270 GoSub 820: '  Special msg? */
  SpecialMessage (M8, W8, V2);

  /* '280 End */
  * sunrise = sunriseTime;
  * sunset = sunsetTime;

  return upDown;
}

static void ConstantsDefinition () {
  double P1;

  /* '300 '        Constants */
  /*  '310 Dim A(2), D(2) */
  /*  '320 P1 = 3.14159265: P2 = 2 * P1 */
  P1 = 3.14159265;
  P2 = 2 * P1;

  /*  '330 DR = P1 / 180: K1 = 15 * DR * 1.0027379 */
  DR = P1 / 180.0;
  K1 = 15 * DR * 1.0027379;

  /*  '340 S$ = "Sunset at  " */
  /*  '350 R$ = "Sunrise at " */
  /*  '360 M1$ = "No sunrise this date" */
  /*  '370 M2$ = "No sunset this date" */
  /*  '380 M3$ = "Sun down all day" */
  /*  '390 M4$ = "Sun up all day" */
  /*  '400 Return */
}

static void ComputeLSTAt0H () {
  /*  '410 '     LST at 0h zone time */
  /*  '420 T0 = t / 36525 */
  T0 = T / 36525.0;

  /*  '430 s = 24110.5 + 8640184.813 * T0 */
  S = 24110.5 + 8640184.813 * T0;

  /*  '440 s = s + 86636.6 * z0 + 86400 * l5 */
  S = S + 86636.6 * Z0 + 86400.0 * L5;

  /*  '450 s = s / 86400: s = s - Int(s) */
  S = S / 86400.0;
  S = S - floor(S);

/*  '460 T0 = s * 360 * dr */
  T0 = S * 360.0 * DR;

  /* '470 Return */
}

static void TestAnHour () {
  double L0;
  double L2;
  double H0;
  double H1;
  double H2;
  double D1;
  double V1;
  double A;
  double B;
  double D;
  double E;
  double T3;
  double H3;
  double M3;
  double H7;
  double N7;
  double D7;
  double AZ;

  /*  '490 '  Test an hour for an event */
  /*  '500 L0 = T0 + C0 * K1: L2 = L0 + K1 */
  L0 = T0 + C0 * K1;
  L2 = L0 + K1;

  /*  '510 H0 = L0 - A0: H2 = L2 - A2 */
  H0 = L0 - A0;
  H2 = L2 - A2;

  /*  '520 H1 = (H2 + H0) / 2: '  Hour angle, */
  H1 = (H2 + H0) / 2.0;

  /*  '530 D1 = (D2 + D0) / 2: '  declination, */
  /*  '540 '                at half hour */
  D1 = (D2 + D0) / 2.0;

  /*  '550 If C0 > 0 Then GoTo 570 */
  if (C0 <= 0) {
    /*      '560 V0 = S * sin(D0) + C * Cos(D0) * Cos(H0) - Z */
    V0 = S * sin(D0) + C * cos(D0) * cos(H0) - Z;
  }

  /*  '570 V2 = S * sin(D2) + C * cos(D2) * cos(H2) - Z */
  V2 = S * sin(D2) + C * cos(D2) * cos(H2) - Z;

  /*  '580 If Sgn(V0) = Sgn(V2) Then GoTo 800 */
  if (Sgn(V0) == Sgn(V2)) {
    return;
  }

  /*  '590 V1 = S * sin(D1) + C * cos(D1) * cos(H1) - Z */
  V1 = S * sin(D1) + C * cos(D1) * cos(H1) - Z;

  /*  '600 A = 2 * V2 - 4 * V1 + 2 * V0: B = 4 * V1 - 3 * V0 - V2 */
  A = 2 * V2 - 4 * V1 + 2 * V0;
  B = 4 * V1 - 3 * V0 - V2;

  /*  '610 D = B * B - 4 * A * V0: If D < 0 Then GoTo 800 */
  D = B * B - 4 * A * V0;
  if (D < 0.0) {
    return;
  }

  /*  '620 D = Sqr(D) */
  D = sqrt(D);

  /*  '630 If V0 < 0 And V2 > 0 Then Print R$; */
  if (V0 < 0.0 && V2 > 0.0) {
    timeBeingSet = & sunriseTime;
  }

  /*  '640 If V0 < 0 And V2 > 0 Then M8 = 1 */
  if (V0 < 0.0 && V2 > 0.0) {
    M8 = 1;
  }

  /*  '650 If V0 > 0 And V2 < 0 Then Print S$; */
  if (V0 > 0.0 && V2 < 0.0) {
    timeBeingSet = & sunsetTime;
  }

  /*  '660 If V0 > 0 And V2 < 0 Then W8 = 1 */
  if (V0 > 0.0 && V2 < 0.0) {
    W8 = 1;
  }

  /*  '670 E = (-B + D) / (2 * A) */
  E = (-B + D) / (2.0 * A);

  /*  '680 If E > 1 Or E < 0 Then E = (-B - D) / (2 * A) */
  if (E > 1.0 || E < 0.0) {
    E = (-B - D) / (2.0 * A);
  }

  /*  '690 T3 = C0 + E + 1 / 120: ' Round off */
  T3 = C0 + E + 1.0 / 120.0;

  /*  '700 H3 = Int(T3): M3 = Int((T3 - H3) * 60) */
  H3 = floor(T3);
  M3 = floor((T3 - H3) * 60);

  /*  '710 Print USING; "##:##"; H3; M3; */
  * timeBeingSet = H3 * 60.0 + M3;

  /*  '720 H7 = H0 + E * (H2 - H0) */
  H7 = H0 + E * (H2 - H0);

  /*  '730 N7 = -cos(D1) * sin(H7) */
  N7 = -cos(D1) * sin(H7);

  /*  '740 D7 = C * sin(D1) - S * cos(D1) * cos(H7) */
  D7 = C * sin(D1) - S * cos(D1) * cos(H7);

  /*  '750 AZ = Atn(N7 / D7) / DR */
  AZ = atan(N7 / D7) / DR;

  /*  '760 If D7 < 0 Then AZ = AZ + 180 */
  if (D7 < 0.0)
    AZ = AZ + 180.0;

  /*  '770 If AZ < 0 Then AZ = AZ + 360 */
  if (AZ < 0.0)
    AZ = AZ + 360.0;

  /*  '780 If AZ > 360 Then AZ = AZ - 360 */
  if (AZ > 360.0)
    AZ = AZ - 360.0;

  /* '790 Print USING; ",  azimuth ###.#"; AZ */
  /*  '800 Return */
}

static void SpecialMessage () {
  /*  '820 '   Special-message routine */
  /*  '830 If M8 = 0 And W8 = 0 Then GoTo 870 */
  if (M8 || W8) {
    
    /*      '840 If M8 = 0 Then Print M1$ */
    if (! M8)
      sunriseTime = -999.0;	/* no sunrise that day */

    /*      '850 If W8 = 0 Then Print M2$ */
    if (! W8)
      sunsetTime = -999.0;	/* no sunset that day */
    
    /*      '860 GoTo 890 */
  } else {

    /*      '870 If V2 < 0 Then Print M3$ */
    if (V2 < 0) {
      upDown = down;
      sunriseTime = -999.0;
      sunsetTime = -999.0;
	}
    
    /*      '880 If V2 > 0 Then Print M4$ */
    if (V2 > 0) {
      upDown = up;
      sunriseTime = -999.0;
      sunsetTime = -999.0;
	}
  }

  /*  '890 Return */
}

static void FundamentalArgs () {
  double R5;
  double L;
  double G;
  double U;
  double V;
  double W;

  /*  '910 '   Fundamental arguments */
  /*  '920 '     (Van Flandern & */
  /*  '930 '     Pulkkinen, 1979) */
  /*  '940 L = 0.779072 + 0.00273790931 * t */
  L = 0.779072 + 0.00273790931 * T;

  /*  '950 g = 0.993126 + 0.0027377785 * t */
  G = 0.993126 + 0.0027377785 * T;

  /*  '960 L = L - Int(L): g = g - Int(g) */
  L = L - floor(L);
  G = G - floor(G);

  /*  '970 L = L * p2: g = g * p2 */
  L = L * P2;
  G = G * P2;

  /*  '980 V = 0.39785 * sin(L) */
  V = 0.39785 * sin(L);

  /*  '990 V = V - 0.01 * sin(L - g) */
  V = V - 0.01 * sin(L - G);

  /*  '1000 V = V + 0.00333 * sin(L + g) */
  V = V + 0.00333 * sin(L + G);

  /*  '1010 V = V - 0.00021 * tt * sin(L) */
  V = V - 0.00021 * TT * sin(L);

  /*  '1020 U = 1 - 0.03349 * cos(g) */
  U = 1 - 0.03349 * cos(G);

  /*  '1030 U = U - 0.00014 * cos(2 * L) */
  U = U - 0.00014 * cos(2 * L);

  /*  '1040 U = U + 0.00008 * cos(L) */
  U = U + 0.00008 * cos(L);

  /*  '1050 W = -0.0001 - 0.04129 * sin(2 * L) */
  W = -0.0001 - 0.04129 * sin(2 * L);

  /*  '1060 W = W + 0.03211 * sin(g) */
  W = W + 0.03211 * sin(G);

  /*  '1070 W = W + 0.00104 * sin(2 * L - g) */
  W = W + 0.00104 * sin(2 * L - G);

  /*  '1080 W = W - 0.00035 * sin(2 * L + g) */
  W = W - 0.00035 * sin(2 * L + G);

  /*  '1090 W = W - 0.00008 * tt * sin(g) */
  W = W - 0.00008 * TT * sin(G);

  /*  '1110 '    Compute Sun's RA and Dec */
  /*  '1120 s = W / Sqr(U - V * V) */
  S = W / sqrt(U - V * V);

  /*  '1130 A5 = L + Atn(s / Sqr(1 - s * s)) */
  A5 = L + atan(S / sqrt(1 - S * S));

  /*  '1140 s = V / Sqr(U): D5 = Atn(s / Sqr(1 - s * s)) */
  S = V / sqrt(U);
  D5 = atan(S / sqrt(1 - S * S));

  /*  '1150 R5 = 1.00021 * Sqr(U) */
  R5 = 1.00021 * sqrt(U);

  /*  '1160 Return */
}

static void CalendarToJD () {
  int G;
  int D1;
  int A;
  double J3 = 0.0;

  /*  '1170 '     Calendar --> JD */
  /*  '1180 INPUT "Year, Month, Day";Y,M,D */
  /*  '1190 G = 1: If y < 1583 Then G = 0 */
  G = 1;
  if (iYear < 1583)
    G = 0;

  /*  '1200 D1 = Int(D): F = D - D1 - 0.5 */
  D1 = floor(iDay);
  F = iDay - D1 - 0.5;

  /*  '1210 J = -Int(7 * (Int((m + 9) / 12) + y) / 4) */
  J = -floor(7 * (floor((iMonth + 9) / 12) + iYear) / 4);

  /*  '1220 If G = 0 Then GoTo 1260 */
  if (G != 0) {

    /*      '1230 S = Sgn(m - 9): A = Abs(m - 9) */
    S = Sgn(iMonth - 9);
    A = abs(iMonth - 9);
    
    /*      '1240 J3 = Int(y + S * Int(A / 7)) */
    J3 = floor(iYear + S * floor(DA / 7));
    
    J3 = -floor((floor(J3 / 100) + 1) * 3 / 4);
    /*      '1250 J3 = -Int((Int(J3 / 100) + 1) * 3 / 4) */
  }

  /*  '1260 J = J + Int(275 * m / 9) + D1 + G * J3 */
  J = J + floor(275 * iMonth / 9) + D1 + G * J3;

  /*  '1270 J = J + 1721027 + 2 * G + 367 * y */
  J = J + 1721027 + 2 * G + 367 * iYear;

  /*  '1280 If F >= 0 Then GoTo 1300 */
  if (F >= 0)
    return;

  /*  '1290 F = F + 1: J = J - 1 */
  F = F + 1;
  J = J - 1;
}
