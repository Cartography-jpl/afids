/* returns -1 if reason for sunrise and sunset being -999 was that the
   sun was down all day returns 1 if reason for sunrise and sunset
   being -999 was that the sun was down all day
*/
int calcSunupDown (double lat, double lon, /* degrees */
		    int year, int month, int day,
		    double * sunrise, double * sunset); /* UTC minutes */

void logDayMonthTimeFlag (char * metaFileName, int day, int month, unsigned short UTCStartYear,
			  double sunLat, double sunLon, double riseOff, double setOff, int utcTimeInMinutes,
			  int unitCount, int * vunits, int echoMeta);

void logDayTimeFlag (char * metaFileName, unsigned short UTCStartDay, unsigned short UTCStartYear,
		     double sunLat, double sunLon, double riseOff, double setOff, int utcTimeInMinutes,
		     int unitCount, int * vunits);
