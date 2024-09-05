#include <math.h>
#include "distanceBetween2Points.h"

double distanceBetween2Points(double lat1, double lon1,
                              double lat2, double lon2) {

// From http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1
// Presuming a spherical Earth with radius R, and 
// the locations of the two points in spherical coordinates (longitude 
// and latitude) are lon1,lat1 and lon2,lat2 then the 
// Haversine Formula gives the distance between the two points.
// Radius of Earth is 6367 km.

const double R = 6367.0;  // Result will be in units of km.
const double pi = acos(-1.0);

// Convert to radians.
lat1 *= pi/180.0;
lon1 *= pi/180.0;
lat2 *= pi/180.0;
lon2 *= pi/180.0;

double dlon = lon2 - lon1;
double dlat = lat2 - lat1;

double x1 = sin(dlat/2.0);
double x2 = sin(dlon/2.0);

double a = x1*x1 + cos(lat1) * cos(lat2) * x2*x2;
double c = 2.0 * atan2( sqrt(a), sqrt(1.0-a) );
double d = R * c;
return d;
// Result is in km
}

