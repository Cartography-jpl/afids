/* Polar Stereographic Projection prototypes psfns.h*/
/* 	Snyder, "Map Projections -- a Working Manual" USGS Professional Paper 1395, p154
	Azimuthal.
	Conformal.
	The central meridian and a particular parallel (if shown) are stright lines.
	All meridians are straight lines.
	All other meridians and parallels are shown as arcs of circles.
	Perspective for the sphere.
	Directions from the center of the projection are true.
	Scale increases away from the center.
	Point opposite the center of the projection cannot be plotted.
	Used for polar maps and miscellaneous special maps.
	Apparently invented by Hipparchus (2nd century B.C. ).
*/

#ifndef _PS_H
#define _PS_H

#include "cnstnts.h"

#ifdef  __cplusplus 
extern "C"{
#endif

extern short  init_polar_stereographic
	( void* cnstsP, const char* ename, double a, double rf,
		double lat0, double lon0, double scale  );
extern const void*  set_polar_stereographic
	( const char* ename, double a, double rf,
		double lat0, double lon0, double scale  );

extern short polar_stereographic_to_geo
	( const void*  tcnsts, 
	  double northing,double easting, 
	  double* lat, double* lon ); 

extern short geo_to_polar_stereographic
	( const void*  tcnsts, 
	  double lat, double lon, 
	  double* northing,double* easting );

extern short polar_stereographic_scale
	( const void*  tcnsts, 
	  double lat, double lon, 
	  double* h, double* k );

extern short polar_stereographic_parameters
	( const void*  tcnsts, char* pname, char* ename,
	  double* axis, double* rf,
	  double* lat0, double* lon0, double* k0 ); 
extern int polar_stereographic_limits
	( 	const void* cnsts, double* mnp, double* mnl, double* mxp, double* mxl );

/* UPS - north & south */
extern const void* set_ups( double a, double rf, double lat0 );
extern short init_ups( void* cnstsP, double a, double rf, double lat  );
extern short ups_to_geo
	( const void* tcnsts, 
	  double northing, double easting,
	  double* lat, double* lon 
	);
extern short geo_to_ups
	( const void* tcnsts, 
	  double lat, double lon, 
	  double* northing,double* easting 
	);
extern short ups_scale
	( const void* tcnsts, 
	  double lat, double lon, 
	  double* h, double* k 
	);
extern int ups_limits
	( 	const void* cnsts, double* mnp, double* mnl, double* mxp, double* mxl );
  
#ifdef  __cplusplus 
}
#endif

#endif /* _PSFNS_H */

