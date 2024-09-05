/* Transverse Mercator  prototypes*/

#ifndef _TMFNS_H
#define _TMFNS_H
/*	Snyder, "Map Projections -- a Working Manual" USGS Professional Paper 1395, p47.
	Conformal.
	Cylindrical (transverse).
	Central meridian, each meridian 90 degrees from central meridian, 
	and Equator are straight lines.
	Other meridians and parallels are complex curves.
	Scale is true along central meridian, or along two straight lines 
	equidistant from and parallel to central meridian.   These lines are only 
	approximately straight for the ellipsoid.
	Scale becomes infinite on sphere 90 degrees from central meridian.
	Used extensively for quadrangle maps at scales from 1:24,000  to 1:250,000.
	Presented by Lambert in 1772.
*/

#ifdef  __cplusplus 
extern "C"{
#endif

/* Set the computation buffer */
extern short init_transverse_mercator
	(	void* cnstsP, const char* opt_name, double a, double rf, 
			double phi0, double lam0, double k0 );
extern const void* set_transverse_mercator
	(	const char* opt_name, double a, double rf, double phi0, double lam0, double k0 );
	/*	a, rf:		semi-major axis (meters) and reciprocal flattening.
					Spherical only - if rf != 0.0, the mean radius is used.
		phi0,lam0:	geographic origin of the projection (radians).
		k0:			Scale factor at origin.
		return:		pointer to the constant array.  NULL => error.
	*/
	
/* Geographic to projection (&scale) */
extern int geo_to_transverse_mercator
	( const void* cnsts, double phi, double lam, double* x, double* y );
	/*	cnsts:	pointer to constants created by set_transverse_mercator.. 
		 phi, lam:	latitude & longitude of the input point (radians).
		 x,y:		computed easting and northing (including fn & fe ) (meters)
	*/
	
/* Metric to Geographic */
extern int transverse_mercator_to_geo
	( const void* cnsts, double x, double y, double* phi, double* lam );
	/*	cnsts:	pointer to constants created by set_transverse_mercator.. 
		 x,y:		easting and northing (including fn & fe ) (meters).
		 phi, lam:	computed latitude & longitude (radians).
	*/

/* scale factors */
extern int transverse_mercator_scale
	( const void* cnsts, double phi, double lam, double* h, double* k );
extern int transverse_mercator_limits
	( 	const void* cnsts, double* mnp, double* mnl, double* mxp, double* mxl );

/* UTM - zones require special handling */
extern short init_utm( void* cnstsP, double a, double rf );
extern const void* set_utm( double a, double rf );
extern short utm_to_geo
	( const void* tcnsts, 
	  long zone, double easting,double northing,
	  double* lat, double* lon 
	); 
extern short geo_to_utm
	( const void* tcnsts, 
	  double lat, double lon, 
	  long* izone, double* easting,double* northing );
extern short utm_scale
	( const void* tcnsts, 
	  double lat, double lon, 
	  double* h, double* k 
	);
extern int utm_limits
	( 	const void* cnsts, double* mnp, double* mnl, double* mxp, double* mxl );

#ifdef  __cplusplus 

#include "mgrsfns.h"

}
#endif

#endif /* _TMFNS_H */
