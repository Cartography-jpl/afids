/* Military Grid Reference System prototypes*/

#ifndef _MGRSFNS_H
#define _MGRSFNS_H
	
#ifdef  __cplusplus 
extern "C"{
#endif

/* Set the computation constants.  This is a completely defined grid. */
extern short init_mgrs( void* cnsts, double a, double rf );
/* Allocate and set the computation constants.  This is a completely defined grid. */
extern const void* set_mgrs( double a, double rf );
	/*	a, rf:		semi-major axis (meters) and reciprocal flattening.
					Spherical only - if rf != 0.0, the radius = axis.
	*/
/* Geographic to projection  */
extern short geo_to_mgrs
	( const void* tcnsts, double lat, double lon, char* mgrs,
		long* zone, double* x, double* y ); 
	/*	cnsts:		pointer to constants created by set_mgrs.
		lat, lon:	latitude & longitude (degrees).
		mgrs:		encoded mgrs label. 
		izone:		UTM zone number.
		x,y:		UTM easting and northing (including fn & fe ) (meters).
	*/

/* Geographic to mgrs */
extern short mgrs_to_geo
	( const void* tcnsts, const char* mgrs, double* lat, 
		double* lon, long* izone, double* x, double* y );
	/*	cnsts:		pointer to constants created by set_mgrs.
		mgrs:		encoded mgrs label. 
		lat, lon:	computed latitude & longitude (degrees).
		izone:		UTM zone number.
		x,y:		UTM easting and northing (including fn & fe ) (meters).
	*/

extern short  utm_to_mgrs
	( const void* tcnsts, long zone, double x, double y,
		char* mgrs, double* lat, double* lon );
extern int mgrs_limits
	( 	const void* cnsts, double* mnp, double* mnl, double* mxp, double* mxl );

extern void free_mgrs( const void* mgrsP );

#ifdef  __cplusplus 
}
#endif

#endif /* _MGRSFNS_H */

