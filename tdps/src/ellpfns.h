#ifndef _ELLPFNS_H 
#define _ELLPFNS_H

#include "cnstnts.h"

/*	Notation
		e = eccentricity
		rf = reciprocal flattening ( 0.0 => sphere )
		phi = geodetic latitude ( radians, [-¹/2,¹/2] )
		lam = geodetic longitude ( radians, [-¹,¹) )
		dlam = difference in longitude ( radians, [-¹,¹) )
		xxcoefs - coefficients for inverse xx latitude
		mdcoefs - coefficients for meridian distance
		B = authalic latitude ( radians, [-¹/2,¹/2] )
		X = conformal latitude ( radians, [-¹/2,¹/2] )
		P = isometric latitude ( radians, [-¹/2,¹/2] )
		R = rectifying latitude ( radians, [-¹/2,¹/2] )
		arc = length of normal section / semi-major axis
		a12 = forward azimuth ( radians east of north, [0,2¹) )
		a21 = back azimuth ( radians east of north, [0,2¹) )
*/
/* All distances are scaled.  Multiply them by the semi-major axis to
   get the true distances.  
*/

#ifdef  __cplusplus 
extern "C"{
#endif

/* Ellipsoid attributes	 */
/* Second Eccentricity squared	 */
extern double eccentricityP2( double e );
#define second_ecc2		eccentricityP2						
/* First Eccentricity squared  */
extern double eccentricity2( double rf );
/* (a-b)/(a+b)	  */
extern double constN( double e );
/* Mean Radius	  */
extern double mean_radius( double e );
/* pow(1+e,1+e)*pow(1-e,1-e) */
extern double pow_EPM( double e );

/* Ellipsoid position functions */
/* Auxiliary latitudes			*/
/* Authalic latitude			 */
extern void   set_inverse_authalic( double e, double* aucoefs );
extern void authalic_lat( double e, double phi, double sphi, double* B, double* sinB ); 	
extern void inverse_authalic_lat( double  B, double sinB, const double* aucoefs, double* phi ); 
extern double authalic_q( double e, double sphi ); 
	
/* Conformal latitude */
extern void   set_inverse_conformal( double e, double* cfcoefs );
extern void conformal_lat( double e, double phi, double sphi, double* X  ); 
extern void inverse_conformal_lat( double X, double sinX, const double* cfcoefs, double* phi ); 
extern double tanHZ( double e, double sphi );
extern double tanz( double ecc, double phi, double sphi );

/* Rectifying latitude */
extern void   set_inverse_rectifying( double e, double* mdcoefs );
extern void rectifying_lat( double phi, double sphi, const double* mdcoefs, double* R ); 
extern void inverse_rectifying_lat( double R, double sinR, const double* mdcoefs, double* phi ); 

/* Distances on spheroid (scaled)- multiply by semi-major axis	*/
extern void   set_meridian_distance( double e, double* mdcoefs );
extern double meridian_distance( double phi, double sphi, const double* mdcoefs ); 
extern double parallel_distance( double e, double sphi, double dlam ); 
/*
extern short nsection_terminus
	( double e, double phi1,double lam1, double Arc, double a12, 
	  double* phi2, double* lam2, double* a21 );
extern double nsection_distance
	( double e, double phi1,double lam1, 
	  double phi12,double lam2,
	  double* a12, double* a21 );
*/
							   		
/* Radii (scaled) - multiply by semi-major axis	 */
/* Radius of curvature of parallel @ phi   */
extern double parallel_radius( double e, double sphi );
/* Radius  of curvature of meridian @ phi	 */
extern double meridian_radius( double e, double sphi );
/* Radius  of curvature of prime vertical @ phi	 */
extern double vertical_radius( double e, double sphi );
/* RMS radius @ phi	  */
extern double RMSR( double e, double sphi );
		
#ifdef  __cplusplus 
}
#endif

#endif /* _ELLPFNS_H */

