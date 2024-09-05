/* Projection utilities   */

#include "ellpfns.h"

/* utilities for trig series expansions */
static void _convertSin2468( double* u )
{   u[0] -= u[2];
    u[1] = 2.0*u[1] - 4.0*u[3];
    u[2] *= 4.0;
    u[3] *= 8.0;
    return;
}
static double _fSin2468( double sphi, const double* u )
{   double cos2x = 1.0 - 2.0*sphi*sphi;
	double sin2x = 2.0* sphi * sqrt( 1.0 - sphi*sphi );
	return sin2x*( u[0] + cos2x*( u[1] + cos2x*( u[2] + cos2x*u[3] )));  
}

/* Ellipsoid attributes	  */
/*	Second Eccentricity squared */
	double eccentricityP2( double e )	{  double e2 = e*e;  return e2/(1.0-e2);  }						
	/* (a-b)/(a+b) */
	double constN( double e )	{  double v = sqrt(1.0-e*e); return ( 1.0-v )/( 1.0+v);  }
	/* Mean Radius */
	double mean_radius( double e )	{  return (2.0 + sqrt( 1.0 - e*e ) )/3.0;  }
	/* Eccentricity squared from reciprocal flattening	*/
	double eccentricity2( double rf )	
	{	double f=( rf > EPSILON ? 1.0/rf : 0.0 ); return f*(2.0-f);  }	
	/* pow(1+e,1+e)*pow(1-e,1-e)  */
	double pow_EPM( double e )
	{	return pow(1.0+e,1.0+e) * pow( 1.0-e,1.0-e);  }
		
/* Auxilliary latitudes	 */
	/* Authalic latitude   */
	double authalic_q( double e, double sphi ) 
	{	if( e < EPSILON )
			return sphi;
		else
		{	double es = e*sphi; 
			return sphi/( 1.0 - es*es ) - log( (1.0-es)/(1.0+es) )/(e+e);
		} 
	}

	void set_inverse_authalic( double e, double* aucoefs )
	{   if( e < EPSILON )
			aucoefs[0] = 0.0;
		else
		{	double e2 = e*e;
			double e4 = e2*e2;
    		aucoefs[0] = e2*( (1.0/3.0) + e2*( (31.0/180.0) + e2*(517.0/5040.0) ));      
    		aucoefs[1] = e4*( (23.0/360.0) + e2*(251.0/3780.0) );
    		aucoefs[2] = e4*e2*(761.0/45360.0);
    		aucoefs[3] = 0.0; 
    		_convertSin2468(aucoefs);
    	}
   		 return;
	}
	void authalic_lat( double e, double phi, double sphi, double* B, double* sinB ) 	
	{	if( e <= 0.0 )
		{	if(B) *B = phi;
			if(sinB) *sinB = sphi;
		}
		else
		{	double q = authalic_q(e,sphi);
			double q90 = authalic_q(e,1.0);
			double sinv = q/q90;
			if(sinB) *sinB = sinv;
			if(B) *B = asin(sinv);
		}
		return;
	}
	void inverse_authalic_lat( double B, double sinB, const double* aucoefs, double* phi ) 
	{	if( aucoefs[0] == 0.0 )
			*phi = B;
		else
			*phi = B +  _fSin2468( sinB, aucoefs );
		return;
	}
	
	/*Conformal latitude	*/
	void set_inverse_conformal( double e, double* cfcoefs )
	{   if( e < EPSILON )
			cfcoefs[0] = 0.0;
		else
		{	double e2 = e*e;
			double e4 = e2*e2;
  	  		cfcoefs[0] = e2*( (1.0/2.0) + e2*( (5.0/24.0) + e2*(1.0/12.0) + e2*(13.0/360.0)));      
    		cfcoefs[1] = e4*( (7.0/48.0) + e2*(29.0/240.0) + e2*(811.0/11520.0));
    		cfcoefs[2] = e4*e2*(7.0/120.0 + e2*(81.0/1120.0));
    		cfcoefs[3] = e4*e4*(4279.0/161280.0); 
    		_convertSin2468(cfcoefs);
    	}             
    	return;
	}
	void conformal_lat( double e, double phi, double sphi, double* X  ) 
	{	if( e < EPSILON )
			*X = phi;
		else
		{	double esp = e*sphi;
			double v1 = ( 1.0 + sphi ) / ( 1.0 - sphi );
			double v2 = ( 1.0 - esp ) / ( 1.0 + esp );
			double hz = sqrt(v1*pow( v2,e ));
			*X = 2.0*atan(hz) - HALF_PI;
		}   
		return;
	}	
	void inverse_conformal_lat( double X, double sinX, const double* cfcoefs, double* phi ) 
	{	if( cfcoefs[0] == 0.0 )
			*phi = X;
		else
			*phi = X + _fSin2468( sinX, cfcoefs );
		return ;
	}
	  
	double tanHZ( double e, double sphi )
	{	double esp = e*sphi;
		double v1 = ( 1.0 + sphi ) / ( 1.0 - sphi );
		double v2 = ( 1.0 - esp ) / ( 1.0 + esp );
		return sqrt(v1*pow( v2,e ));
	}
	double tanz( double ecc, double phi, double sphi )
	{	double f = ( 1 - ecc*sphi)/( 1 + ecc*sphi );
		double z = tan( 0.5*(HALF_PI-phi) )/pow( f, 0.5*ecc );
		return z;
	} 


	/* Rectifying latitude	*/
	void set_inverse_rectifying( double e, double* fpcoefs )
	{	if( e < EPSILON )
			fpcoefs[0] = 0.0;
		else
		{	double n = constN(e);
			double n2 = n*n;
			fpcoefs[0] = n*(3.0/2.0 - n2*27.0/32.0);
			fpcoefs[1] = n2*( 21.0/16.0 -n2*55.0/32.0 );
			fpcoefs[2] = n2*n*151.0/96.0;
			fpcoefs[3] = n2*n2*1097.0/512.0;
   	 		_convertSin2468(fpcoefs); 
   	 	}            
		return;
	}			
	void rectifying_lat( double phi, double sphi, const double* mdcoefs, double* R ) 
	{	if( mdcoefs[0] == 0.0 )
			*R = phi;
		else
			*R = HALF_PI*meridian_distance(phi,sphi,mdcoefs) / 
						meridian_distance(HALF_PI,1.0, mdcoefs);
		return;  
	}
	void inverse_rectifying_lat( double R, double sinR, const double* fpcoefs, double* phi ) 
	{	if( fpcoefs[0] == 0.0 )
			*phi = R;
		else
			*phi = R + _fSin2468( sinR, fpcoefs );  
		return;
	} 

/* Distances on spheroid (scaled- multiply by semi-major axis) */
	/* Meridian distance ( 0 to phi ) */
	void set_meridian_distance( double e, double* mdcoefs ) 
	{   if( e < EPSILON )
			mdcoefs[0] = 1.0;
		else
		{	double e2 = e*e;
			double e4 = e2*e2;
			mdcoefs[0] = 1.0 - e2*( 1.0/4.0 + e2*( 3.0/64.0 + e2*(5.0/256.0) ));
    		mdcoefs[1] =  -e2*( (3.0/8.0) + e2*( (3.0/32.0) + e2*(45.0/1024.0) ));
    		mdcoefs[2] = e4*( (15.0/256.0) + e2*(45.0/1024.0) );
    		mdcoefs[3] = -e4*e2*(35.0/3072);
    		mdcoefs[4] = 0.0; 
    		_convertSin2468(mdcoefs+1);
    	}             
    	return;
	}
	double meridian_distance( double phi, double sphi, const double* mdcoefs ) 
	{	if( mdcoefs[0] == 1.0 )
			return phi;
		else
			return phi*mdcoefs[0] + _fSin2468(sphi, mdcoefs+1); 
	}
	
	/* Distance on a parallel circle */
	double parallel_distance( double e, double sphi, double dlam ) 
		{	return dlam*parallel_radius(e,sphi);  }


/* Radii (scaled) - multiply by semi-major axis	*/
	/* Radius of curvature of parallel @ phi */
	double parallel_radius( double e, double sphi )  
		{	double esp = sphi*e;
			return sqrt( (1.0-sphi*sphi)/(1.0- esp*esp) );
		}
	/* Radius  of curvature of meridian @ phi	 */
	double meridian_radius( double e, double sphi )  
		{	double v = vertical_radius(e,sphi); 
			return (1.0-e*e)*(v*v*v);
		}
	/* Radius  of curvature of prime vertical @ phi	*/
	double vertical_radius( double e, double sphi )
		{	double esp = e*sphi;
			return 1.0/sqrt(1.0-esp*esp);  
		}
	/* RMS radius	 */
	double RMSR( double e, double sphi )	
		{  return sqrt( meridian_radius(e,sphi)*vertical_radius(e,sphi) );  }
			
#if 0 /* nsection not currently used */
	/* Distance(arc) and azimuth functions for normal section -- Robbins ( 10mm @ 1600km ) */
	static double fs( double z, double g, double h, int sgn )
	{	int sgn1 = -sgn;
		double gh = g*h, z2 = z*z, h2 = h*h;
		double t1 = sgn*h2*(1.0-h2)/6.0;
		double t2 = sgn1*gh*(1.0-2.0*h2)/8.0;
		double t3 = sgn1*h2*(4.0-7.0*h2) - 3.0*g*g*(1.0-7.0*h2)/120.0;
		double t4 = sgn*gh/48.0;
		return z*(1.0 + z2*(  t1 + z*( t2 + z*( t3 + z*t4 ))));
	}	 
	/* Terminus given origin, azimuth and distance (arc) */.
	short nsection_terminus
		( double e, double phi1,double lam1, double arc, double a12, 
		  angle* phi2,angle* lam2, angle* a21 )
	{	double mu;
		angle dlam, eta, sigma;
		double ep2 = second_ecc2(e), ep =sqrt(ep2);
		double sphi1 = sinx(phi1), cphi1 = cosx(phi1);
		double sa12 = sinx(a12), ca12 = cosx(a12);
		double g = ep*sphi1, h =ep*cphi1*ca12, neta = arc*verticalK( e,phi1 );
		set_angle( &sigma, fs( neta, g,h, 1 ));
		set_sin( &eta, sphi1*cosx(&sigma) + cphi1*ca12*sinx(&sigma) );
		set_sin(&dlam, sinx(&sigma)*sa12/cosx(&eta));
		mu = 1.0 + 0.5*ep2*(sinx(&eta)-sphi1)*(sinx(&eta)-sphi1);
		set_tan( phi2, (1.0+ep2)*tanx(&eta)*(1.0-e*e*mu*sphi1/sinx(&eta)) );
		set_angle( lam2, radians(lam1) + radians(&dlam) );
		if( a21 )
			set_angle( a21, asin(-cphi1*sa12/cosx(&eta)) - 
							(radians(phi2) - radians(&eta))*sa12*tanHx(&sigma) );
		return OK;
	}
	static void fazm( double e, double phi1, double phi2, double dlam,
					  angle* eta, angle* a12 )
	{	double t, e2 = e*e;
		double f1 = sinx(phi1)*vertical_radius(e,phi1);
		double f2 = sinx(phi2)*vertical_radius(e,phi2);
		set_tan( eta, (1.0-e2)*tanx(phi2) + e2*f1/f2 );
		t = cosx(phi1)*tanx(eta) - cosx(phi1)*cosx(dlam);
		set_tan2( a12, sinx(dlam),t);
		return;
	}
	/* Azimuth Aand distance(arc) between two points; */
	double nsection_distance
		( double e, double phi1,double  lam1, 
		  double phi2,double lam2,
		  angle* a12, angle* a21 )
	{	double X, g,h,s, ep = second_ecc2(e);
		angle eta, sigma, dlam, ta12;
		if( !a12 ) a12 = &ta12;
		set_angle( &dlam, radians(lam2)-radians(lam1));
		fazm( e,phi1,phi2,&dlam, &eta, a12 );
		X = sinx(&dlam)/sinx(a12);
		set_sin(&sigma, X*cosx(&eta) );
		g = ep*sinx(phi1);
		h = ep*cosx(phi1)*cosx(a12);
		s = vertical_radius(e,phi1)*fs( radians(&sigma), g,h, -1 );
		if( a21 )
		{	set_angle( &dlam, radians(lam1)-radians(lam2));
			fazm( e,phi2,phi1,&dlam, &eta, a21 );
		}
		return s;
	}
#endif /* not used */	
