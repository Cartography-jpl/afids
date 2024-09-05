/* Transverse Mercator functions */
#include "projfns.h"
#include "tmfns.h"
#include "projdfn.h"
#include <stdlib.h>

static double poly( double x, int n, double* c )
{	double p = 0.0;
	while( n >= 0 ) p = p*x + c[n--];
	return p;
}

#define NC				(13)

#define EP2				cnsts->other[1]
#define M0				cnsts->other[2]
#define KA				cnsts->other[3]
#define MDCFS			(cnsts->other+4)
#define RCCFS			(cnsts->other+9)
#define LABEL			"Transverse Mercator"

int transverse_mercator_limits
	(	const void* cnstsP, double* mnlat, double* mnlon, double* mxlat, double* mxlon )
	{	if( !cnstsP || !mnlat || !mxlat || !mnlon || !mxlon)
			return -1;
		else
		{	const proj_dfn* cnsts = (proj_dfn*)cnstsP;
			*mnlat = SLIM; *mxlat = NLIM; 
			*mnlon = WLIM; *mxlon = ELIM;
			return 0;
		}
	}
	 
short init_transverse_mercator
	(	void* cnstsP, const char* opt_name, double a, double rf, 
			double lat0, double lon0, double k0 )
	{	if( !cnstsP )
			return -1;
		else
		{	proj_dfn* cnsts = (proj_dfn*)cnstsP;
			_init_std_pars
				( cnsts, NC, (opt_name ? opt_name : LABEL), a,rf, lat0,lon0 );
			K0 = k0; EP2 = ECC2/(1.0-ECC2);
			KA = K0*AXIS; 
	    	/* Meridian distance coefficients */
	    	set_meridian_distance( ECC, MDCFS );
			/* Inverse rectifying latitude coefficients */
			set_inverse_rectifying( ECC, RCCFS );
			M0 = meridian_distance( P0, SINP0, MDCFS )*K0; 
			SLIM = -89.95;
			WLIM = check_180(lon0 - 30.0);
			NLIM = 89.95;
			ELIM = check_180(lon0 + 30.0);
			return 0;
		}
	}

const void* set_transverse_mercator
	(	const char* opt_name, double a, double rf, double lat0, double lon0, double k0 )
	{	void* cnsts = allocate_memory( sizeof(proj_dfn) );
		init_transverse_mercator
			( cnsts, (opt_name ? opt_name : LABEL), a, rf, lat0, lon0,k0 );
		return cnsts;
	}

int geo_to_transverse_mercator
	( const void* cnstsP, double lat, double lon, double* x, double* y )
	{	int status = 0;
		if( !cnstsP  )
			return -1;
		else
		if( (status = projection_limit_check(cnstsP, lat,lon)) < 0 )
			return -2;
		else
		{	double cfs[5],T2,T3,T4,T5;
			const proj_dfn *cnsts = (const proj_dfn*)cnstsP;
			double rphi = lat*RADDEG, rlam = lon*RADDEG, dlam = rlam - L0;
			double dlam2 = dlam*dlam;
			double sphi = sin(rphi), cphi = cos(rphi), tphi = tan(rphi);
			double T = tphi*tphi, G = EP2*cphi*cphi, cp2 = cphi*cphi;
			double N = AXIS*K0*vertical_radius(ECC,sphi);
			double Q = 0.5*sphi*cphi*N;
			double T1 = meridian_distance( rphi,sphi, MDCFS )*K0*AXIS;
			
			T2 = Q;
			
			Q *= (cp2/12.0);
			cfs[0] = 5.0-T; cfs[1] = 9.0; cfs[2] = 4.0;
			T3 = Q*poly(G,2,cfs);

			Q *= (cp2/30.0);
			cfs[0] = 61.0; cfs[1] = -58.0; cfs[2] = 1.0;
			cfs[0] = poly(T,2,cfs);
			cfs[1] = 270.0 - 330.0*T;
			cfs[2] = 445.0 - 680.0*T;
			cfs[3] = 324.0 - 600.0*T;
			cfs[4] =  88.0 - 192.0*T;
			T4 = Q*poly( G,4,cfs );

			Q *= (cp2/56.0);
			cfs[0] = 1385.0; cfs[1] = -3111.0; cfs[2] = 543.0; cfs[3] = -1.0;
			T5 = Q*poly(T,3,cfs);

			cfs[0] = T1; cfs[1] = T2; cfs[2] = T3; cfs[3] = T4; cfs[4] = T5;
			*y = poly( dlam2, 4, cfs ) - M0*AXIS; 

			Q = N*cphi;
			T1 = Q;

			Q *= (cp2/6.0);
			T2 = Q*(1.0 - T + G);

			Q *= (cp2/20.0);
			cfs[0] = 5.0; cfs[1] = -18.0; cfs[2] = 1.0;
			cfs[0] = poly(T,2,cfs);
			cfs[1] = 14.0 - 58.0*T;
			cfs[2] = 13.0 - 64.0*T;
			cfs[3] = 4.0 - 24.0*T;
			T3 = Q*poly( G,3,cfs );

			Q *= (cp2/42.0);
			cfs[0] = 61.0; cfs[1] = -479.0; cfs[2] = 179.0; cfs[3] = -1.0;
			T4 = Q*poly(T,3,cfs);

			cfs[0] = T1; cfs[1] = T2; cfs[2] = T3; cfs[3] = T4;
			*x = dlam*poly( dlam2,3,cfs );

			return status;
		}
	}
	
int transverse_mercator_to_geo
	( const void* cnstsP, double x, double y, double* lat, double* lon )
	{	const proj_dfn* cnsts = (const proj_dfn*)cnstsP;
		double phi1,sphi1,cphi1,tphi1, lam1, T,G,Q,K2, cfs[5], T1,T2,T3,T4, rho,v;
		double yy = y/AXIS, x2 = x*x;
		double mu = (M0 + yy)/K0/MDCFS[0];
		inverse_rectifying_lat( mu,sin(mu), RCCFS, &phi1 );
		sincostan( phi1, &sphi1, &cphi1, &tphi1 );
		T = tphi1*tphi1;
		rho = 1.0/(1.0-ECC2*sphi1*sphi1);
		rho *= AXIS*(1.0-ECC2)*sqrt(rho);
		G = EP2 * cphi1*cphi1;
		v = rho*(1.0+G);
		K2 = K0*K0;
		Q = tphi1/(2.0*rho*v*K2);
		K2 *= (v*v);

		T1 = Q;
		Q /= (12.0*K2);
		cfs[0] = 5.0 + 3.0*T; cfs[1] = 1.0 - 9.0*T; cfs[2] = -4.0;
		T2 = Q*poly(G,2,cfs);

		Q /= (30.0*K2);
		cfs[0] = 61.0; cfs[1] = 90.0; cfs[2] = 45.0;
		cfs[0] = poly(T,2,cfs);
		cfs[1] = 46.0; cfs[2] = -252.0; cfs[3] = -90.0;
		cfs[1] = poly( T,2,cfs+1 );
		cfs[2] = -3.0; cfs[3] = -66.0; cfs[4] = 225.0;
		cfs[2] = poly( T,2,cfs+2 );
		cfs[3] = 100.0 + 84.0*T;
		cfs[4] = 88.0 - 192.0*T;
		T3 = Q*poly( G,4,cfs );

		Q /= (56.0*K2);
		cfs[0] = 1385.0; cfs[1] = 3633.0; cfs[2] = 4095.0; cfs[3] = 1575.0;
		T4 = Q*poly( T,3,cfs);

		cfs[0] = -T1; cfs[1] = T2; cfs[2] = -T3; cfs[3] = T4;
		phi1 += x2*poly( x2,3,cfs ); 

		Q = 1.0/(v*cphi1*K0);
		T1 = Q;

		Q /= (6.0*K2);

		T2 = Q*(1.0 + 2.0*T + G );

		Q /= (20.0*K2);
		cfs[0] = 5.0; cfs[1] = 28.0; cfs[2] = 24.0;
		cfs[0] = poly(T,2,cfs);
		cfs[1] = 6.0 + 8.0*T;
		cfs[2] = -3.0 + 4.0*T;
		cfs[3] = -4.0 + 24.0*T;
		T3 = Q* poly(G,3,cfs);

		Q /= (42.0*K2);
		cfs[0] = 61.0; cfs[1] = 662.0; cfs[2] = 1320.0; cfs[3] = 720.0;
		T4 = Q*poly(T,3,cfs);

 		cfs[0] = T1; cfs[1] = -T2; cfs[2] = T3; cfs[3] = -T4;
		lam1 = L0 + x*poly(x2,3,cfs);


		*lon = check_PI(lam1)/RADDEG;
		*lat = phi1/RADDEG;  
		return projection_limit_check( cnstsP, *lat,*lon );
	}

int transverse_mercator_scale
	( const void* cnstsP, double lat, double lon, double* h, double* k )
	{	int status = 0;
		if( !cnstsP  )
			return -1;
		else
		if( (status = projection_limit_check(cnstsP, lat,lon)) < 0 )
			return -2;
		else
		{	const proj_dfn *cnsts = (const proj_dfn*)cnstsP;
			double rphi = lat*RADDEG, rlam = lon*RADDEG;
			double s,T,N,C,A,A2, sphi,cphi,tphi;
			sincostan( rphi, &sphi, &cphi, &tphi );
			T = tphi*tphi;
			N = vertical_radius( ECC, sphi );   
			C = EP2 * cphi*cphi;
			A = check_PI(rlam-L0)*cphi;
			A2 = A*A;
			s = K0*( 1.0 + A2*( (1.0+C)/2.0 + 
						   A2*( (5.0-4.0*T + C*(42.0+13.0*C) - 28.0*EP2)/24.0 +
						   A2*(61.0-T*(148.0-16.0*T))/720.0 )));
			*k = *h = s;
			return status;
		}
	}

/* UTM functions */
int utm_limits
	(	const void* cnstsP, double* mnlat, double* mnlon, double* mxlat, double* mxlon )
	{	if( !mnlat || !mxlat || !mnlon || !mxlon)
			return -1;
		else
		{	*mnlat = -80.0; *mxlat = 84.0; 
			*mnlon = -180.0; *mxlon = 180.0;
			return 0;
		}
	}
	 
short init_utm( void* cnstsP, double a, double rf )
	{	double k0 = 0.9996;
		double lat0 = 0.0, lon0 = 0.0;
		return init_transverse_mercator
			( cnstsP, "Universal Transverse Mercator", a,rf, lat0,lon0, k0 );
	}
const void* set_utm( double a, double rf )
	{	void* cnsts = allocate_memory( sizeof(proj_dfn) );
		init_utm( cnsts, a,rf );
		return cnsts;
	}
	
#define FN	(0.0)
#define FS	(10000000.0)
#define FE	(500000.0)
short utm_to_geo
	( const void* tcnsts, long zone, double easting, double northing, 
		double* lat, double* lon )
	{	const proj_dfn* cnsts = (proj_dfn*)tcnsts;
		double x = easting - FE;
		/* double y = ( northing < 0.0 ? (northing+FS) : northing ); */
		double y = ( northing < 0.0 ? fabs(northing)-FS : northing );
		short status = transverse_mercator_to_geo( cnsts, x,y, lat,lon );
		*lon += (zone*6-183);
		return status;
	}
short geo_to_utm
	( const void* tcnsts, double lat, double lon, 
		long* izone, double* east, double* north )
	{	const proj_dfn* cnsts = (proj_dfn*)tcnsts;	
		short status;
		long zone = *izone;
		if( zone <= 0 || zone > 60 )	/* if a zone is provided use it, otherwise compute it. */
		{	zone = (long)(31.0 + lon / 6.0);
			if(zone >= 61) zone = 60;	
    		if(zone <= 0) zone = 1;
    		*izone = zone;
		}
    	lon -= (zone*6-183);		/* Change the longitude to offset */
		status = geo_to_transverse_mercator( cnsts, lat,lon, east,north );
		*east += FE;
		if( *north < 0.0 ) *north = -(*north + FS);
		return status;
	}

short utm_scale
	( const void* tcnsts, double lat, double lon, double* h, double* k )
	{	short zone = (short)(31.0 + lon / 6.0);
		const proj_dfn* cnsts = (proj_dfn*)tcnsts;	
		lon -= (zone*6-183);		/* Change the longitude to offset */
		return (short)transverse_mercator_scale( cnsts, lat,lon, h,k );
	}
	
#if 0 /* test transverse_mercator */
#include "stdio.h"

int main()
{	int i;
	double x,y, backp,backl, h,k, rho,theta;
	const void *cnsts;
	FILE* fp =stdout;
	char pname[64], *name[2] = { "Clarke 1866", "Unit Sphere" };
	double a[2] = { 6378206.4, 1.0 }, rf[2] = { 294.9786982,0.0 };
	double lat0 = 0.0, lon0 = -75.0;
	double k0[2] = { 0.9996, 1.0 };
	double lat[] = { 40.5, 40.5}, lon[] = { -73.5, -73.5 };
	
	for( i=0; i<2; i++ )
	{	cnsts = set_transverse_mercator( NULL, a[i],rf[i], lat0, lon0, k0[i] );
		if( !cnsts )
		{	fprintf( fp, "MEM FAILURE\n" );
			return 1;
		}
		print_projection( cnsts, fp );
		fprintf( fp, "\tStart: %lf %lf\n", lat[i],lon[i] );
		geo_to_transverse_mercator( cnsts, lat[i], lon[i], &x, &y );
		fprintf( fp, "\ttransverse_mercator: %lf %lf\n", x,y );
		transverse_mercator_to_geo( cnsts, x,y, &backp, &backl );
		fprintf( fp, "\tBack: %lf[%lf],%lf[%lf]\n", 
			backp,(backp - lat[i])*3600.0,
			backl,(backl - lon[i])*3600.0 );
		transverse_mercator_scale( cnsts, lat[i], lon[i], &h, &k );
		fprintf( fp, "\tScales: %lf %lf\n", h,k );
		free_projection(cnsts);
	}
	return 0;
}	
#endif /* test transverse_mercator */

#if 0
#include "stdiofns.h"

int main()
{	const void* prjP;
	double lat,lon, north,east, blat,blon;
	long zone = -1;
	/* Default is Clarke 1866 */
	double a = 6378206.4, rf = 294.9786982;
	double lat0 = 0.0,lon0 = -75.0, k0 = 0.9996;
	int n,k = 0;
	
	if( (k=proj_pars( &a, &rf, &lat0, &lon0, &k0, NULL, NULL,NULL )) == 5 )
	{	prjP = set_transverse_mercator( NULL, a,rf, lat0,lon0, k0 );
		if( prjP == NULL ) 
		{	printf( "ERROR: INITIALIZATION FAILED.\n");
			return 2;
		}
	}
	else
	{	printf( "Projection not defined: %d.\n",k );
		return 1;
	}
	print_projection( prjP,stdout ); printf("\n");
	print_test_hdr();
	
	n=0;
	while( proj_test_points( n, lat0,lon0, &lat, &lon ) >= 0 )
	{	if( (k=geo_to_transverse_mercator( prjP, lat,lon, &east, &north )) == 0 )
		{	if( (k=transverse_mercator_to_geo( prjP, east, north, &blat,&blon )) == 0 )
				print_test_data( lat,lon, zone,east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf %lf\n", k,blat,blon );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k, lat,lon );
		n++;		
	}
	while( proj_data( &lat,&lon ) == 2 )
	{	if( (k=geo_to_transverse_mercator( prjP, lat,lon, &east, &north )) == 0 )
		{	if( (k=transverse_mercator_to_geo( prjP, east, north, &blat,&blon )) >= 0 )
				print_test_data( lat,lon, zone,east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO\n", k );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION\n", k );		
	}
	free_projection(prjP);
	return 0;
}

#endif
 
#if 0
#include "stdiofns.h"

int main()
{	const void* prjP;
	long zone;
	double lat,lon, north,east, blat,blon;
	/* Default is WGS84 */
	double a = 6378137.000,	 rf = 298.257223563;
	double lat0 = 0.0,lon0 = 0.0, k0 = 0.9996;
	int n,k = 0;
	
	if( (k=proj_pars( &a, &rf, NULL, NULL, NULL, NULL, NULL,NULL )) == 2 )
	{	prjP = set_utm( a,rf );
		if( prjP == NULL ) 
		{	printf( "ERROR: INITIALIZATION FAILED.\n");
			return 2;
		}
	}
	else
	{	printf( "Projection not defined: %d.\n",k );
		return 1;
	}
	print_projection( prjP,stdout ); printf("\n");
	print_test_hdr();
	
	n=0;
	while( proj_test_points( n, lat0,lon0, &lat, &lon ) >= 0 )
	{	zone = -1;
		if( (k=geo_to_utm( prjP, lat,lon, &zone, &east, &north )) == 0 )
		{	if( (k=utm_to_geo( prjP, zone, east, north, &blat,&blon )) == 0 )
				print_test_data( lat,lon, zone, east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf, %lf\n", k, blat,blon );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k, lat,lon );
		n++;		
	}
	while( proj_data( &lat,&lon ) == 2 )
	{	zone = -1;
		if( (k=geo_to_utm( prjP, lat,lon, &zone, &east, &north )) == 0 )
		{	if( (k=utm_to_geo( prjP, zone, east,north, &blat,&blon )) >= 0 )
				print_test_data( lat,lon, zone, east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO\n", k );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION\n", k );		
	}
	free_projection(prjP);
	return 0;
}

#endif
 
