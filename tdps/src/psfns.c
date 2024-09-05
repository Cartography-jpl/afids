/* Polar Stereographic functions */
/* Utility functions */

#include "projdfn.h"
#include "ellpfns.h"
#include "projfns.h"
#include <string.h>
#include "psfns.h"
#include <stdlib.h>

#ifndef deref
#define deref(x)		(x)
#endif
#define cstrcpy( a,b )	strcpy( (a),(b) )

/* Polar Stereographic Projection functions */
#define NC				(5)

#define C0				cnsts->other[1]
#define CFCFS			(cnsts->other+2)
#define LABEL			"Polar Stereographic"

int polar_stereographic_limits
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
	 
short init_polar_stereographic
	( void* cnstsP, const char* ename, 
		double a, double rf, double lat0, double lon0, double k0 )
	{	double phi1 = HALF_PI;
		if( lat0 < 0.0 ) phi1 = -phi1;
		if( !cnstsP )
			return -1;
		else
		{ 	proj_dfn* cnsts = (proj_dfn*)cnstsP;
			_init_std_pars( cnsts,NC,LABEL, a,rf, phi1,lon0 );
			K0 = k0;
			C0 = 2.0*AXIS * pow( (1.0-ECC)/(1.0+ECC), 0.5*ECC ) / sqrt(1.0-ECC2); 
			/* Inverse conformal latitude coefficients */
			set_inverse_conformal( ECC, CFCFS );
			NLIM = ( lat0 >= 0.0 ? 90.0 : 0.0 ); ELIM = 180.0;
			SLIM = ( lat0 >= 0.0 ? 0.0 : -90.0 ); WLIM = -180.0;
			return 0;
		}
	}
const void* set_polar_stereographic
	( const char* ename, double a, double rf, double lat0, double lon0, double k0  )
	{	void* cnsts = allocate_memory(sizeof(proj_dfn));
		init_polar_stereographic( cnsts, LABEL, a,rf, lat0, lon0,k0 );
		return cnsts;
	}

short geo_to_polar_stereographic
	( 	const void* cnstsH, double lat, double lon, double* x, double* y )
	{	int status = 0;
		if( !cnstsH )
			return -1;
		else
		if( (status = projection_limit_check(cnstsH, lat,lon)) < 0 )
			return -2;
		else
		if( fabs(fabs(lat) - HALF_PI) < EPSILON )
		{	*x = *y = 0.0;
			return 0;
		}
		else
		{	const proj_dfn* cnsts = (const proj_dfn*)deref(cnstsH);
			double pole = ( P0 < 0.0 ? 1.0 : -1.0 );
			double rphi = fabs(lat)*RADDEG, rlam = lon*RADDEG;
			double dlam = check_PI( rlam - L0 );
			double sphi = sin(rphi);
			double v = ECC*sphi;
			double t = tan( 0.5*(HALF_PI - rphi) ) * pow( (1.0+v)/(1.0-v), 0.5*ECC );
			double rho = K0*C0*t;
	
			*x = rho * sin(dlam);
			*y = rho * cos(dlam) * pole;
			return status;
		}
	}
	
short polar_stereographic_to_geo
	( const void* cnstsH, double x, double y, double* lat, double* lon )
	{	double rphi,rlam;
		double R,X;
		const proj_dfn* cnsts = (const proj_dfn*)deref(cnstsH);
		double pole = ( P0 < 0.0 ? -1.0 : 1.0 );
		int xzero = fabs(x) < EPSILON, yzero = fabs(y) < EPSILON;
		if( xzero && yzero )
		{	*lat = affix_sign( 90.0,pole );
			*lon = L0/RADDEG;
			return 0;
		}
		
		/* Longitude */
	    rlam = ( yzero ? affix_sign( HALF_PI, x ) : 
					atan2( x,affix_sign(y,-pole) ));
		/* Radius */
		R = fabs( yzero ? x : ( xzero ? y : 
				( fabs(x) < fabs(y) ? y/cos(rlam) : x/sin(rlam) )));
		/* Latitude	*/	
		X = HALF_PI - 2.0*atan(R/(K0*C0));
		inverse_conformal_lat( X, sin(X), CFCFS, &rphi );
		*lat = affix_sign( rphi/RADDEG, pole );
		*lon = check_PI(rlam+L0)/RADDEG;
		if( y < 0 ) *lon = check_180(180.0 - *lon);
		return projection_limit_check(cnsts, *lat,*lon);
	}

short polar_stereographic_scale
	( const void* cnstsH, double lat, double lon, double* h, double* k )
	{	const proj_dfn* cnsts = (const proj_dfn*)deref(cnstsH);
		double rphi = fabs(lat)*RADDEG;
		double sphi = sin(rphi);
		double v = ECC*sphi;
		double t = tan( 0.5*(HALF_PI - rphi) ) * pow( (1.0+v)/(1.0-v), 0.5*ECC );
		double rho = K0*C0*t;
		double s = rho/( AXIS*cos(rphi)*vertical_radius(ECC,sphi) );
		*k = *h = s;
		return 0;
	}

/* Universal Polar Stereographic */
int ups_limits
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
	 
short init_ups( void* cnstsP, double a, double rf,double lat0 )
	{	/* double a = 6378388.0, rf = 297.0;  */	/* International */
		double k0 = 0.994;
		double lon0 = 0.0;
		return init_polar_stereographic
				( cnstsP, "Universal Polar Stereographic",
					 a,rf, lat0, lon0, k0 );
	}
const void* set_ups( double a, double rf, double lat0 )
	{	void* cnsts = allocate_memory(sizeof(proj_dfn));
		init_ups( cnsts,a,rf,lat0 );
		return cnsts;
	}

#define FN (2000000.0)
#define FE (2000000.0)
#define NORTHLIM	(83.5)
#define SOUTHLIM	(-79.5)

short ups_to_geo
	( const void* tcnsts, 
	  double easting, double northing,
	  double* lat, double* lon 
	)
	{	short status;
		status = 
			polar_stereographic_to_geo
				( tcnsts, easting-FE,fabs(northing)-FN, lat,lon );  
		if( status == 0 )
		{	if( ( *lat < 0.0 && *lat > SOUTHLIM ) ||
				( *lat >= 0.0 && *lat < NORTHLIM ) )
				status = 2;
		}
		return status;
	}
short geo_to_ups
	( const void* tcnsts, 
	  double lat, double lon, 
	  double* easting, double*  northing 
	)
	{	short status, south = lat < 0.0;
		status = 
			geo_to_polar_stereographic
				( tcnsts, lat,lon, easting,northing );  
		*northing += FN;
		*easting += FE;
		if( south ) *northing = -fabs(*northing);
		if( status == 0 )
		{	if( ( south && lat > SOUTHLIM ) ||
				( !south && lat < NORTHLIM ) )
				return -2;
		}
		return status;
	}


short ups_scale
	( const void* tcnsts, 
	  double lat, double lon, 
	  double* h, double* k 
	)
	{	return polar_stereographic_scale( tcnsts, lat,lon, h,k );  }


#if 0 /* test polar stereographic */
#include <stdio.h>
extern void GPTUPS( double a, double rf, double phi, double lam, double* y, double *x );
int main()
{	short zone;
	int i,j;
	double x1,y1,x2,y2, backp1,backl1, backp2,backl2, h,k;
	const void*  cnsts;
	FILE* fp = stdout; /*  fopen( "sttst.txt", "w" );  */
	char *name = "WGS-84";
	double a = 6378137.000,	rf = 298.257223563;
	double lat0 = -90.0, lon0 = 0.0;  /* -100.0; */
	double k0 = 0.994;
	
	double lat[] = {  84.28723388889,  73.0,  -87.2873333333,
					  -85.0, -85.0, -85.0, 85.0, 85.0, 85.0 }, 
		   lon[] = { -132.2479891667,  44.0,  132.2478619444,
		   			 0.0, 90.0, -90.0, -90.0, 90.0, 0.0 };
	
	double xt[] =  {  1530125.78, 3320416.75, 2222979.47, 
		   			  2000000.0, 2555457.0, 1444543.0, 1444543.0, 2555457.0,2000000.0 },
		   yt[] =  {  2426773.60,  632668.43, 1797474.90,
					  2555457.0, 2000000.0, 2000000.0, 2000000.0, 2000000.0, 1444543.0 };
		    
	j = sizeof(lat)/sizeof(double);
	cnsts = set_ups(a,rf);
	if( !cnsts ) return 1;
	print_projection( cnsts,fp );
	for( i=0; i<j; i++ )
	{	fprintf( fp, "\tStart: %lf %lf\n", lat[i],lon[i] );
		geo_to_ups( cnsts, lat[i], lon[i], &x1, &y1 );
		fprintf( fp, "\t\t%lf %lf  :  %lf %lf\n", x1,y1, x1-xt[i],fabs(y1)-yt[i] );
		/*
		GPTUPS( a,rf, lat[i]*RADDEG,lon[i]*RADDEG, &y2, &x2 ); 
		fprintf( fp, "\t\t%lf %lf  :  %lf %lf\n", x2,y2, x2-xt[i],fabs(y2)-yt[i] );
		*/
		ups_to_geo( cnsts, x1,y1, &backp1, &backl1 );
		fprintf( fp, "\t\t%lf %lf  :  %lf %lf\n", 
			backp1,backl1, (backp1 - lat[i])*3600.0,(backl1 - lon[i])*3600.0 );
		ups_scale( cnsts, lat[i],lon[i], &x1,&x2 );
		fprintf( fp, "\t\t\t%lf %lf\n", x1,x2 );
		fprintf( fp, "\n" );
	}
	free_projection(cnsts);

	if( fp == stdout )
		fscanf( stdin,"%d", &i );
	else
		fclose(fp);
	return 0;
}	
#endif /* test PS */

 
#if 0
short polar_stereographic_parameters
	( const void* cnstsH, char* pname, char* ename, 
		double* axis, double* rf, double* lat0, double* lon0, double* k0 )
	{	const proj_dfn *cnsts = (const proj_dfn *)deref(cnstsH);
		cstrcpy( pname, (char*)PNM );
		cstrcpy( ename, (char*)ENM );
		*axis = AXIS; *rf = RF;
		*lat0 = P0/RADDEG; *lon0 = L0/RADDEG; *k0 = K0;
		return 0;
	}
#endif

#if 0 /* test polar_stereographic functions */
#include "stdiofns.h"

int main()
{	const void* prjP;
	double lat,lon, north,east, blat,blon;
	/* Default is International */
	/* International */
	double a = 6378388.000,	rf =297.0; 
	/* WGS84 */
	/* double a = 6378137.000,	rf = 298.257223563; */
	/* Clarke 1866 */
	/* double a = 6378206.4, rf  = 294.9786982; */ 
	double lat0 = 90.0,lon0 = 0.0, k0 = 0.9996;
	int n,k = 0;
	short zone = -1;
	
	if( (k=proj_pars( &a, &rf, &lat0, &lon0, &k0, NULL, NULL,NULL )) == 5 )
	{	prjP = set_ups(  );
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
	{	if( (k=geo_to_ups( prjP, lat,lon, &east, &north )) == 0 )
		{	if( (k=ups_to_geo( prjP, east, north, &blat,&blon )) >= 0 )
				print_test_data( lat,lon, zone,east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf %lf\n", k, lat,lon );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k,lat,lon );
		n++;		
	}
	while( proj_data( &lat,&lon ) == 2 )

	{	if( (k=geo_to_ups( prjP, lat,lon, &east, &north )) == 0 )
		{	if( (k=ups_to_geo( prjP, east, north, &blat,&blon )) >= 0 )
				print_test_data( lat,lon, zone,east,north, blat,blon );
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf %lf\n", k, lat,lon );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k, lat,lon );		
	}
	free_projection(prjP);
	return 0;
}

#endif
