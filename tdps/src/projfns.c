/* General projection functions */

#include "ellpfns.h"
#include "projfns.h"
#include "projdfn.h"
#include <stdlib.h>
#include <string.h>

/* Utilities */
double affix_sign( double x, double sgn )
	{	double fx = fabs(x); 
		return ( sgn < 0.0 ? -fx : fx );  
	}	
double check_PI( double a )
	{	int k=2;
		while( fabs(a) > PI && --k >= 0 )
		{	if( a > PI )
				a -= TWO_PI;
			else
			if( a < -PI )
				a += TWO_PI;
		}
		return a;
	}
double check_180( double a )
	{	if( a > 180.0 )
			a -= 360.0;
		else
		if( a < -180.0 )
			a += 360.0;
		return a;
	}
double check_360( double a )
	{	return ( a < 0.0 ? a+360.0 : ( a >=360.0 ? a-360.0 : a ));
	}
double check_90( double a )
	{	if( a > 90.0 )
			a -= (a-90.0);
		else
		if( a < -90.0 )
			a -= (a-90.0);
		return a;
	}
double square( double x )	{	return x*x;  }

double rpoly( int n, const double* cfs, double x )
	{	double p = cfs[n];
		while( --n >= 0 )
			p = p*x + cfs[n];
		return p;
	}

void* allocate_projection( int k )
	{	return allocate_memory( k*sizeof(proj_dfn) );  }
void free_projection( const void* dfnP )
	{	if( dfnP ) free_memory((void*)dfnP); return;  }
short projection_id( const void *cnsts )
	{	return PROJ_ID(cnsts);  }

/* projection attributes */
char* projection_name( const void* cnstsP, char* nameP )
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP;
		if( cnsts )
		{	if( !nameP ) nameP = (char*)allocate_memory( strlen(NM)+1 );
			if( nameP ) strcpy(nameP,NM);
			return nameP;
		}
		else
			return NULL;
	}

double projection_axis( const void *cnstsP )	
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; return ( cnsts ? AXIS : DOUBLE_NULL );  }
double projection_ecc( const void *cnstsP )	
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; return ( cnsts ? ECC : DOUBLE_NULL );  }
double projection_rf( const void *cnstsP )	
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP;
		if( !cnsts )
			return DOUBLE_NULL;
		else
		{ 	double ecc = projection_ecc(cnstsP);
			return ( ecc <= 0.0 ? 0.0 : 1.0/(1.0 - sqrt(1.0-ecc*ecc)));
		}
	}
double projection_lat0( const void *cnstsP )	
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; 
		return ( cnsts ? P0/RADDEG : DOUBLE_NULL );  
	}
double projection_lon0( const void *cnstsP )	
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; return ( cnsts ? L0/RADDEG : DOUBLE_NULL );  }

double projection_lat1( const void *cnstsP )
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; 
		if( !cnsts )
			return DOUBLE_NULL;
		else
		if( fabs(STD1) > 90.0 )
			return DOUBLE_NULL;
		else
			return STD1;
	}
double projection_lat2( const void *cnstsP )
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; 
		if( !cnsts )
			return DOUBLE_NULL;
		else
		if( fabs(STD2) > 90.0 )
			return DOUBLE_NULL;
		else
			return STD2;
	}
double projection_azm0( const void *cnstsP )
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; 
		if( !cnsts )
			return DOUBLE_NULL;
		else
		if( fabs(AZ0) > 360.0 )
			return DOUBLE_NULL;
		else
			return AZ0;
	}
double projection_k0( const void *cnstsP )
	{	const proj_dfn* cnsts = (proj_dfn*)cnstsP; 
		if( !cnsts )
			return DOUBLE_NULL;
		else
			return K0;
	}
int projection_limit_check( const void *cnstsP, double lat, double lon )
{	double min,max;
	int status1 = 0, outofrange = 17;
	proj_dfn* cnsts = (proj_dfn*)cnstsP;
	if( lat < SLIM || lat > NLIM )
	{	min = SLIM-2.0; max = NLIM+2.0;
		if( min < -90.0 ) min = -90.0;
		if( max > 90.0 ) max = 90.0;
		if( lat > max || lat < min )
			return -outofrange;
		else
			status1 = outofrange;
	}

	if( WLIM < ELIM && lon <= ELIM && lon >= WLIM  )
		return status1;
	else
	if( WLIM > ELIM && (lon <= ELIM || lon >= WLIM) )
		return status1;
	else
	{	min = check_180(WLIM-2.0); max = check_180(ELIM+2.0);
		if( min < max)
			return ( lon <= max && lon >= min ? outofrange : -outofrange );
		else
			return ( lon <= ELIM || lon >= WLIM ? outofrange : -outofrange ); 
	}
}
 	

/* Projection operators */
void print_projection( const void* pdfnP, FILE* fp )
{	double tmp;
	char name[64];
	fp = ( fp ? fp : stdout );
	fprintf( fp, "%s projection ", projection_name(pdfnP,name) );
	fprintf( fp, "[%lf, %lf]\n", 
		projection_axis(pdfnP), projection_rf(pdfnP) );
	fprintf( fp, "\tGeo Origin: (%lf, %lf)\n",
		projection_lat0(pdfnP), projection_lon0(pdfnP) );
	
	tmp =  projection_azm0(pdfnP);
	if( tmp < 360.0  && tmp >= 0.0 ) fprintf( fp, "\t azm0 = %lfd\n", tmp ); 
	
	tmp = projection_k0(pdfnP);
	if( tmp > 0.0 ) fprintf( fp, "\tk0 = %lf\n",tmp );
	
	tmp = projection_lat1(pdfnP);
	if( fabs(tmp) < 90.0 )
	{	fprintf( fp, "\tStandard Parallel(s): %lf", tmp );
		tmp = projection_lat2(pdfnP);
		if( fabs(tmp) < 90.0 )
			fprintf( fp, " : %lf\n", tmp );
		else
			fprintf( fp, "\n" );
	}
	return;
}

void* copy_projection( void* destP, const void* srcP )
	{	if( srcP )
		{	if( !destP ) destP = allocate_projection(1);
			if( destP ) memcpy( destP, srcP, sizeof(proj_dfn) );	
		}
		return destP;
	}		

short _init_std_pars
	( proj_dfn* cnsts, int k, const char* name, 
		double a, double rf, double lat0, double lon0  )
	 {	if( !cnsts )
	 		return -1;
		else
		{	int n = _NOTHER;
			cnsts->id = 1; /* sizeof(proj_dfn); */
			while( --n >= 0 ) cnsts->other[n] = DOUBLE_NULL;
			K0 = AZ0 = STD1 = STD2 = DOUBLE_NULL;
			ELIM = 180.0; WLIM = -180.0;NLIM = 90.0; SLIM = -90.0;
			NCNSTS = k;
			P0 = lat0*RADDEG; sincostan( P0, &SINP0, &COSP0,NULL ); 
			L0 = check_PI(lon0*RADDEG);
			AXIS = a;
			if( rf <= 0.0 )
				ECC2 = 0.0;
			else
			if( rf > 1000000.0 )
				ECC2 = 1.0 - (rf*rf)/(AXIS*AXIS); 
			else
			if( rf > 1.0 )
				ECC2 = eccentricity2(rf);
			else
				ECC2 = rf; 
			ECC = sqrt(ECC2); 
			WH = ( fabs(P0) < EPSILON ? 0 :
				 ( P0 > HALF_PI - EPSILON ? 1 :
				 ( P0 < -HALF_PI + EPSILON ? -1 : 2 )));
			if( name[0] == 'Z' )
			{	strcpy( NM, ( EQUATORIAL  ? "Equatorial " : 
							( POLAR ? "Polar " : "Oblique " )) );
				strcat( NM, name+1 );
			}
			else
				strcpy( NM,name );
			return 0;
		}
	}
void* _set_std_pars
	( int k, const char* name, double a, double rf, double lat0, double lon0  )
	{	proj_dfn* cnsts = (proj_dfn*)allocate_projection(1);
		_init_std_pars( cnsts,k,name, a,rf,lat0,lon0 );
		return cnsts;
	}


/* utilities used by the projection functions */
void rotate( double u, double v, double sina,double cosa, double* ru, double* rv )
	{	*ru = v*cosa + u*sina; 
		*rv = u*cosa - v*sina; 
		return;  
	}
		 
void sincostan( double a, double* sina, double* cosa, double* tana )
	{	if( sina ) *sina = sin(a); 
		if( cosa ) *cosa = cos(a);  
		if( tana ) *tana = tan(a);
		return;
	}
void arctan( double n, double d, double* h, 
		double* a, double* sina, double* cosa, double* tana )
	{	double r = sqrt( n*n + d*d );
		if( h ) *h = r;
		if( a ) *a = atan2(n,d);
		if( sina ) *sina = n/r;
		if( cosa ) *cosa = d/r;
		if( tana ) *tana = n/d;
		return;
	}

void r_and_theta( int which, double x, double y, double* r, double* t )
	{	if( r ) *r = sqrt( x*x + y*y );
		if( t ) *t = atan2( x, ( which < 0 ? y : -y ) );
		return;
	}


