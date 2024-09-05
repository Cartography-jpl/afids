/* Constant array map */

#ifndef _PROJDFN_H
#define _PROJDFN_H

#include "ellpfns.h"

#define deref(x)			(x)

/* Projection constants */
#define NM				(cnsts->name)
#define P0				(cnsts->p0)
#define SINP0			(cnsts->sinp0)	/* SIN( ABS(P0) ) */
#define COSP0			(cnsts->cosp0)	/* COS( ABS(P0) ) */
#define L0				(cnsts->l0)
#define RADIUS			(cnsts->axis)
#define AXIS			(cnsts->axis)
#define ECC				(cnsts->e)
#define ECC2			(cnsts->e2)
#define WH				(cnsts->which)
#define NCNSTS			(cnsts->ncnsts)
#define K0				(cnsts->k0)
#define AZ0				(cnsts->a0)
#define STD1			(cnsts->p1)		
#define STD2			(cnsts->p2)
#define SLIM			(cnsts->glims[0])
#define WLIM			(cnsts->glims[1])
#define NLIM			(cnsts->glims[2])
#define ELIM			(cnsts->glims[3])

/* Projection modes */
#define OBLIQUE			( cnsts->which == 2 )
#define POLAR			( abs(cnsts->which) == 1 )
#define SOUTH_POLAR		( cnsts->which == -1 )
#define EQUATORIAL		( cnsts->which == 0 )
#define SPHERE			( ECC < EPSILON )

#define _NOTHER	(16)
typedef struct 
	{	short	id;
		char 	name[32];
		double 	l0, p0,sinp0,cosp0;
		double	k0,a0, p1,p2; 
		double	axis,e,e2;
		double	glims[4];
		short	which, ncnsts;
		double	other[_NOTHER];
	}proj_dfn;
#define PROJ_ID(p)	( *(short*)(p) )

#ifdef  __cplusplus 
extern "C"{
#endif

extern short _init_std_pars
	( proj_dfn* cnsts, int k, const char* name, 
		double a, double rf, double lat0, double lon0  );
extern void* _set_std_pars
	( int k, const char* name, double a, double rf, double lat0, double lon0 );
extern void _set_projection_limits
	( const void* cnstsP, double s, double w, double n, double e );

/* Utilities */
extern double rpoly( int n, const double* cfs, double x );
extern double square( double x );
extern double affix_sign( double x, double sgn );
extern double check_PI( double a );
extern void sincostan( double a, double* sina, double* cosa, double* tana );
extern void arctan( double n, double d, 
		double* h, double* a, double* sina, double* cosa, double* tana );
extern void r_and_theta( int which, double x, double y, double* r, double* t );

#define allocate_ptr(n)	malloc(n)

#ifdef  __cplusplus 
}
#endif
				
#endif /* _PROJDFN_H */

