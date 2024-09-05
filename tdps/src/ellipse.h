/* Ellipsoid structure */

#ifndef _ELLIPSE_H
#define _ELLIPSE_H
 
#include "namelist.h" 
#include <stdio.h>

typedef struct 
	{ 	DTCC_LABELS	label;       /* struct id */
		double axis, rflat, ecc; /* ellipsoid values */
	} ELLIPS_PARAMS;

/* Attributes */
#define dtcc_ellipsoid_ecc(ep)   		( (ep) ? (ep)->ecc : DOUBLE_NULL )
#define dtcc_ellipsoid_ecc2(ep)   		( (ep) ? (ep)->ecc*(ep)->ecc : DOUBLE_NULL )
#define dtcc_ellipsoid_axis(ep)   		( (ep) ? (ep)->axis : DOUBLE_NULL ) 
#define dtcc_ellipsoid_rflat(ep)    	( (ep) ? (ep)->rflat : DOUBLE_NULL ) 
#define dtcc_ellipsoid_userdef(ep)  	( dtcc_type_flag(ep) == _TYPE2_LIST ) 
#define dtcc_ellipsoid_nameP(ep)		dtcc_nameP( ep ) 
#define dtcc_ellipsoid_abbrvP(ep)		dtcc_abbrvP(ep)
#define dtcc_ellipsoid_flat(ep)			( (ep)->rflat==0.0 ? 0.0 : 1.0/(ep)->rflat)	
extern double dtcc_ellipsoid_second_ecc2( const ELLIPS_PARAMS* ep);

#ifdef  __cplusplus 
extern "C"{
#endif

/* Initialize structure to the default elloipsoid. WGS84 is the default. */
extern short                	/* Status code, 0=>OK */
dtcc_reset_default_ellipsoid
    ( 	ELLIPS_PARAMS* elP    	/* (out) pointer to the active ellipsoid */
    );
      	    
extern short
dtcc_set_ellipsoid				/* Implies user-defined */
	(	ELLIPS_PARAMS*	elP,   	/* (out) pointer to the active ellipsoid */
      	const char*		name,	/* (in) new name */
      	double			axis,	/* (in) new semi-major axis (meters) */
      	double			rflat	/* (in) reciprocal flattening */
    );		

/* Write to a formatted stream */
extern short                       /* Status code, 0=>OK */
dtcc_print_ellipsoid
    ( const ELLIPS_PARAMS* ep,     /* (in) pointer to the active struct */
      FILE* fp                     /* (in) open stream */
    );
extern short                       /* Status code, 0=>OK */
dtcc_print_ellipsoid_name
    ( const ELLIPS_PARAMS* ep,     /* (in) pointer to the active struct */
      FILE* fp                     /* (in) open stream */
    );
/* Read from a formatted stream */
extern short                        /* Status code, 0=>OK */
dtcc_scan_ellipsoid
    ( ELLIPS_PARAMS* ep,            /* (in) pointer to the active struct */
      FILE* fp                      /* (in) open stream */
    );
/* Write to an unformatted stream */
extern short                        /* Status code, 0=>OK */ 
dtcc_write_ellipsoid
    ( const ELLIPS_PARAMS* ep,      /* (in) pointer to the active struct */
      FILE* fp                      /* (in) open stream */
    );
/* Read from a formatted stream */
extern short                        /* Status code, 0=>OK */ 
dtcc_read_ellipsoid
    ( ELLIPS_PARAMS* ep,            /* (in) pointer to the active struct */
      FILE* fp                      /* (in) open stream */
     );

/* Copy ellipsoid */
extern short                         /* Status code, 0=>OK */ 
dtcc_copy_ellipsoid
    ( ELLIPS_PARAMS* ep1,            /* (out) pointer to the target ellipsoid */
      const ELLIPS_PARAMS* ep2       /* (in) pointer to the source ellipsoid */
    );     

/* Compare */
/* Ordering doesn't make any sense */
/* Compares axis and eccentricity squared NOT the name */
extern short                 /* same => 0 */
dtcc_compare_ellipsoid
	(	const ELLIPS_PARAMS* elP1,
		const ELLIPS_PARAMS* elP2
	);
		     
/* Invalidate the structure. */
extern short 
dtcc_invalidate_ellipsoid
	(	ELLIPS_PARAMS* dtccP 
	);
	
/* Validate the structure. */
extern short 
dtcc_validate_ellipsoid
	(	const ELLIPS_PARAMS* dtccP 
	);

extern short _reset_ellipsoid( ELLIPS_PARAMS* elP, const char* nm, int type );

extern short
dtcc_make_ellipsoid_name( ELLIPS_PARAMS* csP, char* nm );
extern short
dtcc_is_ellipsoid_axis_OK(	double axis );
extern short
dtcc_is_ellipsoid_flattening_OK(  double rflat );

extern short                 /* same => 1 */
dtcc_compare_ellipsoid_names
	(	const ELLIPS_PARAMS* elP1,
		const ELLIPS_PARAMS* elP2
	);

#if 1 /* Instead of rewriting existing interface */
extern short dtcc_set_ellipsoid_axis( ELLIPS_PARAMS*  epP, double axis );
extern short dtcc_set_ellipsoid_rflat( ELLIPS_PARAMS*  epP, double rflat );
#endif /* rewrite */

#ifdef  __cplusplus 
}
#endif

#endif /* _ELLIPSE_H */


                                                   


