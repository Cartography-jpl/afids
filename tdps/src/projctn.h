/* _PROJECTION_ structure */

#ifndef _PROJCTN_H
#define _PROJCTN_H
 
#include "dtcc.h"
#include "namelist.h"
#include "ellipse.h"
#include "coordorg.h"
#include "coordext.h"
#include "coord3n.h"
#include "projdfn.h"

#define DTCC_NPROJCONS  (20)           /* Maximum number of projection constants */

#define _PROJECTION_NAME_SIZE	(32)
typedef struct
    {  DTCC_LABELS	  label;					 /* projection label  */
       ELLIPS_PARAMS  ellipsoid;                 /* ellipsoid  */ 
       COORD_ORIGIN   origin;                    /* origin */
       double         stdpars[2];                /* std parallels or DOUBLE_NULLS */
       short		  ncnsts;					 /* Number of constants */
       short          fixed;                  	 /* grid zone if grid is fixed, otherwise -1 */
       proj_dfn       cnstsP; 			 		 /* Computation constants */ 
       void*    	  engineP;                   /* Computational engine  */
    } PROJ_PARAMS;
#define _PROJECTION_LABELP( ppP )      		( &((ppP)->label) )
#define _PROJECTION_ELLIPSOIDP( ppP )      	( &((ppP)->ellipsoid) )
#define _PROJECTION_ORIGINP( ppP )      	( &((ppP)->origin) )
#define _PROJECTION_STDPARSP( ppP )      	( (ppP)->stdpars )
#define _PROJECTION_CONSTANTSP( ppP )      	( (&(ppP)->cnstsP) )
#define _PROJECTION_ENGINE( ppP )     		( (ppP)->engineP )

#define dtcc_projection_ellipsoidP(ppP)		\
		( (const ELLIPS_PARAMS*)( (ppP) ? &((ppP)->ellipsoid) : NULL ))
#define dtcc_projection_originP(ppP)		\
		( (const COORD_ORIGIN*)( (ppP) ? &((ppP)->origin) : NULL ))
#define dtcc_projection_enameP(ppP)			\
		dtcc_nameP(dtcc_projection_ellipsoidP(ppP))
#define dtcc_projection_eabbrvP(ppP)		\
		dtcc_abbrvP(dtcc_projection_ellipsoidP(ppP))
#define dtcc_projection_stdparsP(ppP)		\
		( (const double*)( (ppP) ? (ppP)->stdpars : (double*)NULL ))
#define dtcc_projection_fixed_grid(ppP)     \
		( (ppP)->fixed )

#define dtcc_projection_labelP(ppP)		\
		( (const DTCC_LABELS*)( (ppP) ? &((ppP)->label) : NULL ) )
#define dtcc_projection_constantsP(ppP)		\
		( (const void*)( (ppP) ? (&(ppP)->cnstsP) : NULL ) )
/* "Close, but not quite" constants */
#define DTCC_SINMAXLAT (0.9999999999)       /* NOT the sin(MAXLAT) */ 
#define DTCC_SAME_ANGLE (0.00000001)        /* radians */ 

#define DTCC_PROJECTION (3)

#define dtcc_projection_axis( ppP )            \
		dtcc_ellipsoid_axis(dtcc_projection_ellipsoidP(ppP))
#define dtcc_projection_flat( ppP )            \
		dtcc_ellipsoid_flat(dtcc_projection_ellipsoidP(ppP))
#define dtcc_projection_ecc2( ppP )            \
		dtcc_ellipsoid_ecc2(dtcc_projection_ellipsoidP(ppP))
#define dtcc_projection_rflat( ppP )            \
		dtcc_ellipsoid_rflat(dtcc_projection_ellipsoidP(ppP))

#define dtcc_projection_lon0( ppP )           \
        dtcc_origin_lon0( dtcc_projection_originP(ppP) )
#define dtcc_projection_lat0( ppP )           \
        dtcc_origin_lat0( dtcc_projection_originP(ppP) )
#define dtcc_projection_fn( ppP )             \
        dtcc_origin_fn( dtcc_projection_originP(ppP) )
#define dtcc_projection_fe( ppP )             \
        dtcc_origin_fe( dtcc_projection_originP(ppP) )
#define dtcc_projection_zone( ppP )             \
        dtcc_origin_zone( dtcc_projection_originP(ppP) )
#define dtcc_projection_elv( ppP )             \
        dtcc_origin_elv( dtcc_projection_originP(ppP) )
#define dtcc_projection_fnSW( ppP )             \
        dtcc_origin_fnSW( dtcc_projection_originP(ppP) )
#define dtcc_projection_feSW( ppP )             \
        dtcc_origin_feSW( dtcc_projection_originP(ppP) )
#define dtcc_projection_K0( ppP )             \
        dtcc_origin_K0( dtcc_projection_originP(ppP) )
#define dtcc_projection_conv0( ppP )             \
        dtcc_origin_conv0( dtcc_projection_originP(ppP) )
#define dtcc_projection_azm0( ppP )             \
        dtcc_origin_azm0( dtcc_projection_originP(ppP) )
#define dtcc_projection_lower_std_parallel( ppP )  \
        ( dtcc_projection_stdparsP(ppP)[0] )
#define dtcc_projection_upper_std_parallel( ppP )  \
        ( dtcc_projection_stdparsP(ppP)[1] )
#define dtcc_projection_defined(ppP)	( _PROJECTION_ENGINE(ppP) != NULL )


#ifdef  __cplusplus 
extern "C"{
#endif

/* _PROJECTION_ computations */
extern short                                 /* status, 0=>OK */
_projection( const PROJ_PARAMS* projection,  /* (in) pointer to active struct */
             short 				mode,        /* (in) op code */
             const COORD3* 		fromP,       /* (in) List of input points */
             long 				n,			 /* (in) number of points */
             COORD3* 			toP         /* (out) stuff */
           );
#define dtcc_forward_projection( ppP, c3FromP, c3ToP )  \
        _projection( ppP, DTCC_PROJECTION_FORWARD, c3FromP,1L, c3ToP )
#define dtcc_inverse_projection( ppP, c3FromP, c3ToP)  \
        _projection( ppP, DTCC_PROJECTION_INVERSE, c3FromP,1L, c3ToP )
#define dtcc_projection_scale( ppP, c3FromP, c3ToP )  \
        _projection( ppP, DTCC_PROJECTION_SCALEFACTOR, c3FromP,1L, c3ToP )
#define dtcc_projection_intrinsic_limits( ppP, mnmx )	\
		_projection( ppP, DTCC_PROJECTION_LIMITS, NULL, 1L, mnmx )
#define dtcc_forward_projectionN( ppP, c3NFromP, c3NToP )  \
        _projection( ppP, DTCC_PROJECTION_FORWARD, \
        	_COORD3N_DATAP(c3NFromP), dtcc_coord3N_count(c3NFromP), \
        		_COORD3N_DATAP(c3NToP) )
#define dtcc_inverse_projectionN( ppP, c3NFromP, c3NToP )  \
        _projection( ppP, DTCC_PROJECTION_INVERSE, \
        	_COORD3N_DATAP(c3NFromP), dtcc_coord3N_count(c3NFromP), \
        		_COORD3N_DATAP(c3NToP) )
#define dtcc_projection_scaleN( ppP, c3NFromP, c3NToP )  \
        _projection( ppP, DTCC_PROJECTION_SCALEFACTOR, \
        	_COORD3N_DATAP(c3NFromP), dtcc_coord3N_count(c3NFromP), \
        		_COORD3N_DATAP(c3NToP) )
		                
extern short                     		/* status, 0=>OK */
dtcc_reset_projection
    ( PROJ_PARAMS*  ppP,        		/* pointer to active struct */
      const char* 	pname,				/* projection name or abbreviation */
      const ELLIPS_PARAMS* ellipsoid    /* the new ellipsoid */
    );
extern short
dtcc_is_projection_UTM
	(	const PROJ_PARAMS* ppP
	);
extern short
dtcc_is_projection_UPS
	(	const PROJ_PARAMS* ppP
	);
extern short
dtcc_is_projection_MGRS
	(	const PROJ_PARAMS* ppP
	);
	
extern short                     /* status, 0=>OK */
dtcc_reset_projection_ellipsoid
    ( PROJ_PARAMS*  ppP,
      const char* ename          /* name or abbrv of the ellipsoid */
    );
extern short                     /* status, 0=>OK */
dtcc_set_projection_ellipsoid
    ( PROJ_PARAMS*  ppP,
      const char* ename,         /* name or abbrv of the ellipsoid */
      const char* abbrv,         /* name or abbrv of the ellipsoid */
      double axis,
      double rflat
    );
extern short                     /* status, 0=>OK */
dtcc_set_projection_origin    
    ( PROJ_PARAMS*  ppP,
      const COORD_ORIGIN* orgP   /* origin */
    );
extern short dtcc_set_projection_false_origin
	(  PROJ_PARAMS* csP, double fe, double fn, double fw, double fs ); 
extern short                     /* status, 0=>OK */
dtcc_set_projection_std_parallels
    ( PROJ_PARAMS*  ppP,
      double        lower,       /* lower standard parallel (degrees) */
      double        upper        /* upper standard parallel (degrees) */
    );
/* Write to a formatted stream */
extern short                  /* Status code, 0=>OK */
dtcc_print_projection
    ( const PROJ_PARAMS* ppP, /* (in) pointer to the active struct */
      FILE* fp                /* (in) open stream */
    );
/* Read from a formatted stream */
extern short                   /* Status code, 0=>OK */
dtcc_scan_projection
    ( PROJ_PARAMS* ppP,        /* (in) pointer to the active struct */
      FILE* fp                 /* (in) open stream */
    );
/* Write to an unformatted stream */
extern short                  /* Status code, 0=>OK */ 
dtcc_write_projection
    ( const PROJ_PARAMS* ppP, /* (in) pointer to the active struct */
      FILE* fp                /* (in) open stream */
    );
/* Read from n unformatted stream */
extern short                   /* Status code, 0=>OK */ 
dtcc_read_projection
    ( PROJ_PARAMS* ppP,        /* (in) pointer to the active struct */
      FILE* fp                 /* (in) open stream */
    );

/* Copy a projection */
extern short                   /* Status code, 0=>OK */ 
dtcc_copy_projection
    ( PROJ_PARAMS* ppP1,       /* (out) pointer to the target struct */
      const PROJ_PARAMS* ppP2  /* (in) pointer to the source struct */
    );
/* Compare two projections (name, ellipsoid, origin) */
extern short					/* same => 0 */
dtcc_compare_projection
    ( const PROJ_PARAMS* ppP1,  /* (in) pointer to the target struct */
      const PROJ_PARAMS* ppP2   /* (in) pointer to the source struct */
    );
#define dtcc_compare_projection_name( p1,p2 ) \
		strcmp( dtcc_nameP(p1),dtcc_nameP(p2) )
#define dtcc_compare_projection_origin( p1,p2 ) \
		dtcc_compare_origin_geo\
			 ( dtcc_projection_originP(p1),dtcc_projection_originP(p2) )
#define dtcc_compare_projection_ellispoid( p1,p2 ) \
		dtcc_compare_ellipsoid\
			( dtcc_projection_ellipsoidP(p1),dtcc_projection_ellipsoidP(p2) ) 
    
/* Operation constants */
#define DTCC_PROJECTION_FORWARD			(1)        /* Geographics to grid */
#define DTCC_PROJECTION_INVERSE    		(2)        /* Grid to geographics */
#define DTCC_PROJECTION_SCALEFACTOR  	(3)        /* Scale factors */
#define DTCC_PROJECTION_SETUP    		(4)        /* Compute projection constants */
#define DTCC_PROJECTION_DEFAULTS   	  	(5)        /* Setup projection defaults */
#define DTCC_PROJECTION_LIMITS			(6)		   /* Intrinsic geo limits */

/* Set the fixed grid flag */
#define dtcc_set_projection_fixed_grid( ppP,flag )  \
		( dtcc_projection_fixed_grid(ppP) = (flag) )

/* Invalidate the structure. */
extern short 
dtcc_invalidate_projection
	(	PROJ_PARAMS* dtccP 
	);
	
/* Validate the structure. */
extern short 
dtcc_validate_projection
	(	PROJ_PARAMS* dtccP 
	);

extern short
dtcc_clear_projection( PROJ_PARAMS* proP );
extern short
dtcc_initialize_projection( PROJ_PARAMS* proP );

extern short
dtcc_projection_is_displayable( PROJ_PARAMS* proP );
extern 
short map_projection_status( short n);


#ifdef  __cplusplus 
}
#endif

#endif /* _PROJCTN_H */


