/* Origin of a coordinate system */ 

#ifndef _COORDORG_H
#define _COORDORG_H

#include "angle.h"
#include "coord3.h"

typedef struct
    {   COORD3       gridNE;  /* False northing & easting N&E */
        COORD3       gridSW;  /* False northing & easting S&W */
        COORD3       geo;     /* Latitude and longitude of origin */
        COORD3       scales;  /* Scale factors at the origin */
        ANGLE        w0;      /* Convergence of the meridians at the origin */
        ANGLE        azm0;    /* Azimuth at origin */
        short		 valid;
        short		 op_status; /* Status code for last operation */
    } COORD_ORIGIN;
#define _origin_size	\
		( sizeof(short) + 2*sizeof(IEEE_DOUBLE8) + 4*_coord3_size )

/* Pointers to the constituent data structures */   
#define _ORIGIN_GRIDNEP( coP )          (  (coP) ? &((coP)->gridNE) : NULL )
#define _ORIGIN_GRIDSWP( coP )        	(  (coP) ? &((coP)->gridSW)  : NULL )
#define _ORIGIN_GEOP( coP )           	(  (coP) ? &((coP)->geo ) : NULL )
#define _ORIGIN_SCALEP( coP )         	(  (coP) ? &((coP)->scales) : NULL )
#define _ORIGIN_CONVERGENCEP( coP )   	(  (coP) ? &((coP)->w0) : NULL )
#define _ORIGIN_AZIMUTHP( coP )       	(  (coP) ? &((coP)->azm0) : NULL )

/* Grid values at origin ( false northing and false easting N&E of origin) */
#define dtcc_origin_gridNEP(coP)		((const COORD3*)_ORIGIN_GRIDNEP(coP))
#define dtcc_origin_fn( coP )           dtcc_coord32( dtcc_origin_gridNEP(coP) )
#define dtcc_origin_fe( coP )           dtcc_coord31( dtcc_origin_gridNEP(coP) ) 
#define dtcc_origin_zone( coP )         dtcc_coord3Z( dtcc_origin_gridNEP(coP) ) 

/* Grid values at origin ( false northing and false easting S&W of origin) */
#define dtcc_origin_gridSWP(coP)		((const COORD3*)_ORIGIN_GRIDSWP(coP))
#define dtcc_origin_fnSW( coP )         dtcc_coord32( dtcc_origin_gridSWP(coP) )
#define dtcc_origin_feSW( coP )         dtcc_coord31( dtcc_origin_gridSWP(coP) )
#define dtcc_origin_zoneSW( coP )       dtcc_coord3Z( dtcc_origin_gridSWP(coP) )

/* Geodetic position ( latitude and longid=tude ) of the origin */
#define dtcc_origin_geoP(coP)			((const COORD3*)_ORIGIN_GEOP(coP))
#define dtcc_origin_lat0( coP )         dtcc_coord31(dtcc_origin_geoP(coP))
#define dtcc_origin_lon0( coP )         dtcc_coord32(dtcc_origin_geoP(coP))
#define dtcc_origin_elv( coP )			dtcc_coord33(dtcc_origin_geoP(coP))
/* scale factors and convergence of the meridians at the origin */
#define dtcc_origin_scaleP(coP)			((const COORD3*)_ORIGIN_SCALEP(coP))
#define dtcc_origin_K0( coP )           dtcc_coord31(dtcc_origin_scaleP(coP))
#define dtcc_origin_H0( coP )           dtcc_coord32(dtcc_origin_scaleP(coP))
#define dtcc_origin_J0( coP )           dtcc_coord33(dtcc_origin_scaleP(coP))
#define dtcc_origin_convergenceP(coP)	((const ANGLE*)_ORIGIN_CONVERGENCEP(coP))
#define dtcc_origin_W0( coP )           dtcc_angle_value(dtcc_origin_convergenceP(coP))
#define dtcc_origin_conv0( coP )        dtcc_angle_value(dtcc_origin_convergenceP(coP))
#define dtcc_origin_azimuthP(coP)		((const ANGLE*)_ORIGIN_AZIMUTHP(coP))
#define dtcc_origin_azm0( coP )         dtcc_angle_value(dtcc_origin_azimuthP(coP))

#ifdef  __cplusplus 
extern "C"{
#endif

/* Initialize */
extern short                /* status, 0=>OK */
dtcc_set_origin
    ( COORD_ORIGIN* coP,    /* (in) pointer to active struct */
      const COORD3* gorg,   /* geographic origin, NULL=>(0,0,0) */
      const COORD3* morg,   /* metric origin, NULL=>(0,0,0) */
      const COORD3* scales, /* scale factors, NULL=>(1,1,1) */
      double        w0,     /* convergence (decimal degrees) */ 
      double        azm     /* Azimuth, east from north [0,360) in degrees */
    ); 
extern short                  /* status, 0=>OK */
dtcc_set_origin_grid
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        northing, /* (in) false northing north of origin(meters) */
      double        easting,  /* (in) false easting east of origin(meters) */
      double        hgt,      /* (in) height at origin (meters) */
      short         zone      /* (in) zone id */
    );
extern short                  /* status, 0=>OK */
dtcc_set_origin_gridSW
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        northing, /* (in) false northing south of origin(meters) */
      double        easting,  /* (in) false easting west of origin (meters) */
      double        hgt,      /* (in) height at origin (meters) */
      short         zone      /* (in) zone id */
    );
extern short                    /* status, 0=>OK */
dtcc_set_origin_geo
    (  COORD_ORIGIN* coP,       /* (in) pointer to active struct */
       double        latitude,  /* (in) latitude of the origin (degrees) */
       double        longitude, /* (in) longitude of the origin (degrees) */
       double        elv        /* (in) height amsl at the origin */
    );
extern short                  /* status, 0=>OK */
dtcc_set_origin_scale
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        k0,       /* (in) scale along meridian at origin */
      double        h0,       /* (in) scale along parallel at origin */
      double        v0        /* (in) vertical scale, usually 1.0 */
    );
extern short                  /* status, 0=>OK */
dtcc_set_origin_convergence
    ( COORD_ORIGIN* cP,       /* (in) pointer to active struct */
      double        w0        /* (in) meridian convergence (degrees) */
    ); 
extern short                  /* status, 0=>OK */
dtcc_set_origin_azimuth
    ( COORD_ORIGIN* cP,       /* (in) pointer to active struct */
      double  azm             /* (in) meridian azimuth (degrees) */
    ); 
                    
/* Write to a formatted stream */
extern short                    /* status, 0=>OK */
dtcc_print_origin
    ( const COORD_ORIGIN* coP,  /* (in) pointer to active struct */
      FILE* fp                  /* (in) open formatted stream */
    );
/* Read from a formatted stream */
extern short                     /* status, 0=>OK */
dtcc_scan_origin
    ( COORD_ORIGIN* coP,         /* (in) pointer to active struct */
      FILE* fp                   /* (in) open formatted stream */
    );
/* Write to an unformatted stream */
extern short                     /* status, 0=>OK */
dtcc_write_origin
    ( const COORD_ORIGIN* coP,   /* (in) pointer to active struct */
      FILE* fp                   /* (in) open unformatted stream */
    );
/* Read from n unformatted stream */
extern short                     /* status, 0=>OK */
dtcc_read_origin
    ( COORD_ORIGIN* coP,         /* (in) pointer to active struct */
      FILE* fp                   /* (in) open unformatted stream */
    );
/* Copy contents */
extern short                     /* status, 0=>OK */
dtcc_copy_origin
    ( COORD_ORIGIN* coP1,         /* (in) pointer to target struct */
      const COORD_ORIGIN* coP2    /* (in) pointer to source struct */
    );

/* Compare origins */
extern short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_grid
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    );
extern short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_geo
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    );
extern short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_scale
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    );
extern short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_convergence
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    );
extern short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_azimuth
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    );

/* Invalidate the structure. */
extern short 
dtcc_invalidate_origin
	(	COORD_ORIGIN* dtccP 
	);
	
/* Validate the structure. */
extern short 
dtcc_validate_origin
	(	COORD_ORIGIN* dtccP 
	);

extern void dtcc_add_origin( const COORD_ORIGIN* csP, double* x, double* y );

extern void dtcc_sub_origin( const COORD_ORIGIN* csP, double* x, double* y );
#if 1 /* Instead of rewriting existing interface */
extern short  dtcc_set_origin_lon0( COORD_ORIGIN*  csP, double v);
extern short  dtcc_set_origin_lat0(COORD_ORIGIN* csP, double v);						
extern short  dtcc_set_origin_fn(COORD_ORIGIN* csP, double v);							
extern short  dtcc_set_origin_fe(COORD_ORIGIN* csP, double v); 						
extern short  dtcc_set_origin_fnSW(COORD_ORIGIN* csP, double v); 					
extern short  dtcc_set_origin_feSW(COORD_ORIGIN* csP, double v); 						
extern short  dtcc_set_origin_K0(COORD_ORIGIN* csP, double v);
extern void dtcc_add_origin( const COORD_ORIGIN* csP, double* x, double* y );
extern void dtcc_sub_origin( const COORD_ORIGIN* csP, double* x, double* y );
#endif /* rewrite */	
						
#ifdef  __cplusplus 
}
#endif

#endif /* _COORDORG_H */


