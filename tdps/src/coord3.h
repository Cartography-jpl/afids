/* 3D Coordinate structure */
#ifndef _COORD3_H                                                                       
#define _COORD3_H

#include "dtcc.h"
#include <stdio.h>

			
typedef union
    {	double	c[3];
    	char	a[24];
    } coord3_data;
       
typedef struct
    {   coord3_data data;
    	short zone;
    } COORD3;
#define _coord3_size	( 3*sizeof(IEEE_DOUBLE8) + sizeof(short) )

#ifdef  __cplusplus 
extern "C"{
#endif

extern short dtcc_set_coord3K( COORD3* c3P, short k, double v ); 
extern short dtcc_set_coord3Z( COORD3* c3P, short v );
extern short dtcc_check_coord3K( const COORD3* c3P, short k );
#ifdef  __cplusplus 
}
#endif

#define dtcc_coord3A( c3P )   \
		((const char*)(  dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) ? \
			(c3P)->data.a : NULL ))
#define dtcc_coord3K( c3P,k )        \
		( !dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) ? \
			(c3P)->data.c[k] : DOUBLE_NULL )

#define dtcc_coord31( c3P )          dtcc_coord3K( c3P,0 )
#define dtcc_set_coord31( c3P,v )	 dtcc_set_coord3K( c3P, 0, v )
#define dtcc_coord32( c3P )          dtcc_coord3K( c3P,1 )
#define dtcc_set_coord32( c3P,v )	 dtcc_set_coord3K( c3P, 1, v )
#define dtcc_coord33( c3P )          dtcc_coord3K( c3P,2 )
#define dtcc_set_coord33( c3P,v )	 dtcc_set_coord3K( c3P, 2, v )
#define dtcc_coord3Z( c3P )          ( (c3P) ? (c3P)->zone : SHORT_NULL )

/* Encoded coordinates */
#define dtcc_coord3_mgrsP(c3P)		dtcc_coord3A( c3P )
#define dtcc_coord3_grefP(c3P)		dtcc_coord3A( c3P )
#define dtcc_coord3DataP( c3P )        \
	( !(c3P) ? NULL : ( !dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) ? \
			(void*)((c3P)->data.c) : (void*)dtcc_coord3A( c3P ) ))
       
/* (x,y,z) right-handed Cartesian */    
#define dtcc_coord3_x( c3P )          dtcc_coord31( c3P )
#define dtcc_set_coord3_x( c3P,v )	  dtcc_set_coord31( c3P,v )
#define dtcc_coord3_y( c3P )          dtcc_coord32( c3P )
#define dtcc_set_coord3_y( c3P,v )	  dtcc_set_coord32( c3P,v )
#define dtcc_coord3_z( c3P )          dtcc_coord33( c3P )
#define dtcc_set_coord3_z( c3P,v )	  dtcc_set_coord33( c3P,v )

/* (lat,lon,h/azm) left-handed spherical */
#define dtcc_coord3_lat( c3P )        dtcc_coord31( c3P )
#define dtcc_set_coord3_lat( c3P,v )  dtcc_set_coord31( c3P,v )
#define dtcc_coord3_lon( c3P )        dtcc_coord32( c3P )
#define dtcc_set_coord3_lon( c3P,v )  dtcc_set_coord32( c3P,v )
#define dtcc_coord3_elv( c3P )        dtcc_coord33( c3P )
#define dtcc_set_coord3_elv( c3P,v )  dtcc_set_coord33( c3P,v )

/* (north,east,hgt,zone) UTM */
#define dtcc_coord3_north( c3P )      dtcc_coord32( c3P )
#define dtcc_set_coord3_north(c3P,v)  dtcc_set_coord32( c3P,v )
#define dtcc_coord3_east( c3P )       dtcc_coord31( c3P )
#define dtcc_set_coord3_east(c3P,v)   dtcc_set_coord31( c3P,v )
#define dtcc_coord3_hgt( c3P )        dtcc_coord33( c3P )
#define dtcc_set_coord3_zone(c3P,v)   dtcc_set_coord3Z( c3P,v )
#define dtcc_coord3_zone( c3P )       (short)dtcc_coord3Z(c3P)
#define dtcc_set_coord3_hgt(c3P,v)    dtcc_set_coord33( c3P,v )

#define dtcc_coord3_dst( c3P )        dtcc_coord31( c3P )
#define dtcc_set_coord3_dst(c3P,v)    dtcc_set_coord31( c3P,v )
#define dtcc_coord3_azm( c3P )        dtcc_coord32( c3P )
#define dtcc_set_coord3_azm(c3P,v)    dtcc_set_coord32( c3P,v )

#ifdef  __cplusplus 
extern "C"{
#endif

/* Copy the data to another COORD3 */
extern short              /* status code. 0=>OK */
dtcc_copy_coord3
    ( COORD3* c1P,        /* (in) Pointer to the target COORD3 */
      const COORD3* c2P   /* (in) Pointer to the source COORD3 */
     );        

/* Read from unformatted stream */
extern short             /* status code. 0=>OK */
dtcc_read_coord3
    ( COORD3* c3P,       /* (in/out) Pointer to active structure */
      FILE* fp,          /* (in) Pointer to open formatted stream */
      short endian		 /* 1 <=> do not swap */
    );
                
/* Write to unformatted stream */
extern short               /* status code. 0=>OK */
dtcc_write_coord3
    ( const COORD3* c3P,   /* (in/out) Pointer to active structure */
      FILE* fp             /* (in) Pointer to open formatted stream */
    );      

/* Set the values by type (Cartesian, geographic, or grid )*/                       
extern short              /* status code. 0=>OK */
dtcc_set_coord3
	( 	COORD3* c3P,      /* (in/out) Pointer to active structure */
        double u,         /* (in) X value */
        double v,         /* (in) Y value */
        double w,         /* (in) Z value */
        short  zone       /* id */
    );

extern short              /* status code. 0=>OK */
dtcc_set_coord3A
	( 	COORD3* c3P,      /* (in/out) Pointer to active structure */
        const char* s,    /* (in) encoded value */
        short  zone       /* id */
    );
#ifdef  __cplusplus 
}
#endif

#define dtcc_init_coord3( c3P )	\
		dtcc_set_coord3( c3P, 0.0,0.0,0.0, COORD3_UNDEFINED ) 
#define dtcc_set_coord3_cart( c3P, u,v,w )    \
       dtcc_set_coord3( c3P, u,v,w, COORD3_XYZ )
#define dtcc_set_coord3_dazm( c3P, u,v,w )    \
       dtcc_set_coord3( c3P, u,v,w, COORD3_DAZM )
#define dtcc_set_coord3_geo( c3P, u,v,w )     \
       dtcc_set_coord3( c3P, u,v,w, COORD3_GEO )
#define dtcc_set_coord3_grid( c3P, u,v,w, k ) \
       dtcc_set_coord3( c3P, u,v,w,\
       	((short)( (k) <= COORD3_UTM && (k) >= COORD3_UPS ? (k) : COORD3_GRID ) ))

#define dtcc_set_coord3_MGRS( c3P, mgr )	dtcc_set_coord3A( c3P, mgr, COORD3_MGRS )
#define dtcc_set_coord3_mgrs( c3P, mgr )	dtcc_set_coord3A( c3P, mgr, COORD3_MGRS )
#define dtcc_set_coord3_gref( c3P, gr )		dtcc_set_coord3A( c3P, gr, COORD3_GREF )
#define dtcc_set_coord3_dms( c3P, aslat, aslon, h ) \
		dtcc_set_coord3_geo( c3P, dtcc_decode_dms(aslat,NULL), dtcc_decode_dms(aslon,NULL), h )
#define dtcc_set_coord3_dm( c3P, aslat, aslon, h ) \
		dtcc_set_coord3_geo( c3P, dtcc_decode_dm(aslat,NULL), dtcc_decode_dm(aslon,NULL), h )

#ifdef  __cplusplus 
extern "C"{
#endif

/* Get the values by type (Cartesian, geographic, or grid )*/                       
extern short               /* status code. 0=>OK */
dtcc_get_coord3
	( 	const COORD3* c3P, /* (in/out) Pointer to active structure */
        double* u,         /* (out) X value */
        double* v,         /* (out) Y value */
        double* w,         /* (out) Z value */
        short*  zone       /* (out)  id */
    );
extern short
dtcc_get_coord3A
	( 	const COORD3* c3P, /* (in/out) Pointer to active structure */
        char* s,           /* (out) encoded value */
        short*  zone       /* (out)  id  */
    );
/* Write by type (Cartesian, geographic, or grid) to formatted stream */
extern short                     /* status code. 0=>OK */
dtcc_print_coord3
	( 	const COORD3* c3P,       /* (in) Pointer to active structure */
        FILE* fp                 /* (in) Pointer to open formatted stream */
            );
extern short dtcc_scan_coord3
	( COORD3* pt, COORD3_TYPES type, char* comment, int n, FILE* fp  );

/* Swap bytes */
extern short                     /* status code. 0=>OK */
dtcc_swab_coord3
	( COORD3* pt                /* (in) Pointer to active structure */
    );

#ifdef  __cplusplus 
}
#endif

#define dtcc_get_coord3_cart( c3P, u,v,w )    \
       dtcc_get_coord3( c3P, u,v,w, NULL )
#define dtcc_get_coord3_geo( c3P, u,v,w )     \
       dtcc_get_coord3( c3P, u,v,w, NULL )
#define dtcc_get_coord3_grid( c3P, u,v,w, k ) \
       dtcc_get_coord3( c3P, u,v,w,k )       
       
#define dtcc_get_coord3_MGRS( c3P, mgr )	dtcc_get_coord3A( c3P, mgr, NULL )
#define dtcc_get_coord3_mgrs( c3P, mgr )	dtcc_get_coord3A( c3P, mgr, NULL )
#define dtcc_get_coord3_gref( c3P, gr )		dtcc_get_coord3A( c3P, gr, NULL )
                    
#define dtcc_print_coord3_cart( c3P, fp )  dtcc_print_coord3( c3P, fp )
#define dtcc_print_coord3_geo(  c3P, fp )  dtcc_print_coord3( c3P, fp )
#define dtcc_print_coord3_grid( c3P, fp )  dtcc_print_coord3( c3P, fp )

#define dtcc_swab_coord3_cart( c3P )    dtcc_swab_coord3( c3P )
#define dtcc_swab_coord3_geo( c3P )     dtcc_swab_coord3( c3P )
#define dtcc_swab_coord3_grid( c3P )    dtcc_swab_coord3( c3P )

#ifdef  __cplusplus 
extern "C"{
#endif

/* Standard norms */
extern double                     /* status code. 0=>OK */
dtcc_coord3_manhattan( const COORD3* c3P );
extern double                     /* status code. 0=>OK */
dtcc_coord3_euclidean( const COORD3* c3P );
		 
/* Compare two coordinates. Uses a Manhattan norm*/
extern short                     /* 1<2 => -1, 1==2 => 0, 1>2 => l */
dtcc_compare_coord3
	( 	const COORD3* pt1,       /* (in) Pointer to active structure */
		const COORD3* pt2
    ); 
    
/* Get difference between two points  */
/* c1 - c2 -> c3 */
extern short                        /* status code. 0=>OK */
dtcc_subtract_coord3
	( 	const COORD3* c3P1,         /* (in) Pointer to COORD3 structure */
        const COORD3* c3P2,         /* (in) Pointer to COORD3 structure */
        COORD3* diffP               /* (out) Pointer to COORD3 difference */
   );
/* Get sum of two points  */
/* c1 + c2 -> c3 */
extern short                        /* status code. 0=>OK */
dtcc_add_coord3
	( 	const COORD3* c3P1,         /* (in) Pointer to COORD3 structure */
        const COORD3* c3P2,         /* (in) Pointer to COORD3 structure */
        COORD3* diffP               /* (out) Pointer to COORD3 difference */
    );
extern int dtcc_scale_coord3( const COORD3* inc3P, double s, COORD3* outc3P );
extern int dtcc_format_coord3( const COORD3* c3P,char* bfr );

#ifdef  __cplusplus 
}
#endif

#define dtcc_subtract_coord3_cart( c3P1, c3P2, sumP ) \
        dtcc_subtract_coord3( c3P1, c3P2, sumP )
#define dtcc_subtract_coord3_geo( c3P1, c3P2, sumP ) \
        dtcc_subtract_coord3( c3P1, c3P2, sumP )
#define dtcc_subtract_coord3_grid( c3P1, c3P2, sumP ) \
        dtcc_subtract_coord3( c3P1, c3P2, sumP )

#define dtcc_add_coord3_cart( c3P1, c3P2, sumP ) \
        dtcc_add_coord3( c3P1, c3P2, sumP )
#define dtcc_add_coord3_geo( c3P1, c3P2, sumP ) \
        dtcc_add_coord3( c3P1, c3P2, sumP )
#define dtcc_add_coord3_grid( c3P1, c3P2, sumP ) \
        dtcc_add_coord3( c3P1, c3P2, sumP )

#define dtcc_encode_georef( c3P, encoded, ndecs )	\
		encode_georef( dtcc_coord3_lat(c3P), dtcc_coord3_lon(c3P), ndecs, encoded )
	
#endif /* _COORD3_H  */

