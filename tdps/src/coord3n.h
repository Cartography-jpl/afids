/* Array of COORD3 structures */

#ifndef _COORD3N_H
#define _COORD3N_H

#include "coord3.h"

typedef struct
    {   COORD3*      dataP;  	/* Data */ 
        long         n;      	/* Number of Coordinates in the array */
        short   	 type;	 	/* Coordinate type (cart,geo,grid) */
        short		 id;		/* COORD3N id number */
    } COORD3N;
#define _COORD3N_DATAP( c3NP )     ( (c3NP)->dataP )
#define _COORD3N_KP( c3NP,k )         \
       ( (k) >= 0 && (k) <(c3NP)->n ? (c3NP)->dataP + (k) : NULL )

#define dtcc_coord3N_type( c3NP )      ((short)( (c3NP) ? (c3NP)->type : SHORT_NULL ))
#define dtcc_coord3N_count( c3NP )     ( (c3NP) ? (c3NP)->n : SHORT_NULL  )

/* (x,y,z) (lat,lon,elv), (north,east,hgt) */    
#define dtcc_coord31NK( c3NP,k )        dtcc_coord31( _COORD3N_KP( c3NP,k ) ) 
#define dtcc_coord32NK( c3NP,k )        dtcc_coord32( _COORD3N_KP( c3NP,k ) ) 
#define dtcc_coord33NK( c3NP,k )        dtcc_coord33( _COORD3N_KP( c3NP,k ) ) 
 
/* Convenience calls */
#define dtcc_coord3NK_lat( c3NP,k )		 dtcc_coord31NK( c3NP,k )
#define dtcc_coord3NK_lon( c3NP,k )		 dtcc_coord32NK( c3NP,k )
#define dtcc_coord3NK_elv( c3NP,k )		 dtcc_coord33NK( c3NP,k )
#define dtcc_coord3NK_x( c3NP,k )		 dtcc_coord31NK( c3NP,k )
#define dtcc_coord3NK_y( c3NP,k )		 dtcc_coord32NK( c3NP,k )
#define dtcc_coord3NK_z( c3NP,k )		 dtcc_coord33NK( c3NP,k )
#define dtcc_coord3NK_north( c3NP,k )	 dtcc_coord31NK( c3NP,k )
#define dtcc_coord3NK_east( c3NP,k )	 dtcc_coord32NK( c3NP,k )
#define dtcc_coord3NK_hgt( c3NP,k )		 dtcc_coord33NK( c3NP,k )
#define dtcc_coord3NK_zone( c3NP,k )    \
        dtcc_coord3_zone( _COORD3N_KP( c3NP,k ) )

#define dtcc_is_coord3N_valid( c3NP )   \
    ( !(c3NP) ? FALSE : \
    ( _COORD3N_DATAP( c3NP ) != NULL && dtcc_coord3N_count(c3NP) <= 0 ? FALSE : \
    ( _COORD3N_DATAP( c3NP ) == NULL && dtcc_coord3N_count(c3NP) != 0 ? FALSE : \
                 TRUE ))) 

#ifdef  __cplusplus 
extern "C"{
#endif

/* Create N COORD3 structs of type T */
extern short            /* status code, 0=>OK */
dtcc_initialize_coord3N
    ( COORD3N* c3NP,    /* (out) pointer to the active struct */
      long N           /* (in) Number of structures to create on heap */
    );
/* Clear structure - release the buffer  */
extern short 
dtcc_clear_coord3N
    ( COORD3N* c3P        /* (in) Pointer to active struct. */
    );
            
/* Copy the data to another COORD3N.  Target's data, if it has any, is released   */
extern short                          /* status code. 0=>OK */
dtcc_copy_coord3N
    ( COORD3N* c3NP1,          /* (in) Pointer to the target COORD3N */
      const COORD3N* c3NP2     /* (in) Pointer to the source COORD3N */
    );        

/* Read from unformatted stream */
extern short                    /* status code. 0=>OK */
dtcc_read_coord3N
	( COORD3N* c3NP,      		/* (in/out) Pointer to active structure */
      FILE* fp                  /* (in) Pointer to open formatted stream */
    );
        
/* Write to unformatted stream */
extern short                    /* status code. 0=>OK */
dtcc_write_coord3N
	( const COORD3N* c3NP,    	/* (in/out) Pointer to active structure */
      FILE* fp                  /* (in) Pointer to open formatted stream */
    );      

extern short dtcc_extract_coord3NK( COORD3N* c3NP, short k,  COORD3* c3P );
        		
/* Print by type (Cartesian, geographic, or grid) to formatted stream.  */
extern short                    /* status code. 0=>OK */
dtcc_print_coord3N
	(  const COORD3N* c3NP,     /* (in) Pointer to active structure */
       const char* label,       /* Label */
       short  nline,            /* number of points / line  */
       FILE* fp                 /* (in) Pointer to open formatted stream */
    );
/* Add two COORD3N arrays */
extern short                           /* status code. 0=>OK */
dtcc_add_coord3N
    (    const COORD3N* a1,             
         const COORD3N* a2, 
         COORD3N* diff 
    );
/* Subtract two COORD3N arrays */
extern short                           /* status code. 0=>OK */
dtcc_subtract_coord3N
    (    const COORD3N* a1,             
         const COORD3N* a2, 
         COORD3N* diff 
    );
                                                    
#ifdef  __cplusplus 
}
#endif

#define dtcc_copy_coord3NK( c3NP, c3P, k )   \
        dtcc_copy_coord3( _COORD3N_KP( c3NP, k ), c3P )
#define dtcc_read_coord3NK( c3NP, k, fp )  \
        dtcc_read_coord3( _COORD3N_KP(c3NP,k), fp )
#define dtcc_write_coord3NK( c3NP, k, fp ) \
        dtcc_write_coord3( _COORD3N_KP(c3NP,k), fp )

/* Set the values by type (Cartesian, geographic, or grid )*/                       
/* Store data in the kth point */
#define dtcc_set_coord3NK( c3NP, k,u,v,w,z )    \
        dtcc_set_coord3( _COORD3N_KP(c3NP,k), u,v,w, z )
#define dtcc_set_coord3NK_geo( c3NP, k,u,v,w )    \
        dtcc_set_coord3( _COORD3N_KP(c3NP,k), u,v,w, SHORT_NULL )
#define dtcc_set_coord3NK_cart( c3NP, k,u,v,w )    \
        dtcc_set_coord3( _COORD3N_KP(c3NP,k), u,v,w, SHORT_NULL )
#define dtcc_set_coord3NK_grid( c3NP, k,u,v,w,z )    \
        dtcc_set_coord3( _COORD3N_KP(c3NP,k), u,v,w, z )
#define dtcc_store_coord3NK( c3NP, k,c3P )    \
        dtcc_set_coord3( _COORD3N_KP(c3NP,k), \
        	dtcc_coord31(c3P),dtcc_coord32(c3P),\
        		dtcc_coord33(c3P), dtcc_coord3_zone(c3P) )
/* Get the values by type (Cartesian, geographic, or grid )*/                       
/* Extract data in the kth point */
#define dtcc_coord3NKP( c3NP, k ) \
		((const COORD3*)_COORD3N_KP( c3NP,k ))
#define dtcc_get_coord3NK( c3NP, k, uP,vP,wP,zP )    \
		dtcc_get_coord3( _COORD3N_KP(c3NP,k), uP,vP,wP, zP )
#define dtcc_print_coord3NK( c3NP, k, fp )  \
        dtcc_print_coord3( _COORD3N_KP(c3NP,k),fp )

#define dtcc_initialize_coord3N_geo( c3NP, N ) \
		dtcc_initialize_coord3N( c3NP, N )
#define dtcc_initialize_coord3N_cart( c3NP, N ) \
		dtcc_initialize_coord3N( c3NP, N )
#define dtcc_initialize_coord3N_grid( c3NP, N ) \
		dtcc_initialize_coord3N( c3NP, N )

#endif /* _COORD3N_H  */

