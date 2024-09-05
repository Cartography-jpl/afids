/* dtcc_extents_ */

#ifndef _COORDEXT_H
#define _COORDEXT_H

#include "dtcc.h"
#include "coord3.h"

typedef struct
   {   COORD3   	mn_grid;    /* Minimum values for each grid coordinate */
       COORD3   	mx_grid;    /* Maximum values for each grid coordinate */ 
       COORD3   	mn_geo;     /* Minimum values for each geo coordinate */
       COORD3   	mx_geo;     /* Maximum values for each geo coordinate */
       short		valid;
       short		op_status;  /* Status code for last operation */
       short		userdef;
   } COORD_EXTENTS;
#define _extents_size	( sizeof(short) + 4*_coord3_size )    

#define _EXTENTS_MIN_GEOP( ceP )          ( (ceP) ? &((ceP)->mn_geo) : NULL )
#define _EXTENTS_MAX_GEOP( ceP )      	  ( (ceP) ? &((ceP)->mx_geo) : NULL )
#define _EXTENTS_MIN_GRIDP( ceP )     	  ( (ceP) ? &((ceP)->mn_grid) : NULL )
#define _EXTENTS_MAX_GRIDP( ceP )     	  ( (ceP) ? &((ceP)->mx_grid) : NULL  )
   
/* Geographic attributes */
#define dtcc_extents_min_geoP(ceP)	  ((const COORD3*)_EXTENTS_MIN_GEOP(ceP))
#define dtcc_extents_max_geoP(ceP)	  ((const COORD3*)_EXTENTS_MAX_GEOP(ceP))
#define dtcc_extents_min_lat( ceP )       \
		dtcc_coord3_lat( dtcc_extents_min_geoP(ceP) ) 
#define dtcc_set_extents_min_lat(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_lat(_EXTENTS_MIN_GEOP(ceP),v)) 
#define dtcc_extents_min_lon( ceP )       \
		dtcc_coord3_lon( dtcc_extents_min_geoP(ceP) ) 
#define dtcc_set_extents_min_lon(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_lon(_EXTENTS_MIN_GEOP(ceP),v)) 
#define dtcc_extents_min_elv( ceP )       \
		dtcc_coord3_elv( dtcc_extents_min_geoP(ceP) ) 
#define dtcc_set_extents_min_elv(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_elv(_EXTENTS_MIN_GEOP(ceP),v)) 
#define dtcc_extents_max_lat( ceP )       \
		dtcc_coord3_lat( dtcc_extents_max_geoP(ceP) ) 
#define dtcc_set_extents_max_lat(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_lat(_EXTENTS_MAX_GEOP(ceP),v)) 
#define dtcc_extents_max_lon( ceP )       \
		dtcc_coord3_lon( dtcc_extents_max_geoP(ceP) ) 
#define dtcc_set_extents_max_lon(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_lon(_EXTENTS_MAX_GEOP(ceP),v)) 
#define dtcc_extents_max_elv( ceP )       \
		dtcc_coord3_elv( dtcc_extents_max_geoP(ceP) ) 
#define dtcc_set_extents_max_elv(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_elv(_EXTENTS_MAX_GEOP(ceP),v)) 

/* Grid attributes */
#define dtcc_extents_min_gridP( ceP )	  ((const COORD3*)_EXTENTS_MIN_GRIDP(ceP))
#define dtcc_extents_max_gridP( ceP )	  ((const COORD3*)_EXTENTS_MIN_GRIDP(ceP))
#define dtcc_extents_min_north( ceP )     dtcc_coord3_north( _EXTENTS_MIN_GRIDP(ceP) ) 
#define dtcc_set_extents_min_north(ceP,v) \
		( _set_invalid(ceP), dtcc_set_coord3_north(_EXTENTS_MIN_GRIDP(ceP),v)) 
#define dtcc_extents_min_east( ceP )      dtcc_coord3_east( _EXTENTS_MIN_GRIDP(ceP) ) 
#define dtcc_set_extents_min_east(ceP,v)  \
		( _set_invalid(ceP), dtcc_set_coord3_east(_EXTENTS_MIN_GRIDP(ceP),v)) 
#define dtcc_extents_min_hgt( ceP )       dtcc_coord3_hgt( _EXTENTS_MIN_GRIDP(ceP) ) 
#define dtcc_set_extents_min_hgt(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_hgt(_EXTENTS_MAX_GRIDP(ceP),v)) 
#define dtcc_extents_min_zone( ceP )      dtcc_coord3_zone( _EXTENTS_MIN_GRIDP(ceP) ) 
#define dtcc_set_extents_min_zone(ceP,v)  \
		( _set_invalid(ceP), dtcc_set_coord3_zone(_EXTENTS_MAX_GRIDP(ceP),v)) 
#define dtcc_extents_max_north( ceP )     dtcc_coord3_north( _EXTENTS_MAX_GRIDP(ceP) ) 
#define dtcc_set_extents_max_north(ceP,v) \
		( _set_invalid(ceP), dtcc_set_coord3_north(_EXTENTS_MAX_GRIDP(ceP),v)) 
#define dtcc_extents_max_east( ceP )      dtcc_coord3_east( _EXTENTS_MAX_GRIDP(ceP) ) 
#define dtcc_set_extents_max_east(ceP,v)  \
		( _set_invalid(ceP), dtcc_set_coord3_east(_EXTENTS_MAX_GRIDP(ceP),v)) 
#define dtcc_extents_max_hgt( ceP )       dtcc_coord3_hgt( _EXTENTS_MAX_GRIDP(ceP) ) 
#define dtcc_set_extents_max_hgt(ceP,v)   \
		( _set_invalid(ceP), dtcc_set_coord3_hgt(_EXTENTS_MAX_GRIDP(ceP),v)) 
#define dtcc_extents_max_zone( ceP )      dtcc_coord3_zone( _EXTENTS_MAX_GRIDP(ceP) ) 
#define dtcc_set_extents_max_zone(ceP,v)  \
		( _set_invalid(ceP), dtcc_set_coord3_zone(_EXTENTS_MAX_GRIDP(ceP),v)) 

#ifdef  __cplusplus 
extern "C"{
#endif

/* Initialization */
extern short                         /* status, 0=>OK */
dtcc_reset_extents
    ( COORD_EXTENTS* ceP,           
      const COORD3* mngridP,         /* minimum grid values */
      const COORD3* mxgridP,         /* maximum grid values */
      const COORD3* mngeoP,          /* minimum geo values */
      const COORD3* mxgeoP           /* maximum geo values */
    );
extern short                         /* status, 0=>OK */
dtcc_set_extents
    ( COORD_EXTENTS* ceP,           
      const COORD3* mngridP,         /* minimum grid values */
      const COORD3* mxgridP,         /* maximum grid values */
      const COORD3* mngeoP,          /* minimum geo values */
      const COORD3* mxgeoP           /* maximum geo values */
    );
    
extern short                        /* status, 0=>OK */
dtcc_set_grid_extents
    ( COORD_EXTENTS* ceP,           
      const double* mnP,            /* minimum grid values */
      const double* mxP             /* maximum grid values */
    );
extern short                        /* status, 0=>OK */
dtcc_set_geo_extents
    ( COORD_EXTENTS* ceP,        
      const double* mnP,            /* minimum geo values */
      const double* mxP             /* maximum geo values */
    ); 
/* Read & write */
extern short                        /* status, 0=>OK */
dtcc_write_extents
    ( const COORD_EXTENTS* ceP,     
      FILE* fp                      /* open unformatted output stream */
    ); 
extern short                        /* status, 0=>OK */
dtcc_read_extents
    ( COORD_EXTENTS* ceP,           
      FILE*          fp             /* open unformatted input stream */
    ); 
/* Print */
extern short                        /* status, 0=>OK */
dtcc_print_extents
    ( const COORD_EXTENTS* ceP,  
      FILE* fp                      /* open unformatted output stream */
    );
extern short                        /* status, 0=>OK */
dtcc_print_geo_extents
    ( const COORD_EXTENTS* ceP,  
      FILE* fp                      /* open unformatted output stream */
    ); 
extern short                        /* status, 0=>OK */
dtcc_print_grid_extents
    ( const COORD_EXTENTS* ceP,  
      FILE* fp                      /* open unformatted output stream */
    ); 
 
/* Copy */
extern short                        /* status, 0=>OK */
dtcc_copy_extents
    ( COORD_EXTENTS* ceP1,          
      const COORD_EXTENTS* ceP2     /* pointer to source struct */
    ); 

/* Compare.  Uses lexicographical ranking */
extern short              /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_grid_extent
	(	const COORD_EXTENTS* ceP1,  /* First extent */        
      	const COORD_EXTENTS* ceP2   /* Second extent */  
    );
extern short              /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_geo_extent
	(	const COORD_EXTENTS* ceP1,  /* First extent */         
      	const COORD_EXTENTS* ceP2   /* Second extent */  
    );
    
/* Inclusion */
extern short             /* 1 if point is inside the extent, 0 otherwise */
dtcc_extents_contains
	(	const COORD_EXTENTS* ceP,   /* Extent to test against */
		const COORD3*	     C3P    /* Point to test */
	);
	
/* Intersection */ 
extern short            /* 1 if the two extents[geo] intersect, 0 otherwise */
dtcc_extents_intersect
	(	const COORD_EXTENTS* ceP1,          
      	const COORD_EXTENTS* ceP2     
    );
extern short            /* 0 => OK  */
dtcc_extents_intersection
	(	const COORD_EXTENTS* ceP1,   /* First extent */       
      	const COORD_EXTENTS* ceP2,   /* Second extent */  
      	COORD_EXTENTS* ceP3          /* Intersection of 1 & 2 */
    );
        
extern short				/* Status, 0=>OK */
dtcc_validate_coord3_extents
	(	COORD_EXTENTS* csP	/* (in) Pointer to the target COORD_SYS struct */
	);
/* Validate the system. Open the sytem if the validation is OK */
extern short				/* Status, 0=>OK */
dtcc_invalidate_coord3_extents
	(	COORD_EXTENTS* csP	/* (in) Pointer to the target COORD_SYS struct */
	);
#if 1 /* Instead of rewriting existng interface */
extern short dtcc_set_extents_north( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_south( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_east( COORD_EXTENTS* csP, double v ) ;	
extern short dtcc_set_extents_west( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_top( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_bottom( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_left( COORD_EXTENTS* csP, double v ); 	
extern short dtcc_set_extents_right( COORD_EXTENTS* csP, double v );
#endif /* rewrite */
			
#ifdef  __cplusplus 
}
#endif

#endif /* _COORDEXT_H */

