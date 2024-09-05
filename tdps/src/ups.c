/* Universal Polar Stereographic grid engine */   

#include "projctn.h"
#include "psfns.h"

short map_projection_status( short n)
{	return	 ( n == -1 ? MEMORY_ALLOCATION_FAILURE :
			 ( n == -2 ? DATA_OUT_OF_RANGE : n ) );
}

extern short                    /* status, 0=>OK */
UPSEngine
    ( const PROJ_PARAMS* proP,  /* (in) Projection info */
      short mode,               /* 0 => to grid, 2 => to geo, etc */
      const COORD3* inptP,      /* (in) input point or array (degs or meters) */ 
      COORD3* toP         		/* (out) output point or array (degs or meters) */ 
    );

short                           /* status, 0=>OK */
UPSEngine
    ( const PROJ_PARAMS* proP,  /* (in) Projection info */
      short mode,               /* 0 => to grid, 2 => to geo, etc */
      const COORD3* inptP,      /* (in) input point or array (degs or meters) */ 
      COORD3* toP         		/* (out) output point or array (degs or meters) */ 
    )
/* Snyder, John P., MAP PROJECTIONS, A WORKING MANUAL. */
/* US Government Printing Office 1987 */
/* Stereographic Projection, p 38-47.  */    
{   int zone = 0;
	short status = 0;
	double x,y, phi,lam;
    
    if( mode == DTCC_PROJECTION_SETUP || PROJ_ID(dtcc_projection_constantsP(proP)) == 0 )
	{	double fe = 2000000.0, fn = 2000000.0;
		double fs = fn, fw = fe;
		double latorg = dtcc_projection_lat0(proP);
		COORD_ORIGIN* coP = (COORD_ORIGIN*)(&proP->origin);
		dtcc_set_origin_grid( coP, fn,fe, DOUBLE_NULL, SHORT_NULL ); 
		dtcc_set_origin_gridSW( coP, fs,fw, DOUBLE_NULL, SHORT_NULL ); 
		dtcc_set_origin_scale( coP, 0.994, DOUBLE_NULL, DOUBLE_NULL );
		if( latorg == DOUBLE_NULL ) latorg = 90.0; 
		dtcc_set_origin_geo( coP, latorg, 0.0, DOUBLE_NULL );
		init_ups( (void*)dtcc_projection_constantsP(proP), 
					dtcc_projection_axis(proP), dtcc_projection_rflat(proP), 
						latorg );
		if( DTCC_PROJECTION_SETUP == mode ) return OK;
    }       	
            			     	   
	switch (mode)
    {   case DTCC_PROJECTION_FORWARD:
           	phi = dtcc_coord3_lat(inptP);
           	lam = dtcc_coord3_lon(inptP);
           	status = geo_to_ups( dtcc_projection_constantsP(proP), phi,lam, &x, &y );
           	if( status >= OK )
				dtcc_set_coord3_grid
					( toP, x,y, dtcc_coord3_elv(inptP), zone );
			break;              

        case DTCC_PROJECTION_INVERSE:
			x = dtcc_coord3_east(inptP);
			y = dtcc_coord3_north(inptP); 
         	status = ups_to_geo( dtcc_projection_constantsP(proP), x,y, &phi,&lam );  
            dtcc_set_coord3_geo
				( toP, phi,lam, dtcc_coord3_hgt(inptP) );
			break;
        
        case DTCC_PROJECTION_SCALEFACTOR:
        	phi = dtcc_coord3_lat(inptP);
           	lam = dtcc_coord3_lon(inptP);
           	status = ups_scale( dtcc_projection_constantsP(proP), phi,lam, &x, &y ); 
            dtcc_set_coord3_cart( toP, x,y, 1.0 );
			break;
		
     	case DTCC_PROJECTION_LIMITS:
			ups_limits( dtcc_projection_constantsP(proP), &y,&x, &phi,&lam );
			dtcc_set_coord3_geo( toP, y,x,DOUBLE_NULL );
			dtcc_set_coord3_geo( toP+1, phi,lam,DOUBLE_NULL );
			status = 0;
			break;

		default:
            status = UNKNOWN_ITEM_REQUESTED;
            break;
    }
    status = map_projection_status(status);
    return status;
}


