/* Functions associated with the COORD_ORIGIN struct */

#include "dtcc.h"
#include "coordorg.h"
extern FILE* fplook;

static short
_check_it( const COORD_ORIGIN* coP )
	{	short flag = 0; 
		
		/* geo */
        if( (dtcc_origin_lat0(coP)>90.0) || (dtcc_origin_lat0(coP)<-90.0) ||
			(dtcc_origin_lon0(coP)>180.0 ) || (dtcc_origin_lon0(coP)<-180.0) )
		/* if( fabs(dtcc_origin_lat0(coP)) > 90.0 || 
			fabs(dtcc_origin_lon0(coP)) > 180.0 ) */
			flag = NUMERIC_NOT_GEO;
		/* grid */
		else
		if( (/*fabs(dtcc_origin_fn(coP)) > 30000000.0*/( (dtcc_origin_fn(coP)>30000000.0) || (dtcc_origin_fn(coP)<-30000000.0) ) && 
				 dtcc_origin_fn(coP) != DOUBLE_NULL ) || 
			(/*fabs(dtcc_origin_fe(coP)) > 30000000.0*/( (dtcc_origin_fe(coP)>30000000.0) || (dtcc_origin_fe(coP)<-30000000.0) ) &&
				 dtcc_origin_fe(coP) != DOUBLE_NULL ) )  
			flag = NUMERIC_TOO_BIG;
		/* scales */
		else
		if( (dtcc_origin_K0(coP) < 0.0 && dtcc_origin_K0(coP) != DOUBLE_NULL) || 
			(dtcc_origin_H0(coP) < 0.0 && dtcc_origin_H0(coP) != DOUBLE_NULL)  )
			flag = NUMERIC_NEGATIVE;
		/* convergence
		else
		if( 0 )
			flag = 1;
		*/
		/* Azimuth */
		else
		if( dtcc_origin_azm0(coP) != DOUBLE_NULL && 
			/*fabs(dtcc_origin_azm0(coP)) > 360.0*/ ((dtcc_origin_azm0(coP)>360.0) || (dtcc_origin_azm0(coP)<-360.0)) )
			flag = NUMERIC_TOO_BIG;
		else
			flag = OK;
		
		if( flag == OK ) _set_valid((COORD_ORIGIN*)coP);
		return flag;	
		 
	}
/* Initialize */
short                                  /* status, 0=>OK */
dtcc_set_origin
    ( COORD_ORIGIN* coP,    /* (in) pointer to active struct */
      const COORD3* gorg,   /* geographic origin, NULL=>(0,0,0) */
      const COORD3* morg,   /* metric origin, NULL=>(0,0,0) */
      const COORD3* sorg,   /* scale factors, NULL=>(1,1,1) */
      double        worg,   /* convergence (degrees) */ 
      double        azm     /* Azimuth, east from north [0,360) in degrees */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_origin_gridSW( coP, 0.0, 0.0, DOUBLE_NULL, SHORT_NULL );
        	if( morg )
               dtcc_set_origin_grid
                    ( coP, dtcc_coord3_north(morg), dtcc_coord3_east(morg), 
                                DOUBLE_NULL, dtcc_coord3_zone(morg) );
           else
               dtcc_set_origin_grid( coP, 0.0, 0.0, DOUBLE_NULL, SHORT_NULL );
        
           if( gorg )
               dtcc_set_origin_geo
                    ( coP, dtcc_coord3_lat(gorg), dtcc_coord3_lon(gorg), 
                                DOUBLE_NULL );
           else
               dtcc_set_origin_geo( coP, 0.0, 0.0, DOUBLE_NULL );
        
           if( sorg  )
               dtcc_set_origin_scale
                    ( coP, dtcc_coord3_x(sorg), dtcc_coord3_y(sorg), 
                                dtcc_coord3_z(sorg) );
           else
               dtcc_set_origin_scale( coP, 1.0, 1.0, 1.0 );
               
           dtcc_set_origin_azimuth( coP, azm );
           dtcc_set_origin_convergence( coP, worg );
           
           k = _check_it(coP);
           if( k == OK ) _set_valid(coP);
           return k;
       }
        return ILLEGAL_ARG;
    }
             
short                         /* status, 0=>OK */
dtcc_set_origin_grid
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        northing, /* (in) false northing (meters) */
      double        easting,  /* (in) false easting (meters) */
      double        hgt,      /* (in) height at origin (meters) */
      short         zone      /* (in) zone id */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_coord3
        		( _ORIGIN_GRIDNEP(coP),
        			easting,northing,hgt,
						(short)(zone==SHORT_NULL ? COORD3_GRID : zone) );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        return ILLEGAL_ARG;
    }
short                         /* status, 0=>OK */
dtcc_set_origin_gridSW
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        northing, /* (in) false northing (meters) */
      double        easting,  /* (in) false easting (meters) */
      double        hgt,      /* (in) height at origin (meters) */
      short         zone      /* (in) zone id */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_coord3
        		( _ORIGIN_GRIDSWP(coP),
        			easting,northing, hgt,zone );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        return ILLEGAL_ARG;
    }
    
short                           /* status, 0=>OK */
dtcc_set_origin_geo
    (  COORD_ORIGIN* coP,       /* (in) pointer to active struct */
       double        latitude,  /* (in) latitude of the origin (degrees) */
       double        longitude, /* (in) longitude of the origin (degrees) */
       double        elv        /* (in) height amsl at the origin */
    )
    {   if( coP )
        { 	short k;
        	_set_invalid(coP);
        	dtcc_set_coord3
        		( _ORIGIN_GEOP(coP),
        		  latitude,longitude,elv,SHORT_NULL );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        else
        	return ILLEGAL_ARG;
    }

short                                   /* status, 0=>OK */
dtcc_set_origin_scale
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        k0,       /* (in) scale along meridian at origin */
      double        h0,       /* (in) scale along parallel at origin */
      double        v0        /* (in) vertical scale, usually 1.0 */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_coord3
        		( _ORIGIN_SCALEP(coP),
        			k0,h0,v0,SHORT_NULL );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        return ILLEGAL_ARG;
    }

short                        /* status, 0=>OK */
dtcc_set_origin_azimuth
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double  azm             /* (in) meridian azimuth (degrees) */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_angle( _ORIGIN_AZIMUTHP(coP), azm );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        else
            return ILLEGAL_ARG;
    }
    
short                       /* status, 0=>OK */
dtcc_set_origin_convergence
    ( COORD_ORIGIN* coP,      /* (in) pointer to active struct */
      double        w0        /* (in) meridian convergence (degrees) */
    )
    {   if( coP )
        {   short k;
        	_set_invalid(coP);
        	dtcc_set_angle( _ORIGIN_CONVERGENCEP(coP), w0 );
        	/* k = _check_it(coP); */
			k = OK;
        	if( k == OK ) _set_valid(coP);
        	return k;
        }
        else
            return ILLEGAL_ARG;
    }
    
/* Write to a formatted stream */
short                                  /* status, 0=>OK */
dtcc_print_origin
    ( const COORD_ORIGIN* coP,  /* (in) pointer to active struct */
      FILE* fp                  /* (in) open formatted stream */
    )
    {   if( coP )
        {   fprintf( fp, "Origin:" );
            fprintf( fp, "\tGrid N & E:  " ); 
            dtcc_print_coord3_grid( dtcc_origin_gridNEP(coP), fp );
            fprintf( fp, "\n\t\tGrid S & W:  " ); 
            dtcc_print_coord3_grid( dtcc_origin_gridSWP(coP), fp );
            fprintf( fp, "\n\t\tGeographic:  " ); 
            dtcc_print_coord3_geo( dtcc_origin_geoP(coP), fp );
            fprintf( fp, "\n\t\tScales:      " ); 
            dtcc_print_coord3_cart( dtcc_origin_scaleP(coP), fp );
            if( dtcc_origin_azm0(coP) != 0.0 )
            {   dtcc_print_angle( dtcc_origin_azimuthP(coP),
                            "\n\t\tAzimuth:     ", fp );               
            }
            if( dtcc_origin_W0(coP) != 0.0 )
            {   dtcc_print_angle( dtcc_origin_convergenceP(coP),
                            "\n\t\tConvergence: ", fp );               
            } 
            fprintf( fp,"\n" );
            return OK;
        }
        return ILLEGAL_ARG;
    }
         
#if 0  /* Not ready for prime time */
/* Read from a formatted stream */
short                            /* status, 0=>OK */
dtcc_scan_origin
    ( COORD_ORIGIN* coP,         /* (in) pointer to active struct */
      FILE* fp                   /* (in) open formatted stream */
    )
    {   if( coP )
        {   dtcc_scan_coord3_grid( DTCC_ORIGIN_GRIDP(coP), nelabel,vlabel, fp );
            dtcc_scan_coord3_geo( DTCC_ORIGIN_GEOP(coP), anglabel,vlabel, fp );
            dtcc_scan_coord3_cart( DTCC_ORIGIN_CARTP(coP), NULL,NULL, fp );
            return OK;
        }
        return ILLEGAL_ARG;
    }
#endif /* ScanOrigin */
/* Write to an unformatted stream */
short                                  /* status, 0=>OK */
dtcc_write_origin
    ( const COORD_ORIGIN* coP,   /* (in) pointer to active struct */
      FILE* fp                   /* (in) open unformatted stream */
    )
    {   if( coP )
        {   short endianFlag = 1;
            fwrite( &endianFlag, 1,sizeof(short), fp );
            dtcc_write_coord3( dtcc_origin_gridNEP(coP), fp );   
            dtcc_write_coord3( dtcc_origin_gridSWP(coP), fp );   
            dtcc_write_coord3( dtcc_origin_geoP(coP), fp );   
            dtcc_write_coord3( dtcc_origin_scaleP(coP), fp );
            dtcc_write_double8( fp, dtcc_origin_conv0(coP) );
            dtcc_write_double8( fp, dtcc_origin_azm0(coP) );
			fwrite( &(coP->valid),1,sizeof(short), fp ); 
			fwrite( &(coP->op_status),1,sizeof(short), fp ); 
			return OK;
        }
        return ILLEGAL_ARG;
    }   
    
/* Read from an unformatted stream */
short                                  /* status, 0=>OK */
dtcc_read_origin
    ( COORD_ORIGIN* coP,         /* (in) pointer to active struct */
      FILE* fp                   /* (in) open unformatted stream */
    )
    {   if( coP )
        {   double x;
            short endian = 1;
            fread( &endian, 1,sizeof(short), fp );
            dtcc_read_coord3( _ORIGIN_GRIDNEP(coP), fp, endian );
            dtcc_read_coord3( _ORIGIN_GRIDSWP(coP), fp, endian );
            dtcc_read_coord3( _ORIGIN_GEOP(coP), fp, endian );   
            dtcc_read_coord3( _ORIGIN_SCALEP(coP), fp, endian );
            x = dtcc_read_double8( fp, endian ); 
            dtcc_set_origin_convergence( coP, x );    
            x = dtcc_read_double8( fp, endian ); 
            dtcc_set_origin_azimuth( coP, x );
            coP->valid = dtcc_read_short( fp, endian ); 
			coP->op_status = dtcc_read_short( fp, endian ); 
			return OK;
        }
        return ILLEGAL_ARG;
    }   

/* Copy contents */
short                                   /* status, 0=>OK */
dtcc_copy_origin
    ( COORD_ORIGIN* coP1,         /* (in) pointer to target struct */
      const COORD_ORIGIN* coP2    /* (in) pointer to source struct */
    )
	{	return blockcopy( coP1, coP2, sizeof(COORD_ORIGIN) );  }
	    
/* Compare origins */
short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_grid
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    )
    {	const COORD3* p1 = dtcc_origin_gridNEP(coP1);
    	const COORD3* p2 = dtcc_origin_gridNEP(coP2);
    	return dtcc_compare_coord3( p1,p2 );
    }
    	
short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_geo
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    )
    {	const COORD3* p1 = dtcc_origin_geoP(coP1);
    	const COORD3* p2 = dtcc_origin_geoP(coP2);
    	return dtcc_compare_coord3( p1,p2 );
    }
    
short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_scale
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    )
    {	const COORD3* p1 = dtcc_origin_scaleP(coP1);
    	const COORD3* p2 = dtcc_origin_scaleP(coP2);
    	return dtcc_compare_coord3( p1,p2 );
    }
short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_convergence
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    )
    {	const ANGLE* p1 = dtcc_origin_convergenceP(coP1);
    	const ANGLE* p2 = dtcc_origin_convergenceP(coP2);
    	return dtcc_compare_angle( p1,p2 );
    }
short                      /* 1<2 => -1, 1==2 => 0, 1>2 => 1 */
dtcc_compare_origin_azimuth
    ( 	const COORD_ORIGIN* coP1, /* (in) pointer to target struct */
      	const COORD_ORIGIN* coP2  /* (in) pointer to source struct */
    )
    {	const ANGLE* p1 = dtcc_origin_azimuthP(coP1);
    	const ANGLE* p2 = dtcc_origin_azimuthP(coP2);
    	return dtcc_compare_angle( p1,p2 );
    }
    
short				/* Status, 0=>OK */
dtcc_validate_origin
	(	COORD_ORIGIN* csP		/* (in) Pointer to the target COORD_SYS struct */
	)
	{	short k = _check_it( csP );
		if( k != OK ) _set_invalid(csP);
		return k;
	}
/* Validate the system. Open the sytem if the validation is OK */
short				/* Status, 0=>OK */
dtcc_invalidate_origin
	(	COORD_ORIGIN* csP		/* (in) Pointer to the target COORD_SYS struct */
	)
	{	short old = csP->valid;
		_set_invalid(csP);  
		return old;   
	}
#if 1 /* Instead of rewriting existing interface */
short  dtcc_set_origin_lon0( COORD_ORIGIN*  csP, double v)
	{	double lat0 = dtcc_origin_lat0(csP), hgt = dtcc_origin_elv(csP);
		return dtcc_set_origin_geo( csP, lat0,v, hgt );
	}
short  dtcc_set_origin_lat0(COORD_ORIGIN* csP, double v)						
	{	double lon0 = dtcc_origin_lon0(csP), hgt = dtcc_origin_elv(csP);
		return dtcc_set_origin_geo( csP, v, lon0, hgt );
	}
short  dtcc_set_origin_fn(COORD_ORIGIN* csP, double v)							
	{	double fe = dtcc_origin_fe(csP), hgt = dtcc_origin_elv(csP);
		short zone = dtcc_origin_zone(csP);
		return dtcc_set_origin_grid( csP, v,fe, hgt, zone );
	}
short  dtcc_set_origin_fe(COORD_ORIGIN* csP, double v) 						
	{	double fn = dtcc_origin_fn(csP), hgt = dtcc_origin_elv(csP);
		short zone = dtcc_origin_zone(csP);
		return dtcc_set_origin_grid( csP, fn, v, hgt, zone );
	}
short  dtcc_set_origin_fnSW(COORD_ORIGIN* csP, double v) 					
	{	double feSW = dtcc_origin_feSW(csP), hgt = dtcc_origin_elv(csP);
		short zone = dtcc_origin_zoneSW(csP);
		return dtcc_set_origin_gridSW( csP, v,feSW, hgt, zone );
	}
short  dtcc_set_origin_feSW(COORD_ORIGIN* csP, double v) 						
	{	double fnSW = dtcc_origin_fnSW(csP), hgt = dtcc_origin_elv(csP);
		short zone = dtcc_origin_zoneSW(csP);
		return dtcc_set_origin_gridSW( csP, fnSW, v, hgt, zone );
	}
short  dtcc_set_origin_K0(COORD_ORIGIN* csP, double v)
	{	double h0 = dtcc_origin_H0(csP), j0 = dtcc_origin_J0(csP);
		return dtcc_set_origin_scale( csP, v, h0, j0 );
	}
void dtcc_add_origin( const COORD_ORIGIN* csP, double* x, double* y )
{	double xx = *x + dtcc_origin_fe(csP), yy = *y + dtcc_origin_fn(csP);
	if( dtcc_origin_fe(csP) != dtcc_origin_feSW(csP) && xx < 0.0 ) 
	{	xx += dtcc_origin_feSW(csP);
		xx = -xx;
	}
	if( dtcc_origin_fn(csP) != dtcc_origin_fnSW(csP) && yy < 0.0 ) 
	{	yy += dtcc_origin_fnSW(csP);
		yy = -yy;
	}
	*x = xx; *y = yy;
	return;
}
void dtcc_sub_origin( const COORD_ORIGIN* csP, double* x, double* y )
{	double xx = *x, yy = *y;
	if( dtcc_origin_fe(csP) != dtcc_origin_feSW(csP) && xx < 0.0 )
		xx = fabs(xx) - dtcc_origin_feSW(csP);
	*x = xx - dtcc_origin_fe(csP);
	if( dtcc_origin_fn(csP) != dtcc_origin_fnSW(csP) && yy < 0.0 )
		yy = fabs(yy) - dtcc_origin_fnSW(csP);
	*y = yy - dtcc_origin_fn(csP);
	return;
}

							
#endif /* rewrite */	
#if 0 /* check */
int main()
{	FILE* fp = stdout;
	double w0=0.01, azm = 270.0;
	COORD3 gorg, morg, scales;
	COORD_ORIGIN co1, *coP, co2;
	coP = &co1;
	
	dtcc_set_coord3_geo( &gorg, 10.0, 20.0, DOUBLE_NULL );
	dtcc_set_coord3_cart( &morg, 10000000.0, 500000.0,DOUBLE_NULL );
	dtcc_set_coord3_cart( &scales, 0.9996, 0.9996, 1.0 );
	dtcc_set_origin( coP, &gorg, &morg, & scales, w0, azm );
	fprintf( fp, "SET: "); dtcc_print_origin( coP, fp );
	fprintf( fp,  "VALID: %d\n", dtcc_is_valid(coP) );
	dtcc_copy_origin( &co2,coP);
	fprintf( fp, "COPY: "); dtcc_print_origin( &co2, fp );
	dtcc_invalidate_origin(coP);
	dtcc_set_origin_grid( coP, 100.0, 10.0, 0.0, 1 );
	dtcc_set_origin_gridSW( coP, -100.0, -10.0, 0.0, 2 );
	dtcc_set_origin_geo( coP, 25.0,50.0,DOUBLE_NULL );
	dtcc_set_origin_scale( coP, 1.0, 0.9, 1.0 );
	dtcc_set_origin_convergence( coP, 0.2 );
	dtcc_set_origin_azimuth(coP, 45.0 );
	fprintf( fp, "SET MEMBERS:\n"); dtcc_print_origin( coP, fp ) ;
    fprintf( fp,  "VALID: %d\n", dtcc_is_valid(coP) );
	dtcc_validate_origin(coP);
	fprintf( fp,  "VALID: %d\n", dtcc_is_valid(coP) );
	
	fprintf( fp, "GET MEMBERS: " );
	fprintf( fp, "\tfn,fe,z: %lf %lf %d    fs,fw,z: %lf %lf %d\n",
			 dtcc_origin_fn( coP ),dtcc_origin_fe( coP ),dtcc_origin_zone( coP ), 
             dtcc_origin_fnSW( coP ),dtcc_origin_feSW( coP ),dtcc_origin_zoneSW( coP ) );
    fprintf( fp, "\tlat,lon: %lf %lf    k0,h0,j0: %lf %lf %lf\n",
    		 dtcc_origin_lat0( coP ),dtcc_origin_lon0( coP ),
    		 dtcc_origin_K0( coP ),dtcc_origin_H0( coP ),dtcc_origin_J0( coP ) );
    fprintf( fp, "\tw0,azm0: %lf %lf\n",dtcc_origin_W0( coP ),dtcc_origin_azm0( coP ) );    

	return 0;
}
#endif /* check */


