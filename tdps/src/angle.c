/* Angle functions */

#include "angle.h"

#define EPSILON_ANG (0.000000000001)

#define THIS_ID	(1)

double dtcc_angle_tan( const ANGLE* angP )
    {   if( fabs(dtcc_angle_cos(angP)) > EPSILON_ANG )
            return dtcc_angle_sin(angP)/dtcc_angle_cos(angP);
        else
            return DOUBLE_NULL;
    }

/* Initialize (0.0 degrees ) */
short
dtcc_initialize_angle
	(	ANGLE* aP
	)
	{	dtcc_angle_value(aP) = 0.0;
    	dtcc_angle_sin(aP) = 0.0;
        dtcc_angle_cos(aP) = 1.0;
        return OK; 
    }
/* Set by value (decimal degrees) */
short                     /* status, 0=>OK */
dtcc_set_angle
    ( ANGLE* aP,          /* pointer to Angle */
      double v            /* value in decimal degrees */
    )
    {   dtcc_angle_value(aP) = v;
    	if( v > DOUBLE_NULL )
    	{	v *= RADDEG;
        	dtcc_angle_sin(aP) = sin(v);
        	dtcc_angle_cos(aP) = cos(v);
        }
        else
        {	dtcc_angle_sin(aP) = DOUBLE_NULL;
        	dtcc_angle_cos(aP) = DOUBLE_NULL;
        }
        return OK;         
    }
    
/* Set by sine */
short                  /* status, 0=>OK */
dtcc_set_angle_sin
    ( ANGLE* aP,       /* pointer to Angle */
      double sv        /* sine of angle */
    )
    {   if( fabs(sv) <= 1.0 )
    	{	dtcc_angle_value(aP) = asin(sv);
        	dtcc_angle_sin(aP) = sv;
        	dtcc_angle_cos(aP) = cos(dtcc_angle_value(aP));
        	dtcc_angle_value(aP) /= RADDEG;  /* convert to decimal degrees */
        }
        else
        {	dtcc_angle_value(aP) = DOUBLE_NULL;
        	dtcc_angle_sin(aP) = DOUBLE_NULL;
        	dtcc_angle_cos(aP) = DOUBLE_NULL;
        }	
        return OK; 
    }
    
/* Set by cosine */
short                  /* status, 0=>OK */
dtcc_set_angle_cos
    ( ANGLE* aP,       /* pointer to Angle */
      double cv        /* cosine of angle */
    )
    {   if( fabs(cv) <= 1.0 )
    	{	dtcc_angle_value(aP) = acos(cv);
        	dtcc_angle_sin(aP) = sin(dtcc_angle_value(aP));
        	dtcc_angle_cos(aP) = cv;
        	dtcc_angle_value(aP) /= RADDEG;  /* convert to decimal degrees */
        }
        else
        {	dtcc_angle_value(aP) = DOUBLE_NULL;
        	dtcc_angle_sin(aP) = DOUBLE_NULL;
        	dtcc_angle_cos(aP) = DOUBLE_NULL;
        }	
        return OK; 
    }
    
/* Set by tangent */
short                  /* status, 0=>OK */
dtcc_set_angle_tan
    ( ANGLE* aP,       /* pointer to Angle */
      double tv        /* tangent of angle */
    )
    {   if( tv != DOUBLE_NULL )
    	{	dtcc_angle_value(aP) = atan(tv);
        	dtcc_angle_cos(aP) = cos(dtcc_angle_value(aP));
        	dtcc_angle_sin(aP) = tv*dtcc_angle_cos(aP);
        	dtcc_angle_value(aP) /= RADDEG;  /* convert to decimal degrees */
       	}
        else
        {	dtcc_angle_value(aP) = DOUBLE_NULL;
        	dtcc_angle_sin(aP) = DOUBLE_NULL;
        	dtcc_angle_cos(aP) = DOUBLE_NULL;
        }	
        return OK; 
    }
           
/* Set by ratio */
short                 /* status, 0=>OK */
dtcc_set_angle_tan2
    ( ANGLE* aP,      /* pointer to Angle */
      double dx,      /* dx */
      double dy       /* dy, same units as dx */
    )
    {   if( dx != DOUBLE_NULL && dy != DOUBLE_NULL )
    	{	dtcc_angle_value(aP) = atan2( dx,dy );
        	dtcc_angle_sin(aP) = sin(dtcc_angle_value(aP));
        	dtcc_angle_cos(aP) = cos(dtcc_angle_value(aP));
        	dtcc_angle_value(aP) /= RADDEG;  /* convert to decimal degrees */
        }
        else
        {	dtcc_angle_value(aP) = DOUBLE_NULL;
        	dtcc_angle_sin(aP) = DOUBLE_NULL;
        	dtcc_angle_cos(aP) = DOUBLE_NULL;
        }	
        return OK; 
    }

/* IO */
short                   /* status, 0=>OK */
dtcc_print_angle
    ( const ANGLE* aP,  /* pointer to Angle */
      const char* lbl,  /* label, NULL=> no lanel */
      FILE* fp          /* open formatted output stream */
    )
    {   if( !fp )
            return FILE_NOT_OPEN;
        else
        {   if( lbl ) fprintf( fp, "%s ", lbl );
            if( dtcc_angle_value(aP) != DOUBLE_NULL )
            	fprintf( fp,"%f", dtcc_angle_value(aP) );
            else
            	fprintf( fp, "nd" );
            return OK;
        }
    }
    
#if 0 /* Not ready for prime time */            
short                    /* status, 0=>OK */
dtcc_scan_angle
    ( ANGLE* aP,         /* pointer to Angle */ 
      FILE* fp           /* open formatted, input stream */
    
#endif /* ScanAngle */
            
short                    /* status, 0=>OK */
dtcc_read_angle
    ( ANGLE* aP,         /* pointer to Angle */ 
      FILE* fp,          /* open unformatted, input stream */
      short endian		 /* 1 <=> do not swap */
    )
    {   if( fp )
            return FILE_NOT_OPEN;
        else
        {   double x = dtcc_read_double8( fp, endian );
            dtcc_set_angle( aP, x );
        	return OK;
        }
    }           
short                   /* status, 0=>OK */
dtcc_write_angle
    ( ANGLE* aP,        /* pointer to Angle */ 
      FILE* fp          /* open unformatted, output stream */
    )           
    {   if( !fp )
            return FILE_NOT_OPEN;
        else
            return dtcc_write_double8( fp,dtcc_angle_value(aP) );
    }           

/* Copy an angle */
short                    /* status, 0=>OK */
dtcc_copy_angle
    ( ANGLE* dP,         /* pointer to destination Angle */ 
      const ANGLE* sP    /* pointer to source Angle */
    )
	{	return blockcopy( dP, sP, sizeof(ANGLE) );  }


#if 0
/* Rotate (x,y) into (u,v) */
short                  /* status, 0=>OK */ 
dtcc_xy_rotate_angle
    ( const ANGLE* aP, /* pointer to Angle */
      double x,        /* x value to be rotated */
      double y,        /* y value to be rotated */
      double* u,       /* rotated x */
      double* v        /* rotated y */
    )
    {   /* Allow rotation in place */
        double rx = x*dtcc_angle_cos(aP) - y*dtcc_angle_sin(aP);
        double ry = x*dtcc_angle_sin(aP) + y*dtcc_angle_cos(aP);
        *u = rx; *v = ry;
        return OK;
    }
#endif /* 0 */    
