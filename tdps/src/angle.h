/* Angle declaration */

#ifndef _ANGLE_H
#define _ANGLE_H

#include "dtcc.h"
#include <stdio.h>

typedef struct
    {   double  dd;    	 	/* value of the angle in decimal degrees */
        double  sinv;     	/* sine of the angle */
        double  cosv;     	/* cosine of the angle */
        short	id;
    } ANGLE;
#define dtcc_angle_value(aP)     ( (aP)->dd )    
#define dtcc_angle_sin(aP)       ( (aP)->sinv )    
#define dtcc_angle_cos(aP)       ( (aP)->cosv )
#define dtcc_angle_rads(aP)      ( dtcc_angle_value(aP)*RADDEG )    
    
#ifdef  __cplusplus 
extern "C"{
#endif

extern double dtcc_angle_tan( const ANGLE* aP );

/* Initialize (0.0 degrees ) */
extern short
dtcc_initialize_angle
	(	ANGLE* aP
	);
/* Set by value (degrees) */
extern short              /* status, 0=>OK */
dtcc_set_angle
    ( ANGLE* aP,          /* pointer to Angle */
      double v            /* value in degrees */
    );
/* Set by sine */
extern short           /* status, 0=>OK */
dtcc_set_angle_sin
    ( ANGLE* aP,       /* pointer to Angle */
      double sv        /* sine of angle */
    );
/* Set by cosine */
extern short          /* status, 0=>OK */
dtcc_set_angle_cos
    ( ANGLE* aP,       /* pointer to Angle */
      double cv        /* cosine of angle */
    );
/* Set by tangent */
extern short           /* status, 0=>OK */
dtcc_set_angle_tan
    ( ANGLE* aP,       /* pointer to Angle */
      double tv        /* tangent of angle */
    );
/* Set by ratio */
extern short          /* status, 0=>OK */
dtcc_set_angle_tan2
    ( ANGLE* aP,      /* pointer to Angle */
      double dx,      /* dx */
      double dy       /* dy, same units as dx */
    );

/* IO */
extern short            /* status, 0=>OK */
dtcc_print_angle
    ( const ANGLE* aP,  /* pointer to Angle */
      const char* lbl,  /* label, NULL=> no label */
      FILE* fp          /* open formatted, output stream */
    );            
extern short             /* status, 0=>OK */
dtcc_scan_angle
    ( ANGLE* aP,         /* pointer to Angle */ 
      FILE* fp           /* open formatted, input stream */
    );            
extern short             /* status, 0=>OK */
dtcc_read_angle
    ( ANGLE* aP,         /* pointer to Angle */ 
      FILE* fp,          /* open unformatted, input stream */
      short endian		 /* 1 <=> do not swap */
    );            
extern short            /* status, 0=>OK */
dtcc_write_angle
    ( ANGLE* aP,        /* pointer to Angle */ 
      FILE* fp          /* open unformatted, output stream */
    );            

/* Rotate (x,y) into (u,v) */
extern short           /* status, 0=>OK */ 
dtcc_xy_rotate_angle
    ( const ANGLE* aP, /* pointer to Angle */
      double x,        /* x value to be rotated */
      double y,        /* y value to be rotated */
      double* u,       /* rotated x */
      double* v        /* rotated y */
    );

/* Copy an angle */
extern short             /* status, 0=>OK */
dtcc_copy_angle
    ( ANGLE* dP,         /* pointer to destination Angle */ 
      const ANGLE* sP    /* pointer to source Angle */
    );

#ifdef  __cplusplus 
}
#endif

/* Compare two angles */
#define dtcc_compare_angle( aP,bP ) \
		dtcc_compare( dtcc_angle_value(aP),dtcc_angle_value(bP) )
		           
#endif /* _ANGLE_H */
