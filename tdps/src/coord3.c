/* Functions associated with the COORD3 structure */

#include "coord3.h"
#include <string.h>

#define _COORD3A(c3P)		 ((c3P)->data.a) 
#define _COORD3K(c3P,k)		 ((c3P)->data.c[k]) 
#define _COORD31(c3P)		 ((c3P)->data.c[0]) 
#define _COORD32(c3P)		 ((c3P)->data.c[1]) 
#define _COORD33(c3P)		 ((c3P)->data.c[2]) 
#define _COORD3Z(c3P)		 ((c3P)->zone) 

short dtcc_set_coord3K( COORD3* c3P, short k, double v )
	{	if( !c3P )
			return ILLEGAL_ARG;
		else
		if(  k < 0 || k > 2 )
			return -2;
		else
		{	_COORD3K(c3P,k) = v;	
			return OK;
		}
	}

short dtcc_set_coord3A( COORD3* c3P, const char* v, short k )
	{	if( !c3P )
			return ILLEGAL_ARG;
		else
		{	int n = strlen(v);
			if( n >= 23 ) n = 22;
			_COORD3Z(c3P) = k;
			strncpy( _COORD3A(c3P), v, n );
			if( dtcc_coord3_is_MGRS(k) ) 
				while( n < 23 ) _COORD3A(c3P)[n++] = ' ';
			_COORD3A(c3P)[n] = 0;
			return OK;
		}
	}
		
short dtcc_check_coord3K( const COORD3* c3P, short k )		 
	{	if( c3P )
		{	double v = _COORD3K(c3P,k);
			int alpha =  dtcc_coord3_is_encoded(dtcc_coord3Z(c3P));
			return ( v == DOUBLE_NULL || alpha ? COORD_NOT_DEFINED : OK );
		}
		else
			return ILLEGAL_ARG;
	}

short dtcc_set_coord3Z( COORD3* c3P, short v )
	{	if( !c3P  )
			return ILLEGAL_ARG;
		else
		{	_COORD3Z(c3P) = v;
			return OK;
		}
	}
	
/* Extract the values of a coord3 struct */
short dtcc_get_coord3  
	(	const COORD3* c3P,
		double *uP, 
		double *vP, 
		double *wP,
		short* zoneP 
	)
	{	if( !c3P )
			return ILLEGAL_ARG;
		else
		if( dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) )
			return ILLEGAL_ARG;
		else
		{	if( uP ) *uP = dtcc_coord31(c3P);
			if( vP ) *vP = dtcc_coord32(c3P);
			if( wP ) *wP = dtcc_coord33(c3P);
			if( zoneP ) *zoneP = dtcc_coord3_zone(c3P);
			return OK;
		}
	}        
short dtcc_get_coord3A
	( 	const COORD3* c3P, /* (in/out) Pointer to active structure */
        char* s,           /* (out) encoded value */
        short*  zone       /* (out) zone id  */
    )
    {	if( !c3P  )
    		return ILLEGAL_ARG;
    	else
		if( !dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) )
			return ILLEGAL_ARG;
		else
		{	if( s ) strcpy( s,_COORD3A(c3P) );
			if( zone ) *zone = dtcc_coord3_zone(c3P);
			return OK;
		}
	} 

/* Copy the data to another COORD3 */
short                           /* status code. 0=>OK */
dtcc_copy_coord3
    ( COORD3* c1P,        /* (in) Pointer to the target COORD3 */
      const COORD3* c2P   /* (in) Pointer to the source COORD3 */
     )     
    {   if( c1P && c2P )
        {   void* kP = memcpy( c1P, c2P, sizeof(COORD3) );
            return (  (COORD3*)kP == c1P ?  OK : COPY_ERROR ); 
        }
        else
            return ILLEGAL_ARG;
    }
    
/* Write/Read an unformatted stream */
short                        /* status code. 0=>OK */
dtcc_write_coord3
    ( const COORD3* c3P,     /* (in) Pointer to active structure */
      FILE*         fp       /* (in) Pointer to open formatted stream */
    )       
    {   if( c3P && fp ) 
        {   long k=0;
            k += fwrite( &(c3P->zone), 1,sizeof(short), (fp) );
            if( !dtcc_coord3_is_encoded(dtcc_coord3Z(c3P)) )
            {	k += dtcc_write_double8( fp,_COORD31(c3P) );
            	k += dtcc_write_double8( fp,_COORD32(c3P) );
            	k += dtcc_write_double8( fp,_COORD33(c3P) );
			}
			else
				k = fwrite( dtcc_coord3A(c3P), 24,1, fp );
            return OK; 
        }
        else
            return ILLEGAL_ARG;
    }
short                    /* status code. 0=>OK */
dtcc_read_coord3
    ( COORD3* c3P,       /* (in/out) Pointer to active structure */
      FILE* fp,          /* (in) Pointer to open formatted stream */
      short endian		 /* 1 <=> do not swap */
    )        
    {   if( c3P && fp ) 
        {   short zone = dtcc_read_short( fp, endian );
			if( dtcc_coord3_is_encoded(zone) )
			{	char tmp[24];
				if( fread( tmp, 1,24, fp ) != 24 )
					return READ_INCOMPLETE;
				else
					dtcc_set_coord3A(c3P,tmp,zone);
			}
			else
            {	double x = dtcc_read_double8( fp, endian );
            	double y = dtcc_read_double8( fp, endian );
        		double z = dtcc_read_double8( fp, endian );
            	dtcc_set_coord3( c3P, x,y,z, zone );
			}
			return OK;
        } 
        else
            return ILLEGAL_ARG;
    }
/* Scan a COORD3 */
/* Parse coord3 point */

static int read_next_line( FILE* fp, char* bfr )
{	int k=0, nl = '\n', cr = '\r';
	bfr[0] = 0;
	k = fgetc(fp);
	while( k != EOF && (k == nl || k == cr) ) k = fgetc(fp);
	if( k != EOF )
	{	int m = 0;
		while( k != nl && k != cr ) 
		{	bfr[m++] = k;
			k = fgetc(fp);
			if( k == EOF ) break;
		}
		bfr[m] = 0;
		return m;
	}
	return k;
}

static void white_space( const char* bfr, int* pre, int* post )
{	int atstart = 0, atend = 0;
	const char* bfrP = bfr + strlen(bfr);
	while( bfrP >= bfr && isspace(*bfrP)  ) {  bfrP--; atend++;  }
	while( isspace(*bfr) ) {  atstart++; bfr++;	}
	*pre = atstart; *post = atend;
	return;
}

static int count_words( const char* bfr )
{	if( *bfr == 0 )
		return 0;
	else	/* requires no leading or white space */
	{	int n = 0;
		for( ;; )
		{	while( isspace(*bfr) ) bfr++;	 /* skip white space  */
			if( *bfr == 0 ) return n;
			while( !isspace(*bfr) ) 
				if( *bfr != 0 )
					bfr++;	/* skip letters & numbers  */
				else
					return ++n;
			n++; 
		}
	}
}

static int _scan_real( int n, const char* bfr, double* a )
{	int k = -1;
	a[0] = a[1] = a[2] = DOUBLE_NULL;
	if( n == 2 )
		k = sscanf( bfr, "%lf%lf", a, a+1 );
	else
	if( n == 3 )
		k = sscanf( bfr, "%lf%lf%lf", a, a+1, a+2 );
	if( k == 2 || k == 3 ) k = 0;
	return k;
}
static int _scan_dm( int n, const char* bfr, double* a )
{	int k = -1;
	long latdgs,londgs;
	double latmns,lonmns;
	a[0] = a[1] = a[2] = DOUBLE_NULL;
	if( n == 4 )
		k = sscanf( bfr, "%ld%lf%ld%lf", 
				&latdgs,&latmns, &londgs,&lonmns );
	else
	if( n == 5 )
		k = sscanf( bfr, "%ld%lf%ld%lf%lf", 
				&latdgs,&latmns, &londgs,&lonmns, a+2 );
	if( k ==4 || k == 5 )
	{	a[0] = abs(latdgs) + fabs(latmns)/60.0;
		if( latdgs < 0 || latmns < 0.0 ) a[0] = -a[0];
		a[1] = abs(londgs) + fabs(lonmns)/60.0;
		if( londgs < 0 || lonmns < 0.0 ) a[1] = -a[1];
		k = 0;
	}
	return k;
}
static int _scan_dms( int n, const char* bfr, double* a )
{	int k = -1;
	long latdgs,latmns, londgs,lonmns;
	double latscs,lonscs;
	a[0] = a[1] = a[2] = DOUBLE_NULL;
	if( n == 6 )
		k = sscanf( bfr, "%ld%ld%lf%ld%ld%lf", 
				&latdgs,&latmns,&latscs, &londgs,&lonmns,&lonscs );
	else
	if( n == 7 )
		k = sscanf( bfr, "%ld%ld%lf%ld%ld%lf%lf", 
				&latdgs,&latmns,&latscs, &londgs,&lonmns,&lonscs, a+2 );
	if( k == 6 || k == 7 )
	{	a[0] = abs(latdgs) + (abs(latmns)+fabs(latscs)/60.0)/60.0;
		if( latdgs < 0 || latmns < 0 || latscs < 0.0 ) a[0] = -a[0];
		a[1] = abs(londgs) + (abs(lonmns)+fabs(lonscs)/60.0)/60.0;
		if( londgs < 0 || lonmns < 0 || lonscs < 0.0 ) a[1] = -a[1];
		k = 0;
	}
	return k;
}

#include <ctype.h>

static void replace_quad_letters( char* bfr, char* first )
{	char thischar, comma = ',', blank = ' ', minus = '-';
	int find = 0;
	*first = 0;
	while( *bfr != 0 )
	{	thischar = toupper(*bfr);
		if( thischar == comma || thischar == ')' || thischar == '(' )
			*bfr = blank;
		else
		if( thischar == 'N' || thischar == 'E' )
		{	*bfr = blank;
			if( *first == 0 ) *first = thischar;
			find = 0;
		}
		else
		if( thischar == 'W' || thischar == 'S' )
		{	*bfr = blank;
			if( *first == 0 ) *first = thischar;
			find = 1;
		}
		else
		if( find && !isspace(thischar) )
		{	*(bfr-1) = minus;
			find = 0;
		}
		else
		if( isalpha(thischar) )
			*bfr = blank;	
		bfr++;
	}
	return;
}

/* Parse signed coordinate */	
static short parse_coord3_string( COORD3* pt, char* bfr, COORD3_TYPES type )
{	char char1;
	double a[3] = { DOUBLE_NULL,DOUBLE_NULL,DOUBLE_NULL };
	short zone = -1, status = OK;
	int nwords = count_words(bfr);
	if( nwords > 1 ) 
	{ 	replace_quad_letters( bfr, &char1 ); 
		nwords = count_words(bfr); 
	}
	switch (type) 
	{	case COORD3_UPS:
		case COORD3_UTM:
			if( nwords == 3 )
				sscanf( bfr, "%hd%lf%lf", &zone, a,a+1 );
			else
			if( nwords == 4 ) 
				sscanf( bfr, "%hd%lf%lf%lf", &zone, a,a+1,a+2 );
			else
				status = -1;
			dtcc_set_coord3_grid
				( pt, fabs(a[0]),( char1 == 'S' ? -a[1] : a[1]), a[2], zone );
			break;
		
		case COORD3_MGRS:
			if( nwords == 1 )
			{	int atstart,atend;
				white_space(bfr,&atstart,&atend);
				if( !isdigit(*(bfr+atstart)) ) status = 1;
				dtcc_set_coord3_MGRS( pt, bfr );
			}
			else
				status = -1;
			break;
		
		case COORD3_GRID:
			if( nwords < 2 || nwords > 3 )
				status = -1;
			else
			if( char1 != 0 && char1 != 'E' && char1 != 'W' )
				status = -1;
			else
				_scan_real( nwords, bfr, a );
			dtcc_set_coord3_grid( pt, a[0],a[1],a[2],-1 );
			break;

		case COORD3_XYZ:
			if( nwords < 2 || nwords > 3 )
				status = -1;
			else
			if( char1 != 0  )
				status = -1;
			else
				_scan_real( nwords, bfr, a );
			dtcc_set_coord3_cart( pt, a[0],a[1],a[2] );
			break;
		
		case COORD3_DAZM:
			if( char1 != 0  )
				status = -1;
			else
			{	_scan_real( nwords, bfr, a );
				if( a[1] < 0.0 || a[1] > 360.0 )
					status = -1;
		    }
			dtcc_set_coord3_dazm( pt, a[0],a[1],a[2] );
			break;

		case COORD3_GEO:
			if( nwords < 2 || nwords > 7 ) 
				status = -1;
			else
			if( char1 != 0 && char1 != 'N' && char1 != 'S' ) /* not numeric */
				status = -1;
			else
			{	if( nwords == 2 || nwords == 3 ) /* dd and maybe elevation */
				{	_scan_real( nwords, bfr, a );
				}
				else
				if( nwords == 4 || nwords == 5 ) /* dm and maybe elevation */
				{	_scan_dm( nwords, bfr, a );
				}
				else
				if( nwords == 6 || nwords == 7 ) /* dms and maybe elevation */
				{	_scan_dms( nwords, bfr, a );
				}
				if( a[0] < -90.0 || a[0] > 90.0 || a[1] < -180.0 || a[1] > 360.0 )
					status = -1;
				else
					a[1] = dtcc_check_180(a[1]);
			}
			dtcc_set_coord3_geo( pt, a[0],a[1],a[2] );
			break;
		
		case COORD3_GREF:
			if( nwords != 1 )
				status = -1;
			else
			{	int atstart,atend;
				white_space(bfr,&atstart,&atend);
				if( isdigit(*(bfr+atstart)) ) 
					status = -1;
				else
					dtcc_set_coord3_MGRS( pt, bfr );
			}
			break;

		default:
		   status = UNKNOWN_ITEM_REQUESTED;
	};
	return status;
}

#define COMFLAG	'#'
short dtcc_scan_coord3
	( COORD3* pt, COORD3_TYPES type, char* comment, int n, FILE* fp  )
	{	short k=0,status = 0;
		if( !pt )
			status = ILLEGAL_ARG;
		else
		{	char bfr[256], *c=NULL;
			status = read_next_line( fp, bfr );  /* ( fgets(bfr,255,fp)==NULL ? EOF : 0 ); */
			if( status != EOF )
			{	if( bfr[0] == COMFLAG ) /* comment */
				{	copystr( comment, n, bfr );
					status = 2;
				}
				else
				{	status = 1;
					if( (c=strrchr( bfr,COMFLAG )) != NULL )
					{	copystr( comment, n, c );
						status += 2;
						c[0] = 0;  /* NULL terminator for the coordinate */
					}
					k = parse_coord3_string( pt, bfr, type );
					if( k < 0 ) copystr( comment, n, bfr ); 
				}
				status = 10*( k >= 0 ? status : -status );
			}
		}
		return status;
	}			


/* Set coordinates */
short                     /* status code. 0=>OK */
dtcc_set_coord3
	( 	COORD3* c3P,      /* (in/out) Pointer to active structure */
        double u,         /* (in) X value */
        double v,         /* (in) Y value */
        double w,         /* (in) Z value */
        short  zone       /* zone id for Grid */
    )
    {   if( !c3P )
            return ILLEGAL_ARG;
        else
        {	_COORD31(c3P) = u; 
            _COORD32(c3P) = v; 
            _COORD33(c3P) = w; 
            _COORD3Z( c3P ) = zone;
            return ( u != DOUBLE_NULL && v != DOUBLE_NULL ? 
            			OK : COORD_NOT_DEFINED );
        }
    }
    

/* Print by type (Cartesian, geographic, or grid) to formatted stream */
extern short                     /* status code. 0=>OK */
dtcc_print_coord3
	( 	const COORD3* c3P,       /* (in) Pointer to active structure */
        FILE* fp                 /* (in) Pointer to open formatted stream */
            )
    {   if( c3P && fp )
    	{	double NULL_FLAG = DOUBLE_NULL/10.0;
    		COORD3_TYPES type = dtcc_coord3Z(c3P);
    		double v1 = dtcc_coord31(c3P);
    		double v2 = dtcc_coord32(c3P);
    		double v3 = dtcc_coord33(c3P);
    		short zone = dtcc_coord3_zone(c3P);
    		/* fprintf( fp, "( " );	*/
    		if( dtcc_coord3_is_encoded(type) )
    			fprintf( fp, "%s", dtcc_coord3A(c3P) );
    		else
    		if( !dtcc_coord3_is_zonal(type) )
    		{	if( v1 > NULL_FLAG ) 
    				fprintf( fp, "%lf", v1 );
    			
    			if( v2 > NULL_FLAG ) 
    				fprintf( fp, ", %lf", v2 );
    			if( v3 > NULL_FLAG ) 
    				fprintf( fp, ", %lf",  v3 );
    		}
    		else	/* print zone easting, northing, elv */	
    		{	if( zone != SHORT_NULL ) 
    				fprintf( fp, "%c%02hd  ", ( v2 < 0.0 ? 'S' : 'N' ), zone );
				
    			if( v1 > NULL_FLAG ) 
    				fprintf( fp, "%lf", fabs(v1) );
     				
    			if( v2 > NULL_FLAG ) 
    				fprintf( fp, ", %lf", fabs(v2) );
    			
    			if( v3 > NULL_FLAG ) 
    				fprintf( fp, ", %lf",  v3 );
    		}
    			
    		/* fprintf( fp, " )" );	*/
    		return OK;
    	}
    	else
    		return ILLEGAL_ARG;
    }

/* Compare two coordinates. Uses a Manhattan norm, if the zones are different. */
extern short              /* 1<2 => -1, 1==2 => 0, 1>2 => l */
dtcc_compare_coord3
	( 	const COORD3* pt1,       
		const COORD3* pt2
    ) 
    {	short k = dtcc_compare( _COORD3Z(pt1),_COORD3Z(pt2) );
    	if( k != 0 )
    		return k;
    	else
    	if( dtcc_coord3_is_encoded(_COORD3Z(pt1)) )
    		return strcmp( _COORD3A(pt1), _COORD3A(pt2) );
    	else
    	if( dtcc_compare( _COORD31(pt1),_COORD31(pt2)) == 0 &&
    		dtcc_compare( _COORD32(pt1),_COORD32(pt2)) == 0 &&
    		dtcc_compare( _COORD33(pt1),_COORD33(pt2)) == 0 )
    		return 0;
    	else
    	{	double m1 = dtcc_coord3_manhattan(pt1);
    		double m2 = dtcc_coord3_manhattan(pt2);
    		return dtcc_compare( m1,m2 );
    	}
    }
    
/* Subtract points */
/* c1 - c2 -> diff */
short                               /* status code. 0=>OK */
dtcc_subtract_coord3
	( 	const COORD3* c3P1,         /* (in) Pointer to COORD3 structure */
        const COORD3* c3P2,         /* (in) Pointer to COORD3 structure */
        COORD3* diffP               /* (out) Pointer to COORD3 difference */
    )
    {   if( !c3P1 || !c3P2 || !diffP )
    		return ILLEGAL_ARG;
    	else
    	if( _COORD3Z(c3P1) != _COORD3Z(c3P2) )
    		return ILLEGAL_ARG;
    	else
    	if( !dtcc_coord3_is_numeric(_COORD3Z(c3P1)) )
    		return ILLEGAL_ARG;
    	else
    	{	double x = ( dtcc_coord31(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord31(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord31(c3P1) - dtcc_coord31(c3P2) : DOUBLE_NULL );
        	double y = ( dtcc_coord32(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord32(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord32(c3P1) - dtcc_coord32(c3P2) : DOUBLE_NULL );
        	double z = ( dtcc_coord33(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord33(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord33(c3P1) - dtcc_coord33(c3P2) : DOUBLE_NULL );
			short zone =  dtcc_coord3_zone(c3P1);
        	return dtcc_set_coord3( diffP, x,y,z, zone );
        }
    }
    
/* Get sum of two points  */
/* c1 + c2 -> c3 */
short                            /* status code. 0=>OK */
dtcc_add_coord3
	( 	const COORD3* c3P1,      /* (in) Pointer to COORD3 structure */
        const COORD3* c3P2,      /* (in) Pointer to COORD3 structure */
        COORD3* sumP             /* (out) Pointer to COORD3 sum */
    )
    {   if( !c3P1 || !c3P2 || !sumP )
    		return ILLEGAL_ARG;
    	else
    	if( _COORD3Z(c3P1) != _COORD3Z(c3P2) )
    		return ILLEGAL_ARG;
    	else
    	if( !dtcc_coord3_is_numeric(_COORD3Z(c3P1)) )
    		return ILLEGAL_ARG;
    	else
    	{	double x = ( dtcc_coord31(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord31(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord31(c3P1) + dtcc_coord31(c3P2) : DOUBLE_NULL );
        	double y = ( dtcc_coord32(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord32(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord32(c3P1) + dtcc_coord32(c3P2) : DOUBLE_NULL );
        	double z = ( dtcc_coord33(c3P1) != DOUBLE_NULL &&  
    				 	dtcc_coord33(c3P2) != DOUBLE_NULL ? 
    				 	dtcc_coord33(c3P1) + dtcc_coord33(c3P2) : DOUBLE_NULL );
			short zone =  dtcc_coord3_zone(c3P1);
        	return dtcc_set_coord3( sumP, x,y,z, zone );
        }
   }
    
/* Standard norms */
double             /* status code. 0=>OK */
dtcc_coord3_manhattan( const COORD3* c3P )
    {   if( !c3P )
    		return ILLEGAL_ARG;
    	else
    	if( !dtcc_coord3_is_numeric(_COORD3Z(c3P)) )
    		return ILLEGAL_ARG;
    	else
    	if( dtcc_coord31(c3P) == DOUBLE_NULL || dtcc_coord32(c3P) == DOUBLE_NULL )
    		return DOUBLE_NULL;
    	else
    		return fabs(dtcc_coord31(c3P)) + fabs(dtcc_coord32(c3P)) + 
    				( dtcc_coord33(c3P) == DOUBLE_NULL ? 0.0 : 
    					fabs(dtcc_coord33(c3P)) );
    }
double                     /* status code. 0=>OK */
dtcc_coord3_euclidean( const COORD3* c3P )
    {   if( !c3P )
    		return ILLEGAL_ARG;
    	else
    	if( !dtcc_coord3_is_numeric(_COORD3Z(c3P)) )
    		return ILLEGAL_ARG;
    	else
    	if( dtcc_coord31(c3P) == DOUBLE_NULL || dtcc_coord32(c3P) == DOUBLE_NULL )
    		return DOUBLE_NULL;
    	else
    		return 	_COORD31(c3P)*_COORD31(c3P) + 
    				_COORD32(c3P)*_COORD32(c3P) +
    				( _COORD33(c3P) == DOUBLE_NULL ? 0.0 : 
    					_COORD33(c3P)*_COORD33(c3P) ) ;
    }

int dtcc_format_coord3( const COORD3* c3P,char* bfr )
{	char mybfr[24];
	int id = dtcc_coord3_zone(c3P);
	if( dtcc_coord3_is_encoded(id) )
		strcpy( bfr, dtcc_coord3A(c3P) );
	else
	{	int i;
		if( id <= 60 )
			sprintf( bfr, "%hd%c ", 
				dtcc_coord3_zone(c3P),(dtcc_coord3_north(c3P) < 0.0 ? 'S' : 'N') );
		else
			bfr[0] = 0;
		for( i=0; i<3; i++ )
		{	double u = dtcc_coord3K(c3P,i);
			if( u > DOUBLE_NULL )
				sprintf( mybfr, "%lf%s", 
					( i == 1 && id <= 60 ? u : fabs(u) ), ( i < 2 ? ", " : "" ) );
			else
				sprintf( mybfr, "%s", ( i < 2 ? ", " : "" ) );
			strcat( bfr,mybfr );
		}
	}
	return 0;
}

#if 0 /* Check everything */
int main()
{   short i,k; 
	double u,v,w;
	FILE* fp = fopen( "tstcoord3","w" ); /* stdout; */
	COORD3 p,p1,p2,p3, *P, *P1, *P2, *P3;
	P1 = &p1; P2 = &p2; P3 = &p3; P = &p;
    
    for( i=0; i<3; i++ )
    	dtcc_set_coord3K( P1, i, (double)(i+1) );
	dtcc_set_coord3Z( P1, 4 );
    fprintf( fp, "SET K: " ); dtcc_print_coord3_grid(P1,fp); fprintf(fp,"\n");
	fprintf( fp, "CHECK K: " ); 
	for( i=0; i<4; i++ )
    	fprintf( fp, "%d ", dtcc_check_coord3K(P1,i) );
    fprintf(fp,"\n");
    
    fprintf( fp,"RESET: " );
    fprintf( fp, "CARTESIAN:\n" );
    dtcc_set_coord3_x( P1,(double)(-1) );
	fprintf( fp, "%lf ", dtcc_coord3_x( P1 ) );         
    dtcc_set_coord3_y( P1,(double)(-2) );
	fprintf( fp, "%lf ", dtcc_coord3_y( P1 ) );         
    dtcc_set_coord3_z( P1,(double)(-3) );
	fprintf( fp, "%lf ", dtcc_coord3_z( P1 ) );         
    fprintf(fp,"\n");
    
    fprintf( fp, "GEOGRAPHIC:\n" );
    dtcc_set_coord3_lat( P1,(double)(90) );
	fprintf( fp, "%lf ", dtcc_coord3_x( P1 ) );         
    dtcc_set_coord3_lon( P1,(double)(-180) );
	fprintf( fp, "%lf ", dtcc_coord3_y( P1 ) );         
    dtcc_set_coord3_elv( P1,(double)(-500) );
	fprintf( fp, "%lf ", dtcc_coord3_elv( P1 ) );         
    fprintf(fp,"\n");
    
    fprintf( fp, "GRID:\n" );
    dtcc_set_coord3_north( P1,(double)(1000000L) );
	fprintf( fp, "%lf ", dtcc_coord3_north( P1 ) );         
    dtcc_set_coord3_east( P1,(double)(500000L) );
	fprintf( fp, "%lf ", dtcc_coord3_east( P1 ) );         
    dtcc_set_coord3_hgt( P1,(double)(500) );
	fprintf( fp, "%lf ", dtcc_coord3_hgt( P1 ) );         
	dtcc_set_coord3_zone(P1,12);   
	fprintf( fp, "%d ", dtcc_coord3_zone( P1 ) );     
    fprintf(fp,"\n");
    
    fprintf( fp, "AZIMUTH:" );
    dtcc_set_coord3_azm( P1,(double)(270));
    fprintf( fp, "%lf", dtcc_coord3_azm( P1 ) );     
    fprintf(fp,"\n");
    
	dtcc_copy_coord3( P2,P1 );
	fprintf( fp, "COPY: " ); dtcc_print_coord3_cart(P1,fp); fprintf(fp,"\n");
	
	dtcc_set_coord3_cart( P, 10.0,20.0,DOUBLE_NULL );	
	fprintf( fp, "CART-2D: " ); dtcc_print_coord3_cart(P,fp); fprintf(fp,"\n");
	dtcc_set_coord3_cart( P1, 10.0,20.0,30.0 );	
	fprintf( fp, "CART-3D: " ); dtcc_print_coord3_cart(P1,fp); fprintf(fp,"\n");
	dtcc_set_coord3_geo( P, 90.0,135.0,DOUBLE_NULL );	
	fprintf( fp, "GEO-2D: " ); dtcc_print_coord3_geo(P,fp); fprintf(fp,"\n");
	dtcc_set_coord3_geo( P2, 90.0,135.0,30.0 );	
	fprintf( fp, "GEO-3D: " ); dtcc_print_coord3_geo(P2,fp); fprintf(fp,"\n");
	dtcc_set_coord3_grid( P, 90.0,135.0,DOUBLE_NULL,6 );	
	fprintf( fp, "GRID-2D: " ); dtcc_print_coord3_grid(P,fp); fprintf(fp,"\n");
	dtcc_set_coord3_grid( P3, 90.0,135.0,30.0,8 );	
	fprintf( fp, "GRID-3D: " ); dtcc_print_coord3_grid(P3,fp); fprintf(fp,"\n");
    fprintf(fp,"\n");
    
    dtcc_get_coord3_cart( P1, &u,&v,&w );
    fprintf( fp,"CART: %lf %lf %lf\n", u,v,w ); 
    dtcc_get_coord3_geo( P2, &u,&v,&w );
    fprintf( fp,"GEO: %lf %lf %lf\n", u,v,w ); 
    dtcc_get_coord3_grid( P3, &u,&v,&w,&k );
    fprintf( fp,"GRID: %lf %lf %lf : %d\n", u,v,w,k );                   

	return 0;
}	
#endif /* check everyting */
#if 0	   /* check scan */
int main()
{	COORD3 pt;
	char char1, bfr[256];
	COORD3_TYPES type;
	int k;
	FILE* fp = fopen( "data1.txt", "rb" );
	if( !fp ) return 0;
	type = COORD3_UTM;
	for(;;)
	{	k = dtcc_scan_coord3( &pt, type, bfr, 255, fp );
		if( k < 0 ) break;
		printf( "%d %d: ", k,type );
 		if( k < 20 )
		{	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );	 }
		else
			fprintf( stdout, "%s\n", bfr );
	}	
#if 0
	strcpy( bfr,"34MGR1234512345" );
	type = COORD3_MGRS;
	printf( "%s -> ", bfr );
	replace_quad_letters( bfr, &char1 );
	k = parse_coord3_string( &pt, bfr, type );
	printf( "%d %d: ", k,type );
 	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );

	strcpy( bfr,"N    00.00  W    10.0" );
	type = COORD3_GEO;
	printf( "%s -> ", bfr );
	replace_quad_letters( bfr, &char1 );
	k = parse_coord3_string( &pt, bfr, type );
	printf( "%d %d: ", k,type );
 	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );

	strcpy( bfr,"E 500.00  S 10.0" );
	type = COORD3_GRID;
	printf( "%s -> ", bfr );
	replace_quad_letters( bfr, &char1 );
	k = parse_coord3_string( &pt, bfr, type );
	printf( "%d %d: ", k,type );
 	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );

	strcpy( bfr,"32S 500.00  10.0" );
	type = COORD3_UTM;
	printf( "%s -> ", bfr );
	replace_quad_letters( bfr, &char1 );
	k = parse_coord3_string( &pt, bfr, type );
	printf( "%d %d: ", k,type );
 	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );

	strcpy( bfr,"N 00.00  W 10.0" );
	type = COORD3_GRID;
	printf( "%s -> ", bfr );
	replace_quad_letters( bfr, &char1 );
	k = parse_coord3_string( &pt, bfr, type );
	printf( "%d %d: ", k,type );
 	dtcc_print_coord3( &pt, stdout ); printf( "\n\n" );
#endif

	scanf( "%c", bfr );
	return 1;
}
#endif

