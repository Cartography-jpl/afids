/* Functions declared in DTCC.H */

#include "dtcc.h"
long hash( const void* v, long n );

/* DTCC.H functions */
int scanTo( FILE* fp, char s, int n, char* bfr )
{	int item, lead=1;
	while( ((item=fgetc(fp)) != EOF)  &&  (n >= 0) )
	{	char c = (char)item;
		if( isspace(item) && (lead == 1) )
			;
		else
		if( c == '/' )
		{	int xitem;
			char xc;
			while( ((xitem=fgetc(fp)) != EOF) )
			{	xc= (char)xitem;
				if( xc == '*' )
					if( (char)fgetc(fp) == '/' )
						break;
			}
		}
		else
		if( c != s )
		{	*bfr++ = c;
			lead = 0;
			--n;
		}
		else
			break;
	}
	*bfr = 0;
	return item;
}

void* dtcc_check_bfr( void* bfrP, int n )
	{	if( !bfrP ) bfrP = (void*)dtcc_allocate_ptr(n);
		return bfrP;
	}

short blockcopy( void* destP, const void* srcP, int n )
	{	if( !destP || !srcP )
			return ILLEGAL_ARG;
		else
		if( memcpy( destP, srcP,n ) != destP )
			return COPY_ERROR;
		else
			return 0;
	}
			
/* utility function for handling the crossing of 180 */
double                     /* Angle [-180,180) */
dtcc_check_PI
    ( double angle         /* (in) angle to convert */
    )
    {   if( angle >= M_PI ) 
            angle -= TWO_PI;
        else
        if( angle < -M_PI )
            angle += TWO_PI;
        return angle;
    }
double                     /* Angle [-180,180) */
dtcc_check_180
    ( double angle         /* (in) angle to convert */
    )
    {   if( angle >= 180.0 ) 
            angle -= 360.0;
        else
        if( angle < -180.0 )
            angle += 360.0;
        return angle;
    }
double dtcc_check_90( double a )
	{	if( a > 90.0 )
			a -= (a-90.0);
		else
		if( a < -90.0 )
			a -= (a-90.0);
		return a;
	}
double dtcc_check_360( double a )
	{	while( a > 360.0 ) a -= 360.0;
			while( a < 0.0 ) a += 360.0;
		return a;
	}

/*  Test if MACHINE is high-endian */
short dtcc_test_endian()
{   int n = 1;
    unsigned char *c = (unsigned char*)&n;
    return ( c[0] == 0 ? 1 : 0 );
}

/* Byte swapping */    
short dtcc_swab2N( short* A, short n )
    {   short a,k;
    	for( k=0; k<n; k++, A++ )
    	{	a = *A; 
        	*A = ( (a & 0x0000FF) << 8 ) | ( (a & 0x00FF00) >> 8 );
        }
        return 0;
    }    
short dtcc_swab4N( short* A, short n )
    {   short t,k; 
        short*B;
        for( k=0; k<n; k++, A+=2 )
        {	B = A+1;
        	dtcc_swab2N( A,1 ); dtcc_swab2N( B,1 );
        	t = *A; *A = *B; *B = t;
        }
        return 0;
    }    
short dtcc_swab8N( long* A, short n )
   {    long t, *B; short k;
		for( k=0; k<n; k++, A+=2 )
        {	B = A+1;
            dtcc_swab4N( (short*)A,1 ); dtcc_swab4N((short*) B,1 );
        	t = *A; *A = *B; *B = t;
        }
        return 0;
    }

short 	dtcc_read_short( FILE* fp, short endian )
	{	short x;
		if( !fp )
			return SHORT_NULL;
		else
		if( fread( &x, sizeof(short),1, fp ) != 1 )
			return SHORT_NULL;
		else
		if( endian != 1 )
			dtcc_swab_short( &x );
		return x;
	}
long 	dtcc_read_long( FILE* fp, short endian )
	{	long x;
		if( !fp )
			return LONG_NULL;
		else
		if( fread( &x, sizeof(long),1, fp ) != 1 )
			return LONG_NULL;
		else
		if( endian != 1 )
			dtcc_swab_long( &x );
		return x;
	}
float 	dtcc_read_float( FILE* fp, short endian )
	{	float x;
		if( !fp )
			return FLOAT_NULL;
		else
		if( fread( &x, sizeof(float),1, fp ) != 1 )
			return FLOAT_NULL;
		else
		if( endian != 1 )
			dtcc_swab_float( &x );
		return x;
	}

double 	dtcc_read_double8( FILE* fp, short endian )
	{
		double z = DOUBLE_NULL;
		int status = 0;
		IEEE_DOUBLE8 x;
		if( !fp )
			return DOUBLE_NULL;
		else
		if( (status=fread( &x, 8,1, fp )) != 1 )
		{	return DOUBLE_NULL;
		}
		else 
		if( endian != 1 )
			dtcc_swab_double8( &x );
		z = x;
		/* native_double( &z, x, 1L ); */
		if( z < DOUBLE_NULL/10.0 ) z = DOUBLE_NULL;
		return z;
	}
/* write native double as an 8 byte IEEE double */
short dtcc_write_double8( FILE* fp, double dble )
	{	/*	char x[8];
			native_double( &dble, x, 2 );
		*/
		IEEE_DOUBLE8 x = dble;
		return ( fwrite( &x, 8,1,fp ) == 1 ? OK : WRITE_INCOMPLETE );
	}
/* read N IEEE 8 byte doubles and return N native double */
short 	
dtcc_read_double8N
	( 	FILE* fp, 
		double* bfrP, 
		short N, 
		short endian 
	)
	{	if( !fp || !bfrP )
			return ILLEGAL_ARG;
		else
		if( N <= 0 )
			return INDEX_OUT_OF_RANGE;
		else
		{	int i;
			for( i=0; i<N; i++ )
				bfrP[i] = dtcc_read_double8( fp, endian );
			return OK;
		}
	}	
short 	
dtcc_read_shortN
	( 	FILE* fp, 
		short* bfrP, 
		short N, 
		short endian 
	)
	{	if( !fp || !bfrP )
			return ILLEGAL_ARG;
		else
		if( N <= 0 )
			return INDEX_OUT_OF_RANGE;
		else
		{	int i;
			for( i=0; i<N; i++ )
				bfrP[i] = dtcc_read_short( fp, endian );
			return OK;
		}
	}
	
/*
char* dtcc_read_string( FILE* fp, char* bfrP, short flag )
	{	if( fp )
		{	short n = dtcc_read_short( fp, flag );
			bfrP = dtcc_check_bfr(bfrP,n);
			if( bfrP ) fread( bfrP, sizeof(char),(int)n, fp );
			return bfrP;
		}
		else
			return NULL;
	}
short 	dtcc_write_string( FILE* fp, const char* bfrP  )
	{	if( !bfrP )
			return ILLEGAL_ARG;
		else
		if( !fp )
			return FILE_NOT_OPEN;
		else
		{	short n = strlen(bfrP)+1;
			fwrite( &n, sizeof(short),1, fp );
			fwrite( bfrP, sizeof(char),n, fp );
			return OK;
		}
	}
*/
#include <ctype.h>
/* utility function to strip white space from front of a string */
char* 
dtcc_skip_white
	(	const char* strP
	)
	{	char* xP = (char*)strP;
		while( *xP != 0 && isspace(*xP) ) xP++;
		return xP;
	}
/* utility function to strip white space from end of a string */
void 
dtcc_trim_white
	(	char* strP
	)
	{	int k = strlen(strP);
		while( --k >= 0 )
			if( !isspace(strP[k]) )
				return;
			else
				strP[k] = 0;
		return; 
	}
             

FILE* dtcc_open_dat_file( const char* name, const char* mode )
{    	FILE* fp = NULL;
		if( (fp = fopen( name, mode )) == NULL )
    	{	/* Try upper case */
			int n = strlen(name);
			char * tmp = (char*)dtcc_allocate_ptr(n+1);
			if( tmp ) 
			{	while( --n >= 0 ) 
					tmp[n] = toupper(name[n]);
				fp = fopen( tmp, mode );
				dtcc_free_ptr(tmp);
			}
		}
		return fp;
}

long dtcc_file_size( FILE* fp)
{	long n = -1;
	if( fp )
	{
		fseek( fp,0,2);
		n = ftell(fp);
		fseek( fp,0,0 );
	}
	return ( n >= 0 ? n : FILE_NOT_OPEN );
}

/* Copy a cstring into an n byte buffer */
char* copystr( char* bfr, int n, const char* cs )
{	if( !bfr || !cs )
		return NULL;
	else
	{	int k = strlen(cs);
		if( k > n ) k = n;  
		strncpy(bfr,cs,k);
		bfr[k] = 0;
		return bfr;
	}
}

long hash( const void* v, long n )
{	const unsigned char* s = (unsigned char*)v;
	long sum = 0, k = 0;
	while( --n >= 0 )
	{	sum += ( *s++ << k++ );
		if( k >= 16 ) k = 0;
	}
	return sum;
}
