/* DTCC global definitions */      

#ifndef _DTCC_H
#define _DTCC_H
/* All dtcc structures have the following functions associated with them.
   They all return a status such that  
   		status == 0 => OK
   		status <  0 => ERROR
   		status >  0 => WARNING.

	Read from an unformatted file. 
		short dtcc_read_X( X* dtccP, FILE* fp )
	Write to an unformatted file.
		short dtcc_write_X( const X* dtccP, FILE* fp )
	Read from a formatted file.
		short dtcc_scan_X( X* dtccP, FILE* fp )
	Write to a formatted file.
		short dtcc_print_X( const X* dtccP, FILE* fp )
	Make a copy of the structure.
		short dtcc_copy_X( X* destinationP, cosnt X* sourceP )
*/
/* All dtcc structures except COORD3 and ANGLE have the following
   functions associated with them.

	Invalidate the structure.
		short dtcc_invalidate_X( X* dtccP )
	Validate the structure.
		short dtcc_validate_X( const X* dtccP )
	Status message associated with a code (see dtcc_X_status).
		short dtcc_X_status_message( const X* dtccP, short status_no ) 
*/
/* All dtcc structures except COORD3 and ANGLE support the following
   system functions.
   	
   	Is the structure valid?
   		BOOLEAN dtcc_is_valid( dtccP)
	Status code associated with the last operation.
		short dtcc_status( const X* dtccP )
*/
	
/* IEEE 8 byte double  */
#if OS_MAC
typedef short double 	IEEE_DOUBLE8;
#define DELIMITER	":"
#else
typedef double 		IEEE_DOUBLE8;
#if defined(_MSC_VER)
#define DELIMITER	"\\"
#else
#define DELIMITER	"/"
#endif
#endif
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include "statuscd.h"

/* Memory allocation & deallocation  */
#ifdef _USE_XVT
#include "xvt.h"
#define dtcc_allocate_ptr(n)	xvt_mem_zalloc(n)
#define dtcc_free_ptr(aP)       xvt_mem_free((char*)aP)
#else
#define dtcc_allocate_ptr(n)     calloc((n),1)
#define dtcc_free_ptr(aP)        free((void*)(aP))
#endif
#define dtcc_allocate_ptr0(n,k)  dtcc_allocate_ptr((n)*(k))

/* Coordinate types */
/* 3D coordinate structure */
typedef enum{	COORD3_UPS = 0,
				COORD3_UTM = 60,
				COORD3_MGRS,			/* encoded UTM */
				COORD3_GRID,			/* (east,north,h) m */
				COORD3_XYZ,				/* (x,y,z) m */
				COORD3_DAZM,			/* (range, azimuth, h ) m,d,m */
				COORD3_GEO,				/* ( lat, lon, h ) d,d,m */
				COORD3_GREF,			/* encoded geographic */ 
				COORD3_UNDEFINED = 255	/* not defined */
			} COORD3_TYPES;
#define dtcc_coord3_is_UPS(id)			\
		( (id) == COORD3_UPS )
#define dtcc_coord3_is_UTM(id)			\
		( (id) > COORD3_UPS && (id) <= COORD3_UTM )
#define dtcc_coord3_is_MGRS(id)		\
		( (id) == COORD3_MGRS )
#define dtcc_coord3_is_grid(id)		\
		( (id) <= COORD3_GRID )
#define dtcc_coord3_is_xyz(id)		\
		( (id) == COORD3_XYZ )
#define dtcc_coord3_is_distazm(id)		\
		( (id) == COORD3_DAZM )
#define dtcc_coord3_is_geo(id)		\
		( (id) == COORD3_GEO )
#define dtcc_coord3_is_geographic(id)		\
		( (id) >= COORD3_GEO && (id) <= COORD3_GREF )
#define dtcc_coord3_is_metric(id)		\
		( (id) <= COORD3_XYZ  )
#define dtcc_coord3_is_georef(id)		\
		( (id) == COORD3_GREF )
#define dtcc_coord3_is_milgrid(id)  \
		( (id) <= COORD3_MGRS && (id) >= COORD3_UPS )
#define dtcc_coord3_is_defined(id)  \
		( (id) <= COORD3_GREF && (id) >= COORD3_UPS )

#define dtcc_coord3_is_numeric(id)  \
		( dtcc_coord3_is_defined(id) && !dtcc_coord3_is_encoded(id) )
#define dtcc_coord3_is_encoded(id)		\
		( dtcc_coord3_is_MGRS(id) || dtcc_coord3_is_georef(id) )
#define dtcc_coord3_is_zonal(id)		\
		( dtcc_coord3_is_UTM(id) || dtcc_coord3_is_UPS(id) )
		

/* Boolean values */
#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif
#define dtcc_encoded_boolean( b )	( (b) ? "TRUE" : "FALSE" )

/* Mathematical constants and type NULLS*/
#include "cnstnts.h"
#define DTCC_FLAG       SHORT_NULL

/* Math macros */
/* Square a number */
#define dtcc_square(x)		( (x)*(x) )
/* Round a double value into a long */
#define dtcc_round(x)		((long)( (x) < 0.0 ? (x)-0.5 : (x)+0.5 ))
/* Select the smaller of two values */
#define dtcc_minimum( a,b )	( (a) < (b) ? (a) : (b) )
#define dtcc_minimum_abs( a,b )	( fabs(a) < fabs(b) ? (a) : (b) )
/* Select the larger of two values */
#define dtcc_maximum( a,b )	( (a) > (b) ? (a) : (b) )
#define dtcc_maximum_abs( a,b )	( fabs(a) > fabs(b) ? (a) : (b) )
/* Is a < b ? */
#define dtcc_ascending( a,b )	( (a) < (b) ? TRUE : FALSE )
/* Comparison macro.  a<b => -1, a==b => 0, a>b => l */
#define _IS_LESS_THAN		(-1)
#define _IS_EQUAL_TO		(0)
#define _IS_GREATER_THAN	(1)
#define _IS_NOT_EQUAL_TO	(2)
#define dtcc_compare( a,b ) \
		( (a) < (b) ? _IS_LESS_THAN : ( (a) > (b) ? _IS_GREATER_THAN : _IS_EQUAL_TO ) )

#ifdef  __cplusplus 
extern "C"{
#endif

/* Utility functions */
/* Check buffer.  If NULL, allocate n bytes */
extern void* dtcc_check_bfr(  void* bfrP, int n );
/* Check latitude (degrees). TRUE, if abs(lat) <= 90, FALSE otherwise */
#define dtcc_check_latitude(x) 	( fabs(x) <= 90.0 ? TRUE : FALSE )
/* Check latitude (degrees.  TRUE, if abs(lat) <= 180, FALSE otherwise */
#define dtcc_check_longitude(x) 	( fabs(x) <= 180.0 ? TRUE : FALSE )

/* Convert angle into [-180,180) */
extern double               /* Angle [-180,180) */
dtcc_check_180( double angle );
extern double 
dtcc_check_90( double a );
extern double 
dtcc_check_360( double a );
extern double               /* Angle [-pi,pi) */
dtcc_check_PI( double angle );

#include <string.h>
/* String utilities */
/* Skip white space at the front of a string */
extern char* 
dtcc_skip_white
	(	const char* strP
	);
/* Delete white space at the end of a string */
extern void 
dtcc_trim_white
	(	char* strP
	);
/* Copy a block of n bytes */
extern short 
blockcopy( void* destP, const void* srcP, int n );
/* Copy a cstring into an n byte buffer */
extern char* 
copystr( char* bfr, int n, const char* cs );
	
/* Byte swapping  */ 
extern short dtcc_test_endian(void);
extern short dtcc_swab8N( long* v, short n );
extern short dtcc_swab4N( short* v, short n );
extern short dtcc_swab2N( short* v, short n );

#define dtcc_swab_bytes( vP )  \
        ( sizeof(*(vP)) == 2 ? dtcc_swab2((short*)(vP)) : \
        ( sizeof(*(vP)) == 4 ? dtcc_swab4( (short*)(vP)) : \
        ( sizeof(*(vP)) == 8 ? dtcc_swab8( (long*)(vP)) : 0 )))
extern short dtcc_test_endian();                   
#define dtcc_swab_double8( dP )  dtcc_swab8N((long*)(dP),1)
#define dtcc_swab_float( fP )    dtcc_swab4N((short*)(fP),1)
#define dtcc_swab_long( lP )     dtcc_swab4N((short*)(lP),1)
#define dtcc_swab_short( sP )    dtcc_swab2N((short*)(sP),1)

#include <stdio.h>
#include <ctype.h>
extern short native_double( void* native, void* ieee8, long which );
/* File size in bytes */
extern long dtcc_file_size( FILE* fp);
/* Write native double as an 8 byte IEEE double */
extern short 	dtcc_write_double8( FILE* fp, double x );

/* Read numeric types, swabbing as necessary */
extern short 	dtcc_read_short( FILE* fp, short endian );
extern long  	dtcc_read_long( FILE* fp, short endian );
extern float 	dtcc_read_float( FILE* fp, short endian );
extern double 	dtcc_read_double8( FILE* fp, short endian );

extern short 	
	dtcc_read_double8N
		( 	FILE*   fp, 
			double* bfrP, 
			short   N, 
			short   endian 
		);
extern short 	
	dtcc_read_shortN
		( 	FILE*  fp, 
			short* bfrP, 
			short  N, 
			short  endian 
		);
/* Read and write strings < 256 chars */
extern FILE* dtcc_open_dat_file( const char* name, const char* mode );

extern short 	dtcc_read_string( char* bfrP, FILE* fp  );
extern short 	dtcc_write_string( const char* bfrP, FILE* fp  );
extern int		scanTo( FILE* fp, char s, int n, char* bfr );

#ifdef  __cplusplus 
}
#endif

/* Macros for DTCC validation and name fields */
#include "labels.h"
#if 1
#define _DTCC_LABEL_NAME_SIZE			(72)
#define _DTCC_LABEL_ABBRV_SIZE			(16)
#define _DTCC_NAME_OFFSET				(0)
#define _DTCC_ABBRV_OFFSET				_DTCC_LABEL_NAME_SIZE
#define _DTCC_OTHER_OFFSET				\
		(_DTCC_LABEL_NAME_SIZE + _DTCC_LABEL_ABBRV_SIZE)
#if 1
/* Maximum size of names */
#define DTCC_GROUP_NAME_SIZE		(32)
#define DTCC_SYSTEM_NAME_SIZE 		(64)
#define DTCC_HDATUM_NAME_SIZE		(40) 
#define DTCC_HDATUM_AREA_SIZE		(132)
#define DTCC_VDATUM_NAME_SIZE		(16)
#define DTCC_ELLIPSOID_NAME_SIZE	(32)
#define DTCC_PROJECTION_NAME_SIZE 	(32) 
#define DTCC_UNITS_NAME_SIZE    	(32)	 
#define DTCC_FILENAME_SIZE       	(255) 
#else
/* Maximum size of names */
#define DTCC_GROUP_NAME_SIZE		DTCC_LABEL_SIZE
#define DTCC_SYSTEM_NAME_SIZE 		DTCC_LABEL_SIZE
#define DTCC_HDATUM_NAME_SIZE		DTCC_LABEL_SIZE 
#define DTCC_HDATUM_AREA_SIZE		(255)
#define DTCC_VDATUM_NAME_SIZE		DTCC_LABEL_SIZE
#define DTCC_ELLIPSOID_NAME_SIZE	DTCC_LABEL_SIZE
#define DTCC_PROJECTION_NAME_SIZE 	DTCC_LABEL_SIZE 
#define DTCC_UNITS_NAME_SIZE    	DTCC_LABEL_SIZE	 
#define DTCC_FILENAME_SIZE       	(255) 
#endif
#endif
/* Types of sublists */
typedef enum 
	{ 	_ENTIRE_LIST = 0,
		_TYPE1_LIST=1, 
		_TYPE2_LIST=2,
		_TYPE4_LIST=4,
		_TYPE8_LIST=8,
		_TYPE16_LIST=16
	} _LISTMODES;

#include "labels.h"

#define _set_valid( xP )	(	(xP)->valid = 1  )
#define _set_invalid( xP )	(	(xP)->valid = 0  )

#endif /* _DTCC_H */

