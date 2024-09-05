#ifndef _PROJFNS_H
#define _PROJFNS_H

#include "cnstnts.h"
#include <stdio.h>

#ifdef  __cplusplus 
extern "C"{
#endif

#ifdef _USE_XVT
#include "xvt.h"
#define allocate_memory(n) xvt_mem_zalloc( (size_t)(n) )
#define free_memory( aP ) xvt_mem_free(aP)
#else
#define allocate_memory(n) calloc( (size_t)(n),1 )
#define free_memory( aP ) free(aP)
#endif

/* Rotation on a sphere */
extern void rotate( double u, double v, double sina,double cosa, double* ru, double* rv );

/* Standard projection attributes */
extern short projection_id( const void *cnsts );
extern char*  projection_name( const void *cnsts, char* nameP );
extern double projection_axis( const void *cnsts );
extern double projection_ecc( const void *cnsts );
extern double projection_rf( const void *cnstsP );
extern double projection_lat0( const void *cnsts );
extern double projection_lon0( const void *cnsts );
extern double projection_fn( const void *cnsts );
extern double projection_fe( const void *cnsts );
extern double projection_lat1( const void *cnsts );
extern double projection_lat2( const void *cnsts );
extern double projection_azm0( const void *cnsts );
extern double projection_k0( const void *cnsts );
extern int projection_limit_check( const void *cnstsP, double lat, double lon );

#define projection_radius( cnsts )		projection_axis(cnsts)

/* Standard projection operators */
extern void* allocate_projection(int n);
extern void free_projection( const void* pdfnP );
extern void print_projection( const void* cnsts, FILE* fp );
extern void write_projection( const void* cnsts, FILE* fp );
extern void read_projection( const void* cnsts, FILE* fp );
extern void* copy_projection( void* destP, const void* srcP );
extern double check_180( double a );
extern double check_90( double a );
extern double check_360( double a );
	
#ifdef  __cplusplus 
}
#endif
				
#endif  /* _PROJFNS_H */


