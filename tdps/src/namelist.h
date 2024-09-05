/* Namelist structure */
#ifndef _NAMELIST_H
#define _NAMELIST_H

#include "dtcc.h"

/* NAMELIST */
typedef struct {  DTCC_LABELS* listP; short n;  } NAMELIST;
 
#ifdef  __cplusplus 
extern "C"{
#endif

/* initialize and clear the list */
extern short dtcc_initialize_namelist( NAMELIST* nlP );
extern short dtcc_clear_namelist( NAMELIST* ulP );

/* Number of items in the list */
extern int dtcc_namelist_count( const NAMELIST* ulP );
/* Pointer to the list */
extern DTCC_LABELS* dtcc_namelist_dataKP( const NAMELIST* ulP, int k );
/* Pointers to the individual items in the list */
extern const char* dtcc_namelist_labelK( NAMELIST* ulP, int k ); 
extern const char* dtcc_namelist_abbrvK( NAMELIST* ulP, int k );  
extern const char* dtcc_namelist_otherK( NAMELIST* ulP, int k );    
/* Set the kth label in the list */
extern short dtcc_set_namelist_dataK( NAMELIST* ulP, int k, 
						const char* name, const char* abbrv, const char* other,
						short valid, short type  );	
extern short dtcc_reset_namelist_dataK( NAMELIST* ulP, int k, const void* dtccP );	

/* Length (bytes) of the longest composite label in the list */
extern int dtcc_namelist_longest_label( const NAMELIST* ulP );        
/* Allocate a namelist buffer on the heap */
extern short dtcc_allocate_namelist_bfr( NAMELIST* nlP, short n );

#ifdef  __cplusplus 
}
#endif

#endif /* _NAMELIST_H */

