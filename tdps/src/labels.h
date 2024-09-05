#ifndef _LABELS_H
#define _LABELS_H


/* DTCC labels */
#define DTCC_LABEL_SIZE	(256)
typedef struct
	{	char 	labels[DTCC_LABEL_SIZE];
		short	l1,l2,l3;
		short	valid, type;
	} DTCC_LABELS;

#ifdef  __cplusplus 
extern "C"{
#endif

/* Constant pointers to the individual labels */
extern const char* dtcc_nameP( const void* dtccP );
extern const char* dtcc_abbrvP( const void* dtccP );
extern const char* dtcc_otherP( const void* dtccP );

/* Status flags */
extern short dtcc_valid_flag(const void* dtccP);
extern short dtcc_type_flag(const void* dtccP);

/* Size of the labels in bytes */
extern short dtcc_name_size(const void* dtccP);
extern short dtcc_abbrv_size(const void* dtccP);
extern short dtcc_other_size(const void* dtccP);
extern short dtcc_label_length( const void* dtccP );

/* Get a copy of a label */
extern char* dtcc_get_name( const void* dtccP,  char* bfrP );
extern char* dtcc_get_abbrv( const void* dtccP,  char* bfrP );
extern char* dtcc_get_other( const void* dtccP,  char* bfrP );
		
/* Is a pointer NULL or a pointer to an empty string? */
extern int dtcc_is_valid_name( const void* dtccP );
extern int dtcc_is_valid_abbrv( const void* dtccP );
extern int dtcc_is_valid_other( const void* dtccP );
/* Is the valid flag set AND a valid name? */
extern int dtcc_is_valid( const void* dtccP );
/* Is the user flag set to type N?  */
extern int dtcc_is_typeN( short dtccP, int n );

/* Set invalid/valid */
extern void dtcc_set_invalid(void* dtccP);
extern void dtcc_set_valid(void* dtccP);
extern void dtcc_set_type( void* dtccP, int n );

/* IO */
extern short dtcc_write_label( const void* dtccP, FILE* fp );
extern short dtcc_read_label ( void* dtccP, FILE* fp );
extern void dtcc_print_label( const void* dtccP, FILE* fp );
extern short dtcc_scan_label( void* dtccP, FILE* fp );

/* utilities */
extern DTCC_LABELS* 
dtcc_set_label( void* lblP, const char* name, 
				const char* abbrv, const char* other,
				short valid, short type );
extern DTCC_LABELS* 
dtcc_reset_label( void* lblP, const char* label, short valid, short type );
extern short dtcc_copy_label( DTCC_LABELS* destP, const DTCC_LABELS* srcP );
extern int dtcc_clear_label( void* idP );
extern char* dtcc_encode_label( const void* idP, char* lblP );
extern void* dtcc_decode_label( const char* lblP, void* idP );

#ifdef  __cplusplus 
}
#endif

#endif /* _LABELS_H */
