/* Status codes */

#ifndef _STATUS_CODES_H
#define _STATUS_CODES_H

#ifdef  __cplusplus 
extern "C"{
#endif

extern char*
dtcc_get_status_message
	(	short 	msg_no,
		char*	msgP 
	);

#ifdef  __cplusplus 
}
#endif

#define OK                               (0)

/*ERRORS */
#define FILE_ERROR                       (-1)
#define FILE_NOT_OPEN                    (-2)
#define WRITE_INCOMPLETE                 (-3)
#define READ_INCOMPLETE                  (-4) 
#define PRINT_INCOMPLETE                 (-5)
#define FILE_NOT_FOUND                   (-6)

#define MEMORY_ALLOCATION_FAILURE        (-10)
#define ILLEGAL_ARG                      (-11)
#define INDEX_OUT_OF_RANGE               (-12)
#define ITEM_NOT_FOUND                   (-13)
#define ILLEGAL_REQUEST                  (-14)
#define UNKNOWN_ITEM_REQUESTED           (-15)
#define ARITHMETIC_OVERFLOW              (-16)
#define DATA_OUT_OF_RANGE                (-17)
#define FEATURE_NOT_AVAILABLE            (-18)
#define INVALID_DATA					 (-19)

/* PROJECTION ERRORS */
#define CONSTANTS_NOT_INITIALIZED		(-20)
#define POLAR_ORIGIN					(-21)
#define POLAR_STD_PARALLEL				(-22)
#define EQUATORIAL_STD_PARALLEL			(-23)
#define EQUAL_STD_PARALLELS				(-24)
#define EQUATORIAL_ORIGIN				(-25)
#define NUMERIC_NOT_GEO                 (-26)
#define NUMERIC_TOO_BIG                 (-27)
#define NUMERIC_NEGATIVE                (-28)
#define COORD_NOT_DEFINED				(-29)

#define STRUCTURE_INVALID				 (-30)
#define INVALID_STRUCTURE				 (-30)
#define STRUCTURE_TYPE_ERROR			 (-31)
#define TYPE_MISMATCH 					 (-32)
#define DUPLICATE_NAME					 (-33)
#define LIST_NOT_OPEN					 (-34)
#define COPY_ERROR                       (-35)

#endif /* _STATUS_CODES_H */

