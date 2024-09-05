/*
 * indexmerge.h - include file for indexmerge.c
 # formerly:
 * ibis_local.h: Local Include file for program IBIS
 */

#ifndef _indexmerge_h
#define  _indexmerge_h 1

/* INCLUDES */
#include "ibisfile.h"
#include "ibiserrs.h"

/* DEFINES */
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#define MAX_FMT 1024
#define MAX_COLUMN 100
#define MAXGRPNAME 32
#define MAX_DATA 100
#define MAX_FILE 20
#define DBUFSIZE 1000
#define IBUFSIZE (DBUFSIZE*sizeof(double))
#define MAXCOLSIZE 80
#define ROWSIZE 132
#define MAXCOLPERROW 50

/* DECLARES */
/* prototypes */

#endif /* _indexmerge_h */

