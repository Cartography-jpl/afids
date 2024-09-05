/* MGRS functions */
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include "projdfn.h"
#include "projfns.h"
#include "tmfns.h"
#include "psfns.h"
#include "mgrsfns.h"

#ifndef M_PI
#define M_PI        (3.14159265358979323846) 
#endif
#ifndef TWO_PI
#define TWO_PI       (M_PI+M_PI) 
#endif
#ifndef HALF_PI
#define HALF_PI       (M_PI/2.0) 
#endif
#ifndef M_PI_2
#define M_PI_2      (1.57079632679489661923)
#endif
#ifndef RADDEG
#define RADDEG         (M_PI/180.0)
#endif
#ifndef DEGRAD
#define DEGRAD         (180.0/M_PI)
#endif
											 
#ifndef DOUBLE_NULL
#define DOUBLE_NULL	 (-1.7976931348623158e+308)		
#endif

#define unflow( v,k )	\
	( (k) == 1 && fabs(v) < (2.0E-10) ? 0.0 : \
	( (k) == 2 && fabs(v) < (1.0E-4) ? 0.0 : v ))
#define NOREF(x)				( (x) = (x) )
#define DTCC_UNDERFLOW 				(-3)
#define DTCC_UNDEFINED_ERROR			(-4)
#define DTCC_NO_SOLUTION				(-5)

#ifndef check180
#define check180(x)		\
		( (x) >= M_PI ? (x)-TWO_PI : ( (x) < -M_PI ? (x) + TWO_PI : (x) ) )
#endif
#define GROUP	((short*)cnsts->other+13)
#define AZONE	((char*)(&cnsts->a0))

/* legacy code */
/*
FTNSAC: FORTRAN Store a Character String

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/17/89, 8:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static void
ftnsac( char *s1, int n1, char *s2, int n2)
{
/*
Description:

Stores a FORTRAN style character string into another string and
then pads that string with blanks.

Return value:

None, the function is void.

See also:
*/
/*
Process: Make certain that no more than the length of the re-
ceiving string characters are sent. Then copy the string and pad
the remainder of the receiving string with blanks.
*/
    if(n2 > n1) n2 = n1;

    memcpy(s1,s2,n2);
    if(n1 > n2) memset(s1+n2,' ',n1-n2);
}

static void
shiftr( char *mgrs, int P1, long n, long *nchar)
{   long i,k,D2,D3;

    NOREF(P1);
    for(i=1,D2=1,D3=(*nchar-i+D2)/D2; D3>0; D3--,i+=D2) {
        k = *nchar-i+1;
        ftnsac((mgrs+(short)(k+n)-1),(short)(k+n)-((short)(k+n)-1),(mgrs+(short)
          k-1),(short)k-((short)k-1));
    }
/*
     ****** ADJUST THE NUMBER OF CHARACTERS *****
*/
    *nchar += n;
    return;
}

/*
FIFDINT: FORTRAN Intrinsic Function DINT

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/

static double
fifdint( double a)
{
/*
Description:

Truncates its double precision argument to its integer value.
In particular:

     fifdint(a) = 0 if |a| < 1 
                = the largest integer with the same sign as a that
                  does not exceed a if |a| >= 1

Return value:

The double precision value as computed above.

See also:
*/
/* 
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Intrinsic function DINT and the generic function AINT

Process: this value is simply the integal return value from the C
"modf" function
*/
#ifdef LONGDBL
auto double temp;
    modf(a,&temp);
    a = temp;
#else
    modf(a,&a);
#endif
    return a;
}



/*
FIFDNINT: FORTRAN Intrinsic Function DNINT

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static double
fifdnint( double a)
{
/*
Description:

Computes the nearest integer to its double precision argument.
In particular:

     fifdnint(a) = fifdint(a+0.5) if a >= 0.0
                = fifdint(a-0.5) if a <= 0.0

where "fifdint" is the FORTRAN intrinsic function DINT.

Return value:

The value as computed above.

See also:
*/
/* 
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Intrinsic functions ANINT and DNINT

Process: simply do the calculation as specified above.
*/
    if(a < 0.0) a -= 0.5;
    else a += 0.5;
    return fifdint(a);
}

/*
FIFICHAR: FORTRAN Intrinsic Function ICHAR

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static int
fifichar( unsigned char *c1)
{
/*
Description:

Converts a character code into its numeric display code, or "lexical value"
or "collalating weight". The point of this function is that character values
on the host processor are not necessarily the same as those on the machine
for which a given FORTRAN program was written. All character "value" 
references in a source FORTRAN program  are passed through this function
either by the translator directly or by the other runtime functions
included in this library. Note that if you wish this function to return
some value other than the host processor values then you most modify it.
Typically this modification would take the form of a lookup table reference.

Return value:

The character value directly.

See also: None
*/
/*
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Intrinsic function ICHAR
ftncms        Compare two characters in terms of their lexical value.

Process: Just return the integer value.
*/
    return (int) *c1;
}



/*
FIFIDINT: FORTRAN Intrinsic Function IDINT

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static long
fifidint( double a)
{
/*
Description:

Truncates its double precision argument to a long integer value.
In particular:

     fifidint(a) = 0 if |a| < 1 
               = the largest integer with the same sign as a that
                 does not exceed a if |a| >= 1

Return value:

The value as computed above.

See also:
*/
/* 
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Intrinsic function IDINT and the generic functions
              IFIX, INT, INT2, and INT4
fifi2nint     Compute nearest short integer
fifnint       Compute nearest long integer

Process: this value is simply the long cast of the integal return
value from the C "modf" function
*/
#ifdef LONGDBL
auto double temp;
    modf(a,&temp);
    a = temp;
#else
    modf(a,&a);
#endif
    return (long) a;
}



/*
FIFMOD: FORTRAN Intrinsic Function MOD

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static long
fifmod( long num, long dem)
{
/*
Description:

Computes the value of the remainder of num divided by dem. If dem
is zero, then the result is zero.

Return value:

The value as computed above.

See also: None
*/
/* 
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Generic function MOD when used with long integer arguments.

Process: this value is the result of num % dem if dem is nonzero,
else it is zero.
*/
    if(!dem) return 0L;
    else return num % dem;
}



/*
FIFNINT: FORTRAN Intrinsic Function NINT

Copyright 1988 Promula Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static long
fifnint( double a)
{
/*
Description:

Computes the nearest integer to its double precision argument.
In particular:

     fifnint(a) = fifidint(a+0.5) if a >= 0.0
                = fifidint(a-0.5) if a <= 0.0

where "fifidint" is the FORTRAN intrinsic function IDINT.

Return value:

The value as computed above.

See also:
*/
#if 0
extern long fifidint();        /* FORTRAN intrinsic function IDINT */
#endif
/* 
Referenced by:

Element       Description of use
-------       ------------------
FORTRAN       Intrinsic functions IDNINT and NINT

Process: simply do the calculation as specified above.
*/
    if(a < 0.0) a -= 0.5;
    else a += 0.5;
    return fifidint(a);
}



/*
FTNCMS: FORTRAN Compare Strings

Copyright 1988-90 PROMULA Development Corporation ALL RIGHTS RESERVED

Author: Fred Goodman
Revised: 08/16/89, 9:30 AM to reflect the version 2 platform approach.

Synopsis:
*/
static int
ftncms( char *s1, int n1, char *s2, int n2)
{
/*
Description:

Compares two FORTRAN style strings to determine their lexical
relationship. The comparison proceeds character by character
until either the end of both stings is reached or until a spec-
ific character difference is found. If the strings are of unequal
length, the shorter string is treated as though it were padded
with blanks to the length of the longer string. Note that the
"lexical" value of the character is obtained from the "fifichar"
function. Thus, this function does not necessarily assume the
display values of the host processor, but rather can assume the
display code values of another processor.

Return value:

 0  if no character difference is found
-1  if a character in the first string is less than the corres-
    ponding character in the second string.
+1  if a character in the first string is greater that the cor-
    responding character in the secong string.

See also:
*/
/*
Process: Note the hostile user assumption in the while statement.
If a simple zero test were used and if a negative length were
sent then a very long loop would result. Other than that the code 
is straightforward.
*/
auto int blank;              /* Display code for a blank */
auto int c1;                 /* Current display code from s1 */
auto int c2;                 /* Current display code from s2 */

    
  /*blank = fifichar(" ");*/
    blank = fifichar((unsigned char *)" ");
    while(n1 > 0 || n2 > 0) {
      /*if(n1 > 0) c1 = fifichar(s1++);*/ 
        if(n1 > 0) 
           c1 = fifichar((unsigned char *)s1++);
        else c1 = blank;
      /*if(n2 > 0) c2 = fifichar(s2++);*/
        if(n2 > 0) 
           c2 = fifichar((unsigned char *)s2++);
        else c2 = blank;
        if(c1 < c2) 
           return -1;
        else if(c1 > c2) 
           return 1;
        n1--;
        n2--;
    }
    return 0;
}

static long get_group( long group, long zone )
	{	if( group > 2 )
			group = ( zone > 32 ? 1 : 2 );
		return group;
	}
		
static void     
upsset( long n, long *ltrlow, long *ltrhi, 
		double *feltr, double *fnltr, long *ltrhy)
{
/*
     ***** UPS - SOUTH ZONE *****
*/
    if(n == 1) goto S20;
/*
     ** EASTERN HEMISPHERE - SOUTH ZONE ** 2ND LETTER **
*/
    *ltrlow = 1;
    *ltrhi = 18;
/*
     ** FALSE EASTING **
*/
    *feltr = 2000000.0;
    goto S22;
S20:
/*
     ** WESTERN HEMIPSHERE - SOUTH ZONE ** 2ND LETTER **
*/
    *ltrlow = 10;
    *ltrhi = 26;
/*
     ** FALSE EASTING **
*/
    *feltr = 800000.0;
    goto S22;
S22:
/*
     ** FALSE NORTHING FOR 3RD LETTER **
*/
    *fnltr = 800000.0;
/*
     ** 3RD LETTER MAXIMUM LIMIT **
*/
    *ltrhy = 26;
    return;
}
static
void        
upnset( long n, long *ltrlow, long *ltrhi, 
		double *feltr, double *fnltr, long *ltrhy)
{
/*
     ***** UPS - NORTH ZONE *****
*/
    if(n == 25) goto S30;
/*
     ** EASTERN HEMISPHERE - NORTH ZONE ** 2ND LETTER **
*/
    *ltrlow = 1;
    *ltrhi = 10;
/*
     ** FALSE EASTING **
*/
    *feltr = 2000000.0;
    goto S32;
S30:
/*
     ** WESTERN HEMIPSHERE - NORTH ZONE ** 2ND LETTER **
*/
    *ltrlow = 10;
    *ltrhi = 26;
/*
     ** FALSE EASTING **
*/
    *feltr = 800000.0;
    goto S32;
S32:
/*
     ** FALSE NORTHING FOR 3RD LETTER **
*/
    *fnltr = 1300000.0;
/*
     ** 3RD LETTER MAXIMUM **
*/
    *ltrhy = 16;
    return;
}
static
void
utmset( long igroup, long izone, long *ltrlow, long *ltrhi, double *fnltr )
 {	long iset;
/*
     **************************************
     *** DETERMINE SET FROM ZONE NUMBER ***
*/
    iset = 1;
S3000:
    if((izone-iset)/6*6+iset == izone) goto S3100;
    iset += 1;
    if(iset > 6) goto S3350;
    goto S3000;
S3100:
/*
     *** SET 'LOW' AND 'HIGH' (2ND) LETTER OF ZONE BASED ON ISET ***
*/
    switch((short)iset){case 1: goto S3210;case 2: goto S3220;case 3: goto
      S3230;case 4: goto S3210;case 5: goto S3220;case 6: goto S3230;default:
      break;}
S3210:
    *ltrlow = 1;
    *ltrhi = 8;
    goto S3300;
S3220:
    *ltrlow = 10;
    *ltrhi = 18;
    goto S3300;
S3230:
    *ltrlow = 19;
    *ltrhi = 26;
    goto S3300;
S3300:
/*
     *** SET FALSE NORTHINGS FOR 3RD LETTER ***
*/
    switch((short)igroup){case 1: goto S3320;case 2: goto S3330;default: break;}
S3320:
/*
     * GROUP 1
*/
    *fnltr = 0.0;
    if(fifmod(iset,2L) == 0) *fnltr = 1500000.0;
    goto S3350;
S3330:
/*
     * GROUP 2
*/
    *fnltr = 1000000.0;
    if(fifmod(iset,2L) == 0) *fnltr = 500000.0;
    goto S3350;
S3350:
    return;
}
static
void
utmlim( long *n, double lat, long izone, double *spsou, double *spnor,
        double *sleast, double *slwest)
{   double zero=0.0;
  	double 	r3=3.0,r8=8.0,r9=9.0,
  			r21=21.0,r33=33.0,r80=80.0;
	double slcm;
	long ilat,icm;
/*
     *** 1ST LETTER NUMBER FOR MGRS ***
*/
    if(*n > 0) goto S40;
/*
     *** DERIVE 1ST LETTER NUMBER OF MGRS FROM ***
     *** LATITUDE AS BROKEN DOWN INTO 8 DEGREE BANDS ***
     *** STARTING WITH LETTER #3 (C) AT 80 DEGREES SOUTH LATITUDE ***
*/
    *n = fifidint((lat+r80)/r8)+3;
/*
     ** CORRECT FOR LETTERS I & O **
*/
    if(*n > 8) *n += 1;
    if(*n > 14) *n += 1;
/*
     ** LETTER 'Y' (25) WILL BE DERIVED FOR LATITUDES 80-84N
     ** RESET TO LETTER 'X' (24) WHICH HAS BEEN MADE
     ** 12 DEGREES IN SIZE IN THE NORTH-SOUTH DIRECTION
*/
    if(*n >= 25) *n = 24;
/*
     ** LATITUDE OF ZERO SHOULD BE MADE LETTER 'N' (14) **
*/
    if(*n == 13 && lat == zero) *n = 14;
S40:
/*
     *************************
     **** LATITUDE LIMITS ****
     *************************
     ** SOUTH LATITUDE LIMIT **
*/
    ilat = (*n-3)*8-80;
/*
     ** CORRECTION FOR LETTERS I (9) & O (15) **
*/
    if(*n > 8) ilat -= 8;
    if(*n > 14) ilat -= 8;
    *spsou = (double)ilat;
/*
     ** NORTH LATITUDE LIMIT - 8 DEGREES NORTH **
*/
    *spnor = *spsou+r8;
/*
     ** LETTER 'X' (24) LIMIT IS 12 DEGREES NORTH **
*/
    if(*n == 24) *spnor = *spsou+12.0;
/*
     ****************************
     ***** LONGITUDE LIMITS *****
     ****************************
     *** SET LONGITUDE LIMITS FOR 'STANDARD' ZONES ***
     ** CENTRAL MERIDIAN
*/
    icm = izone*6-183;
    slcm = (double)icm;
/*
     ** EAST & WEST LONGITUDE LIMITS **
*/
    *sleast = slcm+r3;
    *slwest = slcm-r3;
/*
     ** ADJUST LONGITUDE LIMITS FOR ODD SIZED AREAS **
     ** ALL IN ZONES 31 - 37
     ** LETTER NUMBER 22 (V) AND ABOVE
*/
    if(izone < 31 || izone > 37) goto S44;
    if(*n < 22) goto S44;
/*
     ** ZONES 31 AND 32 AT LETTER NUMBER 22 (V) **
*/
    if(*n == 22 && izone == 31) *sleast = r3;
    if(*n == 22 && izone == 32) *slwest = r3;
/*
     ** REMAINDER OF ODD LONGITUDE ZONES IN LETTER NUMBER 24 (X) **
*/
    if(*n < 24) goto S44;
    if(izone == 31) *sleast = r9;
    if(izone == 33) *slwest = r9;
    if(izone == 33) *sleast = r21;
    if(izone == 35) *slwest = r21;
    if(izone == 35) *sleast = r33;
    if(izone == 37) *slwest = r33;
S44:
    return;
}
static
void
milref( const proj_dfn* cnsts, long *iarea, double *lat,
        long *izone, double y, double x, char *mgrs  )
{   char *blank = " ", *albet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    double spnlim = 84.0, spslim = -80.0;
    double oneht = 100000.0,twomil=2000000.0;
	double feltr,fnltr=0,slcm,sleast,slwest,spnor,spsou,xltr,xnum,ylow,yltr,ynum,yslow;
	long ixnum,iynum,ltrnum[3],ltrlow=0,ltrhi,ltrhy;
	char   string[20];

/*
     *** BRANCH OFF ACCORDING TO AREA CODE - IAREA ***
     *** IAREA = 1 = UTM
     ***         2 = UPS - NORTH ZONE
     ***         3 = UPS - SOUTH ZONE
*/
    switch((short)*iarea){case 1: goto S620;case 2: goto S640;case 3: goto
      S650;default: break;}
S620:
/*
     *** UTM ***
     *** FROM ZONE NUMBER AND SPHEROID CODES(S) ***
     *** DETERMINE THE FOLLOWING USEING SUBPROGRAM UTMSET
     *** 2ND LETTER RANGE (LTRLOW, LTRHI)
     *** FALSE NORTHING FOR 3RD LETTER (FNLTR)
*/
    utmset( get_group( *GROUP, *izone ), *izone, &ltrlow, &ltrhi, &fnltr );
/*
     *** DETERMINE 1ST LETTER NUMBER AND
     *** LATITUDE & LONGITUDE LIMITS OF UTM GEOGRAPHIC AREA
     *** USING LATITUDE AND ZONE NUMBER
     ***
*/
    ltrnum[0] = 0;
    utmlim(&ltrnum[0],*lat,*izone,&spsou,&spnor,&sleast,&slwest);
/*
     *** DETERMINE LOWEST NORTHING OF GEOGRAPHIC AREA ***
         USING LATITUDE SPSOU AT CENTRAL MERIDIAN
     ** CENTRAL MERIDIAN **
*/
    slcm = (double)(*izone*6-183);
/*
     ** CONVERT TO GRID COORDINATES **
*/
    *izone = -100L;
    geo_to_utm( cnsts,spsou,slcm,izone,&xltr,&yltr );
	if( spsou < 0.0 ) yltr *= -1.0;	 
/*
     ** SCALE DOWN TO NEAREST 100,000 UNIT **
*/
    ylow = fifdnint((double)fifidint(yltr/oneht)*oneht);
/*
     ** SCALE NUMBER DOWN TO LESS THAN 2 MILLION **
*/
    yslow = ylow;
S621:
    if(yslow < twomil) goto S622;
    yslow -= twomil;
    goto S621;
S622:
    yslow = fifdnint(yslow);
/*
     ***********************************
     *** TRANSFER UTM ZONE NUMBER TO ***
     *** 1ST TWO CHARACTERS OF MGRS  ***
     *** AND TWO CHARACTERS OF AZONE ***
*/
    sprintf(mgrs,"%02ld", *izone);
    sprintf(AZONE,"%02ld", *izone);

/*
     *** UTM *** 3RD LETTER ***
     ** OBTAIN NEAREST WHOLE NUMBER **
*/
    yltr = fifdnint(y);
/*
     ***********************************************
     ***   SPECIAL CASE - SOUTHERN HEMISPHERE    ***
     ***     10 MILLION NORTHING (EQUATOR)       ***
     *** NOT POSSIBLE IN MGRS - SUBTRACT 1 METER ***
     ***********************************************
*/
    if(fifdnint(yltr) == fifdnint(10000000.0)) yltr = fifdnint(yltr-1.0);
S624:
/*
     ** SCALE DOWN NUMBER UNTIL LESS THAN 2 MILLION **
*/
    if(yltr < twomil) goto S626;
    yltr -= twomil;
    goto S624;
S626:
/*
     ** SUBTRACT FALSE NORTHING **
*/
    yltr -= fnltr;
/*
     ** IF LESS THAN ZERO CORRECT BY ADDING 2 MILLION **
*/
    if(yltr < 0.0) yltr += twomil;
/*
     *** DETERMINE 3RD LETTER NUMBER ***
*/
    ltrnum[2] = fifidint((yltr+0.1)/oneht)+1;
/*
     ** CORRECT FOR LETTERS 'I' (9) AND 'O' (15)  **
*/
    if(ltrnum[2] > 8) ltrnum[2] += 1;
    if(ltrnum[2] > 14) ltrnum[2] += 1;
/*
     *** UTM - 2ND LETTER ***
     ** OBTAIN NEAREST WHOLE NUMBER **
*/
    xltr = fifdnint(x);
/*
     ******************************************************
     ***   SPECIAL CASE - ZONE 31                       ***
     *** 56-64 DEGREES NORTH LATITUDE - LETTER 'V' (22) ***
     *** 500,000 EASTING NOT POSSIBLE IN MGRS           ***
     *** SUBTRACT 1 METER                               ***
     ******************************************************
*/
    if(ltrnum[0] == 22 && *izone == 31 && fifdnint(xltr) == fifdnint(500000.0))
      xltr = fifdnint(xltr-1.0);
    ltrnum[1] = ltrlow+fifidint((xltr+0.1)/oneht)-1;
/*
     ** CORRECT FOR LETTER 'O' (15) **
     ** WHEN LTRLOW IS 'J' (10)     **
*/
    if(ltrlow == 10 && ltrnum[1] > 14) ltrnum[1] += 1;
/*
     *** DUMMY VALUES FOR UPS VARIABLES FOR *SAVE* ROUTINE ***
*/
    ltrhy = 0;
    feltr = 0.0;
/*
     ***** TRANSFER TO NUMBER SECTION *****
*/
    goto S670;
S640:
/*
     ************************************************************
     *** UPS -  NORTH ZONE ***
     ** 1ST LETTER ** 'Y' OR 'Z' **
*/
    ltrnum[0] = 25;
    if(fifdnint(x) >= twomil) ltrnum[0] = 26;
/*
     ** 1ST LETTER OF ZONE NUMBER 'N' **
*/
    ftnsac(AZONE,1,&albet[13],1);
/*
     *** FROM 1ST LETTER NUMBER DETERMINE
     *** 2ND LETTER RANGE (LTRLOW, LTRHI)
     *** 2ND LETTER FALSE EASTING (FELTR)
     *** 3RD LETTER FALSE NORTHING (FNLTR)
     *** 3RD LETTER 'HIGH' LETTER NUMBER (LTRHY)
*/
    upnset(ltrnum[0],&ltrlow,&ltrhi,&feltr,&fnltr,&ltrhy);
/*
     ** AREA LIMIT
*/
    spsou = spnlim;
    goto S652;
S650:
/*
     ***************************
     *** UPS -  SOUTH ZONE ***
     ** 1ST LETTER ** 'A' OR 'B' **
*/
    ltrnum[0] = 1;
    if(fifdnint(x) >= twomil) ltrnum[0] = 2;
/*
     ** 1ST LETTER OF ZONE NUMBER 'S' **
*/
    ftnsac(AZONE,1,&albet[18],1);
/*
     *** FROM 1ST LETTER NUMBER DETERMINE
     *** 2ND LETTER RANGE (LTRLOW, LTRHI)
     *** 2ND LETTER FALSE EASTING (FELTR)
     *** 3RD LETTER FALSE NORTHING (FNLTR)
     *** 3RD LETTER 'HIGH' LETTER NUMBER (LTRHY)
*/
    upsset(ltrnum[0],&ltrlow,&ltrhi,&feltr,&fnltr,&ltrhy);
/*
     ** AREA LIMIT
*/
    spsou = spslim;
    goto S652;
S652:
/*
     ************
     ***  UPS  - BOTH ZONES ***
     ** 2ND LETTER OF ZONE 'P' **
*/
    ftnsac((AZONE+1),1,&albet[15],1);
/*
     ** ZONE NUMBER PART OF MGRS 'BLANK' FOR UPS **
*/
    ftnsac(mgrs,1,blank,1);
    ftnsac((mgrs+1),1,blank,1);
/*
     *** UPS - 3RD LETTER ***
     ** OBTAIN NEAREST WHOLE NUMBER **
*/
    yltr = fifdnint(y);
    yltr -= fnltr;
    ltrnum[2] = fifidint((yltr+0.1)/oneht)+1;
/*
     ** CORRECT FOR LETTERS 'I' (9) AND 'O' (15)  **
*/
    if(ltrnum[2] > 8) ltrnum[2] += 1;
    if(ltrnum[2] > 14) ltrnum[2] += 1;
/*
     *** UPS - 2ND LETTER ***
     ** OBTAIN NEAREST WHOLE NUMBER **
*/
    xltr = fifdnint(x);
    xltr -= feltr;
    ltrnum[1] = ltrlow+fifidint((xltr+0.1)/oneht);
/*
     *** CORRECT 2ND LETTER NUMBER FOR MISSING LETTERS ***
*/
    if(x >= twomil) goto S654;
/*
     ** WESTERN HEMISPHERE **
     ** LETTERS 'M', 'N', 'O' **
*/
    if(ltrnum[1] > 12) ltrnum[1] += 3;
/*
     ** LETTERS 'V', 'W' **
*/
    if(ltrnum[1] > 21) ltrnum[1] += 2;
    goto S656;
S654:
/*
     ** EASTERN HEMISPHERE **
     ** LETTERS 'D', 'E'  **
*/
    if(ltrnum[1] > 3) ltrnum[1] += 2;
/*
     ** LETTER  'I' **
*/
    if(ltrnum[1] > 8) ltrnum[1] += 1;
/*
     ** LETTERS 'M', 'N' 'O' **
*/
    if(ltrnum[1] > 12) ltrnum[1] += 3;
    goto S656;
S656:
/*
     *** DUMMY VALUES FOR UTM VARIABLES FOR *SAVE* ROUTINE ***
*/
    spnor = sleast = slwest = ylow = yslow = 0.0;
/*
     ***** TRANSFER TO NUMBER SECTION *****
*/
    goto S670;
S670:
/*
     *************************************************
     ***** NUMBER SECTION *****
     *** EASTING PART ***
     ** OBTAIN NEAREST WHOLE NUMBER **
    xnum = fifdnint(x);
*/
/*
     *************************************************
     ***** NUMBER SECTION *****
     *** EASTING PART ***
     ** TRUNCATED WHOLE NUMBER rjl 04/05/96 **/
    xnum = fifdint(x);
/*
     ******************************************************
     ***   SPECIAL CASE - ZONE 31                       ***
     *** 56-64 DEGREES NORTH LATITUDE - LETTER 'V' (22) ***
     *** 500,000 EASTING NOT POSSIBLE IN MGRS           ***
     *** SUBTRACT 1 METER                               ***
     ******************************************************
*/
    if(ltrnum[0] == 22 && *izone == 31 && fifdnint(xnum) == fifdnint(500000.0))
      xnum = fifdnint(xnum-1.0);
    xnum -= (fifdint((xnum+0.1)/oneht)*oneht);
/*
     ****************************************************************
     *** DATA GENERAL SYSTEM DEPENDENT CODE - IDNINT4 - INTRINSIC ***
     *** NOT NEEDED IN UNIVAC VERSION
*/
    ixnum = fifnint(xnum);
/*
     *** NORTHING PART ***
     ** OBTAIN NEAREST WHOLE NUMBER **
    ynum = fifdnint(y);
*/
/*
     *** NORTHING PART ***
     ** TRUNCATED WHOLE NUMBER rjl 04/05/96 **/
    ynum = fifdint(y);
/*
     ***********************************************
     ***   SPECIAL CASE - SOUTHERN HEMISPHERE    ***
     ***     10 MILLION NORTHING (EQUATOR)       ***
     *** NOT POSSIBLE IN MGRS - SUBTRACT 1 METER ***
     ***********************************************
*/
    if(fifdnint(ynum) == fifdnint(10000000.0)) ynum = fifdnint(ynum-1.0);
    ynum -= (fifdint((ynum+0.1)/oneht)*oneht);
/*
     ****************************************************************
     *** DATA GENERAL SYSTEM DEPENDENT CODE - IDNINT4 - INTRINSIC ***
     *** NOT NEEDED UNIVAC VERSION
*/
    iynum = fifnint(ynum);
/*
     ********************************************************
     ********** TRANSFER DATA TO MGRS **********
     **** USING INTERNAL WRITE
     **** EXCEPT FOR THE 1ST TWO CHARACTERS ****
     **** WHICH ARE ALREADY IN MGRS
*/
    /* Zone already placed in mgrs at TRANSFER UTM ZONE NUMBER above.
       Put letter part of GZD and 100Kmsq letters into mgrs */
    string[0] = albet[(int)ltrnum[0]-1];
    string[1] = albet[(int)ltrnum[1]-1];
    string[2] = albet[(int)ltrnum[2]-1];
    string[3] = '\0';
    sprintf(&mgrs[2],"%s%05ld%05ld ",string,ixnum,iynum);
/*
     ********************************************************
     **** SAVE DATA FOR POSSIBLE USE IN NEXT COMPUTATION ****
     ** NUMBER OF CHARACTERS **
*/
    return;
}

static
void
xxtmgr( const proj_dfn* cnsts, double *lat, double *lon,
        long *izone, double *y, double *x, char *mgrs, int mode )
{	const double  	spslim = -80.0, spnlim = 84.0;
	const double	r3=3.0,r9=9.0,
	 				r21=21.0,r33=33.0,r56=56.0,
	 				r64=64.0,r72=72.0;
	const double  	spcorr = 4.85E-10;
	proj_dfn		ups;
	void*			UPS = &ups;
	long    		iarea;

	if( mode == 0 ) 
    {	iarea = ( *izone > 0 ? 1 : ( *y >= 0.0 ? 2 : 3 ));
    	utm_to_geo( cnsts, *izone, *x,*y, lat,lon );
    	goto S570;
    }
    else
		iarea = ( *lat < spslim ? 3 : ( *lat > spnlim ? 2: 1 ));
	
/*
     *** DETERMINE AREA FROM LATITUDE ***
     ** UPS - SOUTH ZONE **
*/
    if(*lat < spslim) goto S555;
/*
     ** UPS - NORTH ZONE **
*/
    if(*lat > spnlim) goto S555;
/*
     *** UTM ***
*/
/*
     *** CONVERT GEOGRAPHIC COORDINATES TO UTM GRID COORDINATES ***
         (LET ZONE BE COMPUTED FROM LONGITUDE - IFIXZ = 0)
*/
    *izone = -100;
    geo_to_utm( cnsts,*lat,*lon,izone,x,y );
	 if( *lat < 0.0 ) *y *= -1.0;
/*
     *** CHECK FOR ODD ZONE LIMITS ***
         ALL NORTH OF 56 DEGREES NORTH LATITUDE
         IN ZONE 31 - 37 AREAS
*/
/*    if(*lat-spcorr <= r56) goto S570;
	Northern limit not included 10/17/97 rjl
*/
    if(*lat+spcorr < r56) goto S570;
    if(*izone < 31 || *izone > 37) goto S570;
/*
     ** ZONES 31 AND 32 - LATITUDES 56 TO 64 **
*/
/*  if(*lat > r64) goto S520;
	64N included 10/17/97 rjl
*/
    if(*lat >= r64) goto S520;
    if(*izone < 31 || *izone > 32) goto S570;
    *izone = 32;
    if(*lon < r3) *izone = 31;
    goto S522;
S520:
/*
     *** ZONES 31 TO 37 ***
     *** NORTH OF 72 DEGREES LATITUDE ***
*/
/*   if(*lat-spcorr <= r72) goto S570; 
	72N not included 10/17/97 rjl
*/
    if(*lat+spcorr < r72) goto S570;
    *izone = 31;
/*	Western limits included 10/17/97 rjl
    if(*lon > r9) *izone += 2;
    if(*lon > r21) *izone += 2;
    if(*lon > r33) *izone += 2;
*/
    if(*lon >= r9) *izone += 2;
    if(*lon >= r21) *izone += 2;
    if(*lon >= r33) *izone += 2;
    goto S522;
S522:
/*
     *** RECOMPUTE GRID COORDINATES FOR ODD SIZED ZONES ***
         (HOLD ZONE NUMBER FIXED - IFIXZ = 1)
*/
    geo_to_utm( cnsts, *lat,*lon,izone,x,y );
	if( *lat < 0.0 ) *y *= -1.0;
	goto S570;

S555:
/*
     *** CONVERT GEOGRAPHIC TO GRID - UPS ***
*/
    init_ups(UPS, AXIS, ECC2, ( *lat < 0.0 ? -90.0 : 90.0 ) );
    geo_to_ups( UPS,*lat,*lon, x,y );
    goto S570;
S570:
/*
     ****************************************************
     ***** CONVERT TO MILITARY GRID REFERENCE COORDINATES *****
*/
    milref( cnsts, &iarea,lat,izone,fabs(*y),*x,mgrs );
    return;
}


typedef struct {
    long ncharo,ltrnmo[3],ltrlco[3];double xltro,yltro,spsouo,spnoro,sleaso,
    slweso,ylowo,yslowo;long iareao,izoneo,ltrlo,ltrho,ltrhyo;double fnltro,
    feltro;
} Comgrda;
static Comgrda Tomgrda;
typedef struct {
    char mgrso[5],azoneo[2];
} Comgrch;
static Comgrch Tomgrch;

static short
_mgrtxx( const proj_dfn* cnsts, char* mgrs, 
			double *lat, double *lon, 
			long *izone, double *y, double *x )
{	char *blank = " ", *num = "0123456789", *albet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	const double 	zero = 0.0;
	const double	oneht = 100000.0, twomil = 2000000.0;
	const double  	spslim = -80.0, spnlim = 84.0;
	const double  	ten = 10.0;
	long 			imax = 15, isize = 20;
	double 			feltr=0,fnltr=0,sign,slcm,sleast,slwest,spnor,spsou,
					xltr,xnum,ylow,yltr,ynum,yslow;
	long 			ltrloc[3],ltrnum[3],nchar,ileft,iright,nltrs,iarea,
    				ltrlow=0,ltrhi=0,ltrhy=0,nnum;
	long 			i,j,k;
	short			ierr = 0, areaOK = 0;
	proj_dfn		ups;
	void*			UPS = &ups;
/*
     ***** SET ERROR CODE TO ZERO *****
     ***** FIND LEFT MOST NON-BLANK *****
*/	
    nchar = 0;
    i = 1;
S1000:
    if(ftncms((mgrs+(short)i-1),1,blank,1) != 0)
      goto S1100;
    if(i == isize) goto S8802;
    i += 1;
    goto S1000;
S1100:
    ileft = i;
S1200:
/*
     ***** FIND RIGHT MOST NON-BLANK *****
*/
/*      printf ("finding right most non-blank in <%s>\n", mgrs); */
    i += 1;
    if(ftncms((mgrs+(short)i-1),1,blank,1) == 0)
      goto S1400;
    if(i == isize) goto S1300;
    goto S1200;
S1300:
    i = isize+1;
S1400:
    iright = i-1;
/*
     *** NUMBER OF CHARACTERS IN MGRS ***
*/
/*      printf ("iright %d ileft %d\n", iright, ileft); */
    nchar = iright-ileft+1;
/*
     *** TEST FOR MININUM NUMBER OF CHARACTERS ***
*/
    if(nchar > imax) {
/*        printf ("bailing 1 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */
/*
     *** TEST FOR MAXIMUM NUMBER OF CHARACTERS ***
*/
    if(nchar > imax) {
/*        printf ("bailing 2 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */
/*
     ***** SHIFT TO LEFT IF NECESSARY TO REMOVE ANY BLANKS *****
*/
    if(ileft == 1) goto S1600;
    for(i=1; i<=nchar; i++) {
        j = i+ileft-1;
        ftnsac((mgrs+(short)i-1),1,(mgrs+(short)j-1),(short)
          j-((short)j-1));
    }
S1600:
/*
     *****************************************************
     *****          MGRS = 2 CHARACTERS = 99         *****
     *****                                           *****
     ***** FOR ABOVE CONDITION NO COMPUTATION IS TO  *****
     ***** BE DONE - USER HAS REQUESTED *MENU*       *****
     ***** MODULE IN MAIN PROGRAM                    *****
     *****         SET IZONE (ZONE) = 99             *****
     *****        RETURN TO MAIN PROGRAM             *****
     *****************************************************
*/
    if(nchar == 2 && ftncms(mgrs,1,&num[9],1) == 0 && 
    	ftncms((mgrs+1),1, &num[9],1) == 0) 
    		goto S8600;
/*
     *****************************************************
     ***** VALIDITY CHECK - AN MGRS CAN ONLY BE MADE *****
     ***** UP OF THE FOLLOWING:                      *****
     *****      1. LETTERS (A-Z) (EXCEPT 'I' & 'O')  *****
     *****      2. NUMBERS (0-9)                     *****
     *****                                           *****
     ***** ALL OTHERS CHARACTERS ARE INVALID         *****
     *****************************************************
*/
    for(i=1; i<=nchar; i++) {
/*
     ** CHECK TO SEE IF CHARACTER IS A LETTER **
*/
        for(j=1; j<=26; j++) {
            if(ftncms((mgrs+(short)i-1),1,&albet[j-1],1)==0) 
            	goto S1750;
        }
/*
     ** CHECK TO SEE IF CHARACTER IS A NUMBER **
*/
        for(j=1; j<=10; j++) {
            if(ftncms((mgrs+(short)i-1),1,&num[j-1],1) == 0)
            	 goto S1750;
        }
/*
     ** CHARACTER IS INVALID - NOT A LETTER OR NUMBER **
     ** TRANSFER TO ERROR DISPLAY AND RETURN TO MAIN PROGRAM **
*/
        goto S8806;
S1750:
        continue;
    }
/*
     *****************************************************
     *****               LETTER PART                 *****
     *****          1-3 LETTERS IN LOCATIONS 1-5     *****
     *****************************************************
*/
    k = 0;
    for(i=1; i<=nchar; i++) {
        for(j=1; j<=26; j++) {
            if(ftncms((mgrs+(short)i-1),1,&albet[j-1],1)!=0) 
            	goto S1800;
            else
            	k += 1;
/*
     ** MAX NUMBER OF LETTERS ALLOWED -3- **
*/
            if(k > 3) goto S8804;
/*
     ** MAX LETTER LOCATION IN MGRS -5- **
*/
            if(i > 5) goto S8804;
/*
     ** LETERS 'I' (9) AND 'O' (15) NOT ALLOWED **
*/
            if(j == 9 || j == 15) goto S8806;
/*
     ** LETTER LOCATION IN MGRS **
*/
            ltrloc[k-1] = i;
/*
     ** LETTER NUMBER IN ALPHABET
*/
            ltrnum[k-1] = j;
S1800:
            continue;
        }
    }
/*
     *** NUMBER OF LETTERS ***
*/
    nltrs = k;
/*
     ***** VALIDITY CHECK *****
     ***   LETTERS MUST BE NEXT TO EACH OTHER (IN SEQUENCE)  ***
*/
    if(nltrs <= 1) goto S2200;
    k = nltrs-1;
    for(i=1; i<=k; i++) {
        if(ltrloc[i-1] != ltrloc[i]-1) goto S8808;
    }
S2200:
/*
     *****************************************************************
     ***** USE NUMBER OF LETTERS TO DETERMINE TYPE OF MGRS INPUT *****
     **                                                             **
     **    NLTRS = 0 = NUMBERS ONLY                                 **
     **                USE OLD GEOGRAPHIC AREA COORDINATES          **
     **                USE OLD ZONE NUMBER AND 1ST LETTER           **
     **                USE OLD 100,000 METER GRID LETTERS           **
     **                                                             **
     **    NLTRS = 1 = ZONE NUMBER AND 1ST LETTER **ONLY**          **
     **                                                             **
     **    NLTRS = 2 = 100,000 METER GRID LETTERS AND NUMBERS **ONLY**
     **                USE OLD GEOGRAPHIC AREA COORDINATES          **
     **                USE OLD ZONE NUMBER AND 1ST LETTER           **
     **                                                             **
     **    NLTRS = 3 = COMPLETELY NEW MGRS ENTRY                    **
     **                                                             **
     *****************************************************************
     ***** NUMBERS ONLY *****
*/
    if(nltrs == 0) goto S6000;
/*
     ***** 100,000 METER LETTERS (2) AND NUMBERS ONLY *****
*/
    if(nltrs == 2) goto S5500;
/*
     ***** NUMBER OF LETTERS IS 1 -OR- 3 *****
     *** SIGN OF HEMISPHERE (NORTH OR SOUTH ) FOR GRID NORTHING  ***
     *** LETTERS A-M ( 1-13) SOUTH - SIGN = -1.0
     *** LETTERS N-Z (14-26) NORTH - SIGN =  1.0
     ** NORTH HEMISPHERE
*/
    sign = 1.0;
/*
     ** SOUTH HEMIPSHERE
*/
    if(ltrnum[0] < 14) sign = -1.0;
/*
     *****************************************************************
     ***      DETERMINE AREA NUMBER (IAREA) FROM 1ST LETTER        ***
     ***                                                           ***
     ***   IAREA = 1 = UTM - LETTERS - C TO W                      ***
     ***                     EXCEPTION OF LETTERS I & O            ***
     ***                                                           ***
     ***           2 = UPS - NORTH ZONE - LETTERS Y & Z            ***
     ***                                                           ***
     ***           3 = UPS - SOUTH ZONE - LETTERS A & B            ***
     *****************************************************************
     ***** UPS -  NORTH ZONE *****
*/
    if(ltrnum[0] == 25 || ltrnum[0] == 26) goto S4000;
/*
     ***** UPS -  SOUTH ZONE *****
*/
    if(ltrnum[0] == 1 || ltrnum[0] == 2) goto S4200;
/*
     *****************************************************************
     ***** UTM ***** UTM ***** UTM ***** UTM ***** UTM ***** UTM *****
*/
    iarea = 1;
/*
     *** VALIDITY CHECKS ***
     ** MUST HAVE A NUMBER FOR ZONE IN LOCATION 1 (NOT A LETTER)**
*/
    if(ltrloc[0] == 1) goto S8810;
/*
     ** ZONE NUMBER CANNOT BE 3 DIGITS OR MORE **
*/
    if(ltrloc[0] > 3) goto S8810;
/*
     ** CHECK IF ZONE NUMBER IS ONE OR TWO DIGITS **
*/
    if(ltrloc[0] == 3) goto S2600;
/*
     ** ZONE NUMBER IS ONE DIGIT  **
     ** SHIFT MGRS TO RIGHT BY 1 SPACE **
*/
    shiftr(mgrs,20,1L,&nchar);
/*
     ** INCREMENT APPLICABLE VARIABLES BY 1 **
*/
    ftnsac(mgrs,1,blank,1);
    for(i=1; i<=nltrs; i++) ltrloc[i-1] += 1;
S2600:
/*
     *** VALIDITY CHECK - MAXIMUM NUMBER OF CHARACTERS ***
*/
    if(nchar > imax) {
/*        printf ("bailing 3 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */
/*
     ***** UTM ZONE NUMBER *****
*/
    *izone = 0;
    for(i=1; i<=2; i++) {
        ftnsac((AZONE+(short)i-1),1,blank,1);
        if(ftncms((mgrs+(short)i-1),1,blank,1) ==
          0) goto S2900;
        for(j=1; j<=11; j++) {
            if(ftncms((mgrs+(short)i-1),1,&num[j-1],1) == 0) 
            	goto S2800;
            if(j == 11) goto S8806;
        }
S2800:
        ftnsac((AZONE+(short)i-1),1,&num[j-1],1);
        *izone = *izone*10+j-1;
S2900:
        continue;
    }
/*
     ***** VALIDITY CHECKS *****
     ** ZONE NUMBER RANGE 1 -60 **
*/
    if(*izone < 1 || *izone > 60) goto S8810;
/*** BASIC Madtran sets mgrs to "  INVALID MGRS  ", sets ierr = 1, and exits
     when X used with zones 32, 34, 36 as below.  CMAD program has a separate
     function, check_for_x_ltr_error() which does this check before calling
     mgrtgp(). */
/*
     ** ZONE 32 - LETTER 'X' DOES NOT EXIST **
*/
    if(*izone == 32 && ltrnum[0] == 24) goto S8810;
/*
     ** ZONE 34 - LETTER 'X' DOES NOT EXIST **
*/
    if(*izone == 34 && ltrnum[0] == 24) goto S8810;
/*
     ** ZONE 36 - LETTER 'X' DOES NOT EXIST **
*/
    if(*izone == 36 && ltrnum[0] == 24) goto S8810;
/*
     *********************************************************
     ***** DETERMINE GEOGRAPHIC COORDINATES OF RECTANGLE *****
     *****    AS DEFINED BY ZONE NUMBER AND 1ST LETTER   *****
     *********************************************************
     *** LATITUDE AND LONGITUDE LIMITS OF UTM GEOGRAPHIC AREA ***
     *** AN 8 DEGREE N-S BY 6 DEGREE E-W AREA FOR STD ZONES   ***
     *** WHERE:
     ***        LTRNUM(1) = 1ST LETTER NUMBER
     ***        lat      = LATITUDE OF POINT
     ***        IZONE     = ZONE NUMBER
     ***        SPSOU     = SOUTH LATITUDE OF AREA
     ***        SPNOR     = NORTH LATITUDE OF AREA
     ***        SLEAST    = EAST LONGITUDE OF AREA
     ***        SLWEST    = WEST LONGITUDE OF AREA
     ***
*/
    utmlim(&ltrnum[0],*lat,*izone,&spsou,&spnor,&sleast,&slwest);
/*
     *** COMPUTE GRID COORDINATES ***
         HOLDING ZONE FIXED
*/
    *lat = spsou;
    *lon = slwest;
	geo_to_utm( cnsts,*lat,*lon,izone,x,y );
	if( *lat < 0.0 ) *y *= -1.0;
/*
     *** CENTRAL MERIDIAN ***
*/
    slcm = (double)(*izone*6-183);
/*
     *** DETERMINE LOWEST NORTHING OF GEOGRAPHIC AREA ***
         SOUTH LATITUDE AT CENTRAL MERIDIAN
*/
    geo_to_utm( cnsts, spsou,slcm,izone,&xltr,&yltr );
	if( *lat < 0.0 ) yltr *= -1.0;
/*
     *** SCALE NUMBER DOWN TO NEAREST 100,000 UNIT ***
*/
    ylow = fifdnint((double)fifidint(yltr/oneht)*oneht);
/*
     *** SCALE NUMBER DOWN TO LESS THAN 2 MILLION ***
*/
    yslow = ylow;
S3020:
    if(yslow < twomil) goto S3030;
    yslow -= twomil;
    goto S3020;
S3030:
    yslow = fifdnint(yslow);
/*
     *** USING THE ZONE NUMBER, SPHEROID CODE, ETC ***
     *** DETERMINE
     *** 'LOW' AND 'HIGH' (2ND) LETTER NUMBER
     *** FALSE NORTHING FOR 3RD LETTER
*/
    utmset( get_group( *GROUP, *izone), *izone,&ltrlow,&ltrhi,&fnltr );
/*
     *** DUMMY VALUES FOR UPS VARIABLES FOR SAVE ROUTINE ***
*/
    ltrhy = 0;
    feltr = zero;
/*
     ****** 3 CHARACTERS ONLY ******
*/
    Tomgrda.ncharo = 3;
    if(nchar == 3) {
/*        printf ("jumping to s8100 from 1528\n"); */
      goto S8100;
    }
S3050:
/*
     *********************************************************
     ****** -UTM- 100,000 METER LETTERS (2) AND NUMBERS ******
     *********************************************************
     **** ENTRY WHEN 'OLD' ZONE NUMBER & 1ST LETTER ARE UNCHANGED ****
     ***** VALIDITY CHECKS *****
     ** 2ND LETTER **
*/
    if(ltrnum[1] < ltrlow) ierr = areaOK = 17;
    if(ltrnum[1] > ltrhi) ierr = areaOK = 17;
/*
     ** 3RD LETTER ** MAY ONLY BE 'A' TO 'V' **
*/
    if(ltrnum[2] > 22) goto S8822;
/*
     ***** -UTM- 100,000 METER GRID SQUARES *****
*/
    yltr = (double)(ltrnum[2]-1)*oneht+fnltr;
    xltr = (double)(ltrnum[1]-ltrlow+1)*oneht;
/*
     ** CORRECT EASTING FOR LETTER 'O' (15) **
     ** WHEN LTRLOW = 10 = LETTER 'J'
*/
    if(ltrlow == 10 && ltrnum[1] > 15) xltr -= oneht;
/*
     ** CORRECT NORTHING FOR LETTERS 'I' AND 'O' **
*/
    if(ltrnum[2] > 15) yltr -= oneht;
    if(ltrnum[2] > 9) yltr -= oneht;
/*
     *** SCALE NUMBER DOWN UNTIL 2 MILLION OR LESS ***
*/
    if(fifdnint(yltr) >= fifdnint(twomil)) yltr -= twomil;
    yltr = fifdnint(yltr);
/*
     *** DIFFERENCE IN NORTHING BETWEEN TWO SCALED DOWN VALUES
     *** YSLOW - VALUE COMPUTED BASED ON LOWEST LATITUDE AT CM
     *** YLTR  - VALUE COMPUTED FOR LETTER INPUT
*/
    yltr -= yslow;
/*
     ** IF LESS THAN ZERO ADD 2 MILLION **
*/
    if(yltr < zero) yltr += twomil;
/*
     *** NORTHING OF MGRS TO NEAREST 100,000 UNIT ***
*/
    yltr = fifdnint(ylow+yltr);
/*
     ***** NORTHING AND EASTING *****
*/
    *x = xltr;
    *y = yltr*sign;
/*
     *****************************************************
     *****    NUMBER OF CHARACTERS  = 5 - DONE       *****
     ***** TRANSFER TO COORDINATE CONVERSION ROUTINE *****
     ***** CHECK AGAINST AREA COORDINATE LIMITS      *****
*/
    Tomgrda.ncharo = nchar;
    if(nchar == 5) {
/*        printf ("jumping to s7900 from 1593\n"); */
      goto S7900;
    }
/*
     ***** TRANSFER TO NUMBER DERIVATION SECTION *****
*/
    goto S7000;
S4000:
/*
     ***** UTM ***** UTM ***** UTM ***** UTM ***** UTM ***** UTM *****
     *****************************************************************
     ***** UPS ***** UPS ***** UPS ***** UPS ***** UPS ***** UPS *****
     ***** UPS - NORTH ZONE *****
*/
    iarea = 2;
/*
     ** LATITUDE LIMIT
*/
    spsou = spnlim;
/*
     ** LONGITUDE FOR 1 LETTER (90 DEG EAST OR WEST) **
*/
    *lon = 90.0;
    if(ltrnum[0] == 25) *lon = -*lon;
/*
     ** ZONE NUMBER 1ST LETTER 'N' **
*/
    ftnsac(AZONE,1,&albet[13],1);
    goto S4300;
S4200:
/*
     ***** UPS - SOUTH ZONE *****
*/
    iarea = 3;
/*
     ** LATITUDE LIMIT **
*/
    spsou = spslim;
/*
     ** LONGITUDE FOR 1 LETTER (90 DEG EAST OR WEST) **
*/
    *lon = 90.0;
    if(ltrnum[0] == 1) *lon = -*lon;
/*
     ** ZONE NUMBER 1ST LETTER 'S' **
*/
    ftnsac(AZONE,1,&albet[18],1);
    goto S4300;
S4300:
/*
     *** VALIDITY CHECK ***  1ST LETTER MUST BE IN LOCATION 1 ***
*/
    if(ltrloc[0] != 1) goto S8830;
/*
     *** SHIFT MGRS RIGHT BY TWO LOCATIONS ***
*/
    shiftr(mgrs,20,2L,&nchar);
/*
     ** SET ZONE NUMBER OF MGRS TO BLANKS **
*/
    ftnsac(mgrs,1,blank,1);
    ftnsac((mgrs+1),1,blank,1);
/*
     ** SET ZONE NUMBER AND 2ND ZONE LETTER 'P' **
*/
    *izone = 0;
    ftnsac((AZONE+1),1,&albet[15],1);
/*
     ** LATITUDE **
*/
    *lat = spsou;
/*
     ** VALIDITY CHECK **
*/
    if(nchar > imax) {
/*        printf ("bailing 4 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */

/*
     *** CONVERT GEOGRAPHIC TO GRID COORDINATES ***
*/
    init_ups(UPS, AXIS, ECC2, ( *lat < 0.0 ? -90.0 : 90.0 ) );
    geo_to_ups( UPS, *lat,*lon, x,y);
/*
     *** DEFINE DUMMY VARIABLES FOR UTM ***
     *** FOR SAVE SUBPROGRAM
*/
    spnor = *lat;
    sleast = slwest = *lon;
    ylow = yslow = zero;
/*
     ***** FOR AREA IN USE SET *****
     *** 'LOW' AND 'HIGH' (2ND) LETTER LIMITS
     *** FALSE EASTING FOR 2ND LETTER
     *** FALSE NORTHING FOR 3RD LETTER
     *** HIGH (3RD) LETTER NUMBR
*/
    if(iarea == 2) upnset(ltrnum[0],&ltrlow,&ltrhi,&feltr,&fnltr,&ltrhy);
    if(iarea == 3) upsset(ltrnum[0],&ltrlow,&ltrhi,&feltr,&fnltr,&ltrhy);
    Tomgrda.ncharo = nchar;
    if(nchar == 3) {
/*        printf ("jumping to s8100 from 1685\n"); */
      goto S8100;
    }
S5000:
/*
     *********************************************************
     ****** -UPS- 100,000 METER LETTERS (2) AND NUMBERS ******
     *********************************************************
     **** ENTRY WHEN 'OLD' ZONE NUMBER & 1ST LETTER ARE UNCHANGED ****
     *** VALIDITY CHECKS ***
     ** 2ND LETTER **
*/
    if(ltrnum[1] < ltrlow) goto S8820;
    if(ltrnum[1] > ltrhi) goto S8820;
/*
     ** 3RD LETTER **
*/
    if(ltrnum[2] > ltrhy) goto S8822;
/*
     ** VALIDITY CHECK FOR EXCLUDED 2ND LETTERS **
     ** 'D'
*/
    if(ltrnum[1] == 4) goto S8820;
/*
     ** 'E'
*/
    if(ltrnum[1] == 5) goto S8820;
/*
     ** 'M'
*/
    if(ltrnum[1] == 13) goto S8820;
/*
     ** 'N'
*/
    if(ltrnum[1] == 14) goto S8820;
/*
     ** 'V'
*/
    if(ltrnum[1] == 22) goto S8820;
/*
     ** 'W'
*/
    if(ltrnum[1] == 23) goto S8820;
/*
     *** NORTHING ***
*/
    yltr = (double)(ltrnum[2]-1)*oneht+fnltr;
/*
     ** CORRECTION FOR 'I' & 'O'
*/
    if(ltrnum[2] > 9) yltr -= oneht;
    if(ltrnum[2] > 15) yltr -= oneht;
/*
     *** EASTING ***
*/
    xltr = (double)(ltrnum[1]-ltrlow)*oneht+feltr;
/*
     *** CORRECT LONGITUDE FOR EXCLUDED LETTERS BASED ON HEMISPHERE ***
*/
    if(ltrlow == 1) goto S5200;
/*
     ** WESTERN HEMISPHERE **
     ** LETTERS 'M', 'N', 'O', 'V', 'W' **
*/
    if(ltrnum[1] > 12) xltr -= (3.0*oneht);
    if(ltrnum[1] > 21) xltr -= (2.0*oneht);
    goto S5300;
S5200:
/*
     ** EASTERN HEMISPHERE **
     ** LETTERS 'D', 'E', 'I', 'M', 'N', 'O'  **
*/
    if(ltrnum[1] > 3) xltr -= (2.0*oneht);
    if(ltrnum[1] > 9) xltr -= oneht;
    if(ltrnum[1] > 13) xltr -= (3.0*oneht);
    goto S5300;
S5300:
/*
     *** NORTHING AND EASTING ***
*/
    *x = xltr;
    *y = yltr*sign;
/*
     *****************************************************
     *****    NUMBER OF CHARACTERS  = 5 - DONE       *****
     ***** TRANSFER TO COORDINATE CONVERSION ROUTINE *****
     ***** CHECK AGAINST AREA COORDINATE LIMITS      *****
*/
    Tomgrda.ncharo = nchar;
    if(nchar == 5) {
/*        printf ("jumping to s7900 from 1778\n"); */
      goto S7900;
    }
/*
     ***** TRANSFER TO NUMBER DERIVATION SECTION *****
*/
    goto S7000;
S5500:
/*
     ***** UPS ***** UPS ***** UPS ***** UPS ***** UPS ***** UPS *****
     *****************************************************************
     ********* 2 LTRS ***** 2 LTRS ***** 2 LTRS ***** 2 LTRS *********
     *****  100,000 METER LETTERS (2) AND NUMBERS  *****
     ***** USE PREVIOUS ZONE NUMBER AND 1ST LETTER *****
     *** VALIDITY CHECK ***
     *** 1ST LETTER MUST BE IN LOCATION 1 ***
*/
    if(ltrloc[0] != 1) goto S8830;
/*
     ** SHIFT MGRS TO RIGHT 3 PLACES **
*/
    shiftr(mgrs,20,3L,&nchar);
/*
     ** RESTORE 'OLD' MGRS **
*/
    for(i=1; i<=3; i++) 
    	ftnsac((mgrs+(short)i-1),1,
    		(Tomgrch.mgrso+(short)i-1),1);
/*
     ** VALIDITY CHECK **
*/
    if(nchar > imax) {
/*        printf ("bailing 5 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */

/*
     ** SHIFT LETTER LOCATIONS **
*/
    for(i=1; i<=3; i++) ltrloc[i-1] = Tomgrda.ltrlco[i-1];
/*
     ** LTR NUMBER CHANGE **
*/
    ltrnum[2] = ltrnum[1];
    ltrnum[1] = ltrnum[0];
/*
     *** USE PREVIOUS 'OLD' VALUES FOR REMAINDER OF MGRS ***
*/
    ltrnum[0] = Tomgrda.ltrnmo[0];
    spsou = Tomgrda.spsouo;
    spnor = Tomgrda.spnoro;
    sleast = Tomgrda.sleaso;
    slwest = Tomgrda.slweso;
    ltrlow = Tomgrda.ltrlo;
    ltrhi = Tomgrda.ltrho;
    ltrhy = Tomgrda.ltrhyo;
    ylow = Tomgrda.ylowo;
    yslow = Tomgrda.yslowo;
    *izone = Tomgrda.izoneo;
    iarea = Tomgrda.iareao;
    fnltr = Tomgrda.fnltro;
    feltr = Tomgrda.feltro;
    ftnsac(AZONE,1,Tomgrch.azoneo,1);
    ftnsac((AZONE+1),1,(Tomgrch.azoneo+1),1);
/*
     *** NORTH OR SOUTH HEMISPHERE SIGN ***
*/
    sign = 1.0;
    if(ltrnum[0] < 14) sign = -1.0;
/*
     ***** TRANSFER TO APPROPRIATE 2 LETTER SECTION BASED ON AREA *****
     *** UTM ***
*/
    if(iarea == 1) goto S3050;
/*
     *** UPS ***
*/
    goto S5000;
S6000:
/*
     ********* 2 LTRS ***** 2 LTRS ***** 2 LTRS ***** 2 LTRS *********
     *****************************************************************
     ***** NUM ***** NUM ***** NUM ***** NUM ***** NUM ***** NUM *****
     ***           MGRS NUMBERS PART ONLY             ***
     ***  USE PREVIOUS ZONE NUMBER AND 1ST LETTER
     ***  USE PREVIOUS 2ND AND 3RD LETTER FOR 100,000 METER GRID SQUARE
     ** VALIDITY CHECK - NCHAR FROM LAST COMP MUST BE 5 OR MORE **
*/
    if(Tomgrda.ncharo < 5) goto S8834;
/*
     *** SHIFT MGRS TO RIGHT 5 PLACES ***
*/
    shiftr(mgrs,20,5L,&nchar);
/*
     ** RESTORE 'OLD' MGRS VALUES
*/
    for(i=1; i<=5; i++) 
    	ftnsac((mgrs+(short)i-1),1,
    		(Tomgrch.mgrso+(short)i-1),1);
/*
     ** VALIDITY CHECK **
*/
    if(nchar > imax) {
/*        printf ("bailing 6 because nchar (%d) > imax (%d)\n", nchar, imax); */
      goto S8802;
    }
/*      printf ("continuing with nchar %d imax %d\n", nchar, imax); */
/*
     *** USE PREVIOUS 'OLD' VALUES FOR REMAINDER OF MGRS ***
*/
    for(i=1; i<=3; i++) {
        ltrnum[i-1] = Tomgrda.ltrnmo[i-1];
        ltrloc[i-1] = Tomgrda.ltrlco[i-1];
    }
    xltr = Tomgrda.xltro;
    yltr = Tomgrda.yltro;
    spsou = Tomgrda.spsouo;
    spnor = Tomgrda.spnoro;
    sleast = Tomgrda.sleaso;
    slwest = Tomgrda.slweso;
    ylow = Tomgrda.ylowo;
    yslow = Tomgrda.yslowo;
    *izone = Tomgrda.izoneo;
    iarea = Tomgrda.iareao;
    ltrlow = Tomgrda.ltrlo;
    ltrhi = Tomgrda.ltrho;
    ltrhy = Tomgrda.ltrhyo;
    fnltr = Tomgrda.fnltro;
    feltr = Tomgrda.feltro;
    ftnsac(AZONE,1,Tomgrch.azoneo,1);
    ftnsac((AZONE+1),1,(Tomgrch.azoneo+1),1);
/*
     *** NORTH OR SOUTH HEMISPHERE SIGN ***
*/                                   
    sign = 1.0;
    if(ltrnum[0] < 14) sign = -1.0;
/*
     ***** TRANSFER TO NUMBER DERIVATION SECTION ******
                   (NEXT AREA OF CODE)
*/
    goto S7000;
S7000:
/*
     *****************************************************************
     *****************************************************************
     ****** NUMBER PART OF MGRS (LOCATIONS 6 TO NCHAR) ******
     ** NUMBER OF NUMBERS **
*/
    nnum = nchar-5;
/*
     *** VALIDITY CHECK - NUMBER OF NUMBERS MUST BE EVEN ***
*/
    if(nnum/2*2 != nnum) goto S8840;
/*
     *** EASTING PART ***
*/
    xnum = 0.0;
    k = nnum/2+5;
    for(i=6; i<=k; i++) {
        for(j=1; j<=11; j++) {
            if(ftncms((mgrs+(short)i-1),1,&num[j-1],1) == 0) 
            	goto S7200;
            if(j == 11) goto S8806;
        }
S7200:
        xnum = xnum*ten+(double)(j-1);
    }
    xnum *= pow(ten,(double)(5-k+6-1));
/*
     *** NORTHING PART ***
*/
    ynum = 0.0;
    k += 1;
    for(i=k; i<=nchar; i++) {
        for(j=1; j<=11; j++) {
	  if(ftncms((mgrs+(short)i-1),1,&num[j-1],1) == 0) {
/*  	    printf ("jumping to s7600 from 1949\n"); */
            	goto S7600;
	  }
	  if(j == 11) goto S8806;
        }
/*  	printf ("continuing to s7600\n"); */
S7600:
        ynum = ynum*ten+(double)(j-1);
    }
    ynum *= pow(ten,(double)(5-nchar+k-1));
/*
     ***** NORTHING AND EASTING *****
*/
    *y = (yltr+ynum)*sign;
    *x = xltr+xnum;
/*      printf ("continuing to s7900\n"); */
S7900:
/*
     ***** COMPUTE GEOGRAPHIC COORDINATES *****
     *************** - MGRS COORDINATES ONLY - ***********************
     *** 1 METER IN GEOGRAPHIC COORD OVERLAP ALLOWANCE FOR ROUNDUP ***
     *** 1 METER OF LATITUDE = DEGRAD/(31*3600) = 1.56D-7 (APPROX) ***
     *** 1 METER OF LONGITUDE = 1 METER LATITUDE/(COS(LATITUDE))   ***
     ** LATITUDE ALLOWANCE FOR OVERLAP **
*/
    ynum = 1.56E-7;
    
    if(iarea > 1) goto S8000;
/*
     ** UTM **
*/
    if( lat && lon ) 
    {	utm_to_geo( cnsts, *izone, *x,*y, lat,lon );
/*
     ** VALIDITY CHECK FOR INSIDE AREA GEOGRAPHIC LIMITS **
*/
    	if(*lat+ynum < spsou || *lat-ynum > spnor) ierr = areaOK = 17;
/*
     ** LONGITUDE ALLOWANCE FOR OVERLAP **
*/
    	xnum = ynum/cos(*lat);
    	if(*lon+xnum < slwest || *lon-xnum > sleast) ierr = areaOK = 17;
    }
/*      printf ("jumping to s8100 from 1976\n"); */
    goto S8100;
S8000:
/*
     ** UPS **
*/
    if( lat && lon )     
    {	ups_to_geo( UPS, *x,*y, lat,lon );
/*
     ** VALIDITY CHECK FOR INSIDE AREA GEOGRAPHIC LIMIT **
     * NORTH ZONE
*/
    	if(iarea == 2 && *lat+ynum < spsou) goto S8844;
/*
     * SOUTH ZONE
*/
    	if(iarea == 3 && *lat-ynum > spsou) goto S8844; 
    }
    else
    	*izone = 0;
/*      printf ("continuing to s8100\n"); */
    goto S8100;
S8100:
/*
     ***** BLANK OUT UNUSED PART OF MGRS *****
*/
    j = nchar+1;
    for(i=j; i<=isize; i++) 
    	ftnsac((mgrs+(short)i-1),1,blank,1);
    mgrs[isize] = 0;
/*      printf ("returning line 2004\n"); */
    return areaOK;
S8600:
/*
     *******************************************************
     ***** MGRS = 99 - USER WANTS MENU MODULE          *****
     ***** SET ZONE TO 99 AND RETURN TO MAIN PROGRAM   *****
*/
    *izone = 99;
/*      printf ("returning line 2013\n"); */
    return ierr;
S8802:
/*
     **********************************************************
     ***** ERROR IN MGRS DATA *****
     *** NUMBER OF CHARARCTERS ***
*/
/*  	printf ("jumping to S8888 from 2039\n"); */
    goto S8888;
S8804:
/*
     *** MAX NUMBER OF LETTERS OR LOCATION OF THEM ***
*/
/*  	printf ("jumping to S8888 from 2044\n"); */
    goto S8888;
S8806:
/*
     *** INVALID CHARACTER - I, O, DECIMAL (.), ETC ***
*/
/*  	printf ("jumping to S8888 from 2049\n"); */
    goto S8888;
S8808:
/*
     *** LETTERS NOT IN SEQUENCE ***
*/
/*  	printf ("jumping to S8888 from 2054\n"); */
    goto S8888;
S8810:
/*
     *** UTM ZONE NUMBER ***
*/
/*  	printf ("jumping to S8888 from 2059\n"); */
    goto S8888;
S8820:
/*
     *** 2ND LETTER ***
*/
/*  	printf ("jumping to S8888 from 2064\n"); */
    goto S8888;
S8822:
/*
     *** 3RD LETTER ***
*/
/*  	printf ("jumping to S8888 from 2069\n"); */
    goto S8888;
S8830:
/*
     *** 1ST CHARACTER MUST BE A LETTER ***
*/
/*  	printf ("jumping to S8888 from 2074\n"); */
    goto S8888;
S8834:
/*
     *** NOT ENOUGH CHARACTERS FROM PERVIOUS COMPUTATION ***
*/
/*  	printf ("jumping to S8888 from 2079\n"); */
    goto S8888;
S8840:
/*  	printf ("jumping to S8888 from 2081\n"); */
	goto S8888;
/*
     *** NUMBER OF CHARACTERS FOR NUMBERS NOT EVEN ***
*/
	/*
     		*** OUTSIDE GEOGRAPHIC AREA LIMITS [Latitude] ***
		*/
S8844:
/*  	printf ("jumping to S8888 from 2089\n"); */
		goto S8888;

#if 0
S8845:
		/*
     		*** OUTSIDE GEOGRAPHIC AREA LIMITS [Longitude] ***
		*/
    		/*	06/06/96 - fix to get SW corner of partial cell for F-16
				avionics system.  RLacey DMA/ATIRS.
		*/
/*      printf ("returning line 2081\n"); */
		return ierr;
#endif
S8888:

/*
     ***** SET ERROR CODE TO 'ON' = 1 *****
*/
    ierr = 1;
/*      printf ("returning line 2091\n"); */
    return ierr;
}


int mgrs_limits
	(	const void* cnstsP, double* mnlat, double* mnlon, double* mxlat, double* mxlon )
	{	if( !cnstsP || !mnlat || !mxlat || !mnlon || !mxlon )
			return -1;
		else
		{	*mnlat = -90.0; *mxlat = 90.0; 
			*mnlon = -180.0; *mxlon = 180.0;
			return 0;
		}
	}

static short select_group( double a, double rf )
	{	short group = 0;
		if( a == 6377397.155 || a == 6378249.145 )	/* Bessel 1841 & Clarke 1880 */
			group = 2;
		else
		if( a == 6378206.4 )   /* Clarke 1866 */
			group = 3;
		else
			group = 1;
		return group;
	}
short init_mgrs( void* cnstsP, double a, double rf )
	{	proj_dfn* cnsts = (proj_dfn*)cnstsP;
		if( cnsts )	
		{	init_utm(cnsts,a,rf);
			*GROUP = select_group(a,rf);
			AZONE[0] = 0;
			return 0;
		}
		else
			return -1;
	}
const void* set_mgrs( double a, double rf )
	{	proj_dfn* cnsts = (proj_dfn*)allocate_projection(1);
		init_mgrs( cnsts, a,rf );
		return cnsts;
	}

#include <ctype.h>
static short mgrs_limit_check( const char* mgrs, double lat, double lon )
	{	if( isdigit(mgrs[0]) )
		{ 	long zone;
			int k = 0;
			char azone[3];
			azone[k++] = mgrs[0];
			if( isdigit(mgrs[1]) ) azone[k++] = mgrs[1];
			azone[k] = 0;
			zone = atol(azone);
			zone = zone*6-186;
			return ( lon >= zone && lon <= zone+6 ? 0 : 1 );
		}
		else
		if( mgrs[0] <= 'B' )
			return ( lat <= -80.0 ? 0 : 1 );
		else
		if( mgrs[0] >= 'Y' )
			return ( lat >= 84.0 ? 0 : 1 );
	  return 1; /* TODO - should this be 1? */
	}
			
short mgrs_to_geo
	( const void* tcnsts, const char* mgrs, double* lat, 
		double* lon, long* izone, double* x, double* y )
	{	char tmp[64];
		short k = _mgrtxx( (proj_dfn*)tcnsts, strcpy(tmp,mgrs),
                                   lat, lon, izone, y,x );
/*  		printf ("_mgrtxx returned %d\n", k); */
		return ( k == 0 ? mgrs_limit_check( tmp,*lat,*lon ) : k ); 
	} 
		 
short geo_to_mgrs
	( const void* tcnsts, double lat, double lon, char* mgrs,
		long* zone, double* x, double* y ) 
	{	xxtmgr( (proj_dfn*)tcnsts, &lat,&lon, zone,y,x, mgrs, 1 );
		return OK;
	} 
															 
#if 0
int main()
{	char mgrs[64],mgrsx[64];
	short status = 0;
	double lat,lon, y,x;   	  							   
	long zone;												 
	double wgs84[] = { 6378137.000,	298.257223563 };		 
	double Bessel1841[] = { 6377397.155, 299.1528128 };	 
	double Clarke1866[] = { 6378206.4, 294.9786982 };	 

	const void* cnstsP = set_mgrs( wgs84[0],wgs84[1] );
	const void* bcnstsP = set_mgrs( Bessel1841[0],Bessel1841[1] );
	const void* ncnstsP = set_mgrs( Clarke1866[0],Clarke1866[1] );
	
	strcpy(mgrs,"18XVK0000000000 ");
	status = mgrs_to_geo( cnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s %d: %lf %lf : %ld %lf %lf\n\n", mgrs, status, lat,lon,zone,x,y );
	geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n\n\n", mgrsx, status, lat,lon,zone,x,y );
	status = mgrs_to_geo( ncnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s %d: %lf %lf : %ld %lf %lf\n\n", mgrs, status, lat,lon,zone,x,y );
	geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n\n\n", mgrsx, status, lat,lon,zone,x,y );

	lat = 31.0; lon = -84.0;
	geo_to_mgrs( cnstsP, lat,lon, mgrs, &zone, &x, &y );	   
	printf( "%s : %lf %lf : %ld %lf %lf\n", mgrs, lat,lon,zone,x,y );
	mgrs_to_geo( cnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s : %lf %lf : %ld %lf %lf\n\n", mgrs, lat,lon,zone,x,y );
	lat = -37.0; lon = 105.0;
	geo_to_mgrs( cnstsP, lat,lon, mgrs, &zone, &x, &y );	   
	printf( "%s : %lf %lf : %ld %lf %lf\n", mgrs, lat,lon,zone,x,y ); 
	mgrs_to_geo( cnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s : %lf %lf : %ld %lf %lf\n\n", mgrs, lat,lon,zone,x,y );

	lat = 31.0; lon = -84.0;
	geo_to_mgrs( bcnstsP, lat,lon, mgrs, &zone, &x, &y );	   
	printf( "%s : %lf %lf : %ld %lf %lf\n", mgrs, lat,lon,zone,x,y );
	mgrs_to_geo( bcnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s : %lf %lf : %ld %lf %lf\n\n", mgrs, lat,lon,zone,x,y );
	lat = -37.0; lon = 105.0;
	geo_to_mgrs( bcnstsP, lat,lon, mgrs, &zone, &x, &y );	   
	printf( "%s : %lf %lf : %ld %lf %lf\n", mgrs, lat,lon,zone,x,y ); 
	mgrs_to_geo( bcnstsP, mgrs, &lat, &lon, &zone, &x, &y );
	printf( "%s : %lf %lf : %ld %lf %lf\n\n", mgrs, lat,lon,zone,x,y );

	scanf( "%d", &zone );

	return 0;
}
#endif /* Test MGRS */

#if 0
#include "stdiofns.h"

int main()
{	const proj_dfn cnsts;
	double lat,lon, north,east,bnorth,beast,  blat,blon;
	/* Default is Clarke 1866 */
	double a = 6378206.4, rf = 294.9786982;
	double lat0 = 40.0,lon0 = -75.0, k0 = 0.9996;
	int n,k = 0;
	long zone1,bzone;
	char mgrs[256];
	
	if( (k=proj_pars( &a, &rf, NULL, NULL, NULL, NULL, NULL,NULL )) == 2 )
	{	;
		if( init_mgrs( &cnsts, a,rf ) != 0 ) 
		{	printf( "ERROR: INITIALIZATION FAILED.\n");
			return 2;
		}
	}
	else
	{	printf( "Projection not defined: %d.\n",k );
		return 1;
	}
	print_projection( cnsts,stdout ); printf("\n");
	print_test_hdr();
	
	n=0;
	while( proj_test_points( n, lat0,lon0, &lat, &lon ) >= 0 )
	{	if( (k=geo_to_mgrs( cnsts, lat,lon, mgrs, &zone1, &east, &north )) == 0 )
		{	if( (k=mgrs_to_geo( cnsts, mgrs, &blat,&blon, &bzone, &beast, &bnorth )) == 0 )
			{	printf( "%s : [%ld %ld]\n", mgrs, zone1, bzone );
				print_test_data( lat,lon, east,north, blat,blon );
				print_test_data( blat,blon, beast,bnorth, lat,lon );
			}
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf %lf\n", k,lat,lon );
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k,lat,lon );
		n++;		
	}
	while( proj_data( &lat,&lon ) == 2 )
	{	if( (k=geo_to_mgrs( cnsts, lat,lon, mgrs, &zone1, &east, &north )) == 0 )
		{	if( (k=mgrs_to_geo( cnsts, mgrs, &blat,&blon, &bzone, &beast, &bnorth )) == 0 )
			{	printf( "%s : [%ld %ld]\n", mgrs, zone1, bzone );
				print_test_data( lat,lon, east,north, blat,blon );
				print_test_data( blat,blon, beast,bnorth, lat,lon );
			}
			else
				printf( "ERROR %d: PROJECTION TO GEO %lf %lf\n", k,lat,lon);
		}
		else
			printf( "ERROR %d: GEO TO PROJECTION %lf %lf\n", k,lat,lon );		
	}
	return 0;
}

#endif
#if 0
int main()
{	FILE* fp = stdout; // fopen( "mgrstest.txt", "w" );
	char mgrsx[64];
	short status = 0;
	double lat,lon, y,x;   	  							   
	long zone;												 
	double wgs84[] = { 6378137.000,	298.257223563 };		 
	
	const void* cnstsP = set_mgrs( wgs84[0],wgs84[1] );
	const char* mgrs[] = { };

	long npts = sizeof(data)/sizeof(data[0]);
	fprintf( fp, "%ld\n" );
	while( --npts >= 0 )
	{	status = mgrs_to_geo( cnstsP, mgrs[npts], &lat, &lon, &zone, &x, &y );
		fprintf( fp, "%d %s: %lf %lf : %ld %lf %lf\n", 
			status, mgrs, status, lat,lon,zone,x,y );
		geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
		fprintf( "%d %s: %lf %lf : %ld %lf %lf\n", 
			status, mgrsx, lat,lon,zone,x,y );
	}
	if( fp != stdout ) 
		fclose(fp);
	else
		scanf( "%d", &status );
	return 0;
}
#endif /* Test MGRS */
#if 0
int main()
{	char mgrsx[64];
	short status = 0;
	double lat=72.0,lon=27.0, y,x;   	  							   
	long zone;												 
	double wgs84[] = { 6378137.000,	298.257223563 };		 

	const void* cnstsP = set_mgrs( wgs84[0],wgs84[1] );
	
	status = geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n", mgrsx, status, lat,lon,zone,x,y );

	mgrs_to_geo (cnstsP, mgrsx, & lat, & lon, & zone, & x, & y);
	printf ("lat %f lon %f\n", lat, lon);

	lon=20.999995;
	status = geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n", mgrsx, status, lat,lon,zone,x,y );

	mgrs_to_geo (cnstsP, mgrsx, & lat, & lon, & zone, & x, & y);
	printf ("lat %f lon %f\n", lat, lon);

	lon=21.0;
	status = geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n", mgrsx, status, lat,lon,zone,x,y );
	
	mgrs_to_geo (cnstsP, mgrsx, & lat, & lon, & zone, & x, & y);
	printf ("%s lat %f lon %f\n", mgrsx, lat, lon);

	printf ("cnstsP %ld\n", cnstsP);
	mgrs_to_geo (cnstsP, mgrsx, & lat, & lon, & zone, & x, & y);
	printf ("%s lat %f lon %f\n", "34WEE0000086527", lat, lon);

	lon=21.000005;
	status = geo_to_mgrs( cnstsP, lat,lon, mgrsx, &zone, &x, &y );	   
	printf( "%s %d: %lf %lf : %ld %lf %lf\n", mgrsx, status, lat,lon,zone,x,y );

	status = mgrs_to_geo (cnstsP, mgrsx, & lat, & lon, & zone, & x, & y);
	printf ("status %d lat %f lon %f\n", status, lat, lon);

	lat = 34.17;
	lon = -118.15;
	status = geo_to_mgrs( cnstsP, lat, lon, mgrsx, &zone, &x, &y );	   
	printf( "Pasadena %s %d: %lf %lf : %ld %lf %lf\n", mgrsx, status, lat,lon,zone,x,y );

	status = mgrs_to_geo (cnstsP, "11SLT9400981602 ", & lat, & lon, & zone, & x, & y);
	printf ("status %d lat %f lon %f\n", status, lat, lon);

	return 0;
}
#endif /* Test odd zones */
