/* Constants  */
#ifndef _CNSTNTS_H
#define _CNSTNTS_H

#ifndef OK
#define OK	(0)
#endif

/* Mathematical constants */
#include <math.h>
#include <float.h>
#ifndef M_PI
#define M_PI        (3.14159265358979323846) 
#endif
#ifndef M_PI_2
#define M_PI_2      (1.57079632679489661923)
#endif
#ifndef M_PI_4
#define M_PI_4      (0.78539816339744830962)
#endif

#define PI				M_PI
#define HALF_PI 		M_PI_2
#define TWO_PI      (M_PI+M_PI)

#ifndef RADDEG
#define RADDEG      (M_PI/180.0)
#endif
#ifndef EPSILON
#define EPSILON		( 0.0000001 )
#endif

/* NULL constants */
#ifndef DOUBLE_NULL
#define DOUBLE_NULL	 (-1.7976931348623158e+308)		/* minimum double8 value */ 
#endif
#ifndef FLOAT_NULL
#define FLOAT_NULL      (-3.402823466e+38F)			/* minimum float value */ 
#endif
#ifndef SHORT_NULL
#define SHORT_NULL      (-32767)       				/* minimum short value */
#endif
#ifndef LONG_NULL
#define LONG_NULL       (-2147483647)  				/* minimum long value */
#endif

#endif /* _CNSTNTS_H */
