This library "libdtcc.a" is build from "GEOTRANS 2.2.3- Geographic
Translator", a geographic projection code library available at
http://www.nima.mil/GandG/geotrans/geotrans.html. This is "Revised
version (2.2.3) added Feb 21, 2003". It has been enhanced to allow
compilation on Sun's "cc: Forte Developer 7 C 5.4 2002/03/09" and
gnu's "gcc version 2.95.3 20010315 (release)". The changes involved
fixing nested /**/ comments, and removing embedded ^M inserted perhaps
from mixing DOS and UNIX source files.

polarst.c and utm.c also were slightly corrected/enhanced:

# diff geotrans-2.2.3/polarst.c geotransdir/polarst.c
371c371,374
<       *Easting = rho * sin(dlam) + Polar_False_Easting;
---
>       /* alz was *Easting = rho * sin(dlam) + Polar_False_Easting; */
>       *Easting = rho * sin(dlam);
>       *Northing = -rho * cos(dlam);
>       /* alz end */
376c379,381
<         *Northing = rho * cos(dlam) + Polar_False_Northing;
---
>         /* alz was *Northing = rho * cos(dlam) + Polar_False_Northing; */
>         *Northing *= -1.0;
>       /* alz end */
377a383
>       /* alz was
379a386
>       */
381a389,392
>     /* alz all handling of false N,E here, note errors in original code */
>     *Easting += Polar_False_Easting;
>     *Northing += Polar_False_Northing;
>     /* alz end */



# diff geotrans-2.2.3/utm.c geotransdir/utm.c
75a76
> #include <stdio.h>
92,93c93,94
< #define MIN_EASTING  100000
< #define MAX_EASTING  900000
---
> #define MIN_EASTING  -3000000 /* alz was 100000 */
> #define MAX_EASTING  4000000 /* alz was 900000 */
216,217c217,218
<     Lat_Degrees = (long)(Latitude * 180.0 / PI);
<     Long_Degrees = (long)(Longitude * 180.0 / PI);
---
>     Lat_Degrees = (long)(Latitude * 180.0 / PI + 0.00000005); /* alz 0.00000005 to force 0.99999999 to 1 */
>     Long_Degrees = (long)(Longitude * 180.0 / PI + 0.00000005); /* alz ditto */
220c221
<       temp_zone = (long)(31 + ((Longitude * 180.0 / PI) / 6.0));
---
>       temp_zone = (long)(31 + ((Longitude * 180.0 / PI) / 6.0) + 0.00000005); /* alz ditto */
222c223
<       temp_zone = (long)(((Longitude * 180.0 / PI) / 6.0) - 29);
---
>       temp_zone = (long)(((Longitude * 180.0 / PI) / 6.0) - 29 + 0.00000005); /* alz ditto */
240a242
>     /* alz was
251a254,274
>     */
> 
>     /* alz replacement code */
>     if (UTM_Override)
>     {
>       if ((temp_zone <= 4) && (UTM_Override >= 57))
>         temp_zone = UTM_Override;
>       else if ((temp_zone >= 57) && (UTM_Override <= 4))
>         temp_zone = UTM_Override;
>       else if (((temp_zone-4) <= UTM_Override) &&
>             (UTM_Override <= (temp_zone+4)))
>         temp_zone = UTM_Override;
>       else
>         {
>         fprintf(stderr, "error B: temp_zone,UTM_Override %d %d\n",
>             temp_zone,UTM_Override);
>         Error_Code = UTM_ZONE_OVERRIDE_ERROR;
>         }
>     }
>     /* alz end */
> 
315c338,341
<     Error_Code |= UTM_NORTHING_ERROR;
---
>     {
>       Error_Code |= UTM_NORTHING_ERROR;
>       fprintf(stderr, "error D: Northing %f\n",Northing); /* alz added */
>     }
319c345
<       Central_Meridian = ((6 * Zone - 183) * PI / 180.0 + 0.00000005);
---
>       Central_Meridian = ((6 * Zone - 183) * PI / 180.0 /* alz removed + 0.00000005 */);
321c347
<       Central_Meridian = ((6 * Zone + 177) * PI / 180.0 + 0.00000005);
---
>       Central_Meridian = ((6 * Zone + 177) * PI / 180.0 /* alz removed + 0.00000005 */);
