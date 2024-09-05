#include <stdio.h>
#include <stdlib.h> 
#include <math.h>
#include <string.h>

/*****/
 #include "SpiceUsr.h"

 #define PAI 3.1415927
 
 #define FILESIZE 256
 
 #define SETUP "setup"

/******/
double calcMeanSunEl(double lat, double lon, char *utc, char *kernel_file);

// double calcMeanSunEl(SpiceDouble lat, SpiceDouble lon, char *utc) {
double calcMeanSunEl(double lat, double lon, char *utc, char *kernel_file) {
	SpiceInt code;
	SpiceChar name[] = "EARTH";
	SpiceBoolean found;
	SpiceDouble lonr, latr, rectan[3], et;
	
	
	double a2;
	a2 = 68.2;	
	printf ("calcMeanSunEl lat_in = %f lon_in = %f utc = %s kernel_file = %s \n", lat, lon, utc, kernel_file);
	
	
	// furnsh_c ( argv[1]);		// Load kernel files
	furnsh_c (kernel_file);
	
	// utc2et_c( argv[4], &et);	// Convert from date/time string to ET
	utc2et_c( utc, &et);	// Convert from date/time string to ET
	
	// lat = rpd_c() * atof(argv[2]); // Convert latitude from degrees to radian
	// lon = rpd_c() * atof(argv[3]); // Convert longitude from degrees to radian
	
	latr = rpd_c() * lat; // Convert latitude from degrees to radian
	lonr = rpd_c() * lon; // Convert longitude from degrees to radian
	
	bodn2c_c( name, &code, &found); // Find the code of the EARTH
	
	srfrec_c( code, lonr, latr, &rectan[0]); // Calculate rectangular coordinate of the surface point
	vhat_c( rectan, rectan);
	
	ConstSpiceChar *method = "Near point: ellipsoid";
	ConstSpiceChar *target = "IAU_EARTH";
	SpiceDouble spoint[3], trgepc, srfvec[3];
	
	subslr_c( method, "EARTH", et, "IAU_EARTH", "lt+s", "SUN", spoint, &trgepc, srfvec); // Calculate sub-solar rectangular coordinate
	vhat_c( spoint, spoint);	// Unitize vector
	
	SpiceDouble dot;
	dot = vdot_c( spoint, rectan); /* Dot product of two vector */
	
	double angle;
	// double a2;
	angle = acos(dot) * dpr_c();	// Convert to degrees
	printf("angle = %f\n", angle);
	a2 = 90.0 - angle ;
	printf("a2 = %f\n",a2);
	
	return a2;
}

