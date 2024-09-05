#ifndef MAPPINGUTILS_H
#define MAPPINGUTILS_H

#include "LPUtils.h"

/***********************************************
mappingUtils.h

Written by Peter Kim

Aug 03 2007: Documented for testing and finalizing. pk
***********************************************/

/********************************************************
dgauss

dgauss solves a set of linear equations via gaussian elimination

arguments:

     1. a: input and output, double *a;
	m by m coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector; output solution.
     3. m: input, int m;
	number of linear equations.
     4. eps: input, double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     5. ierror: output int *ierror;
	result 0=OK, 1=pivot is zero, K=loss of significance warning
	pivot less than eps times max element of a

The matrix a is stored by column order
*********************************************************/
void dgauss(double *a,double* r,int m,double eps,int *ierror);

/*********************************************************
ms_dnum

ms_dnum converts a string to a double and moves the pointer, also
allows for positive and negative exponent with e or E or D or d, for
example 123.45E-002

function return : double

argument :
      1. num_ptr: input, char **num_ptr;
*********************************************************/
double ms_dnum (char** num_ptr);

/*********************************************************
ms_find

ms_find searches string str1 for the substring str2 and returns
a pointer to the first location in string after the substring.

function return : character pointer

arguments :

      1. str1: input, char *str1;

      2. str2: input, char *str2;

Null pointer is returned if substring is not found.
**********************************************************/
char *ms_find(char* str1,char* str2);


/*************************************************************
int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*************************************************************/
int gtgetlab(char inp[],int instance,char** labelstr,int *nl,int *ns);

/****************************************************
int invertmap

invertmap calculates the inverse of a six-vector double precision
map.

function return:
     ier from the dgauss call (see dgauss)

arguments:
      1. map: double[6] map;
	 (input) coefficients to convert pixel to coord OR
	 COORD TO PIXEL
      2. invmap: double[6] invmap;
	 (output) coefficients to convert the other way
*****************************************************/
int invertmap(double t[],double tinv[]);

/******************************************************
int geofix

geofix translates label coordinates into a linear transformation
vectors that can be used for VICAR pixel-to-map or map-to-pixel
conversions.  If the file is a VICAR image then the mapping of 
the corner points is also returned (the (1,1) and (nline,nsamp)
centers of the corner pixels).

The convention for the transforms is (line,samp) -> (East,North)
for the map and (East,North) -> (line,samp) for the invmap
for convenience in working with VICAR.

Note that VICAR pixel referencing is different than GeoTIFF
pixel referencing (both "area" and "point"/"post" types).

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. map: double[6] map;
	 (output) coefficients to convert pixel to coord
      3. invmap: double[6] invmap;
	 (output) coefficients to convert coord to pixel
      4. nl: int nl;
	 (input) number of lines in vicar image to calc corner
      5. ns: int ns;
	 (input) number of samples in vicar image to calc corner
      6. corner: double[4] corner;
	 (output) the mapping of the corners (.5,.5,nl+.5,ns+.5)
	 if the file is a VICAR image, else zero
***********************************************************/
int geofix(char* labelstr,double map[],double invmap[],int nl,int ns,double corner[]);

/***********************************************************
void getT

This function puts the t coefficients to perform the
maptopix or pixtomap transformation into the t array.
If maptopix is 1, t will be set to the coefficients of
t inverse.  The unit parameter is the unit of the image
file with the label in it.
***********************************************************/
void getT(int maptopix, double t[6], int inpParm);

/***********************************************************
void lpPixMapConversion

This function revalues the entities in the given LPList
to either its corresponding map value or pixel value
(determined by the maptopix parameter).  The inpParm tells
the function which input file to get the translation label
from.
***********************************************************/
void lpPixMapConversion(LPList* list,int maptopix, int inpParm);

#endif



