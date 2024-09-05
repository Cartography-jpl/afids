#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>
#include "carto/ImageUtils.h"

#define XX_MAX_FNAME_LEN   50
#define XX_N_BANDS         1

/**********************
newimd.c
creates a .IMD file using a NITF file as the input
The .IMD file is used for the reflectance calculation program
wv2ref

Steve Levoe
august - 2011

example command
 newimd INP="10MAY16183214-P1BS-052366900030_01_P001.NTF"  BAND=1 OUT="test.IMD" KER="kernels" ABSCAL=5.678345e-02 effbw=2.846000e-01
 
 ****/

// global variable
int numRows = 0;
int numColumns = 0;
static char *months[] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};


// float absCalFactor = 5.678345e-02;
// float effectiveBandwidth = 2.846000e-01;



double calcMeanSunEl(double lat, double lon, char *utc, char *kernel_file);
void setInps(char *inps, char *outMeta, char *kernel, int *band, float *abscal, float *effbw);
int getMonthNumber(char *month);

void getGdalInfo(char *inps, char *outMeta, char *kernel, float absCalFactor, float effectiveBandwidth);


void setInps(char *inps, char *outMeta,  char *kernel, int *band, float *abscal, float *effbw)
{
   int i, status, inpcnt, dumcnt, dumdef, dumdef2, outcnt, kercnt, dumdef3;
int abscalcnt, effbwcnt;
	int bands = XX_N_BANDS ;
	int fnamelen = IU_MAX_FNAME_LEN;
	

    char fnames[XX_N_BANDS][IU_MAX_FNAME_LEN];
    char onames[XX_N_BANDS][IU_MAX_FNAME_LEN];


   printf ("setInps newimd \n");
   printf ("fnamelen = %d \n", fnamelen);
   printf ("bands = %d \n", bands);
		   
   status = zvp("band", &band, &dumcnt);
   if(status != 1) zmabend("Error getting band.\n");

	
	// status = zvp("out", outMeta, &outcnt);
	status = zvparm("out", outMeta, &outcnt, &dumdef, XX_N_BANDS, IU_MAX_FNAME_LEN);
	if(status != 1) zmabend("Error getting out.\n");

	printf ("outMeta = %s \n", outMeta);
  
	status = zvparm("ker", kernel, &kercnt, &dumdef3, XX_N_BANDS, IU_MAX_FNAME_LEN);
	if(status != 1) zmabend("Error getting ker.\n");
	
	printf ("ker = %s \n", kernel);
	
   status = zvparm("inp", inps, &inpcnt, &dumdef2, XX_N_BANDS, IU_MAX_FNAME_LEN);
	
   if(status != 1) zmabend("Check input files.\n");
	
	printf ("inp = %s \n", inps);

	status = zvp("ABSCAL", abscal, &abscalcnt);
   if(status != 1) zmabend("Error getting abscal.\n");	

	printf ("abscal = %f %e\n", *abscal, *abscal);

		status = zvp("EFFBW", effbw, &effbwcnt);
   if(status != 1) zmabend("Error getting effbw.\n");	

	printf ("effbw = %f %e \n", *effbw, *effbw);

}

int getMonthNumber(char *month) {
	int mm = 0;
	// printf("getMonthNumber %s \n", month);
	int i=0;
	for (i=0 ; i< 12 ; i++) {
		// printf ("%d) %s \n", i, months[i]);
		if (strcmp(months[i], month) == 0) {
			mm = i+1;
			return mm;
		}
		
	}
	
	return mm;
}






void getGdalInfo(char *inps, char *outMeta, char *kernel, 
      float absCalFactor, float effectiveBandwidth) {

	FILE *fp, *outFp;
	int status, tokCt, j, i;
	char line[1035], prevLine[1035];
	char *token[300];
	
	char cmd[300];
	int ct = 0;
	
	
	printf(cmd, "gdalinfo %s -> %s", inps, outMeta);
	
	ct = sprintf(cmd, "gdalinfo %s ", inps);
	
	// open the output file for writing
	
		
	printf("ct=%d cmd=%s \n", ct, cmd);
	
	fp = popen(cmd, "r");
	if (fp == NULL) {
		printf("Failed to run command\n" );
		exit;
	}
	
	/* Read the output a line at a time - output it. */
	i=0;
	j=0;
	int wct=0;
	char exp[] = " ";
	char w1[20], w2[20];
	char *p;
	char utc_time[30];
	int yyyy,yy, mo,dd,hh,mm,ss;
	char month[5];
	char key[20];
	char utc[20];
	double ullon = 0.0;
	double ullat = 0.0;
	double zero = 0.0;
	float x1,y1;
	
	double meanSunEl = 0.0;
	
	printf("go thru command output\n");
	prevLine[0] = '\0';
		   
	while (fgets(line, sizeof(line)-1, fp) != NULL) {
		// printf("%d) %s", i, line);
		// Size is 35180, 30016
		p = strstr( line, "Size is" );
		if( p != NULL) {
			printf ("+++++++++++++++++++++++++++++++++++\n");
			// remove the , at the end
			// check if last char is "," make it a null?
			// wct = sscanf(line,"%s %s %d, %d", w1,w2,&numRows, &numColumns);
			wct = sscanf(line,"Size is %d, %d", &numColumns, &numRows);
			
			printf ("Size is: wct=%d numRows = %d numColumns = %d \n", wct, numRows, numColumns);
			
		} 
		
		p = strstr( line, "NITF_IDATIM" );
		if( p != NULL) {
			printf ("\n ++++++++++ %s +++++++++++++++++\n", p);
			// there may be more than one time format ???
			// NITF_FTITLE=WV02_SENSOR:2010-05-16T18:32:14.680975Z
			
			// NITF_IDATIM=16183214ZMAY10
			// NITF_IDATIM= 16 18 32 14 Z MAY 10
			// wct = sscanf(line,"NITF_IDATIM=%2d%2d%2d%2dZ%3s%2d",&dd, &hh, &mm, &ss, month, &yy);
			wct = sscanf(line,"%11s=%2d%2d%2d%2dZ%3s%2d",key, &dd, &hh, &mm, &ss, month, &yy);
			// convert month to a number
			mo = getMonthNumber(month) ;
			
			printf ("NITF_IDATIM: %s  20%d - %d - %d | %d : %d : %d - month=%s\n", key, yy,mo,dd,hh,mm,ss,month);
			
			// decimal secs? 
			// 2011-04-19T06:52:53.951750
			// SPICE can't handle a time with Z at the end
			sprintf(utc, "20%2d-%02d-%02dT%02d:%02d:%02d.0", yy,mo,dd,hh,mm,ss);
			printf( "utc = %s \n\n", utc);
		} 
		
		// get lat and lon
		p = strstr( prevLine, "UpperLeft" );
		if( p != NULL) {
			printf ("++++++++++ UpperLeft +++++++++++++++++\n");
		   printf ("prevLine=%s ",prevLine);
		   printf ("line=%s ",line);
			// remove the , at the end
			// check if last char is "," make it a null?
			// wct = sscanf(line,"%s %s %d, %d", w1,w2,&numRows, &numColumns);
			// wct = sscanf(line,"ULLon = %lf",&ullon);
			// GCP[  0]: Id=UpperLeft, Info=
			//          (0.5,0.5) -> (-116.271111111111,36.9058333333333,0)

			wct = sscanf(line,"          (%f,%f) -> (%lf,%lf,%lf)",&x1,&y1,&ullon,&ullat,&zero);
			
			printf ("ullon: wct=%d x1=%f y1=%f ullon = %lf ullat = %lf \n", wct, x1,y1,ullon,ullat);
			
		}
		
		
		
		/***
		 NITF_FDT=04174044ZJUN10
		 NITF_STDIDC_ACQUISITION_DATE=20100516183214
		 
		 NITF_USE00A_SUN_EL=+67.0
		 
		 MIN_LONG=-116.2144
		 MAX_LONG=-116.1006
		 MIN_LAT=36.77330000000001
		 MAX_LAT=36.8617
		 ***/
		
		i++;	
	strcpy(prevLine, line);
	}
	
	/* close */
	pclose(fp);
	
	
	outFp = fopen(outMeta, "w");
	if (outFp == NULL) {
		printf("Error in opening outMeta file: %s\n", outMeta);
		printf("Could not write file \n");
		return;
	}
	
	fprintf(outFp, "version = \"22.2\";\n") ;	
	fprintf(outFp, "inputFilename = %s\n", inps) ;	
	// we have all the info we need, write the file
	// call the spice routine to calculate the meanSunEl
	// kernel is the kernel file name to load to get Spice constants
	meanSunEl = calcMeanSunEl(ullat, ullon, utc, kernel);
	char *esun  = "1580.8140" ; // get this from header file?
			
	fprintf(outFp,"numRows = %d\n", numRows);
	fprintf(outFp,"numColumns = %d\n", numColumns);
	
			
	fprintf(outFp,"BEGIN_GROUP = BAND_P\n");
	fprintf(outFp,"  ULlon = %lf\n", ullon);
	fprintf(outFp,"  ULlat = %lf\n", ullat);
	fprintf(outFp,"  bandId = \"P\"\n");

	fprintf(outFp,"  absCalFactor = %e\n", absCalFactor );
	fprintf(outFp,"  effectiveBandwidth = %e\n", effectiveBandwidth);
	// fprintf(outFp,"  ESUN = %s \n", esun);
	fprintf(outFp,"END_GROUP = BAND_P\n");
			
	fprintf(outFp,"BEGIN_GROUP = IMAGE_1\n");
	fprintf(outFp,"  utc = %s\n", utc);	
	fprintf(outFp,"  TLCTime = %sZ\n", utc);
	fprintf(outFp,"  firstLineTime = %sZ\n", utc);
	fprintf(outFp,"  meanSunEl = %2.02lf\n", meanSunEl);
	fprintf(outFp,"END_GROUP = IMAGE_1\n");
						
	fprintf(outFp,"END;\n");
			
	fclose(outFp);
	
}



void main44(void)
{
   int i, band;
   
	char inps[IU_MAX_FNAME_LEN];
   
   char o_metafile[IU_MAX_FNAME_LEN];
   char kernel[IU_MAX_FNAME_LEN];
   char line[100];
   float abscal, effbw ;



   setInps(inps, o_metafile, kernel, &band, &abscal, &effbw);
	
	printf (" #### inps = %s \n", inps);
    printf (" o_metafile = %s \n", o_metafile);
	printf (" kernel = %s \n", kernel);
printf("abscal = %f %e \n", abscal, abscal);
printf ("effbw = %f %e \n", effbw, effbw);
	
   getGdalInfo(inps, o_metafile, kernel, abscal, effbw);
   
	
   
}
