#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#ifdef R
#undef R
#endif

#ifdef G
#undef G
#endif

#ifdef B
#undef B
#endif

#ifdef I
#undef I
#endif

#ifdef S
#undef S
#endif

#ifdef H
#undef H
#endif

#define maxPixVal 32767

void rgb2ish (short int R, short int G, short int B, double * H, double * S, double * I);
void ish2rgb (double H, double S, double I, double * R, double * G, double * B);

void main44(void)
{
  int status,numimg,numout,pvunit,dummy=0,nline,nsamp,i,vunit[20],lnl,lns,pixsiz,o_unit[20],iline,j;
  char fmt_str[10];
#if 0
   int i,j,iline,o_unit[20],nline,nsamp,status,dummy,lnl,lns,pixsiz,nopre;
   int window,win2,don2,jsamp,pvunit,vunit[20],sigcount,donut,numout,win2m,win2p;
   int iroll,iix,numimg,iw,jw,npair,printls[2],ival,istop,masterband;
   int band;
   int needctr;
   float sum;
   /*float sum[20],avg[20],ctr[20],sumdiv,sigma[20],dif[20];*/
   float background[2][10],rawimg[2][10],pixdif[2][10],rawsig[2][10],
        pixdifadj[2][10],normimg[2][10],mastersigma;
   short int ***inbuf,**outbuf;
#endif
   short int *panLine, **imgLines, *sumLine;
   float *multLine;
   char panfilename[100];
   int parmct, parmdf;
   double R,G,B;
   double I,S,H;

   zifmessage("psrgbish version Wed Aug 11 2010");
   
   /* get the parms */
   
   status = zvpcnt("inp",&numimg);
   status = zvpcnt("out",&numout);
   if (numout!=numimg) zmabend("number of out images must equal input");
   if (numout != 8) zmabend("inp/out must have 8 images"); /* WV2 only */

   /* open the pan file */

   zvparm ("pan", panfilename, &parmct, &parmdf, 1, 99);
   status = zvunit(& pvunit, "U_NAME", dummy, "U_NAME", panfilename, NULL);
   status = zvopen(pvunit,"OPEN_ACT"," ","IO_ACT"," ",NULL);
   zvget(pvunit,"NL",&nline,"NS",&nsamp,NULL);
   zvget(pvunit,"FORMAT",fmt_str,NULL);
   if ( strcmp(fmt_str,"HALF"))
     zmabend("Invalid input PAN data format.  Use HALF.");

   /* open the input files */

   for (i=0;i<numimg;i++)
     {
       printf( "opening inp %d\n", i );
       status = zvunit(&vunit[i],"INP",i+1,NULL);
       status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF",NULL);
       zvget(vunit[i],"NL",&lnl,"NS",&lns,"PIX_SIZE",&pixsiz,NULL);

       if (lnl!=nline||lns!=nsamp) zmabend("images must be same size");
       zvget(vunit[i],"FORMAT",fmt_str,NULL);
       if ( strcmp(fmt_str,"HALF"))
         zmabend("Invalid input data format.  Use HALF.");
     }

   /* open the output files */

   for (i=0;i<numimg;i++)
     {
       status=zvunit( &o_unit[i],"OUT",i+1,NULL);
       status=zvopen( o_unit[i],"U_NL",nline,"U_NS",nsamp, "U_FORMAT","HALF",
		      "OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
     }
   
   panLine = (short int *) malloc( sizeof( short int ) * nsamp );
   sumLine = (short int *) malloc( sizeof( short int ) * nsamp );
   multLine = (float *) malloc( sizeof( float ) * nsamp );
   imgLines = (short int **) malloc( sizeof( short int * ) * numimg );
   for ( i = 0; i < numimg; ++ i )
     imgLines[ i ] = (short int *) malloc( sizeof( short int ) * nsamp );
   
   /* do the rgb2ish using bands 468, 235, 137 */
   for ( iline=0; iline < nline; ++iline )
      {
	/* read the pan line */
	status = zvread(pvunit,panLine,"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);

	/* read the msi lines */
	for (i=0; i < numimg; ++i )
	  status = zvread(vunit[i],imgLines[i],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);

	for (j=0; j < nsamp; ++j ) {
	  /* bands 8, 6, 4 */
	  rgb2ish( MAX(0, imgLines[7][j]), MAX(0, imgLines[5][j]), MAX(0, imgLines[3][j]), &H, &S, &I );
	  ish2rgb( H, S, panLine[j] / (double) maxPixVal, &R, &G, &B );
	  imgLines[7][j] = R;
	  imgLines[5][j] = G;
	  imgLines[3][j] = B;

	  /* bands 7, 3, 1 */
	  rgb2ish( MAX(0, imgLines[6][j]), MAX(0, imgLines[2][j]), MAX(0, imgLines[0][j]), &H, &S, &I );

	  ish2rgb( H, S, panLine[j] / (double) maxPixVal, &R, &G, &B );
	  imgLines[6][j] = R;
	  /* imgLines[2][j] = G; this would be the "33" output */
	  imgLines[0][j] = B;

	  /* bands 5, 3, 2 */
	  rgb2ish( MAX(0, imgLines[4][j]), MAX(0, imgLines[2][j]), MAX(0, imgLines[1][j]), &H, &S, &I );
	  ish2rgb( H, S, panLine[j] / (double) maxPixVal, &R, &G, &B );
	  imgLines[4][j] = R;
	  imgLines[2][j] = G;
	  imgLines[1][j] = B;
	}

	for (i=0; i < numimg; ++i )
	  zvwrit(o_unit[i],imgLines[i],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);
      }

   for (i=0;i<numimg;i++) zvclose(vunit[i],NULL);
   for (i=0;i<numimg;i++) zvclose(o_unit[i],NULL);
   return;
}

/* Converts between RGB and ISH formats. Adapted from vdev/rgb2ish.c,
  which was adapted from Leiming Qian's <lqian@uiuc.edu> conversion of
  Javascript to C++ from
  www.ndirect.co.uk/~thomas.green/javascripts/hexAndRGB.html */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#undef MIN
#undef MAX
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define RINT(x)	  ((x) - floor (x) > 0.5 ? ceil(x) : floor(x))

void rgb2ish (short int R, short int G, short int B, double * H, double * S, double * I) {
  short int min, mid, max;

  if (R < 0 || G < 0 || B < 0)
    zmabend ("RGB must be non-negative!");

  /* find out the min, mid, and max of R, G, B */
  if (R > G && R > B) {
    max = R;
    mid = MAX (G, B);
    min = MIN (G, B);
  } else {
    if (G > B) {
      max = G;
      mid = MAX (R, B);
      min = MIN (R, B);
    } else {
      max = B;
      mid = MAX (R, G);
      min = MIN (R, G);
    }
  }

  * I = (double) max / maxPixVal;
  * S = 0.0;
  * H = 0.0;
  if (* I == 0.0 || max == min) {
    /* this is a black image or grayscale image */
    * S = 0.0;
    * H = 0.0;
  } else {
    * S = (* I - (double) min / maxPixVal) / * I;
    /* domains are 60 degrees of */
    /* red, yellow, green, cyan, blue or magenta */
    {
      double domainBase = 0.0;
      double oneSixth = 1.0/6.0;
      double domainOffset = ((double) (mid - min) / (max - min)) / 6.0;

      if (R == max) {
	if (mid == G) {			/* green is ascending */
	  domainBase = 0.0;		/* red domain */
	} else {				/* blue is descending */
	  domainBase = 5.0/6.0;		/* magenta domain */
	  domainOffset = oneSixth - domainOffset;
	}
      } else {
	if (G == max) {
	  if (mid == B) {	        /* blue is ascending */
	    domainBase = 2.0/6.0;	/* green domain */
	  } else {			/* red is ascending */
	    domainBase = 1.0/6.0;	/* yellow domain */
	    domainOffset = oneSixth - domainOffset;
	  }
	} else {
	  if (mid == R) {		/* red is ascending */
	    domainBase = 4.0/6.0;	/* blue domain */
	  } else {			/* green is descending */
	    domainBase = 3.0/6.0;	/* cyan domain */
	    domainOffset = oneSixth - domainOffset;
	  }
	}
      }
      * H = domainBase + domainOffset;
    }
  }
}

void ish2rgb (double H, double S, double I, double * R, double * G, double * B) {
  double domainOffset = 0.0;

  if (H < 0.0 || H > 1.0 || S < 0.0 || S > 1.0)
    zmabend ("Value out of range!");

  if (I == 0.0) {
    /* black image */
    * R = * G = * B = 0.0;
  } else {
    if (S == 0.0) {
      /* grayscale image */
      * R = * G = * B = I;
    } else {
      if (H<1.0/6.0) {		/* red domain; green acending */
	domainOffset = H;
	* R = I;
	* B = I * (1.0 - S);
	* G = * B + (I - * B) * domainOffset * 6.0;
      } else {
	if (H < 2.0 / 6) {	/* yellow domain; red acending */
	  domainOffset = H - 1.0 / 6.0;
	  * G = I;
	  * B = I * (1.0 - S);
	  * R = * G - (I - * B) * domainOffset * 6.0;
	} else {
	  if (H < 3.0 / 6) {	/* green domain; blue descending */
	    domainOffset = H - 2.0 / 6;
	    * G = I;
	    * R = I * (1.0 - S);
	    * B = * R + (I - * R) * domainOffset * 6.0;
	  } else {
	    if (H < 4.0 / 6.0) { /* cyan domain, green acsending */
	      domainOffset = H - 3.0 / 6;
	      * B = I;
	      * R = I * (1.0 - S);
	      * G = * B - (I - * R) * domainOffset * 6.0;
	    } else {
	      if (H < 5.0 / 6.0) { /* blue domain, red ascending */
		domainOffset = H - 4.0 / 6.0;
		* B = I;
		* G = I * (1.0 - S);
		* R = * G + (I - * G) * domainOffset * 6.0;
	      } else {		/* magenta domain, blue descending */
		domainOffset = H - 5.0 / 6.0;
		* R = I;
		* G = I * (1.0 - S);
		* B = * R - (I - * G) * domainOffset * 6.0;
	      }
	    }
	  }
	}
      }
    }
  }

  * R = RINT (* R * maxPixVal);
  * G = RINT (* G * maxPixVal);
  * B = RINT (* B * maxPixVal);
}
