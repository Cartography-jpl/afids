#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoVicarProtos.h"
#include "cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  brovey W. Bunch Wed Aug 11 2010 */

void main44(void)
{
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
   char fmt_str[10];
   short int *panLine, **imgLines, *sumLine;
   float *multLine;
   char panfilename[100];
   int parmct, parmdf;

   zifmessage("brovey version Thu Aug  5 2010");
   
   /* get the parms */
   
   status = zvpcnt("inp",&numimg);
   status = zvpcnt("out",&numout);
   if (numout!=numimg) zmabend("number of out images must equal input");

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
   
   /* do the brovey */
   for ( iline=0; iline < nline; ++iline )
      {
	/* read the pan line */
	status = zvread(pvunit,panLine,"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);

	/* read the msi lines */
	for (i=0; i < numimg; ++i )
	  status = zvread(vunit[i],imgLines[i],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);

	for (j=0; j < nsamp; ++j ) {

	  /* set sum to first band */
	  sumLine[j] = imgLines[0][j];

	  /* sum in other bands */
	  for (band=1; band<numimg; ++band)
	    sumLine[j] += imgLines[band][j];

	  /* set mult = pan / msiSum */
	  if (sumLine[j])
	    multLine[j] = panLine[j] / (float) sumLine[j];
	  else
	    multLine[j] = 0.0;

	  /* set bandN = bandN * mult */
	  for (band=0; band<numimg; ++band)
	    imgLines[band][j] *= multLine[j];

	}

	for (i=0; i < numimg; ++i )
	  zvwrit(o_unit[i],imgLines[i],"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp,NULL);

      }

   for (i=0;i<numimg;i++) zvclose(vunit[i],NULL);
   for (i=0;i<numimg;i++) zvclose(o_unit[i],NULL);
   return;
}
