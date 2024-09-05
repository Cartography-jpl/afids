#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"

/*
C  PROGRAM WNRM2005  --  APPLY WIENER FILTER TO A FOURIER TRANSFORM

   Nov-07  ...P K...  SPINOFF PROGRAM FROM WNR2005 FOR JOE GREEN(MATLAB)
*/

/*=======================================================*/
int getCorrespondingIndex(i, ns)
   int i, ns;
{
   if(i < ns/2) return (ns+1)/2 + i;

   return i-(ns/2);
}

/*=======================================================*/
void getBuf(unit, buf, nl, ns)
   int unit, nl, ns;
   float **buf;
{
   int i;
   float* linBuf;

   mz_alloc1((unsigned char**)&linBuf, ns*2, sizeof(float));

   for(i = 0; i < nl; i++)
   {
      int j, status;

      status = zvread(unit, linBuf, "LINE", i+1, NULL);
      if(status != 1) zmabend("Error in reading image file.");

      for(j = 0; j < ns*2; j++)
	 buf[i][j] = linBuf[j];
   }

   free(linBuf);
}

/*=======================================================*/
void outputRestor(unit, restor, nl)
   int unit, nl;
   float **restor;
{
   int i;

   for(i = 0; i < nl; i++)
      zvwrit(unit, restor[i], "LINE", i+1, NULL);
}

/*=======================================================*/
void getWnrFltr(otf, snrbuf, wnrFltr, nl, ns)
   float **otf, **snrbuf, **wnrFltr;
   int nl, ns;
{
   int i, j, pcount;
   float sn;

   /* get parameters */
   zvp("SN",&sn,&pcount);
   sn = 1.0/(sn*sn);

   for(i = 0; i < nl; i++)
   {
      for(j = 0; j < ns; j++)
      {
	 float realVal, imagVal, absVal, conjVal;
	 int ir, ic;

         ir = 2*j; ic = 2*j+1;

	 realVal = otf[i][ir];
	 imagVal = otf[i][ic];
	 conjVal = -imagVal;
	 absVal = sqrt(realVal*realVal + imagVal*imagVal);

         if(zvptst("snrimg"))
            sn = 1/(snrbuf[i][j]*snrbuf[i][j]);

	 wnrFltr[i][ir] = realVal/(absVal*absVal+sn);
	 wnrFltr[i][ic] = conjVal/(absVal*absVal+sn);
      }
   }
}

/*=======================================================*/
void wiener(im, otf, nl, ns, unit, snrbuf, units)
   float **im, **otf, **snrbuf;
   int nl, ns, unit, units[];
{
   int i, j;
   float **restor, **wnrFltr;

   mz_alloc2((unsigned char ***)&restor, nl, ns*2, sizeof(float));
   mz_alloc2((unsigned char ***)&wnrFltr, nl, ns*2, sizeof(float));

   getWnrFltr(otf, snrbuf, wnrFltr, nl, ns);

   for(i = 0; i < nl; i++)
   {
      for(j = 0; j < ns; j++)
      {
	 int ir, ic, ir1, ic1, i1, j1;

         ir = 2*j; ic = 2*j+1;

	 ir1 = getCorrespondingIndex(ir, ns*2);
	 ic1 = getCorrespondingIndex(ic, ns*2);
	 i1 = getCorrespondingIndex(i, nl);
	 j1 = getCorrespondingIndex(j, ns);

         restor[i][ir] =  im[i][ir]*wnrFltr[i1][ir1] - im[i][ic]*wnrFltr[i1][ic1];
         restor[i][ic] =  im[i][ic]*wnrFltr[i1][ir1] + im[i][ir]*wnrFltr[i1][ic1];
      }
   }

   outputRestor(unit, restor, nl);
   mz_free2((unsigned char**)restor, nl);
   mz_free2((unsigned char**)wnrFltr, nl);
}

/*=======================================================*/
void main44(void)
{
   int iun[4],oun,nids,status;
   char fmt[9];
   int i,nl=0,ns=0,nl1,ns1, snrimgcase;
   float **im, **otf, **snrbuf;
   
   zifmessage("wnrm2005 version Thu Jan  3 2008");
   
   /* open & check inputs */
   
   snrimgcase = zvptst("snrimg");
   status = zvpcnt("inp",&nids);
   for (i=0;i<nids;i++)
   {
      status = zvunit(&iun[i],"INP",i+1, NULL);
      status = zvopen(iun[i],"OPEN_ACT","SA","IO_ACT","SA", NULL);
      zvget(iun[i],"FORMAT",fmt,"NL",&nl1,"NS",&ns1, NULL);
      if (strcmp(fmt,"COMPLEX")!=0 && strcmp(fmt,"COMP")!=0 &&
          !(snrimgcase && i==nids-1))
         zmabend("ALL INPUT FILES MUST BE COMPLEX, EXCEPT NSR REAL");
      if (strcmp(fmt,"REAL")!=0 && snrimgcase && i==nids-1)
         zmabend("NSR INPUT FILE MUST BE REAL");
      if (i==0) { nl = nl1; ns = ns1; }
      else if (nl!=nl1 || ns!=ns1)
         zmabend("ALL INPUT FILES MUST BE SAME SIZE");
   }

   /* malloc arrays */
   mz_alloc2((unsigned char***)&im, nl, ns*2, sizeof(float));
   mz_alloc2((unsigned char***)&otf, nl, ns*2, sizeof(float));
   if(snrimgcase)
      mz_alloc2((unsigned char***)&snrbuf, nl, ns*2, sizeof(float));

   /* open outputs */
   status = zvunit(&oun,"OUT",1, NULL);
   status=zvopen(oun,"U_NL",nl,"U_NS",ns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);

   /* get input data sets */
   getBuf(iun[0], im, nl, ns);
   getBuf(iun[1], otf, nl, ns);

   if(snrimgcase)
      getBuf(iun[nids-1], snrbuf, nl, ns);

   /* Apply wiener restoration filter */
   wiener(im, otf, nl, ns, oun, snrbuf, nids, iun);

   mz_free2((unsigned char**)im, nl);
   mz_free2((unsigned char**)otf, nl);

   if(snrimgcase) mz_free2((unsigned char**)snrbuf, nl);
   for (i=0;i<nids;i++) zvclose(iun[i], NULL);
   zvclose(oun, NULL);

   return;
}
