#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoVicarProtos.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  image classification using ratios  A. Zobrist    05/24/11   */

void main44(void)
{
   int i,iinp,iline,isamp,nl,ns,inpcnt,i_unit[48],inl[48],ins[48],dummy,status;
   int fcase,o_unit[48],iband,itemp,outpcnt,option,way,iway,pan,opan,oshade;
   int ratonly,inpimcnt,clen,iunit,ibis,*ibrat1,*ibrat2,*ibprod,iclass,maxclass;
   int bndtest,diftest,ix,dnorm,difcount,difthr[100],priority[100],pricount;
   int prevclass,rattest,dthresh[100],dthreshcount,pixdbg[2],pixcount;
   short int **inbuf,**outbuf,**dif,*imtot,inbuftmp,ir1,ir2;
   float gthresh,diffac,l1dist,l2dist,linfdist,icount,sigdist;
   float gratio[8][8],bv[8],minsigma,fdthresh,dblmax,sdist;
   double *ibmean,*ibsigma,*fibprod;
   
   zifmessage("f2ratio version Wed Jun 15 2011");
   
   /* get the parms */

   status = zvpcnt("INP",&inpcnt);
   status = zvpcnt("OUT",&outpcnt);
   zvp("OPTION",&option,&dummy);
   zvp("WAY",&way,&dummy);
   zvp("FCASE",&fcase,&dummy);
   zvp("SHADOW",&gthresh,&dummy);
   zvp("diffac",&diffac,&dummy);
   zvp("PAN",&pan,&dummy);
   zvp("dnorm",&dnorm,&dummy);
   zvp("RATONLY",&ratonly,&dummy);
   if (pan==1&&way==2) zmabend("cannot do two-way in pan case");
   opan = 6*pan;
   oshade = 2*(1-pan);
   if (fcase==2) inpimcnt = inpcnt-1; else inpimcnt = inpcnt;
   zvparm("difthr",difthr,&difcount,&dummy,100,0);
   zvparm("dthresh",dthresh,&dthreshcount,&dummy,100,0);
   zvparm("priority",priority,&pricount,&dummy,100,0);
   zvparm("pixdbg",pixdbg,&pixcount,&dummy,2,0);
   zvp("minsigma",&minsigma,&dummy);
   maxclass = 0;
   
   /* open the inputs and outputs */
   
   for (i=0;i<inpimcnt;i++)
      {
      status = zvunit(&i_unit[i],"INP",i+1, NULL);
      status = zvopen(i_unit[i],"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
      zvget(i_unit[i],"NL",&inl[i],"NS",&ins[i], NULL);
      if (inl[i]!=inl[0]||ins[i]!=ins[0]) zmabend("Images must be same size");
      }
   nl = inl[0];
   ns = ins[0];

   if (fcase==2)
      {
      status = zvunit(&iunit,"inp",inpcnt, NULL);
      status = IBISFileOpen(iunit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(iunit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1,0);
      
      mz_alloc1((unsigned char **)&ibprod,clen,4);
      mz_alloc1((unsigned char **)&ibrat1,clen,4);
      mz_alloc1((unsigned char **)&ibrat2,clen,4);
      mz_alloc1((unsigned char **)&ibmean,clen,8);
      mz_alloc1((unsigned char **)&ibsigma,clen,8);
      mz_alloc1((unsigned char **)&fibprod,clen,8);
      
      status = IBISColumnSet(ibis,"U_FORMAT","FULL",1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)ibrat1,1,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      
      status = IBISColumnSet(ibis,"U_FORMAT","FULL",1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)ibrat2,2,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",1); /* idiot */
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)fibprod,3,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      
      for (i=0;i<clen;i++)
         {
         ibprod[i] = (int)fibprod[i];
         if (ibprod[i]>maxclass) maxclass = ibprod[i];
         }

      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)ibmean,4,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);

      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)ibsigma,5,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   for (i=0;i<outpcnt;i++)
      {
      status=zvunit(&o_unit[i],"OUT",i+1, NULL);
      status=zvopen(o_unit[i],"U_NL",nl,"U_NS",ns,"U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
      }
   
   /* dynamically allocate the buffers */
   
   mz_alloc2((unsigned char ***)&inbuf,inpimcnt,ns,2);
   mz_alloc2((unsigned char ***)&outbuf,outpcnt,ns,2);

   mz_alloc2((unsigned char ***)&dif,8,ns,2);
   mz_alloc1((unsigned char **)&imtot,ns,2);
   
   /* read the inputs calculate per line and output */

   for (iline=0;iline<nl;iline++)
      {
      if (iline%1000==3) printf("iline %d\n",iline);
      for (iinp=0;iinp<inpimcnt;iinp++)
         {
         status = zvread(i_unit[iinp],inbuf[iinp],"LINE",iline+1,
               "SAMP",1,"NSAMPS",ns, NULL);
         }
      for (isamp=0;isamp<ns;isamp++) outbuf[0][isamp] = 0;
      for (iway=0;iway<way;iway++)
      {
      if (iway==1)
         {
         for (iband=0;iband<8;iband++)
            for (isamp=0;isamp<ns;isamp++)
            {
            inbuftmp = inbuf[iband][isamp];
            inbuf[iband][isamp] = inbuf[iband+8][isamp];
            inbuf[iband+8][isamp] = inbuftmp;
            }
         }
      switch (fcase)
         {
         case 1:  /* testing purposes only */
            for (isamp=0;isamp<ns;isamp++)
               outbuf[0][isamp] = inbuf[0][isamp]+inbuf[1][isamp];
         break;
         case 2:
            for (isamp=0;isamp<ns;isamp++)
               {
               if (pan==0)
                  {
                  itemp = 0;
                  for (iband=0;iband<8;iband++)
                     {
                     dif[iband][isamp] =
                        max(0,min(255,inbuf[iband+8][isamp]-inbuf[iband][isamp]+128));
                     itemp += abs(dif[iband][isamp]-128);
                     }
                  imtot[isamp] = itemp/8;
                  }
               else
                  {
                  imtot[isamp] = abs(inbuf[1][isamp]-inbuf[0][isamp]);
                  }
               
               /* new ratios 0.0000037 avoids divide by 0 */
               
               for (i=0;i<8;i++) bv[i] = (float)(inbuf[i+8-opan][isamp]);
               for (ir1=0;ir1<7;ir1++)
                  for (ir2=ir1+1;ir2<8;ir2++)
                     {
                     gratio[ir1][ir2] = 
                       100.0*((bv[ir1]-bv[ir2])/(bv[ir1]+bv[ir2]+0.0000037)+1.0);
                     }
               
               outbuf[1][isamp] = imtot[isamp];
               /*if (option==1)
                  {
                  if (pan==0) for (i=1;i<outpcnt-9;i++) outbuf[i][isamp] = dif[i-1][isamp];
                  outbuf[outpcnt-9][isamp] = imtot[isamp];
                  outbuf[outpcnt-8][isamp] = gratio[0][1];
                  outbuf[outpcnt-7][isamp] = gratio[0][2]; this section obsolete
                  outbuf[outpcnt-6][isamp] = gratio[0][3];
                  outbuf[outpcnt-5][isamp] = gratio[0][4];
                  outbuf[outpcnt-4][isamp] = gratio[0][5];
                  outbuf[outpcnt-3][isamp] = gratio[0][6];
                  outbuf[outpcnt-2][isamp] = gratio[0][7];
                  outbuf[outpcnt-1][isamp] = gratio[1][2];
                  }*/
               
               /* unified class detector  */
               
               l1dist = 0.0;
               l2dist = 0.0;
               linfdist = 0.0;
               outbuf[0][isamp] = 0;
               dblmax = 9999999.0;
               for (iclass=1;iclass<=maxclass;iclass++)
                  {
                  fdthresh = 0.01*(float)dthresh[iclass-1];
                  if (difthr[iclass-1]==0) zmabend("need to provide thresh for all categories");
                  diftest = (imtot[isamp]>difthr[iclass-1])&&(inbuf[oshade][isamp]>gthresh);
                  switch (iclass)
                     {
                     /* place for specific band requirements */
                     case 1: bndtest = inbuf[8-opan][isamp]>0.0&&inbuf[8-opan][isamp]<32767.0&&
                                       inbuf[13-opan][isamp]>0.0&&inbuf[13-opan][isamp]<32767.0;
                             break;
                     case 2: bndtest = 1;
                             break;
                     default: bndtest = 1;
                     }
                  icount = 0.0;
                  l1dist = 0.0;
                  linfdist = 0.0;
                  for (ix=0;ix<clen;ix++)
                     {
                     /* variable dimension rules out l2 norm, using modified l2 norm */
                     if (ibprod[ix]!=iclass) continue;
                     icount += 1.0;
                     sigdist =
                        fabs(gratio[ibrat1[ix]-1][ibrat2[ix]-1]-ibmean[ix])/max(ibsigma[ix],minsigma);
                     l1dist += sigdist;
                     l2dist += sigdist*sigdist;
                     if (sigdist>linfdist) linfdist = sigdist;
                     /* for looking at pixels */
                     if (iline==pixdbg[0]&&isamp==pixdbg[1])
                     {printf("A####ix %d\n",ix);
                     printf("A####ibrat1[ix],ibrat2[ix] %d %d\n",ibrat1[ix],ibrat2[ix]);
                     printf("A####ibmean[ix],ibsigma[ix] %f %f\n",ibmean[ix],ibsigma[ix]);
                     printf("A####gratio[ibrat1[ix]-1][ibrat2[ix]-1] %f\n",
                                  gratio[ibrat1[ix]-1][ibrat2[ix]-1]);
                     printf("A####sigdist %f\n",sigdist);
                     printf("A####l1dist,l2dist,linfdist %f %f %f\n",l1dist,l2dist,linfdist);}
                     }
                  l1dist = l1dist/icount;
                  l2dist = sqrt(l2dist/icount);
                  if (dnorm==1) sdist = l1dist;
                  else if (dnorm==2) sdist = l2dist;
                  else sdist = linfdist;
                  rattest = (sdist<fdthresh);
                  if (diftest&&(bndtest||ratonly)&&rattest)
                     {
                     prevclass = outbuf[0][isamp];
                     /* for looking at pixels */
                     if (iline==pixdbg[0]&&isamp==pixdbg[1])
                     {printf("#####iline,isamp,iclass,prevclass %d %d %d %d\n",
                                   iline,isamp,iclass,prevclass);
                     printf("#####dnorm,diftest,rattest %d %d %d\n",dnorm,diftest,rattest);
                     printf("#####l1dist,l2dist,linfdist %f %f %f\n",l1dist,l2dist,linfdist);
                     printf("#####icount %f\n",icount);}
                     
                     if ((prevclass==0)||(priority[iclass-1]<priority[prevclass-1]))
                        {
                        outbuf[0][isamp] = iclass;
                        dblmax = sdist;
                        }
                     else if ((priority[iclass-1]==priority[prevclass-1])&&(sdist<dblmax))
                        {
                        outbuf[0][isamp] = iclass;
                        dblmax = sdist;
                        }
                     }
                  } /* end of class detector */
                
               } /* end isamp loop */
         break;
         }
      }
      for (i=0;i<outpcnt;i++)
         zvwrit(o_unit[i],outbuf[i],"LINE",iline+1,"SAMP",1,"NSAMPS",ns, NULL);
      }
   
   for (i=0;i<inpimcnt;i++) zvclose(i_unit[i], NULL);
   for (i=0;i<outpcnt;i++) zvclose(o_unit[i], NULL);
   return;
}
