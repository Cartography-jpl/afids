#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "carto/cartoVicarProtos.h"
#include "carto/cartoMemUtils.h"
#include "carto/cartoTaeUtils.h"

/*  image copy   A. Zobrist    11/28/89   */

#ifndef MIN
#define MIN(a,b)        (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b)        (((a)>(b))?(a):(b))
#endif


#define jsize 800000
#define jsize1 jsize+1
#define lgstint jsize

void main44(void)
{
   int  zedge,thresh;
   int coin[jsize1],join[jsize1];
   unsigned char bjoin[jsize1];

   int **lines,*buffer;
   unsigned char *bbuffer,*bfinal;
   int i_unit,w_unit,o_unit,b_unit,status,bufsiz,bufval;
   int lnl,lns,ns1,sl,ss,nl,ns,dummy;
   int bordx,i,j,ix,new,dcntr,lft,abv,sizptr,newcntr,p,startcmp;
   int nrg,q,erase,pt,cntr,nsbig,newcry;
   int pprev,pline,nlsc,nssc,bset,bnl,bns;
   
   zifmessage("concompi version Thu Jan  3 2008");
   
   /* open files */

   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT",
      "SA","U_FORMAT","FULL", NULL);
   zvget(i_unit,"NL",&lnl,"NS",&lns,"PIX_SIZE",&sizptr, NULL);
      
   status = zvunit(&b_unit,"INP",2, NULL);
   status = zvopen(b_unit,"OPEN_ACT","SA","IO_ACT",
      "SA","U_FORMAT","BYTE", NULL);
   zvget(b_unit,"NL",&bnl,"NS",&bns, NULL);
   if (bnl!=lnl||bns!=lns) zmabend("input images must be same (nl,ns)");
      
   zvp("STARTCMP",&startcmp,&dummy);
   
   zvp("SL",&sl,&dummy);
   zvp("SS",&ss,&dummy);
   zvp("NL",&nl,&dummy);
   zvp("NS",&ns,&dummy);
   if (nl<1) nl = lnl;
   if (ns<1) ns = lns;
   if (sl<1) zmabend("parameter sl < 1");
   if (ss<1) zmabend("parameter ss < 1");
   if (nl>lnl) zmabend("parameter nl larger than image");
   if (ns>lns) zmabend("parameter ns larger than image");
   zvp("THRESH",&thresh,&dummy);
   zedge = (zvptst("zedge"));
   
   status=zvunit(&w_unit,"OUT",2, NULL);
   status=zvopen(w_unit,"U_NL",nl,"U_NS",ns,"O_FORMAT","FULL",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   bufsiz = MAX(ns+2,jsize);
   mz_alloc1((unsigned char **)&buffer,bufsiz,4);
   mz_alloc1((unsigned char **)&bbuffer,bufsiz,1);
   mz_alloc2((unsigned char ***)&lines,2,bufsiz,4);

   /* set up for scanning loop */

   ns1 = ns+1;
   nsbig = ns+2;
   bordx = 0; nlsc = nl; nssc = ns1;
   if (zedge) { bordx = 1; nlsc++; nssc++; }
   new = bordx;
   for (j=0;j<2;j++) for (i=0;i<nsbig;i++) lines[j][i] = bordx;
   for (i=1;i<=jsize;i++) {coin[i] = 0; join[i] = i; bjoin[i] = 0;}
   
   /* scanning loop, each pixel set according to left and above nbrs */
   /* must do one extra row and column for zedge parameter */

   pprev = 0; pline = 1;
   for (ix=0;ix<nlsc;ix++)
      {
      if (ix<nl)
         {
	 status = zvread(i_unit,&lines[pline][1],"LINE",ix+sl,
            "SAMP",ss,"NSAMPS",ns, NULL);
         status = zvread(b_unit,bbuffer,"LINE",ix+sl,
            "SAMP",ss,"NSAMPS",ns, NULL);
         }
      else
         for (i=0;i<nsbig;i++) { lines[pline][i] = bordx; bbuffer[i] = 0;}
      newcry = TRUE; cntr = 0;
      for (i=1;i<nssc;i++)
	 {
	 bset = bbuffer[i-1]!=0;
	 if (lines[pline][i]==0)
	    {
	    dcntr = i-cntr-1; cntr = i; if (dcntr<=0) continue;
	    lft = lines[pline][i-1]; if (lft==0) continue;
	    newcntr = dcntr+coin[lft];
	    coin[lft] = MIN(lgstint,newcntr); continue;
	    }
	 lft = lines[pline][i-1]; abv = lines[pprev][i];
	 if (abv==lft)
	    {
	    if (abv>0) {lines[pline][i] = abv; if (bset) bjoin[abv] = 1; continue;}
	    new = new+1;
	    if (new>jsize)
	       {
	       printf("line %d nregion %d\n",ix,new);
	       zmabend("too many regions");
	       }
	    newcry = FALSE; lines[pline][i] = new; if (bset) bjoin[new] = 1; continue;
	    }
	 if (abv==0) {lines[pline][i] = lft; if (bset) bjoin[lft] = 1; continue;}
	 if (lft==0) {lines[pline][i] = abv; if (bset) bjoin[abv] = 1; continue;}
	 if ((lft==new)&&!newcry)
	    {
	    erase = i;
	    do {lines[pline][erase] = abv; erase = erase-1;}
	       while (lines[pline][erase]==new);
	    if (bset) {bjoin[new] = 0; bjoin[abv] = 1;}
	    new = new-1; newcry = TRUE; continue;
	    }
	 lines[pline][i] = lft; p = lft;
	 do {p = join[p]; if (p==abv) break;}
	    while (join[p]!=lft);
	 if (p!=abv)
	    {
	    join[p] = join[abv];
	    join[abv] = lft;
	    }
	 }
      if (ix<nl) zvwrit(w_unit,&lines[pline][1],"LINE",ix+1,
         "SAMP",1,"NSAMPS",ns, NULL);
      i = pprev; pprev = pline; pline = i;
      }

   /* process the join list to equivalence each connected component
      to the smallest available component number.  small regions
      are also eliminated here. */
   
   mz_alloc1((unsigned char **)&bfinal,new+1,1);
   for (i=0;i<new+1;i++) bfinal[i] = 0;
   
   nrg = 0; if (zedge) nrg = -1;
   for (ix=1;ix<=new;ix++) {
      if ((join[ix]<ix)|(join[ix]==0)) continue;
      nrg = nrg+1; p = ix; pt = 0; dcntr = 0;
      do { pt = pt+1; buffer[pt] = p; dcntr = dcntr+coin[p];
	       q = join[p]; join[p] = nrg; if (bjoin[p]!=0) bfinal[nrg] = 1; p = q; }
	       while (p!=ix);
      if (dcntr>=thresh) continue;
      nrg = nrg-1;
      for (i=1;i<=pt;i++) {p = buffer[i]; join[p] = 0; }
   }
   printf("%d preliminary, %d final regions\n",new,nrg);
      
   /* use join to transform work image to the final filled image */

   zvclose(w_unit, NULL);
   status = zvopen(w_unit,"OPEN_ACT","SA","IO_ACT",
      "SA","U_FORMAT","FULL", NULL);

   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,"O_FORMAT","FULL",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);

   startcmp--;
   join[0] = -startcmp;
   for (ix=0;ix<nl;ix++)
      {
      zvread(w_unit,buffer,"LINE",ix+1,"SAMP",1,"NSAMPS",ns, NULL);
      for (i=0;i<ns;i++)
         {
         bufval = join[buffer[i]];
         if (bufval==0) buffer[i] = 0;
         else
            {
            if (bfinal[bufval]==0) buffer[i] = 0;
            else buffer[i] = bufval+startcmp;
            }
         }
      zvwrit(o_unit,buffer,"LINE",ix+1,"SAMP",1,"NSAMPS",ns, NULL);
      }

   mq_out_int("LASTCMP",nrg+startcmp);

   zvclose(i_unit, NULL);
   zvclose(w_unit, NULL);
   zvclose(o_unit, NULL);
   return;
}
