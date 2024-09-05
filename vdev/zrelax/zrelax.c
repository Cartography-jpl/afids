#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoVicarProtos.h"

/************************************************************************/
/* program zrelax                                                      */
/************************************************************************/
/*  03-08 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

void main44(void)
{
   int i,j,unit,ibis,status,clen,cols[10],colcount,coldef,val;
   int niter,nsum,ibig,pval,pvaltop;
   int *indx,*nbrpath,*nbrrow,*nbrix;
   double *zframe,*znbr,*deltaz,*wtavgz,*curwtavgz,*curzframe,*curznbr;
   double *csum,*corr,sum,val0,relaxconst,lastsum;   
   
   zifmessage("zrelax version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   zvparm("cols",cols,&colcount,&coldef,10,0);
   relaxconst = 1.0;
   /*niter = 1000;fast enuf, but had drift downward, unknown why*/
   niter = 25;
    
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&indx,clen,4);
   mz_alloc1((unsigned char **)&nbrpath,clen,4);
   mz_alloc1((unsigned char **)&nbrrow,clen,4);
   mz_alloc1((unsigned char **)&nbrix,clen,4);
   
   mz_alloc1((unsigned char **)&zframe,clen,8);
   mz_alloc1((unsigned char **)&znbr,clen,8);
   mz_alloc1((unsigned char **)&deltaz,clen,8);
   mz_alloc1((unsigned char **)&wtavgz,clen,8);
   mz_alloc1((unsigned char **)&curwtavgz,clen,8);
   mz_alloc1((unsigned char **)&curzframe,clen,8);
   mz_alloc1((unsigned char **)&curznbr,clen,8);
   mz_alloc1((unsigned char **)&csum,clen,8);
   mz_alloc1((unsigned char **)&corr,clen,8);
   
   status = IBISColumnSet(ibis,"U_FORMAT","FULL",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) indx,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) zframe,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","FULL",cols[2]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) nbrpath,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","FULL",cols[3]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) nbrrow,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[4]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) znbr,cols[4],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[5]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) deltaz,cols[5],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[6]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) wtavgz,cols[6],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* initial values */
   
   for (i=0;i<clen;i++)
      {
      curwtavgz[i] = wtavgz[i];
      curzframe[i] = zframe[i];
      curznbr[i] = znbr[i];
      }
   
   for (i=0;i<clen;i++)
      for (j=0;j<clen;j++)
         if (indx[i]==(nbrpath[j]*100+nbrrow[j])&&
             indx[j]==(nbrpath[i]*100+nbrrow[i])) nbrix[i] = j;
   
   /* relaxation scheme, the n-squared alg below could be linear */
   
   lastsum = 999999999.0;
   for (ibig=0;ibig<niter;ibig++)
   {
   
   pvaltop = indx[clen-1];
   for (pval=1;pval<=pvaltop;pval++)
      {
      sum = 0.0; nsum = 0;
      for (i=0;i<clen;i++)
         {
         val = indx[i];
         val0 = curznbr[i]-curzframe[i];
         if (val==pval)
           {
           sum += val0;
           nsum++;
           }
         if (val>pval) break;
         }
      for (i=0;i<clen;i++)
         {
         val = indx[i];
         if (val==pval) csum[i] = sum/((double)nsum*2.0*relaxconst);
         if (val>pval) break;
         }
      }
   
   for (i=0;i<clen;i++)
      {
      curwtavgz[i] += csum[i];
      curzframe[i] += csum[i];
      curznbr[nbrix[i]] += csum[i];
      }
   
   sum = 0.0;
   for (i=0;i<clen;i++) sum += fabs(curzframe[i]-curznbr[i]);
   printf("iteration sum = %f\n",sum);
   if (sum<0.001) break;
   if (sum>lastsum) break;
   lastsum = sum;
   }
   
   /* Output desired columns to the ibis interface file */
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[7]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) curwtavgz,cols[7],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
     
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[8]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) curzframe,cols[8],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[9]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) curznbr,cols[9],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
